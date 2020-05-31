import "../lib/github.com/diku-dk/lys/lys"
import "voxel_renderer"
import "effects"
import "render_functions"
module input_handler = import "interactive_input"

type render_mode = #math | #png

type sized_state [h][w] =   
    {   
        mode : render_mode,
        smoothing_mode : smoothing,
        cam : camera,
        lsc : landscape[h][w],
        height : i32,
        width : i32,
        inputs : input_handler.input_state,
        random: f32,
        sun_height: f32,
        sun_ang : f32,
        sun : [3]f32
    }
    
type~ state = sized_state [][]

let shape [n][m] 't (_: [n][m]t) = (n,m)

let init (seed: u32): state =
    let init_camera = { x = 0.98f32,
                        y = 0.6f32,
                        height = 58f32, --camera height above ground.. does not work as intended due to PNG readouts resulting in huge values (line 83).
                        angle = 2.2f32, --angle of the camera around the y-axis.
                        horizon = 200f32, --emulates camera rotation around the x-axis.
                        distance = 800f32, --max render distance.
                        fov = 1.2,
                        sky_color = 0}

    let init_landscape = {  width = 0, --dummy values, as map loading actually happens when the update_map entrypoint is called from interactive.c
                            height = 0,
                            altitude = [[0],[0]],
                            color =  [[0],[0]],
                            shadowed_color = [[0],[0]]
                            }
    in
    {   
        mode = #png,
        smoothing_mode = #off,
        cam = init_camera,
        lsc = init_landscape,
        height = 1024,
        width = 1024,
        inputs = input_handler.init,
        random = 1.0f32,
        sun_height = 0.1,
        sun_ang = 0.1,
        sun = [0, 1.0/(f32.sqrt 1), 0]--unit vector pointing up
    }

let resize (height: i32) (width: i32) (s: state) =
    s   with height = height
        with width = width

let grab_mouse = false
let mouse _ _ _ s = s
let wheel _ _ s = s

let terrain_collision (s : state) : state =
    match s.mode
    case #math -> 
        let terrain_height = squares s.cam.x s.cam.y
        in
        s with cam.height =
            if s.cam.height <= terrain_height then 
                terrain_height
            else 
                s.cam.height
    case #png ->
        let (h,w) = shape s.lsc.altitude
        let x = i32.f32 s.cam.x
        let y = i32.f32 s.cam.y
        let terrain_height = f32.i32 s.lsc.altitude[y%h,x%w]
        in
        s with cam.height =
            if s.cam.height <= terrain_height then 
                terrain_height
            else 
                s.cam.height

let process_inputs (s: state) : state =
    s   with cam.x = 
            (if s.inputs.w == 1 then (s.cam.x - 3*(f32.sin s.cam.angle)) 
            else if s.inputs.s == 1 then (s.cam.x + 3*(f32.sin s.cam.angle)) 
            else s.cam.x)
        with cam.y = 
            (if s.inputs.w == 1 then (s.cam.y - 3*(f32.cos s.cam.angle)) 
            else if s.inputs.s == 1 then (s.cam.y + 3*(f32.cos s.cam.angle)) 
            else s.cam.y)
        with cam.angle =
            (if s.inputs.d == 1 then s.cam.angle - 0.10
            else if s.inputs.a == 1 then s.cam.angle + 0.10
            else s.cam.angle)
        with cam.horizon =
            (if s.inputs.e == 1 then s.cam.horizon - 20
            else if s.inputs.q == 1 then s.cam.horizon + 20
            else s.cam.horizon)
        with cam.height =
            (if s.inputs.r == 1 then s.cam.height + 10
            else if s.inputs.f == 1 then s.cam.height - 10
            else s.cam.height)
        with cam.distance =
            (if s.inputs.arrowup == 1 then s.cam.distance + 30.0
            else if s.inputs.arrowdown == 1 then s.cam.distance - 30.0
            else s.cam.distance)
        with cam.fov =
            (if s.inputs.o == 1 then s.cam.fov + 0.1
            else if s.inputs.l == 1 then s.cam.fov - 0.1
            else s.cam.fov)
        with sun_height =
            (if s.inputs.u == 1 then s.sun_height + 0.005
            else if s.inputs.j == 1 then s.sun_height - 0.005
            else s.sun_height)
        with sun_ang =
            (if s.inputs.n == 1 then s.sun_ang + 0.005
            else if s.inputs.m == 1 then s.sun_ang - 0.005
            else s.sun_ang)
        with lsc.shadowed_color =
            (let shadowfun = generate_shadowmap_accumulated
            in
            if s.inputs.u == 1 then 
                (let partial_png_height = png_height s.lsc.altitude
                let partial_png_color = png_color s.lsc.color
                in shadowfun partial_png_color partial_png_height (vec3_rotate #y s.sun_ang (vec3_rotate #z s.sun_height s.sun)))
            else if s.inputs.j == 1 then 
                (let partial_png_height = png_height s.lsc.altitude
                let partial_png_color = png_color s.lsc.color
                in shadowfun partial_png_color partial_png_height (vec3_rotate #y s.sun_ang (vec3_rotate #z s.sun_height s.sun)))
            else if s.inputs.n == 1 then 
                (let partial_png_height = png_height s.lsc.altitude
                let partial_png_color = png_color s.lsc.color
                in shadowfun partial_png_color partial_png_height (vec3_rotate #y s.sun_ang (vec3_rotate #z s.sun_height s.sun)))
            else if s.inputs.m == 1 then 
                (let partial_png_height = png_height s.lsc.altitude
                let partial_png_color = png_color s.lsc.color
                in shadowfun partial_png_color partial_png_height (vec3_rotate #y s.sun_ang (vec3_rotate #z s.sun_height s.sun)))
            else s.lsc.shadowed_color)
        with mode =
            (if s.inputs.1 == 1 then
                match s.mode
                case #png -> #math
                case #math -> #png
            else
                s.mode)
        with smoothing_mode =
            (if s.inputs.2 == 1 then
                match s.smoothing_mode
                case #off -> #on
                case #on -> #off
            else
                s.smoothing_mode)

let step (s: state) : state =
    (process_inputs (s with random = s.random + 0.005
                        with cam.sky_color = argb.scale 0xFF9090e0 s.sun_height)) 
    |> terrain_collision

let event (e: event) (s: state) =
    match e
    case #keydown {key} -> s with inputs = (input_handler.keydown key s.inputs)
    case #keyup {key} -> s with inputs = (input_handler.keyup key s.inputs)
    case #step _td -> step s
    case _ -> s

let render (s: state) =
    match s.mode
    case #math ->
        let shadow_partial = png_color (generate_shadowmap_accumulated function_coloring expo2 (vec3_rotate #y s.sun_ang (vec3_rotate #z s.sun_height s.sun)))
        in
        render s.cam shadow_partial expo2 s.height s.width s.smoothing_mode
    case #png ->
        let partial_png_height = png_height_filtered s.lsc.altitude
        let partial_png_color = png_color_filtered s.lsc.shadowed_color
        let img = render s.cam partial_png_color partial_png_height s.height s.width s.smoothing_mode 
        in img--s.lsc.shadowed_color

let text_content (s: state) =
    (s.cam.x, s.cam.y, s.cam.angle, s.cam.height, s.cam.horizon, s.cam.distance, s.sun_height, s.sun_ang, s.cam.fov)

let update_map [h][w] (color_map: [h][w]argb.colour) (height_map: [h][w]argb.colour) (s: state) : state =
    let new_height_map = map (\row -> map (\elem -> elem & 0xFF) row ) height_map
    let new_color_map = color_map
    in
    s  with lsc.color = new_color_map
        with lsc.altitude = new_height_map
        with lsc.shadowed_color = (let partial_png_height = png_height new_height_map
                                    let partial_png_color = png_color new_color_map
                                    in generate_shadowmap_accumulated partial_png_color partial_png_height (vec3_rotate #y s.sun_ang (vec3_rotate #z s.sun_height s.sun)))
        with lsc.height = h
        with lsc.width = w