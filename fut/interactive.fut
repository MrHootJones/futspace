import "../lib/github.com/diku-dk/lys/lys"
import "voxel_renderer"
import "effects"
module input_handler = import "interactive_input"

type sized_state [h][w][s] =   
    {   
        cam : camera,
        lsc : landscape[h][w],
        shadowmaps: [s][h][w]f32,
        height : i32,
        width : i32,
        inputs : input_handler.input_states,
        random: f32,
        sun_height: f32,
        i: i32
    }
    
type~ state = sized_state [][][]

let shape [n][m][k] 't (_: [n][m][k]t) = (n,m,k)

let init (seed: u32): state =
    let init_camera = { x = 430.98f32,
                        y = 818.6f32,
                        height = 58f32, --camera height above ground.. does not work as intended due to PNG readouts resulting in huge values (line 83).
                        angle = 2.2f32, --angle of the camera around the y-axis.
                        horizon = 200f32, --emulates camera rotation around the x-axis.
                        distance = 800f32, --max render distance.
                        fov = 1.2}

    let init_landscape = {  width = 0, --dummy values, as map loading actually happens when the update_map entrypoint is called from interactive.c
                            height = 0,
                            altitude = [[0],[0]],
                            color =  [[0],[0]],
                            sky_color = 0xFF9090e0}
    in
    {   cam = init_camera,
        lsc = init_landscape,
        shadowmaps = replicate 1 (replicate 1 (replicate 1 0.0)),
        height = 1024,
        width = 1024,
        inputs = input_handler.init,
        random = 1.0f32,
        sun_height = -0.1,
        i = 0
    }

let resize (height: i32) (width: i32) (s: state) =
    s   with height = height
        with width = width

let grab_mouse = false
let mouse _ _ _ s = s
let wheel _ _ s = s

let terrain_collision [r][s] (c: camera) (lsc: landscape[r][s]) : f32 =
    let x = i32.f32 c.x
    let y = i32.f32 c.y
    let terrain_height = f32.i32 lsc.altitude[y%r,x%s]
    in
    if c.height < terrain_height then terrain_height
    else c.height

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
            else if s.inputs.f == 1 then terrain_collision (s.cam with height = s.cam.height - 10) s.lsc
            else terrain_collision s.cam s.lsc)
        with cam.distance =
            (if s.inputs.arrowup == 1 then s.cam.distance + 100
            else if s.inputs.arrowdown == 1 && s.cam.distance > 100 then s.cam.distance - 100
            else s.cam.distance)
        with cam.fov =
            (if s.inputs.o == 1 then s.cam.fov + 0.1
            else if s.inputs.l == 1 then s.cam.fov - 0.1
            else s.cam.fov)
        with sun_height =
            (if s.inputs.u == 1 then s.sun_height + 0.02
            else if s.inputs.j == 1 then s.sun_height - 0.02
            else s.sun_height)
        with i =
            (if s.inputs.i == 1 then s.i + 1
            else if s.inputs.k == 1 then s.i - 1
            else s.i)

let step (s: state) : state =
    process_inputs (s with random = s.random + 0.004)

let event (e: event) (s: state) =
    match e
    case #keydown {key} -> s with inputs = (input_handler.keydown key s.inputs)
    case #keyup {key} -> s with inputs = (input_handler.keyup key s.inputs)
    case #step _td -> step s
    case _ -> s

let render (s: state) =
    --let (h,w,s) = shape s.shadowmaps
    --let s_prime = s :> sized_state [h][w][s]t
    let color_map = s.lsc.color
    let img = render s.cam (s.lsc with color = (generate_shadowmap_seq s.lsc.color s.lsc.altitude s.sun_height)) s.height s.width
    in img

let text_content (s: state) =
    (s.cam.x, s.cam.y, s.cam.angle, s.cam.height, s.cam.horizon, s.cam.distance, s.sun_height, s.cam.fov)

let update_map [h][w] (color_map: [h][w]argb.colour) (height_map: [h][w]argb.colour) (s: state) : state =
    let new_height_map = interpolate 2 (map (\row -> map (\elem -> elem & 0xFF) row ) height_map)
    let shadow_map_array = map (\scalar -> generate_shadowmap new_height_map (s.sun_height + (0.03 * f32.i32 scalar))) (1...1)
    let new_color_map = color_map
    in
    s  with lsc.color = new_color_map
        with lsc.altitude = new_height_map
        with lsc.height = h
        with lsc.width = w
        with shadowmaps = shadow_map_array