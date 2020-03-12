import "../lib/github.com/diku-dk/lys/lys"
import "futspace"
import "map_ops"
module input_handler = import "inputs"

type sized_state [h][w] =   
    {   
        cam : camera,
        lsc : landscape[h][w],
        height : i32,
        width : i32,
        inputs : input_handler.input_states,
        random: f32
    }
    
type~ state = sized_state [][]

let shape [n][m] 't (_: [n][m]t) = (n,m)

let init [h][w] (seed: u32) (color_map: [h][w]argb.colour) (height_map: [h][w]argb.colour): state =
    let init_camera = { x = 512f32,
                        y = 800f32,
                        height = 78f32, --camera height above ground.. does not work as intended due to PNG readouts resulting in huge values (line 83).
                        angle = 0f32, --angle of the camera around the y-axis.
                        horizon = 100f32, --emulates camera rotation around the x-axis.
                        distance = 800f32,
                        magic_number = 20000f32,
                        invz_param1 = 1f32,
                        invz_param2 = 240f32 } --max render distance.

    let init_landscape = {  width = w,
                            height = h,
                            altitude = height_map,
                            color =  color_map,
                            sky_color = 0xFF9090e0}
    in
    {   cam = init_camera,
        lsc = init_landscape,
        height = 4096,
        width = 4096,
        inputs = input_handler.init,
        random = 1.0f32
    }

let resize (height: i32) (width: i32) (s: state) =
    s   with height = height
        with width = width

let grab_mouse = false
let mouse _ _ _ s = s
let wheel _ _ s = s

let process_inputs (s: state) : state =
    s   with cam.x = 
            (if s.inputs.w == 1 then (s.cam.x - 2*(f32.sin s.cam.angle)) 
            else if s.inputs.s == 1 then (s.cam.x + 2*(f32.sin s.cam.angle)) 
            else s.cam.x)
        with cam.y = 
            (if s.inputs.w == 1 then (s.cam.y - 2*(f32.cos s.cam.angle)) 
            else if s.inputs.s == 1 then (s.cam.y + nd_y2*(f32.cos s.cam.angle)) 
            else s.cam.y)
        with cam.angle =
            (if s.inputs.d == 1 then s.cam.angle - 0.03
            else if s.inputs.a == 1 then s.cam.angle + 0.03
            else s.cam.angle)
        with cam.horizon =
            (if s.inputs.e == 1 then s.cam.horizon - 20
            else if s.inputs.q == 1 then s.cam.horizon + 20
            else s.cam.horizon)
        with cam.height =
            (if s.inputs.r == 1 then s.cam.height - 20
            else if s.inputs.f == 1 then s.cam.height + 20
            else s.cam.height)
        with cam.distance =
            (if s.inputs.arrowup == 1 then s.cam.distance + 100
            else if s.inputs.arrowdown == 1 then s.cam.distance - 100
            else s.cam.distance)
        with cam.magic_number =
            (if s.inputs.o == 1 then s.cam.magic_number + 1000
            else if s.inputs.l == 1 then s.cam.magic_number - 1000
            else s.cam.magic_number)
        with cam.invz_param1 =
            (if s.inputs.u == 1 then s.cam.invz_param1 + 0.1
            else if s.inputs.j == 1 then s.cam.invz_param1 - 0.1
            else s.cam.invz_param1)
        with cam.invz_param2 =
            (if s.inputs.i == 1 then s.cam.invz_param2 + 2
            else if s.inputs.k == 1 then s.cam.invz_param2 - 2
            else s.cam.invz_param2)

let step (s: state) : state =
    process_inputs (s with random = s.random + 1)

let event (e: event) (s: state) =
    match e
    case #keydown {key} -> s with inputs = (input_handler.keydown key s.inputs)
    case #keyup {key} -> s with inputs = (input_handler.keyup key s.inputs)
    case #step _td -> step s
    case _ -> s

let render (s: state) =
    let (h,w) = shape s.lsc.color
    let s_prime = s :> sized_state [h][w]
    let img = render s_prime.cam s_prime.lsc s_prime.height s_prime.width
    in img

let text_content (s: state) =
    (s.cam.x, s.cam.y, s.cam.angle, s.cam.height, s.cam.horizon, s.cam.distance, s.cam.magic_number, s.cam.invz_param1, s.cam.invz_param2)

let update_map [h][w] (color_map: [h][w]argb.colour) (height_map: [h][w]argb.colour) (s: state) : state =
    s  with lsc.color = color_map
        with lsc.altitude = height_map

e