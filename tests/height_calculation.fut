type camera = { x : f32, 
                y : f32, 
                height : f32, 
                angle : f32, 
                horizon : f32, 
                distance : f32 }

type landscape [h][w] = { width : i32,
                       height : i32,
                       shift : i32,
                       altitude : [h][w]i32,
                       color : [h][w]i32,
                       sky_color : i32 }
--this record definition is simply for convenience, i.e. to encapsulate the data returned by get_h_line function below. the data encapsulated is simply the startpoint of the given horizontal line and the size of its segments. we do not need any more for our purposes
type line = { start_x : f32, 
              start_y : f32, 
              segment_width : f32, 
              segment_height : f32 } 

let get_zs (c : f32) (d: f32) (z_0 : f32) : []f32 =
    let sqrt =  f32.sqrt ((d-2*z_0)**2 + 8*d*c)
    let num = sqrt - 2*z_0 + d
    let div = 2*d
    let n = i32.f32 (f32.floor (num / div))
    let is = map (\i -> f32.i32 i) (1...n)
    in map (\i -> (i/2) * (2*z_0 + (i-1) * d)) is

let get_h_line (z : f32) (c : camera) (w : i32) : line =
    let sin_ang = f32.sin c.angle
    let cos_ang = f32.cos c.angle
    let left_x = - cos_ang * z - sin_ang * z
    let left_y = sin_ang * z - cos_ang * z
    let right_x = cos_ang * z - sin_ang * z
    let right_y = - sin_ang * z - cos_ang * z
    
    let dx = (right_x - left_x) / (f32.i32 w)
    let dy = (right_y - left_y) / (f32.i32 w)
    
    let left_x = left_x + c.x
    let left_y = left_y + c.y

    in { start_x = left_x, 
         start_y = left_y, 
         segment_width = dx, 
         segment_height = dy }

--as with the horizontal lines we only need the startpoint of a line segment
let get_segment (l : line) (i : i32) : (i32, i32) =
    let left_x_int = i32.f32 (l.start_x + (f32.i32 i) * l.segment_width)
    let left_y_int = i32.f32 (l.start_y + (f32.i32 i) * l.segment_height)
    in (left_x_int, left_y_int)

let reverse_bits (value : i32) : i32 =
    let step1 = (value & 0x000000FF) << 24
    let step2 = step1 | ((value & 0x0000FF00) << 8)
    let step3 = step2 | ((value & 0x00FF0000) >> 8)
    let step4 = step3 | ((value & 0xFF000000) >> 24)
    in step4

let main [h][w] (color_map : [h][w]i32 ) (height_map: [h][w]i32) : [][]i32 =
    let c = {   x = 512f32, 
                y = 800f32, 
                height = 78f32, 
                angle = 0f32, 
                horizon = 100f32,
                distance = 400f32 }

    let lsc = {  width = w,
                            height = h,
                            shift = 10i32,
                            altitude = height_map,
                            color =  color_map,
                            sky_color = 0xFF9090e0}
    let z_0 = 1.0
    let d = 0.005
    let zs = get_zs c.distance d z_0
    
    let height_color_map = map (\z -> 
                                let h_line = get_h_line z c w
                                let inv_z = 1000.0 / (z * 2.0)
                                --let inv_z = 1.0 / (z * 2.0)
                                in map (\i -> 
                                        let (x, y) = get_segment h_line i
                                        let mod = h * w
                                        --let offset = ((y & (lsc.width - 1)) << lsc.shift) + (x & (lsc.height - 1))
                                        --let relative_height = (c.height - (f32.i32 ( (lsc.altitude[y_mod, x_mod]) >> 16 ))) * inv_z + c.horizon
                                        let relative_height = (c.height - ( f32.i32 (lsc.altitude[y%mod, x%mod]))) * inv_z + c.horizon                                        in (i32.f32 relative_height))
                                        --let alt = lsc.altitude[y%mod, x%mod]
                                        --in alt)
                                    (iota w)
                            ) zs
    in height_color_map