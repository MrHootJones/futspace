
type camera = 
  { x : f32, 
    y : f32, 
    height : f32, 
    angle : f32, 
    horizon : f32, 
    distance : f32,
    fov : f32 }

type landscape [h][w] = 
  { width : i32,
    height : i32,
    altitude : [h][w]i32,
    color : [h][w]i32,
    sky_color : i32 }

type line = 
  { x_0 : f32, 
    y_0 : f32, 
    dx : f32, 
    dy : f32 } 

let get_zs (c : f32) (d: f32) (z_0 : f32) : []f32 =
  let sqrt =  f32.sqrt ((d-2*z_0)**2 + 8*d*c)
  let num = sqrt - 2*z_0 + d
  let div = 2*d
  let n = i32.f32 (num / div)
  let is = map (\i -> f32.i32 i) (1...n)
  in map (\i -> (i/2) * (2*z_0 + (i-1) * d)) is

let get_h_line (z : f32) (c : camera) (w : i32) : line =
  let sin_ang = f32.sin c.angle
  let cos_ang = f32.cos c.angle
  let view = c.fov
  let left_x = (- cos_ang  - sin_ang * view )*z
  let left_y = (sin_ang  - cos_ang * view)*z
  let right_x = (cos_ang  - sin_ang * view)*z
  let right_y = (- sin_ang  - cos_ang * view)*z

  let seg_dim_x = (right_x - left_x) / (f32.i32 w)
  let seg_dim_y = (right_y - left_y) / (f32.i32 w)

  let left_x = left_x + c.x
  let left_y = left_y + c.y

  in
  { x_0 = left_x, 
    y_0 = left_y, 
    dx = seg_dim_x, 
    dy = seg_dim_y }

let get_segment_start (l : line) (i : i32) : (i32, i32) =
  let x_j = i32.f32 (l.x_0 + (f32.i32 i) * l.dx)
  let y_j = i32.f32 (l.y_0 + (f32.i32 i) * l.dy)
  in (x_j, y_j)

let render [q][r] (c: camera) (lsc : landscape [q][r]) 
                  (h : i32) (w: i32) : [h][w]i32 =
  unsafe
  let z_0 = 1.0
  let d = 0.005
  let zs = get_zs c.distance d z_0

  let height_color_pairs = 
    map (\z -> 
         let h_line = get_h_line z c w
         let inv_z = (1.0 / z) * 240.0
         in 
         map (\i -> 
             let (x_j, y_j) = get_segment_start h_line i
             let seg_color = lsc.color[y_j%q,x_j%r]
             let seg_height = lsc.altitude[y_j%q,x_j%r]
             let height_diff = c.height - (f32.i32 seg_height)
             let relative_height = height_diff * inv_z + c.horizon
             let seg_abs_height = i32.max 0 (i32.f32 relative_height)
             in (seg_color, seg_abs_height)
             ) (iota w)
        ) zs

  let occlude (c_1 : i32, h_1 : i32 ) 
              (c_2 : i32, h_2 : i32 ) : (i32, i32) =
    if (h_1 <= h_2)
    then (c_1, h_1)
    else (c_2, h_2)

  let fill (c_1 : i32) (c_2 : i32) : i32 =
    if (c_2 == lsc.sky_color)
    then c_1
    else c_2 

  let rendered_image = 
    map (\i -> 
         let (cs, hs) = unzip (scan (occlude) (0, h) i)
         let v_line = scatter (replicate h lsc.sky_color) hs cs
         in scan (fill) lsc.sky_color v_line
        ) (transpose height_color_pairs)

  in transpose rendered_image

let main [h][w] (color_map : [h][w]i32 ) 
                (height_map: [h][w]i32) : [][]i32 =

  let init_camera = 
    { x = 512f32,
      y = 800f32,
      height = 78f32,
      angle = 0f32,
      horizon = 100f32,
      distance = 800f32,
      fov = 1f32 } 

  let init_landscape = 
    { width = w,
      height = h,
      altitude = height_map,
      color =  color_map,
      sky_color = 0xFF9090e0 }

  in render init_camera init_landscape 400 800