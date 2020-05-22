
-- benchmark program as a function of d, m and l respectively
-- ==
-- input @ d_10
-- input @ d_100
-- input @ d_1000
-- input @ d_10000
-- input @ d_100000
-- input @ d_500000
-- input @ d_1000000
-- input @ m_10
-- input @ m_100
-- input @ m_1000
-- input @ m_10000
-- input @ m_100000
-- input @ m_500000
-- input @ m_1000000
-- input @ l_10
-- input @ l_100
-- input @ l_1000
-- input @ l_10000
-- input @ l_100000
-- input @ l_500000
-- input @ l_1000000
type camera = 
  { x : f32, 
    y : f32, 
    height : f32, 
    angle : f32, 
    horizon : f32, 
    distance : f32,
    fov : f32 }

type landscape [q][r] = 
  { width : i32,
    height : i32,
    color : [q][r]i32,
    altitude : [q][r]i32,
    sky_color : i32 }

type line = 
  { x_0 : f32, 
    y_0 : f32, 
    dx : f32, 
    dy : f32 } 

let get_zs (delta: f32)(d : f32) (z_0 : f32) : []f32 =
  let sqrt =  f32.sqrt ((delta-2*z_0)**2 + 8*delta*d)
  let num = sqrt - 2*z_0 + delta
  let div = 2*delta
  let n = i32.f32 (num / div)
  let is = map (\i -> f32.i32 i) (1...n)
  in map (\i -> (i/2) * (2*z_0 + (i-1) * delta)) is

let get_h_line (z : f32) (cam : camera) (m : i32) : line =
  let sin_ang = f32.sin cam.angle
  let cos_ang = f32.cos cam.angle
  let view = cam.fov
  let left_x = (- cos_ang  - sin_ang * view )*z
  let left_y = (sin_ang  - cos_ang * view)*z
  let right_x = (cos_ang  - sin_ang * view)*z
  let right_y = (- sin_ang  - cos_ang * view)*z

  let seg_dim_x = (right_x - left_x) / (f32.i32 m)
  let seg_dim_y = (right_y - left_y) / (f32.i32 m)

  let left_x = left_x + cam.x
  let left_y = left_y + cam.y

  in
  { x_0 = left_x, 
    y_0 = left_y, 
    dx = seg_dim_x, 
    dy = seg_dim_y }

let get_segment_point (l : line) (j : i32) : (i32, i32) =
  let x = i32.f32 (l.x_0 + (f32.i32 j) * l.dx)
  let y = i32.f32 (l.y_0 + (f32.i32 j) * l.dy)
  in (x, y)

let render [q][r] (cam: camera) (lsc : landscape [q][r]) 
                  (l : i32) (m: i32) : [l][m]i32 =
  unsafe
  let z_0 = 1.0
  let delta = 0.005
  let zs = get_zs delta cam.distance z_0

  let color_height_pairs = 
    map (\z -> 
         let h_line = get_h_line z cam m
         let inv_z = (1.0 / z) * 240.0
         in 
         map (\j -> 
             let (x, y) = get_segment_point h_line j
             let seg_point_color = lsc.color[y%q,x%r]
             let seg_point_height = lsc.altitude[y%q,x%r]
             let height_diff = cam.height - (f32.i32 seg_point_height)
             let relative_height = height_diff * inv_z + cam.horizon
             let nonneg_height = i32.max 0 (i32.f32 relative_height)
             in (seg_point_color, nonneg_height)
             ) (iota m)
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

  let rendered_frame = 
    map (\j -> 
         let col_occluded = scan (occlude) (0, l) j
         let (cs, hs) = unzip col_occluded
         let init__col = replicate l lsc.sky_color
         let screen_col = scatter init__col hs cs
         in scan (fill) lsc.sky_color screen_col
        ) (transpose color_height_pairs)

  in transpose rendered_frame

let main [q][r] (color_map : [q][r]i32) 
                (height_map: [q][r]i32) : [][]i32 =

  let init_camera = 
    { x = 512f32,
      y = 800f32,
      height = 78f32,
      angle = 0f32,
      horizon = 100f32,
      distance = 800f32,
      fov = 1f32 } 

  let init_landscape = 
    { width = q,
      height = r,
      color =  color_map,
      altitude = height_map,
      sky_color = 0xFF9090e0 }

  in render init_camera init_landscape 400 800