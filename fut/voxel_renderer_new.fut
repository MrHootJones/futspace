import "../lib/github.com/athas/matte/colour"
import "effects"

type camera = { x : f32, 
                y : f32, 
                height : f32, 
                angle : f32, 
                horizon : f32, 
                distance : f32,
                fov : f32}

type landscape [q][r] = { width : i32,
                       height : i32,
                       altitude : [q][r]i32,
                       color : [q][r]i32,
                       shadowed_color : [q][r]i32,
                       sky_color : i32 }
--this record definition is simply for convenience, i.e. to encapsulate the data returned by get_h_line function below. 
--The data encapsulated is simply the startpoint of the given horizontal line and the size of its segments. we do not need any more for our purposes
type line = { x_0 : f32, 
              y_0 : f32, 
              dx : f32, 
              dy : f32 } 

--calculates an arithmetic series of values that represent depth-layers in the rendering algorithm.
let get_zs (delta: f32)(d : f32) (z_0 : f32) : []f32 =
  let sqrt =  f32.sqrt ((delta-2*z_0)**2 + 8*delta*d)
  let num = sqrt - 2*z_0 + delta
  let div = 2*delta
  let n = i32.f32 (num / div)
  let is = map (\i -> f32.i32 i) (1...n)
  in map (\i -> (i/2) * (2*z_0 + (i-1) * delta)) is

--attempt at some type of LOD system by keeping the amount of depth samples fixed, but manipulating the distance between each sample instead.
let get_zs2 (c : f32) (z: f32) : []f32 =
    let amount_samples = 800
    in map (\i -> ((f32.i32 i)/(c))**2.0) (0...amount_samples)

--calculates the endpoints of a line at depth z from the leftmost coordinate in the field of view
--to the rightmost coordinate in the field of view. field of view is hardcoded at 90 degs.
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

--calculates a segment of the line calculated in get_h_line at pixel i in the range 0..w
let get_segment_point (l : line) (j : i32) : (f32, f32) =
  let x = (l.x_0 + (f32.i32 j) * l.dx)
  let y = (l.y_0 + (f32.i32 j) * l.dy)
  in (x, y)

let filter_pred (color: i32, index: i32) : bool =
  if color == 0 then false else true

let squares (x: f32) (y: f32) : (i32, f32) =
    let x = x % 1024
    let y = y % 1024
    in
    if x > 256 && f32.abs x < 768 && f32.abs y > 256 && f32.abs y < 768 then
        (argb.mix 1.0 argb.blue (f32.abs x/f32.abs y) argb.red, 200)
    else
        (argb.black, 0)

let expo (x: f32) (y: f32) : (i32, f32) =
    let x = x % 1024
    let y = y % 1024
    let output = (x**2+y**2)
    in
    if x > 0 && x < 1024 && y > 0 && y < 1024 then
        if (i32.f32 x) % 128 == 0 || (i32.f32 y) % 128 == 0 then
            (argb.black, output/1000)
        else
        (argb.mix 1.0 argb.white ((x + y) / 1024) argb.black, output/1000)
    else
    (argb.black, 0)

let expo2 (x: f32) (y: f32) : (i32, f32) =
    let output = (x**2-y**2)/1024
    in
    if x > -1024 && x < 1024 && y > -1024 && y < 1024 then
        if (i32.f32 x) % 128 == 0 || (i32.f32 y) % 128 == 0 then
            (argb.black, output)
        else
        (argb.white, output)
    else
    (argb.black, 0)

let wavy (x: f32) (y: f32) : (i32, f32) =
    let output = (f32.cos ((x*y)/2048.0)) * 100.0
    let output = 500 + (f32.cos (x/512) * f32.sin (y/512)) * 500.0
    in
    (argb.scale argb.white (output/500.0), output)
    

    
--Work = O(width * height)
-- [r] is size annotation denoting height of color and altitude in landscape record. [s] likewise denotes the width of these. 
-- h (screen height) and w (screen width) are variables which double as size annotations denoting size of final output map/screen buffer.
let render [q][r] (cam: camera) (lsc : landscape [q][r]) (l : i32) (m: i32) : [l][m]i32 =
    unsafe
    let z_0 = 0.0
    let delta = 0.001

    --Work = O(depth)
    --Span = O(1)
    let zs = get_zs delta cam.distance z_0
    --let zs = get_zs2 c.distance z_0

    --len(zs) * w array of color/height tuples. 
    --Currently does not compute expected results. Height values from lsc.altitude for some reason need to be inverted,
    --and the inverted values end up being too big, resulting in the camera height parameter not doing anything.
    --Therefore all pictures tend to have a birds' eye view at the moment. :-)
    
    --Work = O(depth*width)
    --Span = O(1)
    let render_map (x : f32) (y : f32) : (i32, f32) =
        --let color = lsc.color[(i32.f32 y)%1024, (i32.f32 x)%1024]
        --let height = f32.i32 lsc.altitude[(i32.f32 y)%1024, (i32.f32 x)%1024]
        let floor_x = f32.floor x
        let ceil_x = f32.ceil x
        let floor_y = f32.floor y
        let ceil_y = f32.ceil y
        --let x_interpolated = ((ceil_x - x)*(f32.i32 lsc.altitude[(i32.f32 y)%r,(i32.f32 floor_x)%s]) + 
        --            (x - floor_x)*(f32.i32 lsc.altitude[(i32.f32 y)%r,(i32.f32 ceil_x)%s]))
        --let y_interpolated = ((ceil_y - y)*(f32.i32 lsc.altitude[(i32.f32 floor_y)%r,(i32.f32 x)%s]) + 
        --            (y - floor_y)*(f32.i32 lsc.altitude[(i32.f32 ceil_y)%r,(i32.f32 x)%s]))
        --let x_color_interp =
        --    argb.mix (ceil_x - x) lsc.color[(i32.f32 y)%r,(i32.f32 floor_x)%s] (x - floor_x) lsc.color[(i32.f32 y)%r,(i32.f32 ceil_x)%s]
        --let y_color_interp =
        --    argb.mix (ceil_y - y) lsc.color[(i32.f32 floor_y)%r,(i32.f32 x)%s] (y - floor_y) lsc.color[(i32.f32 ceil_y)%r,(i32.f32 x)%s]
        --let color = argb.mix 0.5 x_color_interp 0.5 y_color_interp
        --let height = ((x_interpolated + y_interpolated) / 2.0)
        let x_interpolated1 = (ceil_x - x) * f32.i32 lsc.altitude[(i32.f32 floor_y)%q,(i32.f32 floor_x)%r]
                           + (x - floor_x) * f32.i32 lsc.altitude[(i32.f32 floor_y)%q,(i32.f32 ceil_x)%r]
        let x_interpolated2 = (ceil_x - x) * f32.i32 lsc.altitude[(i32.f32 ceil_y)%q,(i32.f32 floor_x)%r]
                           + (x - floor_x) * f32.i32 lsc.altitude[(i32.f32 ceil_y)%q,(i32.f32 ceil_x)%r]
        let interpolated = (ceil_y - y) * x_interpolated1 + (y - floor_y) * x_interpolated2
        let height = interpolated
        let x_interpolated1 = argb.mix (ceil_x - x) lsc.color[(i32.f32 floor_y)%q,(i32.f32 floor_x)%r] (x - floor_x) lsc.color[(i32.f32 floor_y)%q,(i32.f32 ceil_x)%r]
        let x_interpolated2 = argb.mix (ceil_x - x) lsc.color[(i32.f32 ceil_y)%q,(i32.f32 floor_x)%r] (x - floor_x) lsc.color[(i32.f32 ceil_y)%q,(i32.f32 ceil_x)%r]
        let color = argb.mix (ceil_y - y) x_interpolated1 (y - floor_y) x_interpolated2
        in
        (color, height)

    
    let color_height_pairs = 
        map (\z -> 
            let h_line = get_h_line z cam m
            let inv_z = (1.0 / z) * f32.i32 (m / 2)
            in 
            map (\j -> 
                let (x, y) = get_segment_point h_line j
                let (seg_point_color, seg_point_height) = render_map x y
                let height_diff = cam.height - seg_point_height
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
        -- h * w 'screen buffer'
        -- Work = O(width * depth + width * height)
        -- Span = O(lg(depth) + lg(height))
    let rendered_frame = 
        map (\m -> 
            let col_occluded = scan (occlude) (0, l) m
            let (cs, hs) = unzip col_occluded
            let screen_col = scatter (replicate l lsc.sky_color) hs cs
            in scan (fill) lsc.sky_color screen_col
            ) (transpose color_height_pairs)
        --finally tranpose rendered image from w*h to h*w

        --Work = O(width * height)
        --Span = O(1)
        in transpose rendered_frame

let filter_pred2 (height: i32) : bool =
    if height == -1 then false else true

let redstuff (height1: i32, dist1: i32) (height2: i32, dist2: i32) : (i32, i32)=
    if dist1 < dist2 then (height1, dist1) else (height2, dist2)

--very slow 'path tracing' algorithm. calculate intersections of rays from camera to terrain iteratively
let render2 [r][s] (c: camera) (lsc : landscape [r][s]) (h : i32) (w: i32) : [h][w]i32 =
    unsafe
    let heightmap = lsc.altitude
    let colormap = lsc.color
    let render_map (x : f32) (y : f32) : (i32, f32) =
        --let color = lsc.color[(i32.f32 y)%1024, (i32.f32 x)%1024]
        --let height = f32.i32 lsc.altitude[(i32.f32 y)%1024, (i32.f32 x)%1024]
        let floor_x = f32.floor x
        let ceil_x = f32.ceil x
        let floor_y = f32.floor y
        let ceil_y = f32.ceil y
        --let x_interpolated = ((ceil_x - x)*(f32.i32 lsc.altitude[(i32.f32 y)%r,(i32.f32 floor_x)%s]) + 
        --            (x - floor_x)*(f32.i32 lsc.altitude[(i32.f32 y)%r,(i32.f32 ceil_x)%s]))
        --let y_interpolated = ((ceil_y - y)*(f32.i32 lsc.altitude[(i32.f32 floor_y)%r,(i32.f32 x)%s]) + 
        --            (y - floor_y)*(f32.i32 lsc.altitude[(i32.f32 ceil_y)%r,(i32.f32 x)%s]))
        --let x_color_interp =
        --    argb.mix (ceil_x - x) lsc.color[(i32.f32 y)%r,(i32.f32 floor_x)%s] (x - floor_x) lsc.color[(i32.f32 y)%r,(i32.f32 ceil_x)%s]
        --let y_color_interp =
        --    argb.mix (ceil_y - y) lsc.color[(i32.f32 floor_y)%r,(i32.f32 x)%s] (y - floor_y) lsc.color[(i32.f32 ceil_y)%r,(i32.f32 x)%s]
        --let color = argb.mix 0.5 x_color_interp 0.5 y_color_interp
        --let height = ((x_interpolated + y_interpolated) / 2.0)
        let x_interpolated1 = (ceil_x - x) * f32.i32 lsc.altitude[(i32.f32 floor_y)%r,(i32.f32 floor_x)%s]
                           + (x - floor_x) * f32.i32 lsc.altitude[(i32.f32 floor_y)%r,(i32.f32 ceil_x)%s]
        let x_interpolated2 = (ceil_x - x) * f32.i32 lsc.altitude[(i32.f32 ceil_y)%r,(i32.f32 floor_x)%s]
                           + (x - floor_x) * f32.i32 lsc.altitude[(i32.f32 ceil_y)%r,(i32.f32 ceil_x)%s]
        let interpolated = (ceil_y - y) * x_interpolated1 + (y - floor_y) * x_interpolated2
        let height = interpolated
        let x_interpolated1 = argb.mix (ceil_x - x) lsc.color[(i32.f32 floor_y)%r,(i32.f32 floor_x)%s] (x - floor_x) lsc.color[(i32.f32 floor_y)%r,(i32.f32 ceil_x)%s]
        let x_interpolated2 = argb.mix (ceil_x - x) lsc.color[(i32.f32 ceil_y)%r,(i32.f32 floor_x)%s] (x - floor_x) lsc.color[(i32.f32 ceil_y)%r,(i32.f32 ceil_x)%s]
        let color = argb.mix (ceil_y - y) x_interpolated1 (y - floor_y) x_interpolated2
        in
        (color, height)

    let colors = map (\y ->
                    let vert_ang = c.horizon/50.0 + (f32.i32 y) / ((f32.i32 h)-0.0) * (1.5-1.0)
                    in
                    map (\x ->
                        let horz_ang = (f32.i32 x) / ((f32.i32 w)) * (2.5-1.0)
                        let hits = map (\dist -> 
                            --let height_test = heightmap[((i32.f32 c.y) + i32.f32 (f32.i32 dist*(f32.sin (c.angle+horz_ang))))%1024, ((i32.f32 c.x) + i32.f32 (f32.i32 dist*(f32.cos (c.angle+horz_ang))))%1024]
                            let (color, height) = render_map (c.x + f32.i32 dist*(f32.cos (c.angle+horz_ang))) (c.y + f32.i32 dist*(f32.sin (c.angle+horz_ang)))
                            --let color = colormap[((i32.f32 c.y) + i32.f32 (f32.i32 dist*(f32.sin (c.angle+horz_ang))))%1024, ((i32.f32 c.x) + i32.f32 (f32.i32 dist*(f32.cos (c.angle+horz_ang))))%1024]
                            in
                            if c.height - f32.i32 dist*vert_ang < height then (color, dist) else (0,i32.f32 (c.distance+1.0))
                        ) (0...(i32.f32 (c.distance/4.0)))
                        --let filtered_hits = filter filter_pred2 hits
                        --in head filtered_hits
                        in (reduce (redstuff) (0,100000) hits).0
                        ) (0..<w)
                    
                    ) (0..<h)
    in colors