import "../lib/github.com/athas/matte/colour"
type camera = { x : f32, 
                y : f32, 
                height : f32, 
                angle : f32, 
                horizon : f32, 
                distance : f32,
                fov : f32}

type landscape [h][w] = { width : i32,
                       height : i32,
                       altitude : [h][w]i32,
                       color : [h][w]i32,
                       sky_color : i32 }
--this record definition is simply for convenience, i.e. to encapsulate the data returned by get_h_line function below. 
--The data encapsulated is simply the startpoint of the given horizontal line and the size of its segments. we do not need any more for our purposes
type line = { start_x : f32, 
              start_y : f32, 
              end_x : f32, 
              end_y : f32 } 

--calculates an arithmetic series of values that represent depth-layers in the rendering algorithm.
let get_zs (c : f32) (d: f32) (z_0 : f32) : []f32 =
    let sqrt =  f32.sqrt ((d-2*z_0)**2 + 8*d*c)
    let num = sqrt - 2*z_0 + d
    let div = 2*d
    let n = i32.f32 (f32.floor (num / div))
    let is = map (\i -> f32.i32 i) (1...n)
    in map (\i -> (i/2) * (2*z_0 + (i-1) * d)) is

--calculates the endpoints of a line at depth z from the leftmost coordinate in the field of view
--to the rightmost coordinate in the field of view. field of view is hardcoded at 90 degs.
let get_h_line (z : f32) (c : camera) (w : i32) : line =
    let sin_ang = f32.sin c.angle
    let cos_ang = f32.cos c.angle
    let view = c.fov
    let left_x = (- cos_ang  - sin_ang * view )*z
    let left_y = (sin_ang  - cos_ang * view)*z
    let right_x = (cos_ang  - sin_ang * view)*z
    let right_y = (- sin_ang  - cos_ang * view)*z
    
    let dx = (right_x - left_x) / (f32.i32 w)
    let dy = (right_y - left_y) / (f32.i32 w)
    
    let left_x = left_x + c.x
    let left_y = left_y + c.y
    in { start_x = left_x, 
         start_y = left_y, 
         end_x = dx, 
         end_y = dy }

--calculates a segment of the line calculated in get_h_line at pixel i in the range 0..w
let get_segment (l : line) (i : i32) : (i32, i32) =
    let left_x_int = i32.f32 (l.start_x + (f32.i32 i) * l.end_x)
    let left_y_int = i32.f32 (l.start_y + (f32.i32 i) * l.end_y)
    in (left_x_int, left_y_int)

--scan operator used to sort through depth-slices in order to determine voxel-column colors and heights.
let occlude (color1 : i32, height1 : i32 ) (color2 : i32, height2 : i32 ) : (i32, i32) =
    if (height1 <= height2)
    then (color1, height1)
    else (color2, height2)

--used in conjunction with scan at line 94 (let v_line_filled_no_sky) to fill color gaps in pixel-columns.
let fill_vline (color1 : i32) (color2 : i32) : i32 =
    if (color2 == 0)
    then color1
    else color2 
    
--Work = O(width * height)
-- [r] is size annotation denoting height of color and altitude in landscape record. [s] likewise denotes the width of these. 
-- h (screen height) and w (screen width) are variables which double as size annotations denoting size of final output map/screen buffer.
let render [r][s] (c: camera) (lsc : landscape [r][s]) (h : i32) (w: i32) : [h][w]i32 =
    unsafe
    let z_0 = 1.0
    let d = 0.001

    --Work = O(depth)
    --Span = O(1)
    let zs = get_zs c.distance d z_0

    --len(zs) * w array of color/height tuples. 
    --Currently does not compute expected results. Height values from lsc.altitude for some reason need to be inverted,
    --and the inverted values end up being too big, resulting in the camera height parameter not doing anything.
    --Therefore all pictures tend to have a birds' eye view at the moment. :-)
    
    --Work = O(depth*width)
    --Span = O(1)
    let height_color_map = map (\z -> 
                                 let h_line = get_h_line z c w
                                 let inv_z = (1.0 / z) * 240.0
                                 in map (\i -> 
                                          let (x, y) = get_segment h_line i
                                          let map_height = lsc.altitude[y%r,x%s]
                                          let height_diff = c.height - (f32.i32 map_height)
                                          let relative_height = height_diff * inv_z + c.horizon
                                          let abs_height = i32.max 0 (i32.f32 relative_height)
                                          in (lsc.color[y%r,x%s], abs_height)
                                        ) (iota w)
                                ) zs

    
    -- h * w 'screen buffer'
    -- Work = O(width * depth + width * height)
    -- Span = O(lg(depth) + lg(height))
    let rendered_image = map (\i -> 
                               -- Iterates over depth-slice at width i and calculates array of (height, color) tuples.
                               let (colors, heights) = unzip (scan (occlude) (0, h) i)
                               -- Projects a column of height and color tuples to an h-length array.
                               let v_line_incomplete = scatter (replicate h 0) heights colors
                               -- fill color gaps in v_line_incomplete. 
                               let v_line_filled_no_sky = scan (fill_vline) 0 (v_line_incomplete)
                               --Fill sky with sky color, as this is not covered by the previous operation.
                               in map (\col -> if (col == 0) then lsc.sky_color else col) v_line_filled_no_sky
                            ) (transpose height_color_map)
    --finally tranpose rendered image from w*h to h*w

    --Work = O(width * height)
    --Span = O(1)
    in transpose rendered_image