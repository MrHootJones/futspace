import "../lib/github.com/athas/matte/colour"

let interpolate [h][w] (pd: i32) (rendered_image: [h][w]i32) : [h][w]i32 =
    let m1 = 1.0
    let m2 = 1.0
    let smoothed_image = 
        map (\y ->
            map (\x -> 
                    let c = unsafe rendered_image[y, x]
                    let u = unsafe rendered_image[(y-pd)%h, x]
                    let d = unsafe rendered_image[(y+pd)%h, x]
                    let l = unsafe rendered_image[y, (x-pd)%h]
                    let r = unsafe rendered_image[y, (x+pd)%h]
                    let ul = unsafe rendered_image[(y-pd)%h, (x-pd)%h]
                    let ur = unsafe rendered_image[(y-pd)%h, (x+pd)%h]
                    let dl = unsafe rendered_image[(y+pd)%h, (x-pd)%h]
                    let dr = unsafe rendered_image[(y+pd)%h, (x+pd)%h]
                    in argb.mix m1 c m2 (argb.mix m1 u m1 (argb.mix m1 d m1 (argb.mix m1 r m1 (argb.mix m1 l m1 (argb.mix m1 ul m1 (argb.mix m1 ur m1 (argb.mix m1 dl m1 dr)))))))
                    ) (0..<w)
                        ) (0..<h)
    in smoothed_image

let modulate [h][w] (num: f32) (height_map: [h][w]i32) = 
    map (\heights -> 
        map2 (\height j -> 
            height + i32.f32 ((f32.sin num * (f32.i32 j)) * f32.i32 height)
            ) heights (iota w)
                ) height_map

--(very) slow but nice shadowmapping. debating on whether to blend the shadowmap with the colormap here or do it separately. right now we do it here
let generate_shadowmap [h][w] (height_map: [h][w]i32) (sun_dy: f32) : [h][w]f32=
    map (\height_row -> 
            map2 (\height x ->
                    --let longest_shadow = i32.f32 (255/sun_dy)
                    --let range_start = i32.min (i32.max 0 (x-(i32.abs longest_shadow))) 1023
                    let elements_before = height_row[0:x:1]
                    let conds = map2 (\elem idx -> if f32.i32 height + f32.i32 (x-idx) * sun_dy <= f32.i32 elem then 1.0 else 0.0) elements_before (0..<x)
                    let truthval = reduce (+) 0.0 conds
                    in truthval
                    --if truthval > 0.0 then
                    --    (argb.mix (0.8 * truthval) argb.black 0.8 color)
                    --else
                    --    color
                    ) height_row (0..<w)
            ) height_map

let blend_color_shadow [h][w] (color_map: [h][w]i32) (shadow_map: [h][w]f32) : [h][w]i32 =
    map2 (\colors shadows -> map2 (\color shadow -> (argb.mix (0.8 * shadow) argb.black 0.8 color)) colors shadows) color_map shadow_map

--heightmap axis-aligned shadows by scanning across the height and color maps and adjusting the colors of the colormap based on whether the height of the previous voxel
--intersects with a vector representing the sun. now rests here as a backup for inspiration for a potentially sequential implementation of shadowmapping in case we do not find a better way to do it in parallel.
--is called sequential as it only works on the futhark c compiler, as the shade function is not associative, leading to bugs.
let shade (color1: i32, height1: f32, sun_descent1: f32) (color2: i32, height2: f32, sun_descent2: f32) : (i32, f32, f32)=
    if height1 > height2 then
        --((argb.from_rgba 0.0 0.0 0.0 (f32.max 0 (f32.min 1 (f32.abs (height1 - height2))))), height1 - sun_descent1, sun_descent1)
        ((argb.mix (1.5 * (f32.max 0 (f32.min 1 (f32.abs (height1 - height2))))) argb.black 1.0 color2), height1 - sun_descent1, sun_descent1)
    else
        (color2, height2, sun_descent1)

let sunlight_sequential [h][w] (sun_height: f32) (sun_descent: f32) (color_map: [h][w]i32) (height_map: [h][w]i32) : [h][w]i32 =
    let neutral = replicate h (replicate w (argb.from_rgba 0.0 0.0 0.0 0.0))
    let tuples = map2 (\color_row height_row -> map2 (\x y -> (x, f32.i32 y, sun_descent)) color_row height_row) color_map height_map
    let shadowed = map (\rows -> map (\elem -> elem.0) (scan (shade) (0, 0.0, sun_descent) rows)) tuples
    in shadowed

let generate_shadowmap_seq [h][w] (color_map: [h][w]i32) (height_map: [h][w]i32) (sun_dy: f32) : [h][w]i32 =
    let float_heights = map (\row -> map (\height -> f32.i32 height) row) height_map
    let (heights, cols) =unzip (map2 (\heights colors -> 
        loop (heights, colors) for i < (w-1) do
        if heights[i] > heights[i+1] then
            (copy heights with [i+1] = heights[i]-sun_dy, (copy colors with [i+1] = argb.mix 1.0 argb.black 1.0 colors[i+1]))
        else
            (heights, colors)
    ) float_heights color_map)
    in cols
    
