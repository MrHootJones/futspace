import "../lib/github.com/athas/matte/colour"
import "voxel_renderer"
import "../lib/github.com/diku-dk/linalg/linalg"
module linalg_f32 = mk_linalg f32

let vec3_rotate (axis : #x|#y|#z) (angle : f32) (vec3 : [3]f32) : [3]f32 =
    match axis
    case #x ->
        let R_x =
            [[1, 0, 0],
            [0, f32.cos angle, -f32.sin angle],
            [0, f32.sin angle, f32.cos angle]]
        in linalg_f32.matvecmul_row R_x vec3
    case #y ->
        let R_y =
            [[f32.cos angle, 0, f32.sin angle],
            [0, 1, 0],
            [-f32.sin angle, 0, f32.cos angle]]
        in linalg_f32.matvecmul_row R_y vec3
    case #z ->
        let R_z =
            [[f32.cos angle, -f32.sin angle, 0],
            [f32.sin angle, f32.cos angle, 0],
            [0, 0, 1]]
        in linalg_f32.matvecmul_row R_z vec3

let interpolate [h][w] (pd: i32) (rendered_image: [h][w]argb.colour) : [h][w]argb.colour =
    let m1 = 1.0
    let m2 = 1.0
    let smoothed_image = 
        map (\y ->
            map (\x -> 
                    let c = #[unsafe] rendered_image[y, x]
                    let u = #[unsafe] rendered_image[(y-pd)%h, x]
                    let d = #[unsafe] rendered_image[(y+pd)%h, x]
                    let l = #[unsafe] rendered_image[y, (x-pd)%h]
                    let r = #[unsafe] rendered_image[y, (x+pd)%h]
                    let ul = #[unsafe] rendered_image[(y-pd)%h, (x-pd)%h]
                    let ur = #[unsafe] rendered_image[(y-pd)%h, (x+pd)%h]
                    let dl = #[unsafe] rendered_image[(y+pd)%h, (x-pd)%h]
                    let dr = #[unsafe] rendered_image[(y+pd)%h, (x+pd)%h]
                    in argb.mix m1 c m2 (argb.mix m1 u m1 (argb.mix m1 d m1 (argb.mix m1 r m1 (argb.mix m1 l m1 (argb.mix m1 ul m1 (argb.mix m1 ur m1 (argb.mix m1 dl m1 dr)))))))
                    ) (0..<w)
                        ) (0..<h)
    in smoothed_image

let interpolate2 [h][w] (img: [h][w]argb.colour) : [h][w]argb.colour =
        let img = map (\row -> map (\pixel -> pixel) row) img
        in
        map3 (\mids highs lows ->
            map5 (\l c r u d -> argb.mix 1.0 l 1.0 (argb.mix 1.0 c 1.0 (argb.mix 1.0 r 1.0 (argb.mix 1.0 u 1.0 d)))) (rotate (-1) mids) mids (rotate 1 mids) highs lows 
            ) img (rotate (-1) img) (rotate 1 img) 

let modulate [h][w] (num: f32) (height_map: [h][w]i32) = 
    map (\heights -> 
        map2 (\height j -> 
            height + i32.f32 ((f32.sin num * (f32.i32 j)) * f32.i32 height)
            ) heights (iota w)
                ) height_map

let generate_shadowmap [h][w] (height_map: [h][w]i32) (sun_dy: f32) : [h][w]f32 =
    map (\height_row -> 
            map2 (\x height->
                    --Why is the following outcommented line of code more than 50% slower than the new map expression below?
                    --Intuition tells me it must be related to the fact that the result of each parallel execution of the out-commented map will potentially result
                    --in arrays of varying length.

                    --let conds = map (\idx -> if f32.i32 height_row[x] + f32.i32 (x-idx) * sun_dy < f32.i32 height_row[idx] then 1.0 else 0.0) (0..<x)

                    let conds = map2 (\idx test_height-> if f32.i32 height + f32.i32 (w-idx) * sun_dy < f32.i32 test_height then 1.0 else 0.0) (0..<w) (rotate x height_row)
                    in reduce (+) 0.0 conds
                    ) (0..<w) height_row
            ) height_map

--generates a colormap with shadows from a pair of color and height maps.
let generate_shadowmap2 [h][w] (color_map : [h][w]argb.colour) (height_map : [h][w]i32) (ang : f32) (sun_dy : f32) : [h][w]argb.colour =
    #[unsafe]
    let intensities = 
        map (\y -> 
            map (\x ->
                let height = height_map[y%h,x%h]
                let conds = map (\dist-> if f32.i32 height + f32.i32 (dist) * sun_dy < f32.i32 height_map[(y + i32.f32 ((f32.i32 dist) * (f32.sin ang)) )%h, (x + i32.f32 ((f32.i32 dist) * (f32.cos ang)) )%w] then 1.0 else 0.0) (1..<512)
                in reduce (+) 0.0 conds
            )(0..<w)
            ) (0..<h)
    in
    map2 (\colors intensities_row -> map2 (\color intensity -> (argb.mix (0.4 * intensity) argb.black 0.8 color)) colors intensities_row) color_map intensities

let generate_shadowmap3 (color_fun : f32 -> f32 -> argb.colour) (height_fun : f32 -> f32 -> f32) (ang : f32) (sun_dy : f32) : [][]argb.colour =
    let max_dist = 1024
    let steps_per_ray = 512
    let step_size = f32.i32 (max_dist / steps_per_ray)
    in
    map (\y -> 
        map (\x ->
            let fx = f32.i32 x
            let fy = f32.i32 y
            let height = height_fun fx fy
            let conds = map (\dist -> 
                                let fdist = f32.i32 dist
                                in
                                if height + fdist * step_size * sun_dy - height_fun (fx + fdist * step_size * f32.cos ang) (fy + fdist * step_size * f32.sin ang) < -0.5 then 1 else 0) (1..<steps_per_ray)
            let amount = f32.i32 (reduce (+) 0 conds)
            in (argb.mix (step_size * amount) argb.black (sun_dy*5) (color_fun (f32.i32 x) (f32.i32 y)))
        )(0..<1024)
    ) (0..<1024)

let generate_shadowmap_accumulated (color_fun : f32 -> f32 -> argb.colour) (height_fun : f32 -> f32 -> f32) (sun : [3]f32) : [][]argb.colour =
    let max_dist = 1024
    let steps_per_ray = 256
    let step_size = f32.i32 (max_dist / steps_per_ray)
    in
    map (\y -> 
        map (\x ->
            let fx = f32.i32 x
            let fy = f32.i32 y
            let height = height_fun fx fy
            let conds = map (\dist -> 
                                let fdist = f32.i32 dist
                                in
                                if height + fdist * step_size * sun[1] - height_fun (fx + fdist * step_size * sun[0]) (fy + fdist * step_size * sun[2]) < -0.5 then 1 else 0) (1..<steps_per_ray)
            let amount = f32.i32 (reduce (+) 0 conds)
            in (argb.mix (step_size * amount) argb.black 1.0 (color_fun (f32.i32 x) (f32.i32 y)))
        )(0..<1024)
    ) (0..<1024)

let generate_shadowmap_weighted_nearest (color_fun : f32 -> f32 -> argb.colour) (height_fun : f32 -> f32 -> f32) (sun : [3]f32) : [][]argb.colour =
    let max_dist = 1024
    let steps_per_ray = 256
    let step_size = f32.i32 (max_dist / steps_per_ray)
    in
    map (\y -> 
        map (\x ->
            let fx = f32.i32 x
            let fy = f32.i32 y
            let height = height_fun fx fy
            let conds = map (\dist -> 
                                let fdist = f32.i32 dist
                                in
                                if height + fdist * step_size * sun[1] - height_fun (fx + fdist * step_size * sun[0]) (fy + fdist * step_size * sun[2]) < -0.5 then (dist * i32.f32 step_size) else max_dist + 1) (1..<steps_per_ray)
            let amount = f32.i32 max_dist * 0.2 / (f32.i32 (reduce (i32.min) (max_dist + 1) conds))
            in (argb.mix amount argb.black 1.0 (color_fun (f32.i32 x) (f32.i32 y)))
        )(0..<1024)
    ) (0..<1024)
--heightmap axis-aligned shadows by scanning across the height and color maps and adjusting the colors of the colormap based on whether the height of the previous voxel
--intersects with a vector representing the sun. now rests here as a backup for inspiration for a potentially sequential implementation of shadowmapping in case we do not find a better way to do it in parallel.
--is called sequential as it only works on the futhark c compiler, as the shade function is not associative, leading to bugs.
let shade (color1: argb.colour, height1: f32, sun_descent1: f32) (color2: argb.colour, height2: f32, sun_descent2: f32) : (argb.colour, f32, f32)=
    if height1 > height2 then
        --((argb.from_rgba 0.0 0.0 0.0 (f32.max 0 (f32.min 1 (f32.abs (height1 - height2))))), height1 - sun_descent1, sun_descent1)
        ((argb.mix (1.5 * (f32.max 0 (f32.min 1 (f32.abs (height1 - height2))))) argb.black 1.0 color2), height1 - sun_descent1, sun_descent1)
    else
        (color2, height2, sun_descent1)

let sunlight_sequential [h][w] (sun_height: f32) (sun_descent: f32) (color_map: [h][w]argb.colour) (height_map: [h][w]i32) : [h][w]argb.colour =
    let neutral = replicate h (replicate w (argb.from_rgba 0.0 0.0 0.0 0.0))
    let tuples = map2 (\color_row height_row -> map2 (\x y -> (x, f32.i32 y, sun_descent)) color_row height_row) color_map height_map
    let shadowed = map (\rows -> map (\elem -> elem.0) (scan (shade) (0, 0.0, sun_descent) rows)) tuples
    in shadowed
