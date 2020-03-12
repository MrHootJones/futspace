import "../lib/github.com/athas/matte/colour"

let interpolate [h][w] (pd: i32) (rendered_image: [h][w]i32) : [h][w]i32 =
    let m1 = 1.0
    let m2 = 1.0
    let smoothed_image = 
        map (\y ->
            map (\x -> 
                    let c = rendered_image[y, x]
                    let u = rendered_image[(y-pd)%h, x]
                    let d = rendered_image[(y+pd)%h, x]
                    let l = rendered_image[y, (x-pd)%h]
                    let r = rendered_image[y, (x+pd)%h]
                    let ul = rendered_image[(y-pd)%h, (x-pd)%h]
                    let ur = rendered_image[(y-pd)%h, (x+pd)%h]
                    let dl = rendered_image[(y+pd)%h, (x-pd)%h]
                    let dr = rendered_image[(y+pd)%h, (x+pd)%h]
                    in argb.mix m1 c m2 (argb.mix m1 u m1 (argb.mix m1 d m1 (argb.mix m1 r m1 (argb.mix m1 l m1 (argb.mix m1 ul m1 (argb.mix m1 ur m1 (argb.mix m1 dl m1 dr)))))))
                    ) (0..<w)
                        ) (0..<h)
    in smoothed_image

let modulate [h][w] (num: f32) (height_map: [h][w]i32) = 
    map (\heights -> 
        map2 (\height j -> 
            height + i32.f32 ((f32.sin (num * 0.01 + (f32.i32 j) * 0.2 )) * 0.03 * f32.i32 height)
            ) heights (iota w)
                ) height_map