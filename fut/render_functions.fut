import "../lib/github.com/athas/matte/colour"

--just some example functions that can be drawn in 'math mode'.
let squares (x: f32) (y: f32) : f32 =
    let x = x % 1024
    let y = y % 1024
    in
    if x > 256 && f32.abs x < 768 && f32.abs y > 256 && f32.abs y < 768 then
        200.0
    else
        0.0

let expo (x: f32) (y: f32) : f32 =
    let x = x % 1024
    let y = y % 1024
    let output = (x**2+y**2)
    in
    if x > 0 && x < 1024 && y > 0 && y < 1024 then
        output / 1000.0
    else
        0.0

let expo2 (x: f32) (y: f32) : f32 =
    let output = (x**2-y**2)/1024
    in
    if x > -1024 && x < 1024 && y > -1024 && y < 1024 then
        output
    else
        0.0

let wavy (x: f32) (y: f32) : f32 =
    let output = (f32.cos ((x*y)/2048.0)) * 100.0
    let output = 500 + (f32.cos (x/512) * f32.sin (y/512)) * 500.0
    in
    output

--example of coloring function.
let function_coloring (x: f32) (y: f32) : i32 =
    if (i32.f32 x) % 2 == 0 || (i32.f32 y) % 2 == 0 then
            argb.black
        else
            argb.white

--the original bilinear filtering implementation we used to draw slopes in 'png mode'. now filtering is simply a part of the two following functions png_height and png_color.
let bilinear_filter (color_fun : f32 -> f32 -> argb.colour) (height_fun : f32 -> f32 -> f32) (x : f32) (y : f32) : (argb.colour, f32) =
        let floor_x = f32.floor x
        let ceil_x = f32.ceil x
        let floor_y = f32.floor y
        let ceil_y = f32.ceil y

        let x_interpolated1 = (ceil_x - x) * height_fun floor_x floor_y
                           + (x - floor_x) * height_fun ceil_x floor_y
        let x_interpolated2 = (ceil_x - x) * height_fun floor_x ceil_y
                           + (x - floor_x) * height_fun ceil_x ceil_y 
        let interpolated_height = (ceil_y - y) * x_interpolated1 + (y - floor_y) * x_interpolated2

        let x_interpolated1 = argb.mix (ceil_x - x) (color_fun floor_x floor_y) (x - floor_x) (color_fun ceil_x floor_y)
        let x_interpolated2 = argb.mix (ceil_x - x) (color_fun floor_x ceil_y) (x - floor_x) (color_fun ceil_x ceil_y)
        let interpolated_color = argb.mix (ceil_y - y) x_interpolated1 (y - floor_y) x_interpolated2

        in (interpolated_color, interpolated_height)

--bilinearly filtered height from png heightmap
let png_height [h][w] (heights : [h][w]i32) (x : f32) (y : f32) : f32 =
        let floor_x = f32.floor x
        let ceil_x = f32.ceil x
        let floor_y = f32.floor y
        let ceil_y = f32.ceil y

        let x_interpolated1 = (ceil_x - x) * f32.i32 heights[(i32.f32 floor_y)%h,(i32.f32 floor_x)%w]
                           + (x - floor_x) * f32.i32 heights[(i32.f32 floor_y)%h,(i32.f32 ceil_x)%w]
        let x_interpolated2 = (ceil_x - x) * f32.i32 heights[(i32.f32 ceil_y)%h,(i32.f32 floor_x)%w]
                           + (x - floor_x) * f32.i32 heights[(i32.f32 ceil_y)%h,(i32.f32 ceil_x)%w]
        in (ceil_y - y) * x_interpolated1 + (y - floor_y) * x_interpolated2

--bilinearly filtered color from png colormap        
let png_color [h][w] (colors : [h][w]i32) (x : f32) (y : f32) : i32 =
        let floor_x = f32.floor x
        let ceil_x = f32.ceil x
        let floor_y = f32.floor y
        let ceil_y = f32.ceil y

        let x_interpolated1 = argb.mix (ceil_x - x) colors[(i32.f32 floor_y)%h,(i32.f32 floor_x)%w] (x - floor_x) colors[(i32.f32 floor_y)%h,(i32.f32 ceil_x)%w]
        let x_interpolated2 = argb.mix (ceil_x - x) colors[(i32.f32 ceil_y)%h,(i32.f32 floor_x)%w] (x - floor_x) colors[(i32.f32 ceil_y)%h,(i32.f32 ceil_x)%w]
        let color = argb.mix (ceil_y - y) x_interpolated1 (y - floor_y) x_interpolated2
        in
        color