let main (c : f32) (z_0 : f32) (d : f32) : []f32 =
    let sqrt =  f32.sqrt ((d-2*z_0)**2 + 8*d*c)
    let num = sqrt - 2*z_0 + d
    let div = 2*d
    let n = i32.f32 (f32.floor (num / div))
    --in map (\i -> f64.i32 i) (1...n)
    let is = map (\i -> f32.i32 i) (1...n)
    in map (\i -> (i/2) * (2*z_0 + (i-1) * d)) is