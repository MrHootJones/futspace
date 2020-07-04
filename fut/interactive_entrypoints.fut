import "../lib/github.com/diku-dk/lys/lys"
module core = import "interactive"

type^ state = core.state

entry resize (h: i32) (w: i32) (s: state): state =
  core.resize h w s

entry key (e: i32) (key: i32) (s: state): state =
  let e' = if e == 0 then #keydown {key} else #keyup {key}
  in core.event e' s

entry mouse (buttons: i32) (x: i32) (y: i32) (s: state): state =
  core.event (#mouse {buttons, x, y}) s

entry wheel (dx: i32) (dy: i32) (s: state): state =
  core.event (#wheel {dx, dy}) s

entry render (s: state) =
  core.render s

entry init (seed: u32): state =
  core.init seed

entry text_content (s: state) =
    core.text_content s
    
entry step (td: f32) (s: state): state =
  core.event (#step td) s

entry update_map [h][w] (color_map: [h][w]argb.colour) (height_map: [h][w]i32) (s: state) : state =
  core.update_map color_map height_map s
