# futspace

futspace is a parallelized implementation of the [Voxel Space rendering algorithm](https://en.wikipedia.org/wiki/Voxel_Space) written in [futhark](https://futhark-lang.org/). The design is partially based on [the open-source javascript implementation](https://github.com/s-macke/VoxelSpace) provided by [s-macke](https://github.com/s-macke). Because development is still underway significant changes may be introduced in the future. 

![](demo.gif)

## Interactive Demo

To run the interactive demo you need linux (or a similar unix-based system) with a recent build of futhark installed, as well as the following dependencies:

* SDL2
* FreeImage
* Beignet or Intel Neo (if running with integrated graphics)

Given these prerequisites, the interactive demo is built with

```bash
futhark pkg sync
make
```
It can then be run with

```bash
./futspace
```

### Controls 

W/S: Forward/backward

A/D: Turn left/right

Q/E: Look up/down

R/F: Raise/lower camera

U/J: change invz_param1

I/K: change invz_param2

O/L: change magic_number