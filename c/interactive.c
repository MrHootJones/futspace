#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <stdbool.h>
#include <getopt.h>
#include "lib/github.com/diku-dk/lys/liblys.h"

#include <FreeImage.h>
#include "freeimage_futspace.h"
#define MAX_FPS 3000000
#define FONT_SIZE 20

#define DISPLAY_WIDTH 1024
#define DISPLAY_HEIGHT 1024

struct internal {
  TTF_Font *font;
  bool show_text;
};

int map_num = 0;

/* Loads a color/heightmap pair using the freeimage library and calls the update_map entrypoint to futspace, which updates the state with the new maps*/
void load_map(struct lys_context *ctx){
  map_num += 1;
  map_num = (map_num > 29) ? 1 : map_num;
  char* colormap_path[100];
  char* heightmap_path[100];
  sprintf(colormap_path, "data/converted-maps/C%dW.png", map_num);
  sprintf(heightmap_path, "data/converted-maps/D%d.png", map_num);
  printf("%s\n%s\n", colormap_path, heightmap_path);
  int width, height;

  FILE *colormap;
  FILE *heightmap;
  colormap = fopen(colormap_path, "r");
  assert(colormap != NULL);
  heightmap = fopen(heightmap_path, "r");
  assert(heightmap != NULL);

  FreeImage_Initialise(false);

  int32_t* colormap_data = freeimage_load(colormap_path, colormap, (unsigned int*) &width, (unsigned int*) &height);
  int32_t* heightmap_data = freeimage_load(heightmap_path, heightmap, (unsigned int*) &width, (unsigned int*) &height);
  assert(colormap_data != NULL);
  assert(fclose(colormap) != EOF);
  assert(heightmap_data != NULL);
  assert(fclose(heightmap) != EOF);
  struct futhark_i32_2d *colormap_fut = futhark_new_i32_2d(ctx->fut, colormap_data, height, width);
  struct futhark_i32_2d *heightmap_fut = futhark_new_i32_2d(ctx->fut, heightmap_data, height, width);
  free(colormap_data);
  free(heightmap_data);
  futhark_entry_update_map(ctx->fut, &(ctx->state), colormap_fut, heightmap_fut, ctx->state);
  futhark_free_i32_2d(ctx->fut, colormap_fut);
  futhark_free_i32_2d(ctx->fut, heightmap_fut);
}

/* Only responsible for rendering text on top of the image right now. */
void loop_iteration(struct lys_context *ctx, struct internal *internal) {
  if(internal->show_text){
    float fps = ctx->fps;
    float x, y, angle, height, horizon, distance, sun_height, sun_ang, fov;
    FUT_CHECK(ctx->fut, futhark_entry_text_content(ctx->fut, &x, &y, &angle, &height, &horizon, &distance, &sun_height, &sun_ang, &fov, ctx->state));
    char *text = malloc(300);
    sprintf(text, 
    "FPS: %f\nX: %f\nY: %f\nAngle: %f\nHeight: %f\nHorizon: %f\nRendering distance: %f\nsun_height: %f\nsun_ang: %f\nfov: %f\n",
    fps, x, y, angle, height, horizon, distance, sun_height, sun_ang, fov);
    draw_text(ctx, internal->font, FONT_SIZE, text, 0xff00ffff, 10, 10);
  }
}

/* Handles certain lys events that are not processed in lysspace.fut */
void handle_event(struct lys_context *ctx, enum lys_event event) {
  struct internal *internal = (struct internal *) ctx->event_handler_data;
  switch (event) {
  case LYS_LOOP_ITERATION:
    loop_iteration(ctx, internal);
    break;
  case LYS_F1:
    load_map(ctx);
    break;
  default:
    return;
  }
}

/* Responsible for initializing lys */
int32_t* run_interactive(struct futhark_context *futctx,
                         int width, int height, int seed,
                         bool show_text_initial) {
  struct lys_context ctx;
  lys_setup(&ctx, width, height, MAX_FPS, SDL_WINDOW_RESIZABLE);
  ctx.fut = futctx;

  ctx.event_handler_data = NULL;
  ctx.event_handler = handle_event;

  futhark_entry_init(ctx.fut, &ctx.state, seed);

  load_map(&ctx);

  SDL_ASSERT(TTF_Init() == 0);

  struct internal internal;
  ctx.event_handler_data = (void*) &internal;
  internal.show_text = show_text_initial;
  internal.font = TTF_OpenFont("c/NeomatrixCode.ttf", FONT_SIZE);
  SDL_ASSERT(internal.font != NULL);

  lys_run_sdl(&ctx);

  TTF_CloseFont(internal.font);

  return ctx.data;
}

int main(int argc, char** argv) {
  char *deviceopt = NULL;
  bool device_interactive = false;

  bool show_text = false;

  int c;
  while ( (c = getopt(argc, argv, "w:h:r:Rd:b:i:t")) != -1) {
    switch (c) {
    case 'd':
      deviceopt = optarg;
      break;
    case 'i':
      device_interactive = true;
      break;
    case 't':
      show_text = true;
      break;
    default:
      fprintf(stderr, "unknown option: %c\n", c);
      return EXIT_FAILURE;
    }
  }

  uint32_t seed = (int32_t) lys_wall_time();

  struct futhark_context_config *futcfg;
  struct futhark_context *futctx;
  char* opencl_device_name = NULL;
  lys_setup_futhark_context(deviceopt, device_interactive,
                            &futcfg, &futctx, &opencl_device_name);
  if (opencl_device_name != NULL) {
    fprintf(stderr, "Using OpenCL device: %s\n", opencl_device_name);
    fprintf(stderr, "Use -d or -p to change this.\n");
    free(opencl_device_name);
  }

  run_interactive(futctx, DISPLAY_WIDTH, DISPLAY_HEIGHT, seed, show_text);
}
