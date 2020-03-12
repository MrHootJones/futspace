#include <assert.h>

static unsigned int wrap_fread(void *buffer, unsigned size, unsigned count, fi_handle handle) {
  return (unsigned) fread(buffer, size, count, (FILE*) handle);
}

static unsigned int wrap_fwrite(void *buffer, unsigned size, unsigned count, fi_handle handle) {
  return (unsigned) fwrite(buffer, size, count, (FILE*) handle);
}

static int wrap_fseek(fi_handle handle, long offset, int origin) {
  return fseek((FILE*) handle, offset, origin);
}

static long wrap_ftell(fi_handle handle) {
  return ftell((FILE*) handle);
}

int32_t* freeimage_load(const char* filename, FILE *f, unsigned int *width, unsigned int *height) {
  FreeImageIO io;
  io.read_proc = (FI_ReadProc) wrap_fread;
  io.write_proc = NULL;
  io.seek_proc = (FI_SeekProc) wrap_fseek;
  io.tell_proc = (FI_TellProc) wrap_ftell;

  FREE_IMAGE_FORMAT fif = FreeImage_GetFileTypeFromHandle(&io, (fi_handle) f, 0);
  if (fif == FIF_UNKNOWN && filename != NULL) {
    fif = FreeImage_GetFIFFromFilename(filename);
  }
  if (fif == FIF_UNKNOWN) {
    return NULL;
  }

  FIBITMAP *dib = FreeImage_LoadFromHandle(fif, &io, (fi_handle) f, 0);
  if (dib == NULL) {
    return NULL;
  }

  *width = FreeImage_GetWidth(dib);
  *height = FreeImage_GetHeight(dib);

  int32_t* image = (int32_t*) malloc(*width * *height * sizeof(int32_t));
  if (image == NULL) {
    FreeImage_Unload(dib);
    return NULL;
  }
  for (int y = 0; y < (int) *height; y++) {
    for (int x = 0; x < (int) *width; x++) {
      RGBQUAD rgb;
      assert(FreeImage_GetPixelColor(dib, x, y, &rgb));
      int i = (*height - y - 1) * *width + x;
      image[i] = 0xff000000 | (rgb.rgbRed << 16) | (rgb.rgbGreen << 8) | rgb.rgbBlue;
    }
  }

  FreeImage_Unload(dib);

  return image;
}

void freeimage_save(const char* filename, FILE* f, const int32_t* image,
                    unsigned int width, unsigned int height) {
  FreeImageIO io;
  io.read_proc = NULL;
  io.write_proc = (FI_WriteProc) wrap_fwrite;
  io.seek_proc = (FI_SeekProc) wrap_fseek;
  io.tell_proc = (FI_TellProc) wrap_ftell;

  FREE_IMAGE_FORMAT fif = FreeImage_GetFIFFromFilename(filename);
  assert(fif != FIF_UNKNOWN);

  FIBITMAP *dib = FreeImage_Allocate((int) width, (int) height, 24, 0, 0, 0);
  for (int y = 0; y < (int) height; y++) {
    for (int x = 0; x < (int) width; x++) {
      int i = (height - y - 1) * width + x;
      int32_t color = image[i];
      RGBQUAD rgb;
      rgb.rgbRed = (color & 0xff0000) >> 16;
      rgb.rgbGreen = (color & 0x00ff00) >> 8;
      rgb.rgbBlue = color & 0x0000ff;
      assert(FreeImage_SetPixelColor(dib, x, y, &rgb));
    }
  }

  assert(FreeImage_SaveToHandle(fif, dib, &io, (fi_handle) f, 0));

  FreeImage_Unload(dib);
}