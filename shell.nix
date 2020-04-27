with import <nixpkgs> {};
stdenv.mkDerivation {
    name = "futspace";
    buildInputs = [ pkgconfig SDL2 SDL2_ttf ocl-icd opencl-headers freeimage ];
}
