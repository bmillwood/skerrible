{ stdenvNoCC
, elmPackages
, ... }:

stdenvNoCC.mkDerivation {
  name = "skerrible-web-client";
  src = ./.;
  buildInputs = [
    elmPackages.elm
  ];
  configurePhase = elmPackages.fetchElmDeps {
    elmPackages = import ./elm-srcs.nix;
    elmVersion = "0.19.1";
    registryDat = ./registry.dat;
  };
  placeSound = builtins.fetchurl {
    # Dropping by jakebagger -- https://freesound.org/s/499782/ -- License: Attribution 4.0
    # Converted to mp3 by ffmpeg -ss 0.1, but since I have to rehost it anyway
    # (freesound requires logging in), might as well rehost the converted
    # version.
    url = "https://www.rpm.cc/skerrible/media/place.mp3";
    sha256 = "10vish3mc7k1n5wsk80gzys87cnlqpkcgrjhwb5y28yimj395z2r";
  };
  buildPhase = ''
    mkdir "$out"
    cp *.html *.js "$out"
    mkdir "$out/media"
    ln -s "$placeSound" "$out/media/place.mp3"
    elm make --output="$out/elm.js" src/Main.elm
  '';
}
