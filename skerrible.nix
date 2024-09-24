{ mkDerivation, aeson, async, base, bytestring, containers, HUnit
, lens, lib, random, text, transformers, wai, wai-app-static
, wai-websockets, warp, websockets
}:
mkDerivation {
  pname = "skerrible";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  enableLibraryProfiling = false;
  enableSharedLibraries = false;
  doHaddock = false;
  isExecutable = true;
  libraryHaskellDepends = [ aeson base containers text ];
  executableHaskellDepends = [
    aeson async base bytestring containers lens random text
    transformers wai wai-app-static wai-websockets warp websockets
  ];
  testHaskellDepends = [
    base containers HUnit lens random transformers
  ];
  description = "terrible word game";
  license = lib.licenses.bsd3;
  mainProgram = "skerrible-server";
}
