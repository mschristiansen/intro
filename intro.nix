{ mkDerivation, aeson, base, blaze-html, containers, hspec
, QuickCheck, random, scotty, stdenv, stm, text, transformers
}:
mkDerivation {
  pname = "intro";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    aeson base blaze-html containers random scotty stm text
    transformers
  ];
  testDepends = [ base hspec QuickCheck ];
  license = stdenv.lib.licenses.gpl3;
}
