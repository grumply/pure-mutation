{ mkDerivation, base, pure, pure-lifted, pure-json, pure-prop, ghcjs-base, stdenv
}:
mkDerivation {
  pname = "pure-mutation";
  version = "0.8.0.0";
  src = ./.;
  libraryHaskellDepends = [ base pure pure-lifted pure-json pure-prop ghcjs-base ];
  homepage = "github.com/grumply/pure-mutation";
  license = stdenv.lib.licenses.bsd3;
}
