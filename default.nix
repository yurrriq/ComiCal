{ mkDerivation, base, hpack, stdenv }:
mkDerivation {
  pname = "ComiCal";
  version = "0.0.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [ base ];
  preConfigure = "hpack";
  homepage = "https://github.com/yurrriq/ComiCal#readme";
  description = "Track the publish dates of your favorite comics";
  license = stdenv.lib.licenses.unfree;
  hydraPlatforms = stdenv.lib.platforms.none;
  maintainers = with stdenv.lib.maintainers; [ yurrriq ];
}
