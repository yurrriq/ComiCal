{ mkDerivation, base, bytestring, hpack, modern-uri, req, stdenv
, tagsoup, text, time
}:
mkDerivation {
  pname = "ComiCal";
  version = "0.0.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring modern-uri req tagsoup text time
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base bytestring modern-uri req text time
  ];
  preConfigure = "hpack";
  homepage = "https://github.com/yurrriq/ComiCal#readme";
  description = "Track the publish dates of your favorite comics";
  license = stdenv.lib.licenses.unfree;
  hydraPlatforms = stdenv.lib.platforms.none;
  maintainers = with stdenv.lib.maintainers; [ yurrriq ];
}
