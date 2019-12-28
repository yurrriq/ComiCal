{ mkDerivation, base, bytestring, hpack, modern-uri, req, stdenv
, tagsoup, text, time
}:
mkDerivation {
  pname = "ComiCal";
  version = "0.0.2.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring modern-uri req tagsoup text time
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [ base text ];
  prePatch = "hpack";
  homepage = "https://github.com/yurrriq/ComiCal#readme";
  description = "Track the publish dates of your favorite comics";
  license = stdenv.lib.licenses.mit;
  maintainers = with stdenv.lib.maintainers; [ yurrriq ];
}
