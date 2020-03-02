{ mkDerivation
, base
, bytestring
, hpack
, lens
, modern-uri
, optparse-applicative
, req
, stdenv
, tagsoup
, text
, time
}:
mkDerivation {
  pname = "ComiCal";
  version = "0.4.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base
    bytestring
    lens
    modern-uri
    req
    tagsoup
    text
    time
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base
    bytestring
    lens
    optparse-applicative
    text
  ];
  prePatch = "hpack";
  homepage = "https://github.com/yurrriq/ComiCal#readme";
  description = "Track the publish dates of your favorite comics";
  license = stdenv.lib.licenses.mit;
  maintainers = with stdenv.lib.maintainers; [ yurrriq ];
}
