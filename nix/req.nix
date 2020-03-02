{ mkDerivation
, aeson
, authenticate-oauth
, base
, blaze-builder
, bytestring
, case-insensitive
, connection
, fetchgit
, hspec
, hspec-core
, hspec-discover
, http-api-data
, http-client
, http-client-tls
, http-types
, modern-uri
, monad-control
, mtl
, QuickCheck
, retry
, stdenv
, text
, time
, transformers
, transformers-base
, unordered-containers
}:
mkDerivation {
  pname = "req";
  version = "3.0.0";
  src = fetchgit {
    url = "https://github.com/mrkkrp/req.git";
    sha256 = "1nprj59p06pjar2vxhdmn6wcj06466w220qb4yjk927d6d9ml1ry";
    rev = "fd460ba5224ec09cfe7d28c3031b84c2074f8b0d";
    fetchSubmodules = true;
  };
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson
    authenticate-oauth
    base
    blaze-builder
    bytestring
    case-insensitive
    connection
    http-api-data
    http-client
    http-client-tls
    http-types
    modern-uri
    monad-control
    mtl
    retry
    text
    time
    transformers
    transformers-base
  ];
  testHaskellDepends = [
    aeson
    base
    blaze-builder
    bytestring
    case-insensitive
    hspec
    hspec-core
    http-client
    http-types
    modern-uri
    monad-control
    mtl
    QuickCheck
    retry
    text
    time
    unordered-containers
  ];
  testToolDepends = [ hspec-discover ];
  doCheck = false;
  homepage = "https://github.com/mrkkrp/req";
  description = "Easy-to-use, type-safe, expandable, high-level HTTP client library";
  license = stdenv.lib.licenses.bsd3;
  maintainers = with stdenv.lib.maintainers; [ yurrriq ];
}
