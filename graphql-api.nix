{ mkDerivation, aeson, attoparsec, base, containers, criterion
, directory, doctest, exceptions, ghc-prim, hspec, lens, mtl
, protolude, QuickCheck, raw-strings-qq, scientific, stdenv, tasty
, tasty-hspec, text, transformers
}:
mkDerivation {
  pname = "graphql-api";
  version = "0.2.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson attoparsec base containers exceptions ghc-prim lens mtl
    protolude QuickCheck scientific text transformers
  ];
  testHaskellDepends = [
    aeson attoparsec base containers directory doctest exceptions hspec
    lens mtl protolude QuickCheck raw-strings-qq tasty tasty-hspec
    transformers
  ];
  benchmarkHaskellDepends = [
    attoparsec base criterion exceptions lens mtl protolude
    transformers
  ];
  homepage = "https://github.com/jml/graphql-api#readme";
  description = "GraphQL API";
  license = stdenv.lib.licenses.asl20;
}
