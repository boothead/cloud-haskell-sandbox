{ cabal, HUnit, parsec, testFramework, testFrameworkHunit
, testFrameworkQuickcheck2
}:

cabal.mkDerivation (self: {
  pname = "network";
  version = "2.4.2.3";
  sha256 = "0vf1cg9y2njd8di2v00dy9zzxrwh28micxb1m7s6nqmf5mjrq6w0";
  buildDepends = [ parsec ];
  testDepends = [
    HUnit testFramework testFrameworkHunit testFrameworkQuickcheck2
  ];
  meta = {
    homepage = "https://github.com/haskell/network";
    description = "Low-level networking interface";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
