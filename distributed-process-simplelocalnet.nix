{ cabal, binary, dataAccessor, distributedProcess, network
, networkMulticast, networkTransport, networkTransportTcp
, transformers
}:

cabal.mkDerivation (self: {
  pname = "distributed-process-simplelocalnet";
  version = "0.2.1.0";
  sha256 = "02d7i9aknnlaqkw9crj9sb5vqg63lgschdnldhx2adiq4lalap3w";
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    binary dataAccessor distributedProcess network networkMulticast
    networkTransport networkTransportTcp transformers
  ];
  meta = {
    homepage = "http://haskell-distributed.github.com";
    description = "Simple zero-configuration backend for Cloud Haskell";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
