{ cabal, ansiTerminal, binary, dataAccessor, deepseq
, distributedProcess, distributedStatic, fingertree, hashable
, HUnit, mtl, network, networkTransport, networkTransportTcp
, QuickCheck, rematch, stm, testFramework, testFrameworkHunit
, testFrameworkQuickcheck2, time, transformers, unorderedContainers
}:

cabal.mkDerivation (self: {
  pname = "distributed-process-platform";
  version = "0.1.0";
  sha256 = "0bxfynvqkzvah7gbg74yzwpma8j32bamnyysj6dk39da0v880abm";
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    binary dataAccessor deepseq distributedProcess fingertree hashable
    mtl stm time transformers unorderedContainers
  ];
  testDepends = [
    ansiTerminal binary dataAccessor deepseq distributedProcess
    distributedStatic fingertree hashable HUnit mtl network
    networkTransport networkTransportTcp QuickCheck rematch stm
    testFramework testFrameworkHunit testFrameworkQuickcheck2 time
    transformers unorderedContainers
  ];
  meta = {
    homepage = "http://github.com/haskell-distributed/distributed-process-platform";
    description = "The Cloud Haskell Application Platform";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
