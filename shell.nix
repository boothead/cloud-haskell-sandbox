let
  pkgs = import <nixpkgs> {};
  haskellPackages = pkgs.haskellPackages.override {
    extension = self: super: {
      network = self.callPackage ./network.nix {};
      # distributedProcessPlatform = self.callPackage ./distributed-process-platform.nix {};
      distributedProcessLocal = self.callPackage ./distributed-process-simplelocalnet.nix {};
    };
  };
in pkgs.myEnvFun {
     name = "cloudHaskell";
     buildInputs = [
       (haskellPackages.ghcWithPackages (hs: ([
         hs.ghc
         hs.network
         hs.networkTransport
         hs.networkTransportTcp
         hs.cabalInstall
         hs.distributedProcess
         # hs.distributedProcessPlatform
         hs.distributedProcessLocal
       ]))) # ++ hs.profunctors.propagatedNativeBuildInputs)))
     ];
   }      
