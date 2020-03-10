{ compiler ? "ghc865" }:

let
  overlayShared = pkgsNew: pkgsOld: {
    haskell = pkgsOld.haskell // {
      packages = pkgsOld.haskell.packages // {
        "${compiler}" = pkgsOld.haskell.packages."${compiler}".override (old: {
            overrides =
              let
                appendConfigureFlag = pkgsNew.haskell.lib.appendConfigureFlag;
                dontCheck = pkgsNew.haskell.lib.dontCheck;
                failOnAllWarnings = pkgsNew.haskell.lib.failOnAllWarnings;
                overrideCabal = pkgsNew.haskell.lib.overrideCabal;
                optionalString = pkgsNew.stdenv.lib.optionalString;
                isDarwin = pkgsNew.stdenv.isDarwin;

                packages =
                  haskellPackagesNew: haskellPackagesOld: {
                    "helios" =
                        (haskellPackagesOld.callCabal2nix
                          "helios"
                          ./.
                          { }
                        );
                  };
                extension =
                  haskellPackagesNew: haskellPackagesOld: {
                    "memcache-haskell" =
                       dontCheck
                        (haskellPackagesNew.callHackageDirect
                          {
                            pkg = "memcache-haskell";
                            ver = "0.0.10.1";
                            sha256 = "0inyh3igriqhwmn0zvcwxxb7fxvdizns52xjpsfzq8ii3myvp33q";
                          }
                          { }
                        );
                    "memcache-conduit" =
                       dontCheck
                        (haskellPackagesNew.callHackageDirect
                          {
                            pkg = "memcache-conduit";
                            ver = "0.0.3";
                            sha256 = "12qcy22b3y14h6iafyj1k1arj7bhzwzhhhc274qygjcflg1009vh";
                          }
                          { }
                        );
                  };
              in
                pkgsNew.lib.fold
                  pkgsNew.lib.composeExtensions
                  (old.overrides or (_: _: {}))
                  [ extension
                    packages
                  ];
          }
        );
      };
    };
  };

  bootstrap = import <nixpkgs> { };

  nixpkgs = builtins.fromJSON (builtins.readFile ./nixpkgs.json);

  src = bootstrap.fetchFromGitHub {
    owner = "NixOS";
    repo  = "nixpkgs";
    inherit (nixpkgs) rev sha256;
  };

  pkgs = import src {
    config = { allowUnsupportedSystem = true; allowUnfree = true; };
    overlays = [ overlayShared ];
  };

  pkgs-linux = pkgs // {
    system = "x86_64-linux";
  };

  doBenchmark = pkgs.haskell.lib.doBenchmark;
  base-compiler = pkgs.haskell.packages."${compiler}";
in
  rec {
    inherit overlayShared;

    inherit (base-compiler)
      helios
    ;
    shell-helios = (doBenchmark base-compiler.helios).env;
  }

