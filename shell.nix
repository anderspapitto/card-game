let pkgs = import <nixpkgs> ({
  config.packageOverrides = super: let self = super.pkgs; in
    {
      haskell = super.haskell // {
        packages = super.haskell.packages // {
          lts-3_5 = super.haskell.packages.lts-3_5.override {
            overrides = self: super: {
              reflex-dom = self.callPackage /etc/nixos/nixos-configuration/reflex-dom-hamishmack.nix { };
            };
          };
        };
      };
    };

});

in (import ./default.nix) {
  stdenv          = pkgs.stdenv;
  ghc = pkgs.haskell.packages.lts-3_5;
  ghcjs = pkgs.haskell.packages.ghcjs;
#  cabal-install = pkgs.haskellPackages.cabal-install;
  nodejs          = pkgs.nodejs;
}
