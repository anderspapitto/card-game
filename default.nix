{ stdenv, haskellPackages, nodejs }:

let env = haskellPackages.ghcWithPackages (p: with p; [
      cabal-install lens random reflex-dom servant network wai warp servant-server network HTTP servant-client free clay text
    ]);
in
  stdenv.mkDerivation {
    name = "card-game";
    buildInputs = [ env nodejs ];
    shellHook   = ''
      export NIX_GHC="${env}/bin/ghc"
      export NIX_GHCPKG="${env}/bin/ghc-pkg"
      export NIX_GHC_DOCDIR="${env}/share/doc/ghc/html"
      export NIX_GHC_LIBDIR=$( $NIX_GHC --print-libdir )
    '';
  }
