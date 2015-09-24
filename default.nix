{ stdenv, ghc, ghcjs, nodejs }:

let ghc-env = ghc.ghcWithPackages (p: with p; [
      lens random text servant servant-server clay HTTP wai warp reflex-dom # network network free text cabal-install servant-client
    ]);
    ghcjs-env = ghcjs.ghcWithPackages (p: with p; [
      gtk2hs-buildtools # reflex-dom # lens random network free clay text servant network wai warp servant-server  # servant-client cabal-install
    ]);



    ghc-str = "ghc";
    env = ghc-env;
    full-env = [ env ];

#    ghc-str = "ghcjs";
#    env = ghcjs-env;
#    full-env = [ env nodejs ];



in
  stdenv.mkDerivation {
    name = "card-game";
    buildInputs = full-env ++ [ ghc.cabal-install ];
    shellHook = ''
      export NIX_GHC="${env}/bin/${ghc-str}"
      export NIX_GHCPKG="${env}/bin/${ghc-str}-pkg"
      export NIX_GHC_DOCDIR="${env}/share/doc/${ghc-str}/html"
      export NIX_GHC_LIBDIR=$( $NIX_GHC --print-libdir )
    '';
  }
