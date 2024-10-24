{ repoRoot, inputs, pkgs, system, lib }:

rec {

  # Need a newer version for 2.6.2 compatibility
  agda-stdlib = agda-packages.standard-library.overrideAttrs (oldAtts: rec {

    version = "1.7.3";

    src = pkgs.fetchFromGitHub {
      repo = "agda-stdlib";
      owner = "agda";
      rev = "v${version}";
      sha256 = "sha256-vtL6VPvTXhl/mepulUm8SYyTjnGsqno4RHDmTIy22Xg=";
    };

    # This is preConfigure is copied from more recent nixpkgs that also
    # uses version 1.7 of standard-library. Old nixpkgs (that used 1.4)
    # had a preConfigure step that worked with 1.7. Less old nixpkgs
    # (that used 1.6) had a preConfigure step that attempts to `rm`
    # files that are now in the .gitignore list for 1.
    preConfigure = ''
      runhaskell GenerateEverything.hs
      # We will only build/consider Everything.agda, in particular we don't want Everything*.agda
      # do be copied to the store.
      rm EverythingSafe.agda
    '';
  });


  # We want to keep control of which version of Agda we use, so we supply our own and override
  # the one from nixpkgs.
  #
  # The Agda builder needs a derivation with:
  # - The 'agda' executable
  # - The 'agda-mode' executable
  # - A 'version' attribute
  #
  # So we stitch one together here.
  #
  # Furthermore, the agda builder uses a `ghcWithPackages` that has to have ieee754 available.
  # We'd like it to use the same GHC as we have, if nothing else just to avoid depending on
  # another GHC from nixpkgs! Sadly, this one is harder to override, and we just hack
  # it into pkgs.haskellPackages in a fragile way. Annoyingly, this also means we have to ensure
  # we have a few extra packages that it uses in our Haskell package set.
  agda-packages =
    let
      Agda = agda-project.hsPkgs.Agda;

      frankenAgdaBin = pkgs.symlinkJoin {
        name = "agda";
        version = Agda.identifier.version;
        paths = [
          Agda.components.exes.agda
          Agda.components.exes.agda-mode
        ];
      };

      frankenAgda = frankenAgdaBin // {
        # Newer Agda is built with enableSeparateBinOutput, hence this hacky workaround.
        # https://github.com/NixOS/nixpkgs/commit/294245f7501e0a8e69b83346a4fa5afd4ed33ab3
        bin = frankenAgdaBin;
      };

      frankenPkgs =
        pkgs //
        {
          haskellPackages = pkgs.haskellPackages //
          {
            inherit (agda-project) ghcWithPackages;
          };
        };
    in
    pkgs.agdaPackages.override {
      Agda = frankenAgda;
      pkgs = frankenPkgs;
    };


  # Agda is a huge pain. They have a special custom setup that compiles the
  # interface files for the Agda that ships with the compiler. These go in
  # the data files for the *library*, but they require the *executable* to
  # compile them, which depends on the library! They get away with it by
  # using the old-style builds and building everything together, we can't
  # do that.
  # So we work around it:
  # - turn off the custom setup
  # - manually compile the executable (fortunately it has no extra dependencies!)
  #   and do the compilation at the end of the library derivation.
  agda-project-module-patch = {
    packages.Agda.package.buildType = lib.mkForce "Simple";
    packages.Agda.components.library.enableSeparateDataOutput = lib.mkForce true;
    packages.Agda.components.library.postInstall = ''
      # Compile the executable using the package DB we've just made, which contains
      # the main Agda library
      ghc src/main/Main.hs -package-db=$out/package.conf.d -o agda

      # Find all the files in $data
      shopt -s globstar
      files=($data/**/*.agda)
      for f in "''${files[@]}" ; do
        echo "Compiling $f"
        # This is what the custom setup calls in the end
        ./agda --no-libraries --local-interfaces $f
      done
    '';
  };


  agda-with-stdlib = agda-packages.agda.withPackages [ agda-stdlib ];


  agda-project = pkgs.haskell-nix.hackage-project {
    name = "Agda";
    version = "2.6.4";
    compiler-nix-name = "ghc96";
    cabalProjectLocal = "extra-packages: ieee754, filemanip";
    modules = [ agda-project-module-patch ];
  };


  shell-hook-exports = ''
    export AGDA_STDLIB_SRC="${agda-stdlib}/src"
    export PLUTUS_METHATHEORY_SRC="${../plutus-metatheory/src}"
  '';


  wrap-program-args = ''
    --set AGDA_STDLIB_SRC "${agda-stdlib}/src" \
    --set PLUTUS_METHATHEORY_SRC "${../plutus-metatheory/src}"
  '';
}
