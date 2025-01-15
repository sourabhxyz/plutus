{ repoRoot, inputs, pkgs, system, lib }:

let
  project = repoRoot.nix.project;

  ghc96 = project.variants.ghc96;
  ghc96-mingwW64 = project.variants.ghc96.cross.mingwW64;
  ghc96-musl64 = project.variants.ghc96.cross.musl64;
  ghc96-profiled = project.variants.ghc96-profiled;
  ghc810 = project.variants.ghc810;

in [
  {
    inherit (project) cabalProject;
    inherit project repoRoot;

    devShells.default = ghc96.devShell;
    devShells.profiled = ghc96-profiled.devShell;
    devShells.ghc96 = ghc96.devShell;
    devShells.ghc810 = ghc810.devShell;

    packages = ghc96.packages;
    apps = ghc96.apps;

    latex-documents = repoRoot.nix.latex-documents;

    hydraJobs.required = lib.iogx.mkHydraRequiredJob { };
  }

  {
    packages.plutus-metatheory-site = repoRoot.nix.plutus-metatheory-site;
    packages.pre-commit-check = ghc96.pre-commit-check;
  }

  (lib.optionalAttrs (system == "x86_64-linux" || system == "x86_64-darwin") {
    packages.plutus-metatheory-site = repoRoot.nix.plutus-metatheory-site;

    hydraJobs.ghc96 = ghc96.hydraJobs;
    hydraJobs.ghc810 = ghc810.hydraJobs;
  })

  (lib.optionalAttrs (system == "x86_64-linux") {
    hydraJobs.latex-documents = repoRoot.nix.latex-documents;
    hydraJobs.pre-commit-check = ghc96.pre-commit-check;

    hydraJobs.mingwW64.ghc96 = ghc96-mingwW64.hydraJobs;

    hydraJobs.musl64.ghc96.pir =
      ghc96-musl64.cabalProject.hsPkgs.plutus-executables.components.exes.pir; # editorconfig-checker-disable-line
    hydraJobs.musl64.ghc96.plc =
      ghc96-musl64.cabalProject.hsPkgs.plutus-executables.components.exes.plc; # editorconfig-checker-disable-line
    hydraJobs.musl64.ghc96.uplc =
      ghc96-musl64.cabalProject.hsPkgs.plutus-executables.components.exes.uplc; # editorconfig-checker-disable-line
    hydraJobs.musl64.ghc96.plutus =
      ghc96-musl64.cabalProject.hsPkgs.plutus-core.components.exes.plutus; # editorconfig-checker-disable-line
  })

  (lib.optionalAttrs (system == "aarch64-darwin") {
    # Plausibly if things build on x86 darwin then they'll build on aarch darwin.
    # Se we only build roots and dev sshells on aarch to avoid overloading the builders.
    hydraJobs.ghc810.devShell = ghc810.devShell;
    hydraJobs.ghc96.devShell = ghc96.devShell;

    hydraJobs.ghc810.roots = ghc810.hydraJobs.roots;
    hydraJobs.ghc810.plan-nix = ghc810.hydraJobs.plan-nix;

    hydraJobs.ghc96.roots = ghc96.hydraJobs.roots;
    hydraJobs.ghc96.plan-nix = ghc96.hydraJobs.plan-nix;
  })
  {
    checks = repoRoot.nix.utils.flattenDerivationTree "garnix" "-"
      inputs.self.hydraJobs.${system};
  }
]
