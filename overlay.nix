final: prev: {
  mill = prev.mill.overrideAttrs (oldAttrs: rec {
    version = "0.11.1";
    src = prev.fetchurl {
      url = "https://github.com/com-lihaoyi/mill/releases/download/${version}/${version}-assembly";
      hash = "sha256-qG+Ddn0BHUZX1VX5hO84exgRz8YuUgYF/fH6MmgkrXE=";
    };
  });
  riscvTests = final.pkgsCross.riscv64-embedded.stdenv.mkDerivation rec {
    pname = "riscv-tests";
    version = "f2f748ebb9cf8ea049103f85c4cbf7e8a2927b16";
    src = final.fetchgit {
      url = "https://github.com/riscv-software-src/riscv-tests.git";
      rev = "${version}";
      fetchSubmodules = true;
      sha256 = "sha256-E3RfrP+PFIYy9c/pY04jYPxeGpnfgWwjV8iwL5+s+9w=";
    };

    enableParallelBuilding = true;

    configureFlags = [
	  # to match rocket-tools path
      "--prefix=${placeholder "out"}/riscv64-unknown-elf"
    ];
    buildPhase = "make RISCV_PREFIX=riscv64-none-elf-";
    installPhase = ''
      runHook preInstall
      make install
      mkdir -p $out/debug/
      cp debug/*.py $out/debug/
      runHook postInstall
    '';
  };
}
