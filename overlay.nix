final: prev: {
  riscvTests = final.pkgsCross.riscv64-embedded.stdenv.mkDerivation rec {
    pname = "riscv-tests";
    version = "55bbcc8c06637a31cc01970881ba8072838a9121";
    src = final.fetchgit {
      url = "https://github.com/riscv-software-src/riscv-tests.git";
      rev = "${version}";
      fetchSubmodules = true;
      sha256 = "sha256-TcIU+WFQxPqAG7lvfKPgHm4CnBpTkosqe+fYOxS+J7I=";
    };

    enableParallelBuilding = true;

    configureFlags = [
	  # to match rocket-tools path
      "--prefix=${placeholder "out"}/riscv64-unknown-elf"
    ];
    buildPhase = "make RISCV_PREFIX=riscv64-none-elf-";
  };
}
