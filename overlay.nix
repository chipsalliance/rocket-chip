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
  firtool = let
    src = final.fetchFromGitHub {
      owner = "llvm";
      repo = "circt";
      rev = "firtool-1.28.0";
      sha256 = "sha256-nsLE+zXRXBUwBkkVuOGxeVecoZzsXVXrn7+5FQhdY8s=";
      fetchSubmodules = true;
    };
  in final.stdenv.mkDerivation {
    pname = "circt";
    version = "firtool-1.28.0";
    nativeBuildInputs = with final; [ cmake ninja python3 git ];
    dontUnpack = true;
    cmakeFlags = [
      "-S${src}/llvm/llvm"
      "-DLLVM_EXTERNAL_CIRCT_SOURCE_DIR=${src}"
      "-DCMAKE_BUILD_TYPE=Release"
      "-DLLVM_STATIC_LINK_CXX_STDLIB=ON"
      "-DLLVM_ENABLE_PROJECTS=mlir"
      "-DLLVM_TARGETS_TO_BUILD="
      "-DCIRCT_LLHD_SIM_ENABLED=OFF"
      "-DLLVM_ENABLE_ASSERTIONS=OFF"
      "-DLLVM_BUILD_EXAMPLES=OFF"
      "-DLLVM_INCLUDE_EXAMPLES=OFF"
      "-DLLVM_INCLUDE_TESTS=OFF"
      "-DLLVM_INSTALL_UTILS=OFF"
      "-DLLVM_ENABLE_OCAMLDOC=OFF"
      "-DLLVM_ENABLE_BINDINGS=OFF"
      "-DLLVM_CCACHE_BUILD=OFF"
      "-DLLVM_BUILD_TOOLS=ON"
      "-DLLVM_OPTIMIZED_TABLEGEN=ON"
      "-DLLVM_USE_SPLIT_DWARF=ON"
      "-DLLVM_BUILD_LLVM_DYLIB=OFF"
      "-DLLVM_LINK_LLVM_DYLIB=OFF"
      "-DLLVM_EXTERNAL_PROJECTS=circt"
      "-DBUILD_SHARED_LIBS=OFF"
    ];
    installPhase = ''
      mkdir -p $out/bin
      mv bin/firtool $out/bin/firtool
    '';
  };
}
