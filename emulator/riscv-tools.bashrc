# Remove any old tools from PATH:
export PATH="$(perl -e 'print join(":", grep { ! m{^/work/tools/riscv-tools/[0-9a-f]+/bin$} } split(/:/, $ENV{PATH}))')"

# Add latest tools to RISCV and PATH:

export RISCV=/work/tools/riscv-tools/$(cat $(dirname $(readlink -f ${BASH_SOURCE[0]}))/../../rocket-chip/riscv-tools.hash)
export VAGCC=/work/tools/vector_assembler
export PATH=$RISCV/bin:$PATH
echo Set \$RISCV to $RISCV
echo Set \$FSGCC to $FSGCC
echo Set \$VAGCC to $VAGCC
