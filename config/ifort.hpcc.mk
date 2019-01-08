# Compilers
CC := gcc
FC := ifort
LD := $(FC)

# Flags
F90_MOD_FLAG := -module
CPPFLAGS := -DUSE_POPCNT

FFLAGS := -O3 -i8 -I${MKLROOT}/include -132  -xCORE-AVX2
FFLAGS_DEBUG := -O0 -g -traceback -i8 -I${MKLROOT}/include -132  -xCORE-AVX2

# Linking
LIOMP := /opt/software/ifort/2018.3.222-GCC-7.3.0-2.30/compilers_and_libraries_2018.3.222/linux/compiler/lib/intel64_lin/libiomp5.a
LDFLAGS := -static-intel
LIBS := -static-intel -Wl,--start-group ${MKLROOT}/lib/intel64/libmkl_intel_ilp64.a ${MKLROOT}/lib/intel64/libmkl_intel_thread.a ${MKLROOT}/lib/intel64/libmkl_core.a -Wl,--end-group ${LIOMP} -lpthread -lm -ldl -luuid

