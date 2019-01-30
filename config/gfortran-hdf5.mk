# Compilers
CC := gcc
FC := gfortran
LD := $(FC)

# Flags
F90_MOD_FLAG := -J
CPPFLAGS := -DUSE_POPCNT -DENABLE_HDF5

FFLAGS := -O3 -fdefault-integer-8 -m64 -I${MKLROOT}/include -I/usr/include -ffree-line-length-none -ffixed-line-length-132 -mtune=native
FFLAGS_DEBUG := -O0 -fcheck=all -fdefault-integer-8 -m64 -I${MKLROOT}/include -I/usr/include -fbacktrace -mtune=native -ffixed-line-length-132 -ffree-line-length-none -mpopcnt -g

# Linking
LDFLAGS :=
LIBS := -Wl,--start-group ${MKLROOT}/lib/intel64/libmkl_gf_ilp64.a ${MKLROOT}/lib/intel64/libmkl_gnu_thread.a ${MKLROOT}/lib/intel64/libmkl_core.a -Wl,--end-group -lgomp -lpthread -lm -ldl -luuid -lhdf5_fortran

