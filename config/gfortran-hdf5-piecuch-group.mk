# Compilers
CC := gcc
FC := gfortran
LD := $(FC)

# Flags
F90_MOD_FLAG := -J
CPPFLAGS := -DUSE_POPCNT -DENABLE_HDF5 -DDISABLE_OPT_T3


# You also need to export the following line when using EDS compiled
# HDF5 library
# export LD_LIBRARY_PATH=/home2/edeustua/downloads/hande_build/lib:$LD_LIBRARY_PATH

EDS_HDF5_LIB := /home2/edeustua/downloads/hande_build/lib
EDS_HDF5_INC := /home2/edeustua/downloads/hande_build/include


FFLAGS := -O3 -fdefault-integer-8 -m64 -I${MKLROOT}/include -I/usr/include -I$(EDS_HDF5_INC) -ffree-line-length-none -ffixed-line-length-132 -mtune=native
FFLAGS_DEBUG := -O0 -fcheck=all -fdefault-integer-8 -m64 -I${MKLROOT}/include -I/usr/include -I$(EDS_HDF5_INC) -fbacktrace -mtune=native -ffixed-line-length-132 -ffree-line-length-none -mpopcnt -g

# Linking
LDFLAGS := -L$(EDS_HDF5_LIB)
LIBS := -Wl,--start-group ${MKLROOT}/lib/intel64/libmkl_gf_ilp64.a \
		${MKLROOT}/lib/intel64/libmkl_gnu_thread.a \
		${MKLROOT}/lib/intel64/libmkl_core.a -Wl,--end-group \
		-lgomp -lpthread -lm -ldl -luuid -lhdf5_fortran

