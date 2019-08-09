# Compilers
CC := gcc
FC := gfortran
LD := $(FC)

# Flags
F90_MOD_FLAG := -J
CPPFLAGS := -DUSE_POPCNT -DDISABLE_OPT_T3 -DENABLE_HDF5

FFLAGS := -O3 -fopenmp -fdefault-integer-8 -m64 -I${MKLROOT}/include -ffree-line-length-none -ffixed-line-length-132 -mtune=native
FFLAGS_DEBUG := -O0 -fopenmp -fcheck=all -fdefault-integer-8 -m64 -I${MKLROOT}/include -fbacktrace -mtune=native -ffixed-line-length-132 -ffree-line-length-none -mpopcnt -g -pg
#FFLAGS_DEBUG := -O0 -fopenmp -fcheck=all -fdefault-integer-8 -m64 -I${MKLROOT}/include -fbacktrace -mtune=native -ffixed-line-length-132 -ffree-line-length-none -mpopcnt -g -pg
#FFLAGS_DEBUG := -O0 -fcheck=all -fdefault-integer-8 -m64 -I${MKLROOT}/include -mtune=native -ffixed-line-length-132 -ffree-line-length-none -fbacktrace -mpopcnt -g
FFLAGS += -I/home2/edeustua/downloads/hande_build/include
FFLAGS_DEBUG += -I/home2/edeustua/downloads/hande_build/include



# Linking
LDFLAGS := -pg
LIBS := -L/home2/edeustua/downloads/hande_build/lib -Wl,--start-group ${MKLROOT}/lib/intel64/libmkl_gf_ilp64.a ${MKLROOT}/lib/intel64/libmkl_gnu_thread.a ${MKLROOT}/lib/intel64/libmkl_core.a -Wl,--end-group -lgomp -lpthread -lm -ldl -luuid -lhdf5_fortran
#LIBS := -Wl,--start-group /usr/lib64/libz.a /home2/edeustua/downloads/hande_build/lib/libhdf5_fortran.a /home2/edeustua/downloads/hande_build/lib/libhdf5.a -Wl,--end-group -Wl,--start-group ${MKLROOT}/lib/intel64/libmkl_gf_ilp64.a ${MKLROOT}/lib/intel64/libmkl_gnu_thread.a ${MKLROOT}/lib/intel64/libmkl_core.a -Wl,--end-group -lgomp -lpthread -lm -ldl -luuid

#LIBS += 
