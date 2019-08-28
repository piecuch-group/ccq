# Compilers
CC := gcc
FC := ifort
LD := $(FC)

# Flags
F90_MOD_FLAG := -module
CPPFLAGS := -DUSE_POPCNT -DDISABLE_OPT_T3 -DENABLE_HDF5

FFLAGS := -O3 -i8 -I${MKLROOT}/include -132 -xCOMMON-AVX512
FFLAGS_DEBUG := -O0 -g -traceback -i8 -I${MKLROOT}/include -132 -xCORE-AVX2

# Linking
LDFLAGS := -static-intel

# MKL
LIBS := -Wl,--start-group \
${MKLROOT}/lib/intel64/libmkl_intel_ilp64.a \
${MKLROOT}/lib/intel64/libmkl_intel_thread.a \
${MKLROOT}/lib/intel64/libmkl_core.a -Wl,--end-group

# HDF5
LIBS += -Wl,--start-group ${VIRTUAL_ENV}/lib/libhdf5.a \
${VIRTUAL_ENV}/lib/libhdf5_fortran.a \
${VIRTUAL_ENV}/lib/libhdf5_hl.a \
${VIRTUAL_ENV}/lib/libhdf5hl_fortran.a \
/usr/lib/libz.a -Wl,--end-group

# IOMP5
LIBS += /opt/intel/compilers_and_libraries_2017.4.196/linux/compiler/lib/intel64_lin/libiomp5.a
# Dynamic ones
LIBS += -lpthread -lm -ldl -luuid
