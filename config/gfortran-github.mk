# Compilers
CC := gcc
FC := gfortran
LD := $(FC)

# Flags
F90_MOD_FLAG := -J
CPPFLAGS := -DUSE_POPCNT -DDISABLE_OPT_T3

FFLAGS := -O3 -fopenmp -fdefault-integer-8 -m64 -I/usr/include/mkl -ffree-line-length-none -ffixed-line-length-132 -mtune=native
FFLAGS_DEBUG := -O0 -fopenmp -fcheck=all -fdefault-integer-8 -m64 -I/usr/include/include/mkl -fbacktrace -mtune=native -ffixed-line-length-132 -ffree-line-length-none -mpopcnt -g

# Linking
LDFLAGS :=
LIBS := -Wl,--start-group /lib/x86_64-linux-gnu/libmkl_gf_ilp64.a \
		/lib/x86_64-linux-gnu/libmkl_gnu_thread.a \
		/lib/x86_64-linux-gnu/libmkl_core.a -Wl,--end-group \
		-lgomp -lpthread -lm -ldl -luuid
