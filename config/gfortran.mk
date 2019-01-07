FC := gfortran
FFLAGS := -O3 -fdefault-integer-8 -m64 -I${MKLROOT}/include -ffree-line-length-none -ffixed-line-length-132 -mtune=native
FFLAGS_DEBUG := -O0 -fcheck=all -fdefault-integer-8 -m64 -I${MKLROOT}/include -fbacktrace -g -mtune=native -ffixed-line-length-132 -ffree-line-length-none -mpopcnt
LDFLAGS := -Wl,--start-group ${MKLROOT}/lib/intel64/libmkl_gf_ilp64.a ${MKLROOT}/lib/intel64/libmkl_gnu_thread.a ${MKLROOT}/lib/intel64/libmkl_core.a -Wl,--end-group -lgomp -lpthread -lm -ldl -luuid
FEATURES := -DUSE_POPCNT

CC := gcc
