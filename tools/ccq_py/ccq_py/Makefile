# Build the ccq_py python module
MODULE := api
EXT_SUFFIX := $(shell python -c "import sysconfig; print(sysconfig.get_config_var('EXT_SUFFIX'))")
TARGET := $(MODULE)$(EXT_SUFFIX)

SRC := compute.f90
LDFLAGS :=
LIBS := ../../../bin/libccq.a -L${MKLROOT}/lib/intel64 -lmkl_gf_ilp64 -lmkl_gnu_thread -lmkl_core -lmkl_avx2 -lgomp -lpthread -lm -ldl -luuid -lhdf5_fortran

.PHONY: all clean

all: $(TARGET)

$(TARGET): $(SRC)
	f2py --f90flags="-fdefault-integer-8 -fcheck=all -fbacktrace" -I../../../build -c $(SRC) -m $(MODULE) $(LIBS)

clean:
	rm -f $(TARGET)

