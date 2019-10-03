# Build the ccq_py python module
MODULE := ccq_py
EXT_SUFFIX := $(shell python -c "import sysconfig; print(sysconfig.get_config_var('EXT_SUFFIX'))")
TARGET := $(MODULE)$(EXT_SUFFIX)

SRC := compute.f90
LDFLAGS :=
LIBS := ../../bin/libccq.a -L${MKLROOT}/lib/intel64 -lmkl_gf_ilp64 -lmkl_gnu_thread -lmkl_core -lmkl_avx2 -lgomp -lpthread -lm -ldl -luuid

.PHONY: clean

$(TARGET): $(SRC)
	f2py -I../../build -c $(SRC) -m $(MODULE) $(LIBS)

clean:
	rm -f $(TARGET)

