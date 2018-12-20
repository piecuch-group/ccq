FC := gfortran
FFLAGS := -O3 -fdefault-integer-8 -m64 -I${MKLROOT}/include -ffree-line-length-none -ffixed-line-length-132 -mtune=native
FFLAGS_DEBUG := -O0 -fcheck=all -fdefault-integer-8 -m64 -I${MKLROOT}/include -fbacktrace -g -mtune=native -ffixed-line-length-132 -ffree-line-length-none
LDFLAGS := -Wl,--start-group ${MKLROOT}/lib/intel64/libmkl_gf_ilp64.a ${MKLROOT}/lib/intel64/libmkl_gnu_thread.a ${MKLROOT}/lib/intel64/libmkl_core.a -Wl,--end-group -lgomp -lpthread -lm -ldl -luuid

@LIOMP := /opt/intel/compilers_and_libraries_2017.4.196/linux/compiler/lib/intel64_lin/libiomp5.a
@FC := ifort
@FFLAGS := -O3 -i8 -I${MKLROOT}/include -132  -xCORE-AVX512
@LDFLAGS := -static-intel -Wl,--start-group ${MKLROOT}/lib/intel64/libmkl_intel_ilp64.a ${MKLROOT}/lib/intel64/libmkl_intel_thread.a ${MKLROOT}/lib/intel64/libmkl_core.a -Wl,--end-group ${LIOMP} -lpthread -lm -ldl -luuid

CC := gcc

cc_SRCS := $(wildcard cc/*.f cc/*.f90)
main_SRCS := $(wildcard *.f *.f90 *.F90)
ext_cor_SRCS := $(wildcard ext_cor/*.f90)

cc_OBJS := $(cc_SRCS:.f=.o)
cc_OBJS := $(cc_OBJS:.f90=.o)
main_OBJS := $(main_SRCS:.f=.o)
main_OBJS := $(main_OBJS:.f90=.o)
main_OBJS := $(main_OBJS:.F90=.o)
ext_cor_OBJS := $(ext_cor_SRCS:.f90=.o)

PROGRAM := ../bin/ccq

TEST_DIR := ''
EXE := $(PWD)/$(PROGRAM)

COMP_HOST := $(shell hostname --fqdn)
COMP_TIME := $(shell date)
GIT_SHA1 := $(shell git rev-parse HEAD 2> /dev/null || echo "not available.")
# Append -dirty to SHA1 if source code contains changes.
GIT_SHA1 := $(GIT_SHA1)$(shell test -z "$$(git status --porcelain 2>/dev/null)" || echo -dirty)

.PHONY: all debug test clean printing.o

all: $(PROGRAM)

debug: FFLAGS = $(FFLAGS_DEBUG)
debug: $(PROGRAM)
#debug: ccq.so

test: $(PROGRAM)
	cd $(TEST_DIR) && rm h8.out && $(EXE) h8.gjf && ./check.sh

$(PROGRAM): $(cc_OBJS) $(main_OBJS) $(ext_cor_OBJS) gen_uuid.o
	$(FC) $^ -o $@ $(LDFLAGS)

gen_uuid.o: gen_uuid.c
	$(CC) -c -o $@ $<

# Dependencies
main.o: integrals.o solve_cc.o

cc/antisym.o main.o update_cc.o system.o solve_cc.o parser.o utils.o: const.o

main.o diis.o solve_cc.o parser.o: printing.o

main.o: parser.o

solve_cc.o: diis.o utils.o update_cc.o ext_cor/cluster_analysis.o

main.o solve_cc.o diis.o integrals.o parser.o printing.o update_cc.o: system.o

update_cc.o: cc/antisym.o

# Cluster analysis
ext_cor/cluster_analysis.o: ext_cor/process_ci.o ext_cor/symmetry.o ext_cor/sys_data.o ext_cor/utilities.o

ext_cor/process_ci.o: printing.o

ext_cor/utilities.o: ext_cor/sys_data.o


# Object compiling
printing.o: printing.F90
	$(FC) -c $(FFLAGS) -DCOMP_HOST="'$(COMP_HOST)'" -DCOMP_TIME="'$(COMP_TIME)'" -DVERSION="'$(GIT_SHA1)'" -DFLAGS="'$(FFLAGS)'" -o $@ $<

%.o: %.f90
	$(FC) -c $(FFLAGS) -o $@ $<

%.o: %.f
	$(FC) -c $(FFLAGS) -o $@ $<

%.mod: ;

clean:
	@echo Cleaning...
	rm -f *.a
	rm -f *.mod */*.mod
	rm -f $(cc_OBJS) $(main_OBJS) $(ext_cor_OBJS) $(PROGRAM)
	@echo Done.
