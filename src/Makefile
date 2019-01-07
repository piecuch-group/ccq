include ../config.mk

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
BIN_DIR := ../bin

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

test: $(PROGRAM)
	cd $(TEST_DIR) && rm h8.out && $(EXE) h8.gjf && ./check.sh

$(PROGRAM): $(cc_OBJS) $(main_OBJS) $(ext_cor_OBJS) gen_uuid.o | $(BIN_DIR)
	$(FC) $^ -o $@ $(LDFLAGS)

gen_uuid.o: gen_uuid.c
	$(CC) -c -o $@ $<

# Dependencies
cc/antisym.o basis_types.o bit_utils.o checking.o determinants.o dgemm.o diis.o excitations.o integrals.o calc_driver.o main.o parser.o printing.o solve_cc.o system.o update_cc.o cc_utils.o utils.o: const.o

bit_utils.o: bit_table_256.o

main.o: integrals.o calc_driver.o

printing.o: system.o

ext_cor/cluster_analysis.o solve_cc.o: energy.o

calc_driver.o energy.o proc_pointers.o ext_cor/ext_cor.o determinants.o integrals.o update_cc.o cc_utils.o: system.o

calc_driver.o: cc_utils.o ext_cor/cluster_analysis.o solve_cc.o

determinants.o: bit_utils.o bit_table_256.o

main.o: parser.o

solve_cc.o: diis.o cc_utils.o update_cc.o ext_cor/cluster_analysis.o

ext_cor/symmetry.o diis.o solve_cc.o parser.o: printing.o

update_cc.o: cc/antisym.o

system.o: basis_types.o ext_cor/ext_cor_types.o

excitations.o: basis_types.o

basis_types.o: checking.o

excitations.o: checking.o bit_utils.o

checking.o: utils.o

utils.o: errors.o

# Cluster analysis
ext_cor/utilities.o: system.o

ext_cor/cluster_analysis.o: ext_cor/process_ci.o ext_cor/symmetry.o ext_cor/utilities.o ext_cor/t4_generation.o ext_cor/ext_cor.o

ext_cor/directed_t4_analysis.o: ext_cor/ext_cor_types.o excitations.o

ext_cor/t4_generation.o: combinatorics.o checking.o determinants.o ext_cor/symmetry.o

ext_cor/process_ci.o: printing.o ext_cor/directed_t4_analysis.o ext_cor/symmetry.o determinants.o ext_cor/utilities.o


# Object compiling
printing.o: printing.F90
	$(FC) -c $(FFLAGS) -DCOMP_HOST="'$(COMP_HOST)'" -DCOMP_TIME="'$(COMP_TIME)'" -DVERSION="'$(GIT_SHA1)'" -DFLAGS="'$(FFLAGS)'" -o $@ $<

bit_utils.o: bit_utils.F90
	$(FC) -c $(FFLAGS) $(FEATURES) -o $@ $<

%.o: %.F90
	$(FC) -c $(FFLAGS) -o $@ $<

%.o: %.f90
	$(FC) -c $(FFLAGS) -o $@ $<

%.o: %.f
	$(FC) -c $(FFLAGS) -o $@ $<

%.mod: ;

$(BIN_DIR):
	mkdir -p $@

clean:
	@echo Cleaning...
	rm -f *.a
	rm -f *.mod */*.mod
	rm -f $(cc_OBJS) $(main_OBJS) $(ext_cor_OBJS) $(PROGRAM) gen_uuid.o
	@echo Done.
