ccq Coupled-Cluster Program
===========================

``ccq`` is a coupled-cluster (CC) software package capable of running calculations with up to
quadruply excited cluster components. This package includes the standard CCSD, CCSDT,
and CCSDTQ approaches, the active-space CCSDt, CCSDtq, and CCSDTq methods, and the CR-CC(2,3) and CC(t;3)
approaches based on the CC(*P* ; *Q*) hierarchy.


Building
--------

A ``BLAS`` library is required to compile this program. It is strongly advised
to use an optimized version, such as OpenBLAS or Intel's MKL.

To compile, just link or copy a configuration file in ``config/`` to
``config.mk``. Make sure that it contains the right configuration options (e.g.
compilers, libraries, etc.)

For example::

   ln -s config/gfortran.mk config.mk
   make -j4


Input file
----------


System information
^^^^^^^^^^^^^^^^^^
The configuration file must include the following information for any
calculation type

froz:
   number of frozen spatial orbitals

nel:
   number of electrons (occupied spin-orbitals)

nvir:
   number of virtual orbitals (unoccupied spin-orbitals)

multiplicity:
   spin multiplicity

onebody:
   path to the onebody.inp integral file as provided by CC_PACKAGE

twobody:
   path to the twobody.inp integral file as provided by CC_PACKAGE


Runtime options
^^^^^^^^^^^^^^^
tol:
   energy convergence tolerance. Can be an integer, in which case tol=10^tol.
   The default value is 1.0E-4.

shift:
   shift energy used to help converge certain difficult calculations. It's used
   at the end of each Jacobi iteration and shifts the denominator's value.

diis_space:
   the size of the DIIS space. Default is 5.

max_iterations:
   maximum number of iterations before exiting the calculation. Default is 60.

calc_type:
   select calcualtion type. Currently the supported calculations are CCSD,
   CCSDT, CCSDTQ, CCSDt, CC(t;3), CADFCIQMC, and DCSD-MC.

label:
   project label. Will be shown in the output. Useful to help identify the calculation.

num_threads:
   number of threads to use in the shared-memory parallel sections (default is 1)

sym_file:
   symmetry file containing the spatial symmetries of the molecular orbitals. The first
   value of the file is the point group (C1 is 1; C2v, 3; D2h, 4)

rhf:
   force a closed-shell calculation (default is .false.)


Active space options
^^^^^^^^^^^^^^^^^^^^
act_occ:
   number of active occupied spatial orbitals

act_unocc:
   number of active unoccupied spatial orbitals

act_ind_t:
   number of active indices in a triply excited cluster

act_ind_q:
   number of active indices in a quadruply excited cluster


ACC options
^^^^^^^^^^^
ACC options to scale various diagrams. All values are space separated.

t2t2_t2:
   T_2^2 projected on doubles. Takes 5 values.

t3_t2:
   T_3 projected on doubles. Takes 2 values.

t1t3_t2:
   T_1 * T_3 projected on doubles. Takes 4 values.

t2t2_t3:
   T_2^2 projected on triples. Takes 3 values.

t2t3_t3:
   T_2 * T_3 projected on triples. Takes 5 values.

Development Team
----------------

| Dr. J. Emiliano Deustua
| COO and Co-founder, Examol
| e-mail: edeustua@gmail.com
|
| Dr. Nicholas P. Bauman
| Chemist, Chemical Physics Theory Team, Pacific Northwest National Laboratory
|
| Dr. Ilias Magoulas
| Postdoctoral Research Fellow, Department of Chemistry, Emory University
|
| Dr. Jun Shen
| Senior Research Associate, Department of Chemistry, Michigan State University
|
| Professor Piotr Piecuch
| University Distinguished Professor and Michigan State University Foundation Professor, Department of Chemistry, Michigan State University  
| Adjunct Professor, Department of Physics and Astronomy, Michigan State University
