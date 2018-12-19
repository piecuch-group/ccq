ccq Coupled-Cluster Program
===========================

``ccq`` is a coupled-cluster (CC) code capable of running calculations with up to
quadruple excitations. Notable methods included are CCSDT, CCSDTQ, as well as
their active-space counterparts, CCSDt, CCSDtq, and CCSDTq, and ACC-type
methods. Also, externally corrected CC is available


Installation
-----------

Make sure that the ``Makefile`` contains valid paths to the blas routine
available::

   make

Getting started
---------------

There is a folder with tests

Input file
----------

The available keywords are:

nel:
   number of electrons (occupied spin-orbitals)

nvir:
   number of virtual orbitals (unoccupied spin-orbitals)

froz:
   number of frozen spatial orbitals

tol:
   energy convergence tolerance. Can be an integer, in which case tol=10^tol.

max_iterations:
   maximum number of iterations before exiting

calc_type:
   select calcualtion type (e.g. CCSDT, CCSDTQ, etc.)

act_occ:
   number of active occupied spatial orbitals

act_unocc:
   number of active unoccupied spatial orbitals

act_ind_t:
   number of active indices in a triply excited cluster

act_ind_q:
   number of active indices in a quadruply excited cluster

