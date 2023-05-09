import sys
import numpy as np

from pyscf import gto, scf, lib, ao2mo
import ccq_py

import warnings

warnings.filterwarnings("ignore")

GAMESS_RHF = -1.8582409575
GAMESS_MO_COEFF = np.array([[0.313255,   0.634493,    0.634493,    1.090201],
                  [0.313255, -0.634493,  0.634493, -1.090201],
                  [0.313255,  0.634493, -0.634493, -1.090201],
                  [0.313255, -0.634493, -0.634493,  1.090201]])


# H4 MINI test
# ------------

# Square H4 model
geom = [['H', (-1.0, -1.0, 0.0)],
        ['H', (1.0, -1.0, 0.0)],
        ['H', (-1.0, 1.0, 0.0)],
        ['H', (1.0, 1.0, 0.0)]]

# Huzinaga MINI basis
basis = {'H': gto.basis.parse('''
H    S
      4.5003800               0.070479937812
      0.6812770               0.407889640099
      0.1513740               0.647669428530
''')}


# Build molecule
mol = gto.M(atom=geom,
        basis=basis,
        unit='bohr',
        symmetry=False,
        verbose=0)

# Run SCF
mf = scf.RHF(mol)
mf.kernel()

# Create 1e density matrix with GAMESS MO coefficients. This
# is required because pyscf will not converge to the right solution
dm = mf.make_rdm1(GAMESS_MO_COEFF, mf.mo_occ)

# Run SCF again with GAMESS initial guess
mf = scf.RHF(mol)
mf.kernel(dm)

# Get nuclear repulsion energy
nuc = mol.energy_nuc()

calc = ccq_py.PySCFCalculation(mol=mol, wfn=mf, calc_type="CCSD")
res = calc.run()
print(res)
