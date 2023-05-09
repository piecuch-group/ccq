import sys
import numpy as np
import time


import ccq_py
import psi4

import warnings

warnings.filterwarnings("ignore")

# Set memory
#psi4.set_memory('2 GB')
psi4.core.set_output_file('psi4output.dat', False)

numpy_memory = 2

# H4 MINI test
# ------------

# Square H4 model

mol = psi4.geometry("""
H -1.0 -1.0 0.0
H  1.0 -1.0 0.0
H -1.0  1.0 0.0
H  1.0  1.0 0.0
units bohr
symmetry c1
""")

psi4.basis_helper("""
cartesian
****
H     0
S   3   1.00
      4.5003800               0.070479937812
      0.6812770               0.407889640099
      0.1513740               0.647669428530
****
""", name='huzinaga')

psi4.set_options({'basis': 'huzinaga',
                  'scf_type': 'pk',
                  'mp2_type': 'conv',
                  'freeze_core': 'false',
                  'e_convergence': 1e-10,
                  'd_convergence': 1e-10})

# Run SCF
scf_e, wfn = psi4.energy('SCF', return_wfn=True)

calc = ccq_py.Psi4Calculation(wfn, calc_type="CCSD")
res = calc.run()
print(res)
