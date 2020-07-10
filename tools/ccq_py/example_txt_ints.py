import sys
import numpy as np

from ccq_py import compute

import warnings

warnings.filterwarnings("ignore")

GAMESS_RHF = -1.8582409575
GAMESS_MO_COEFF = np.array([[0.313255,   0.634493,    0.634493,    1.090201],
                  [0.313255, -0.634493,  0.634493, -1.090201],
                  [0.313255,  0.634493, -0.634493, -1.090201],
                  [0.313255, -0.634493, -0.634493,  1.090201]])


# Get molecule parameters
nel = 4
norb = 4
nvirt = 2*norb - nel
nocc = nel // 2

# Initialize CCQ
compute.configure(0, nel, nvirt, "onebody.inp", "twobody.inp", False, 1e-10, "CCSDT")

# Run CCQ
hf_energy, correlation_energy, total_energy = compute.run_calculation()
str_fmt = "{:<16} {:>25.10f}"
print(str_fmt.format("HF energy", hf_energy))
print(str_fmt.format("Correlation", correlation_energy))
print(str_fmt.format("Total energy", total_energy))
