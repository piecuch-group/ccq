from pathlib import Path
import re
import subprocess as sp

import pytest


CONFIG_PATH = Path("../config.mk")


def get_enabled_methods(config_path):
    out = dict()
    res = re.search(r"-DDISABLE_OPT_T3", config_path.read_text())
    if res:
        out["CCSDt"] = False
        out["CCT3"] = False
    return out


ENABLED_METHODS = get_enabled_methods(CONFIG_PATH)


def form_tests(tests):
    """Form tests"""

    res = []
    for mol, vals in tests.items():
        ref = vals['reference_energy']
        for method, pars in vals['methods'].items():
            energy = pars['total_energy']
            tol = pars['tolerance']

            res.append((mol, method, (ref, energy, tol)))

    return res


def run_ccq(method, input_file):
    """Run ccq with test input"""

    out = sp.check_output(["../bin/ccq", input_file])
    out = out.decode('utf-8').split("\n")
    out.reverse()
    for line in out:
        if method in line or "CC" in line:
            fields = line.split()
            res = float(fields[1])
        elif "Reference" in line:
            fields = line.split()
            return float(fields[1]), res


def fetch_energy(input_dir, method):
    """Parse the output energy from the output file"""

    input_file = "{}/{}.inp".format(input_dir, method)

    ref, energy = run_ccq(method, input_file)
    return ref, energy


def tol_diff(target, test, tol):
    """Compare test energy with target"""

    return abs(target - test) < tol


def check(mol, method, energies):
    """Perform the assertions"""

    if not ENABLED_METHODS.get(method, True):
        pytest.skip("method not compiled")

    # Run test
    ref, energy = fetch_energy(mol, method)

    assert tol_diff(energies[0], ref, energies[2]), \
            "Ref difference is {:.3e} in {} {}".format(energies[0] - ref,
                    mol, method)

    assert tol_diff(energies[1], energy, energies[2]), \
            "Energy difference is {:.3e} in {} {}".format(energies[1] - energy,
                    mol, method)
