from pathlib import Path
import re
import subprocess as sp

import pytest


CONFIG_PATH = Path("../config.mk")


def get_enabled_methods(config_path):
    enabled_methods = dict()
    disable_optimized_t3 = re.search(r"-DDISABLE_OPT_T3", config_path.read_text())
    if disable_optimized_t3:
        enabled_methods["CCSDt"] = False
        enabled_methods["CCT3"] = False
    return enabled_methods


ENABLED_METHODS = get_enabled_methods(CONFIG_PATH)


def form_tests(tests):
    """Build tests tuple to parametrize."""
    test_parameters = []
    for molecule, values in tests.items():
        ref = values["reference_energy"]
        for method, pars in values["methods"].items():
            energy = pars["total_energy"]
            tol = pars["tolerance"]

            test_parameters.append((molecule, method, (ref, energy, tol)))

    return test_parameters


def run_ccq(method, input_file):
    """Run ccq with test input"""

    execution_output = (
        sp.check_output(["../bin/ccq", input_file])
        .decode("utf-8")
        .split("\n")
    )

    ref_energy, cc_energy = 0.0, 0.0

    for line in reversed(execution_output):
        if method in line or "CC" in line:
            method, energy = line.split()
            cc_energy = float(energy)

        elif "Reference" in line:
            method, energy = line.split()
            ref_energy = float(energy)
            break

    return ref_energy, cc_energy


def fetch_energy(input_dir, method):
    """Parse the output energy from the output file"""

    input_file = "{}/{}.inp".format(input_dir, method)
    ref, energy = run_ccq(method, input_file)
    return ref, energy


def difference_tolerance(target, test, tol):
    """Compare test energy with target"""

    return abs(target - test) < tol


def check(mol, method, energies):
    """Perform the assertions"""

    if not ENABLED_METHODS.get(method, True):
        pytest.skip("method not compiled")

    # Run test
    ref, energy = fetch_energy(mol, method)

    assert difference_tolerance(
        energies[0], ref, energies[2]
    ), "Ref difference is {:.3e} in {} {}".format(energies[0] - ref, mol, method)

    assert difference_tolerance(
        energies[1], energy, energies[2]
    ), "Energy difference is {:.3e} in {} {}".format(energies[1] - energy, mol, method)
