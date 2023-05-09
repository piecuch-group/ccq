import pytest

from utils import check, form_tests

CC_TEST_LARGE = {
        "h2o_vdz_1.0": {
            "reference_energy": -76.024038511720,
            "methods": {
                #"CCSDt": -75.952052348049,
                "CADFCIQMC": {
                    "total_energy": -76.241932775249,
                    "tolerance": 1e-8,
                    },
                "DCSD-MC": {
                    "total_energy": -76.2418790528234,
                    "tolerance": 1e-8,
                    },
                #"CCT3": -75.9530560282
                }
            },

        "h2o_vdz_2.0": {
            "reference_energy": -75.587711249380,
            "methods": {
                "CCSDt": {
                    "total_energy": -75.952052348038,
                    "tolerance": 1e-8,
                    },
                "CADFCIQMC": {
                    "total_energy": -75.943180331898,
                    "tolerance": 1e-8,
                    },
                #"DCSD-MC": -76.241933,
                # This is GAMESS value, its off by 1e-8 from
                # the new value. Might be tolerance?
                #"CCT3": -75.9530560282
                "CCT3": {
                    "total_energy": -75.953056042281,
                    "tolerance": 1e-8,
                    },
                }
            },

        "f2_vdz_2.0": {
            "reference_energy": -198.420096395020,
            "methods": {
                "CCSD": {
                    "total_energy": -199.012562579950,
                    "tolerance": 1e-8,
                    },
                }
            },
        }

@pytest.mark.parametrize("mol,method,energies",
        form_tests(CC_TEST_LARGE))
def test_molecules(mol, method, energies):
    check(mol, method, energies)
