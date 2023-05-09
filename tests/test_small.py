import pytest

from utils import check, form_tests

CC_TEST_SMALL = {
        "h8_huzinaga_0.0001": {
            "reference_energy": -4.065562674280,
            "methods": {
                "CCSD": {
                    "total_energy": -4.1997673790549435,
                    "tolerance": 1e-10,
                    },
                "CCSDt": {
                    "total_energy": -4.211118991054,
                    "tolerance": 1e-10,
                    },
                "CCSDT": {
                    "total_energy": -4.21316509560829,
                    "tolerance": 1e-10,
                    },
                #"stoch-CC": {
                #    "total_energy": -4.21280623739618,
                #    "tolerance": 1e-10,
                #    },
                }
            },

        "h6_sto6g_2.5": {
            "reference_energy": -2.166274535400,
            "methods": {
                "CADFCIQMC": {
                    "total_energy": -3.243977779403,
                    "tolerance": 1e-10,
                    },
                "DCSD-MC": {
                    "total_energy": -2.848602651955,
                    "tolerance": 1e-10,
                    },
                }
            },

        "h4_sto3g": {
            # [TODO], find out the geometry of these numbers
            "reference_energy": -1.858240957484,
            "methods": {
                "CCSDT": {
                    "total_energy": -1.978696408825,
                    "tolerance": 1e-10,
                    },
                "CCSDTQ": {
                    "total_energy": -1.975861644132,
                    "tolerance": 1e-10,
                    },
                "CADFCIQMC": {
                    "total_energy": -1.975861644132,
                    "tolerance": 1e-10,
                    },
                }
            },

        "h2o_431g_1.5": {
            "reference_energy": -75.705469198840,
            "methods": {
                "CCSD": {
                    "total_energy": -75.900373816605,
                    "tolerance": 1e-10,
                    },
                }
            },
        }



@pytest.mark.parametrize("mol,method,energies",
        form_tests(CC_TEST_SMALL))
def test_molecules(mol, method, energies):
    check(mol, method, energies)
