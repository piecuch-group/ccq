import numpy as np
from warnings import warn

from .api import compute

try:
    import psi4
    PSI4_AVAIL = True
except ModuleNotFoundError:
    #warn("psi4 is not installed.", RuntimeWarning)
    PSI4_AVAIL = False

try:
    import pyscf
    PYSCF_AVAIL = True
except ModuleNotFoundError:
    #warn("pyscf is not installed.", RuntimeWarning)
    PYSCF_AVAIL = False


class Calculation:

    def __init__(self,
            froz=0,
            nel=None,
            nvirt=None,
            mult=None,
            en_repul=None,
            calc_type=None,
            onebody_file='',
            twobody_file='',
            rhf=False,
            conv_tol=1e-6,
            backend=None):

        self.backend = backend

        self.nel = nel
        self.nvirt = nvirt
        self.mult = mult
        self.calc_type = calc_type

        assert calc_type, \
                "Calculation type must be provided"

        # setup ccq
        compute.configure(froz,
                nel,
                nvirt,
                onebody_file,
                twobody_file,
                rhf,
                conv_tol,
                calc_type)

    def run(self):

        ccq_res = compute.run_calculation()

        self.ref_en = ccq_res[0]
        self.corr_en = ccq_res[1]
        self.total_en = ccq_res[2]

        return ccq_res




class Psi4Calculation(Calculation):

    def __init__(self, wfn, **kwargs):

        # fail if PSI4 is not installed
        assert PSI4_AVAIL, \
                "psi4 is not installed"

        # setup psi4
        self.mol = wfn.molecule()
        self.wfn = wfn

        # grab Psi4 data
        occ_a = wfn.doccpi()[0]
        norb = wfn.nmo()
        en_repul = self.mol.nuclear_repulsion_energy()

        nel = 2 * occ_a
        nvirt = 2 * norb - nel


        super().__init__(nel=nel,
                nvirt=nvirt,
                en_repul=en_repul,
                mult=0,
                onebody_file='',
                twobody_file='',
                **kwargs)

        # compute one- and twobody arrays
        self.z = self._get_z()
        self.v = self._get_v()

        compute.set_ints(self.z, self.v, en_repul)


    def _get_z(self):

        wfn = self.wfn
        mints = psi4.core.MintsHelper(wfn.basisset())

        z = np.asarray(mints.ao_kinetic()) + \
                np.asarray(mints.ao_potential())

        c = wfn.Ca()

        z = np.einsum('uj,vi,uv', c, c, z)

        return z

    def _get_v(self):

        wfn = self.wfn
        mints = psi4.core.MintsHelper(wfn.basisset())

        c = wfn.Ca()
        v = np.asarray(mints.mo_eri(c, c, c, c))

        v = np.einsum('iajb->ijab', v)

        return v


class PySCFCalculation(Calculation):

    def __init__(self,
            mol=None,
            wfn=None,
            **kwargs):

        # fail if PSI4 is not installed
        assert PYSCF_AVAIL, \
                "pyscf is not installed"

        self.mol = mol
        self.wfn = wfn


        # grab pyscf data
        nel = mol.nelectron
        norb = len(wfn.mo_occ)
        nvirt = 2 * norb - nel
        en_repul = mol.energy_nuc()
        nocc = nel // 2

        if not kwargs:
            kwargs = {}


        super().__init__(nel=nel,
                nvirt=nvirt,
                en_repul=en_repul,
                mult=0,
                onebody_file='',
                twobody_file='',
                **kwargs)

        # compute one- and twobody arrays
        self.z = self._get_z()
        self.v = self._get_v(nocc, norb)

        compute.set_ints(self.z, self.v, en_repul)


    def _get_z(self):

        wfn = self.wfn

        z = wfn.get_hcore()
        c = wfn.mo_coeff

        z = np.einsum('mi,mn,nj', c, z, c)

        return z

    def _get_v(self, nocc, norb):

        wfn = self.wfn
        c = wfn.mo_coeff
        eri = pyscf.ao2mo.incore.full(wfn._eri, c)

        v = np.zeros((norb, norb, norb, norb))

        ij = 0
        outbuf = np.empty((norb, norb, norb))
        for i in range(nocc):
            buf = pyscf.lib.unpack_tril(eri[ij:ij+i+1],
                    out=outbuf[:i+1])
            for j in range(i+1):
                v[i,j,:,:] = buf[j,:,:]
                v[j,i,:,:] = buf[j,:,:]

            ij += i + 1

        for i in range(nocc, norb):
            buf = pyscf.lib.unpack_tril(eri[ij:ij+i+1], out=outbuf[:i+1])
            v[:nocc,i,:,:] = buf[:nocc,:,:]
            v[i,:nocc,:,:] = buf[:nocc,:,:]
            for j in range(nocc, i+1):
                v[i,j,:,:] = buf[j,:,:]
                v[j,i,:,:] = buf[j,:,:]

            ij += i + 1

        v = np.einsum("iajb->ijab", v)

        return v
