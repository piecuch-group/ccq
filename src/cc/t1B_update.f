       subroutine t1b_update(n0,n1,n2,n3,k1,k2,k3,k4,lvl_t,shift,v1b,
     & fockr,fockb,intr,intb,intm,t1a,t1b,t2a,t2b,t2c,t3a,t3b,t3c,t3d)
!     & t4a,t4b,t4c,t4d,t4e)
c
       integer a,b,c,e,f,g,h,i,j,k,m,n,o,p
       logical lvl_t
       real*8 shift,pp,coeleft
       real*8 fockr(n3,n3)
       real*8 fockb(n3,n3)
       real*8 intr(n0+1:n3,n0+1:n3,n0+1:n3,n0+1:n3)
       real*8 intb(n0+1:n3,n0+1:n3,n0+1:n3,n0+1:n3)
       real*8 intm(n0+1:n3,n0+1:n3,n0+1:n3,n0+1:n3)
       real*8 t1a(n1+1:n3,n0+1:n1)
       real*8 t1b(n2+1:n3,n0+1:n2)
       real*8 t2a(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1)
       real*8 t2b(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1)
       real*8 t2c(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2)
       real*8 t3a(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1)
       real*8 t3b(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1)
       real*8 t3c(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1)
       real*8 t3d(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2)
!     & n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1)
!       real*8 t4b(n2+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,
!     & n0+1:n2,n0+1:n1,n0+1:n1,n0+1:n1)
!       real*8 t4c(n2+1:n3,n2+1:n3,n1+1:n3,n1+1:n3,
!     & n0+1:n2,n0+1:n2,n0+1:n1,n0+1:n1)
!       real*8 t4d(n2+1:n3,n2+1:n3,n2+1:n3,n1+1:n3,
!     & n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n1)
!       real*8 t4e(n2+1:n3,n2+1:n3,n2+1:n3,n2+1:n3,
!     & n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n2)
       real*8 v1b(n2+1:n3,n0+1:n2)
c
       real*8,allocatable::b1(:,:)
       real*8,allocatable::b2(:,:)
       real*8,allocatable::d1(:,:,:,:)
       real*8,allocatable::d2(:,:,:,:)
       real*8,allocatable::f2(:,:,:,:,:,:)
!       real*8,allocatable::h2(:,:,:,:,:,:,:,:)
c
       real*8,allocatable::q14(:,:)
       real*8,allocatable::q16(:,:)
       real*8,allocatable::q18(:,:)
       real*8,allocatable::q20(:,:)
       real*8,allocatable::q40(:,:)
       real*8,allocatable::q22(:,:)
       real*8,allocatable::q24(:,:)
       real*8,allocatable::q26(:,:)
       real*8,allocatable::q28(:,:)
       real*8,allocatable::q30(:,:)
       real*8,allocatable::q32(:,:)
       real*8,allocatable::q34(:,:)
       real*8,allocatable::q36(:,:)
       real*8,allocatable::q38(:,:)
       real*8,allocatable::q42(:,:)
       real*8,allocatable::z1(:,:)
       real*8,allocatable::x1(:,:)
       real*8,allocatable::z2(:,:)
       real*8,allocatable::x2(:,:)
       real*8,allocatable::z3(:,:)
       real*8,allocatable::z4(:,:)
       real*8,allocatable::x3(:,:)
       real*8,allocatable::z5(:,:)
       real*8,allocatable::z6(:,:)
       real*8,allocatable::z7(:,:)
       real*8,allocatable::x4(:,:)
       real*8,allocatable::z8(:,:)
       real*8,allocatable::z9(:,:)
       real*8,allocatable::z10(:,:)
       real*8,allocatable::z11(:,:)
       real*8,allocatable::z12(:,:)
       real*8,allocatable::z13(:,:)
c
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n2,n0+1:n2))
       call reorder1324(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n0,n2,n0,n2,intm,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(q14(n0+1:n2,n0+1:n2))
       i1=k2*k2
       i3=k3*k1
       call egemm1(i1,i3,d1,b2,q14)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x1(n0+1:n2,n0+1:n2))
       x1=0.0d0
       x1=x1+q14
       deallocate(q14)
c
       allocate(d1(n0+1:n1,n1+1:n3,n2+1:n3,n2+1:n3))
       call reorder1342(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n2,n3,n2,n3,intm,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(q16(n2+1:n3,n2+1:n3))
       i1=k4*k4
       i3=k3*k1
       call egemm1(i1,i3,d1,b2,q16)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x2(n2+1:n3,n2+1:n3))
       x2=0.0d0
       x2=x2+q16
       deallocate(q16)
c
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n1,n1+1:n3))
       call reorder1324(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n0,n1,n1,n3,intr,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(q18(n0+1:n1,n1+1:n3))
       i1=k3*k1
       i3=k3*k1
       call egemm1(i1,i3,d1,b2,q18)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x3(n0+1:n1,n1+1:n3))
       x3=0.0d0
       x3=x3+q18
       deallocate(q18)
c
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n2,n2+1:n3))
       call reorder1324(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n0,n2,n2,n3,intm,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(q20(n0+1:n2,n2+1:n3))
       i1=k4*k2
       i3=k3*k1
       call egemm1(i1,i3,d1,b2,q20)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x4(n0+1:n2,n2+1:n3))
       x4=0.0d0
       x4=x4+q20
c
       allocate(b1(n2+1:n3,n0+1:n2))
       call reorder21(n0,n2,n2,n3,
     & n2,n3,n0,n2,q20,b1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(q40(n0+1:n2,n0+1:n2))
       i1=k2
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,b1,b2,q40)
       deallocate(b1)
       deallocate(b2)
       deallocate(q20)
c
       call sum21(n0,n2,n0,n2,x1,q40, 1.000)
       deallocate(q40)
c
       allocate(b1(n2+1:n3,n0+1:n2))
       call reorder12(0,n3,0,n3,
     & n2,n3,n0,n2,fockb,b1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(q22(n0+1:n2,n0+1:n2))
       i1=k2
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,b1,b2,q22)
       deallocate(b1)
       deallocate(b2)
c
       call sum21(n0,n2,n0,n2,x1,q22, 1.000)
       deallocate(q22)
c
       allocate(d1(n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder2413(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n0,n2,n0,n2,intb,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(q24(n0+1:n2,n0+1:n2))
       i1=k2*k2
       i3=k4*k2
       call egemm1(i1,i3,d1,b2,q24)
       deallocate(d1)
       deallocate(b2)
c
       x1=x1+q24
       deallocate(q24)
c
       allocate(d1(n0+1:n2,n2+1:n3,n2+1:n3,n2+1:n3))
       call reorder2431(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n2,n3,n2,n3,intb,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(q26(n2+1:n3,n2+1:n3))
       i1=k4*k4
       i3=k4*k2
       call egemm1(i1,i3,d1,b2,q26)
       deallocate(d1)
       deallocate(b2)
c
       x2=x2+q26
       deallocate(q26)
c
       allocate(d1(n0+1:n2,n0+1:n1,n1+1:n3,n2+1:n3))
       call reorder2134(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n0,n1,n1,n3,n2,n3,intm,d1)
       allocate(d2(n0+1:n2,n0+1:n1,n1+1:n3,n2+1:n3))
       call reorder3421(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n0,n1,n1,n3,n2,n3,t2b,d2)
       allocate(q28(n2+1:n3,n2+1:n3))
       i1=k4
       i2=k4
       i3=k3*k1*k2
       call egemm(i1,i2,i3,d1,d2,q28)
       deallocate(d1)
       deallocate(d2)
c
       call sum21(n2,n3,n2,n3,x2,q28,-1.000)
       deallocate(q28)
c
       allocate(d1(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
       call reorder1432(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n2,n3,n1,n3,n0,n2,intm,d1)
       allocate(d2(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
       call reorder4123(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n2,n3,n1,n3,n0,n2,t2b,d2)
       allocate(q30(n0+1:n2,n0+1:n2))
       i1=k2
       i2=k2
       i3=k3*k4*k1
       call egemm(i1,i2,i3,d1,d2,q30)
       deallocate(d1)
       deallocate(d2)
c
       call sum21(n0,n2,n0,n2,x1,q30, 1.000)
       deallocate(q30)
c
       allocate(d1(n0+1:n2,n2+1:n3,n0+1:n1,n1+1:n3))
       call reorder2413(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n0,n1,n1,n3,intm,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(q32(n0+1:n1,n1+1:n3))
       i1=k3*k1
       i3=k4*k2
       call egemm1(i1,i3,d1,b2,q32)
       deallocate(d1)
       deallocate(b2)
c
       x3=x3+q32
       deallocate(q32)
c
       call sumx12(0,n3,n0,n1,n1,n3,x3,fockr, 1.000)
c
       allocate(d2(n0+1:n1,n1+1:n3,n2+1:n3,n0+1:n2))
       call reorder4213(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n1,n3,n2,n3,n0,n2,t2b,d2)
       allocate(z5(n2+1:n3,n0+1:n2))
       i2=k2*k4
       i3=k3*k1
       call egemm2(i2,i3,x3,d2,z5)
       deallocate(d2)
c
       v1b=v1b+z5
       deallocate(z5)
       deallocate(x3)
c
       allocate(d1(n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3))
       call reorder2143(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n0,n2,n2,n3,n2,n3,intb,d1)
       allocate(d2(n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3))
       call reorder3412(n2,n3,n2,n3,n0,n2,n0,n2,
     & n0,n2,n0,n2,n2,n3,n2,n3,t2c,d2)
       allocate(q34(n2+1:n3,n2+1:n3))
       i1=k4
       i2=k4
       i3=k4*k2*k2
       call egemm(i1,i2,i3,d1,d2,q34)
       deallocate(d1)
       deallocate(d2)
c
       call sum21(n2,n3,n2,n3,x2,q34,-0.500)
       deallocate(q34)
c
       call sumx21(0,n3,n2,n3,n2,n3,x2,fockb, 1.000)
c
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(z3(n0+1:n2,n2+1:n3))
       i1=k4
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,x2,b2,z3)
       deallocate(b2)
c
       call
     & sum21(n2,n3,n0,n2,v1b,z3, 1.000)
       deallocate(z3)
       deallocate(x2)
c
       allocate(d1(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
       call reorder2431(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n2,n3,n0,n2,intb,d1)
       allocate(d2(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
       call reorder3124(n2,n3,n2,n3,n0,n2,n0,n2,
     & n0,n2,n2,n3,n2,n3,n0,n2,t2c,d2)
       allocate(q36(n0+1:n2,n0+1:n2))
       i1=k2
       i2=k2
       i3=k4*k4*k2
       call egemm(i1,i2,i3,d1,d2,q36)
       deallocate(d1)
       deallocate(d2)
c
       call sum21(n0,n2,n0,n2,x1,q36, 0.500)
       deallocate(q36)
c
       allocate(d1(n0+1:n2,n2+1:n3,n0+1:n2,n2+1:n3))
       call reorder1324(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n0,n2,n2,n3,intb,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(q38(n0+1:n2,n2+1:n3))
       i1=k4*k2
       i3=k4*k2
       call egemm1(i1,i3,d1,b2,q38)
       deallocate(d1)
       deallocate(b2)
c
       x4=x4+q38
c
       call sumx12(0,n3,n0,n2,n2,n3,x4,fockb, 1.000)
c
       allocate(d2(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
       call reorder3124(n2,n3,n2,n3,n0,n2,n0,n2,
     & n0,n2,n2,n3,n2,n3,n0,n2,t2c,d2)
       allocate(z8(n2+1:n3,n0+1:n2))
       i2=k2*k4
       i3=k4*k2
       call egemm2(i2,i3,x4,d2,z8)
       deallocate(d2)
c
       v1b=v1b+z8
       deallocate(z8)
       deallocate(x4)
c
       allocate(b1(n2+1:n3,n0+1:n2))
       call reorder21(n0,n2,n2,n3,
     & n2,n3,n0,n2,q38,b1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(q42(n0+1:n2,n0+1:n2))
       i1=k2
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,b1,b2,q42)
       deallocate(b1)
       deallocate(b2)
       deallocate(q38)
c
       call sum21(n0,n2,n0,n2,x1,q42, 1.000)
       deallocate(q42)
c
       call sumx12(0,n3,n0,n2,n0,n2,x1,fockb, 1.000)
c
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(z2(n2+1:n3,n0+1:n2))
       i1=k2
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,x1,b2,z2)
       deallocate(b2)
c
       v1b=v1b-z2
       deallocate(z2)
       deallocate(x1)
c
       allocate(d1(n0+1:n1,n1+1:n3,n2+1:n3,n0+1:n2))
       call reorder1324(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n2,n3,n0,n2,intm,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(z1(n2+1:n3,n0+1:n2))
       i1=k2*k4
       i3=k3*k1
       call egemm1(i1,i3,d1,b2,z1)
       deallocate(d1)
       deallocate(b2)
c
       v1b=v1b+z1
       deallocate(z1)
c
       allocate(d1(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
       call reorder2413(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n2,n3,n0,n2,intb,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(z4(n2+1:n3,n0+1:n2))
       i1=k2*k4
       i3=k4*k2
       call egemm1(i1,i3,d1,b2,z4)
       deallocate(d1)
       deallocate(b2)
c
       v1b=v1b+z4
       deallocate(z4)
c
       allocate(d1(n0+1:n2,n0+1:n1,n1+1:n3,n0+1:n2))
       call reorder2134(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n0,n1,n1,n3,n0,n2,intm,d1)
       allocate(d2(n0+1:n2,n0+1:n1,n1+1:n3,n2+1:n3))
       call reorder3421(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n0,n1,n1,n3,n2,n3,t2b,d2)
       allocate(z6(n2+1:n3,n0+1:n2))
       i1=k2
       i2=k4
       i3=k3*k1*k2
       call egemm(i1,i2,i3,d1,d2,z6)
       deallocate(d1)
       deallocate(d2)
c
       v1b=v1b-z6
       deallocate(z6)
c
       allocate(d1(n0+1:n1,n2+1:n3,n1+1:n3,n2+1:n3))
       call reorder1432(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n2,n3,n1,n3,n2,n3,intm,d1)
       allocate(d2(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
       call reorder4123(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n2,n3,n1,n3,n0,n2,t2b,d2)
       allocate(z7(n0+1:n2,n2+1:n3))
       i1=k4
       i2=k2
       i3=k3*k4*k1
       call egemm(i1,i2,i3,d1,d2,z7)
       deallocate(d1)
       deallocate(d2)
c
       call
     & sum21(n2,n3,n0,n2,v1b,z7, 1.000)
       deallocate(z7)
c
       allocate(d1(n0+1:n2,n0+1:n2,n2+1:n3,n0+1:n2))
       call reorder2143(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n0,n2,n2,n3,n0,n2,intb,d1)
       allocate(d2(n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3))
       call reorder3412(n2,n3,n2,n3,n0,n2,n0,n2,
     & n0,n2,n0,n2,n2,n3,n2,n3,t2c,d2)
       allocate(z9(n2+1:n3,n0+1:n2))
       i1=k2
       i2=k4
       i3=k4*k2*k2
       call egemm(i1,i2,i3,d1,d2,z9)
       deallocate(d1)
       deallocate(d2)
c
       v1b=v1b-0.500*z9
       deallocate(z9)
c
       allocate(d1(n0+1:n2,n2+1:n3,n2+1:n3,n2+1:n3))
       call reorder2431(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n2,n3,n2,n3,intb,d1)
       allocate(d2(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
       call reorder3124(n2,n3,n2,n3,n0,n2,n0,n2,
     & n0,n2,n2,n3,n2,n3,n0,n2,t2c,d2)
       allocate(z10(n0+1:n2,n2+1:n3))
       i1=k4
       i2=k2
       i3=k4*k4*k2
       call egemm(i1,i2,i3,d1,d2,z10)
       deallocate(d1)
       deallocate(d2)
c
       call
     & sum21(n2,n3,n0,n2,v1b,z10, 0.500)
       deallocate(z10)
c
      if(lvl_t)then
       allocate(d1(n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3))
       call reorder2143(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n0,n1,n1,n3,n1,n3,intr,d1)
       allocate(f2(n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3,n2+1:n3,n0+1:n2))
       call reorder562314(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n0,n1,n0,n1,n1,n3,n1,n3,n2,n3,n0,n2,t3b,f2)
       allocate(z11(n2+1:n3,n0+1:n2))
       i2=k2*k4
       i3=k3*k3*k1*k1
       call egemm2(i2,i3,d1,f2,z11)
       deallocate(d1)
       deallocate(f2)
c
       v1b=v1b+0.250*z11
       deallocate(z11)
c
       allocate(d1(n0+1:n2,n0+1:n1,n2+1:n3,n1+1:n3))
       call reorder2143(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n0,n1,n2,n3,n1,n3,intm,d1)
       allocate(f2(n0+1:n2,n0+1:n1,n2+1:n3,n1+1:n3,n2+1:n3,n0+1:n2))
       call reorder461325(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & n0,n2,n0,n1,n2,n3,n1,n3,n2,n3,n0,n2,t3c,f2)
       allocate(z12(n2+1:n3,n0+1:n2))
       i2=k2*k4
       i3=k3*k4*k1*k2
       call egemm2(i2,i3,d1,f2,z12)
       deallocate(d1)
       deallocate(f2)
c
       v1b=v1b+z12
       deallocate(z12)
c
       allocate(d1(n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3))
       call reorder2143(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n0,n2,n2,n3,n2,n3,intb,d1)
       allocate(f2(n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
       call reorder451236(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,
     & n0,n2,n0,n2,n2,n3,n2,n3,n2,n3,n0,n2,t3d,f2)
       allocate(z13(n2+1:n3,n0+1:n2))
       i2=k2*k4
       i3=k4*k4*k2*k2
       call egemm2(i2,i3,d1,f2,z13)
       deallocate(d1)
       deallocate(f2)
c
       v1b=v1b+0.250*z13
       deallocate(z13)
      endif
c
      do i=n0+1,n2
       do a=n2+1,n3
         coeleft=fockb(a,a)
     &          -fockb(i,i)
     &          +shift
         t1b(a,i)=t1b(a,i)-v1b(a,i)/coeleft
       enddo
      enddo
c
       end
