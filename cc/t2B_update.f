       subroutine t2b_update(n0,n1,n2,n3,k1,k2,k3,k4,lvl,lvl1,lvlq,shift
     & ,v2b,ext_cor,fockr,fockb,intr,intb,intm,
     & diag1,diag2,diag3,diag4,diag5,
     & t1diag1,t1diag2,t1diag3,t1diag4,
     & dt3diag3,dt3diag4,
     & t1a,t1b,t2a,t2b,t2c,t3a,t3b,t3c,t3d,
     & t2a_mc,t2b_mc,t2c_mc)
!     & t4a,t4b,t4c,t4d,t4e)
c
       integer a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p
       character lvl*6,lvl1*6,lvlq*6
       logical ext_cor
       real diag1,diag2,diag3,diag4,diag5,factor
       real t1diag1,t1diag2,t1diag3,t1diag4
       real dt3diag3,dt3diag4
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
! variables that hold the fciqmc value of t2
       real*8 t2a_mc(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1)
       real*8 t2b_mc(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1)
       real*8 t2c_mc(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2)
!       real*8 t4a(n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,
!     & n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1)
!!!!!       real*8 t4a(k3*k3*k3*k3,k1*k1*k1*k1)
!       real*8 t4b(n2+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,
!     & n0+1:n2,n0+1:n1,n0+1:n1,n0+1:n1)
!!!!!       real*8 t4b(k4*k3*k3*k3,k2*k1*k1*k1)
!       real*8 t4c(n2+1:n3,n2+1:n3,n1+1:n3,n1+1:n3,
!     & n0+1:n2,n0+1:n2,n0+1:n1,n0+1:n1)
!!!!!       real*8 t4c(k4*k4*k3*k3,k2*k2*k1*k1)
!       real*8 t4d(n2+1:n3,n2+1:n3,n2+1:n3,n1+1:n3,
!     & n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n1)
!!!!!       real*8 t4d(k4*k4*k4*k3,k2*k2*k2*k1)
!       real*8 t4e(n2+1:n3,n2+1:n3,n2+1:n3,n2+1:n3,
!     & n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n2)
!!!!!       real*8 t4e(k4*k4*k4*k4,k2*k2*k2*k2)
       real*8 v2b(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1)
c
!       real*8,allocatable::t4a(:,:)
       real*8,allocatable::t4b(:,:,:,:,:,:,:,:)                     !ilias: if no quadruples comment out the following 3 lines
       real*8,allocatable::t4c(:,:,:,:,:,:,:,:)
       real*8,allocatable::t4d(:,:,:,:,:,:,:,:)
!       real*8,allocatable::t4e(:,:)
c
       integer ta,tb,tc,td,te                                       !ilias: if no quadruples comment out the following 2 lines
       parameter(ta=29,tb=30,tc=31,td=32,te=33)
c
       integer i1,i2
       integer,allocatable::ind1(:,:,:,:)
       integer,allocatable::ind2(:,:,:,:)
c
       real*8,allocatable::b1(:,:)
       real*8,allocatable::b1_mc(:,:)
       real*8,allocatable::b2(:,:)
       real*8,allocatable::d1(:,:,:,:)
       real*8,allocatable::d1_mc(:,:,:,:)
       real*8,allocatable::d2(:,:,:,:)
       real*8,allocatable::d2_mc(:,:,:,:)
       real*8,allocatable::f2(:,:,:,:,:,:)
       real*8,allocatable::h2(:,:,:,:,:,:,:,:)
c
       real*8,allocatable::s30(:,:,:,:)
       real*8,allocatable::s32(:,:,:,:)
       real*8,allocatable::s34(:,:,:,:)
       real*8,allocatable::s36(:,:,:,:)
       real*8,allocatable::s38(:,:,:,:)
       real*8,allocatable::q1(:,:)
       real*8,allocatable::q2(:,:)
       real*8,allocatable::s42(:,:,:,:)
       real*8,allocatable::q3(:,:)
       real*8,allocatable::s45(:,:,:,:)
       real*8,allocatable::q4(:,:)
       real*8,allocatable::s48(:,:,:,:)
       real*8,allocatable::s126(:,:,:,:)
       real*8,allocatable::s50(:,:,:,:)
       real*8,allocatable::s128(:,:,:,:)
       real*8,allocatable::s52(:,:,:,:)
       real*8,allocatable::q5(:,:)
       real*8,allocatable::s55(:,:,:,:)
       real*8,allocatable::q6(:,:)
       real*8,allocatable::s58(:,:,:,:)
       real*8,allocatable::s60(:,:,:,:)
       real*8,allocatable::s138(:,:,:,:)
       real*8,allocatable::s62(:,:,:,:)
       real*8,allocatable::q25(:,:)
       real*8,allocatable::s130(:,:,:,:)
       real*8,allocatable::s64(:,:,:,:)
       real*8,allocatable::q7(:,:)
       real*8,allocatable::q26(:,:)
       real*8,allocatable::s67(:,:,:,:)
       real*8,allocatable::q27(:,:)
       real*8,allocatable::s142(:,:,:,:)
       real*8,allocatable::s140(:,:,:,:)
       real*8,allocatable::s158(:,:,:,:)
       real*8,allocatable::s134(:,:,:,:)
       real*8,allocatable::s69(:,:,:,:)
       real*8,allocatable::q8(:,:)
       real*8,allocatable::q29(:,:)
       real*8,allocatable::q28(:,:)
       real*8,allocatable::s72(:,:,:,:)
       real*8,allocatable::s74(:,:,:,:)
       real*8,allocatable::s76(:,:,:,:)
       real*8,allocatable::q9(:,:)
       real*8,allocatable::q10(:,:)
       real*8,allocatable::s80(:,:,:,:)
       real*8,allocatable::s136(:,:,:,:)
       real*8,allocatable::s82(:,:,:,:)
       real*8,allocatable::q11(:,:)
       real*8,allocatable::s85(:,:,:,:)
       real*8,allocatable::s87(:,:,:,:)
       real*8,allocatable::q12(:,:)
       real*8,allocatable::s90(:,:,:,:)
       real*8,allocatable::q13(:,:)
       real*8,allocatable::s93(:,:,:,:)
       real*8,allocatable::q14(:,:)
       real*8,allocatable::s96(:,:,:,:)
       real*8,allocatable::s152(:,:,:,:)
       real*8,allocatable::s145(:,:,:,:)
       real*8,allocatable::s98(:,:,:,:)
       real*8,allocatable::q15(:,:)
       real*8,allocatable::q30(:,:)
       real*8,allocatable::s101(:,:,:,:)
       real*8,allocatable::q31(:,:)
       real*8,allocatable::s154(:,:,:,:)
       real*8,allocatable::s103(:,:,:,:)
       real*8,allocatable::q16(:,:)
       real*8,allocatable::q32(:,:)
       real*8,allocatable::s106(:,:,:,:)
       real*8,allocatable::s106_mc(:,:,:,:)
       real*8,allocatable::q17(:,:)
       real*8,allocatable::q17_mc(:,:)
       real*8,allocatable::q18(:,:)
       real*8,allocatable::q18_mc(:,:)
       real*8,allocatable::s110(:,:,:,:)
       real*8,allocatable::s110_mc(:,:,:,:)
       real*8,allocatable::q19(:,:)
       real*8,allocatable::q19_mc(:,:)
       real*8,allocatable::q20(:,:)
       real*8,allocatable::q20_mc(:,:)
       real*8,allocatable::s114(:,:,:,:)
       real*8,allocatable::s114_mc(:,:,:,:)
       real*8,allocatable::s148(:,:,:,:)
       real*8,allocatable::q21(:,:)
       real*8,allocatable::q21_mc(:,:)
       real*8,allocatable::s117(:,:,:,:)
       real*8,allocatable::s117_mc(:,:,:,:)
       real*8,allocatable::s119(:,:,:,:)
       real*8,allocatable::s119_mc(:,:,:,:)
       real*8,allocatable::q22(:,:)
       real*8,allocatable::q22_mc(:,:)
       real*8,allocatable::q23(:,:)
       real*8,allocatable::q23_mc(:,:)
       real*8,allocatable::q24(:,:)
       real*8,allocatable::q24_mc(:,:)
       real*8,allocatable::s124(:,:,:,:)
       real*8,allocatable::s124_mc(:,:,:,:)
       real*8,allocatable::x1(:,:,:,:)
       real*8,allocatable::z1(:,:,:,:)
       real*8,allocatable::z2(:,:,:,:)
       real*8,allocatable::x2(:,:,:,:)
       real*8,allocatable::z3(:,:,:,:)
       real*8,allocatable::x3(:,:,:,:)
       real*8,allocatable::z4(:,:,:,:)
       real*8,allocatable::x4(:,:,:,:)
       real*8,allocatable::z5(:,:,:,:)
       real*8,allocatable::x5(:,:)
       real*8,allocatable::z6(:,:,:,:)
       real*8,allocatable::z6_mc(:,:,:,:)
       real*8,allocatable::x6(:,:)
       real*8,allocatable::z7(:,:,:,:)
       real*8,allocatable::z7_mc(:,:,:,:)
       real*8,allocatable::x7(:,:)
       real*8,allocatable::z8(:,:,:,:)
       real*8,allocatable::z8_mc(:,:,:,:)
       real*8,allocatable::x8(:,:)
       real*8,allocatable::z9(:,:,:,:)
       real*8,allocatable::z9_mc(:,:,:,:)
       real*8,allocatable::x9(:,:,:,:)
       real*8,allocatable::z10(:,:,:,:)
       real*8,allocatable::z10_mc(:,:,:,:)
       real*8,allocatable::x10(:,:,:,:)
       real*8,allocatable::z11(:,:,:,:)
       real*8,allocatable::z11_mc(:,:,:,:)
       real*8,allocatable::x11(:,:,:,:)
       real*8,allocatable::z12(:,:,:,:)
       real*8,allocatable::x12(:,:,:,:)
       real*8,allocatable::z13(:,:,:,:)
       real*8,allocatable::z13_mc(:,:,:,:)
       real*8,allocatable::z14(:,:,:,:)
       real*8,allocatable::x13(:,:,:,:)
       real*8,allocatable::z15(:,:,:,:)
       real*8,allocatable::x14(:,:,:,:)
       real*8,allocatable::z16(:,:,:,:)
       real*8,allocatable::z16_mc(:,:,:,:)
       real*8,allocatable::x15(:,:)
       real*8,allocatable::z17(:,:,:,:)
       real*8,allocatable::x16(:,:,:,:)
       real*8,allocatable::z18(:,:,:,:)
       real*8,allocatable::z19(:,:,:,:)
       real*8,allocatable::x17(:,:,:,:)
       real*8,allocatable::z20(:,:,:,:)
       real*8,allocatable::z21(:,:,:,:)
       real*8,allocatable::x18(:,:)
       real*8,allocatable::z22(:,:,:,:)
       real*8,allocatable::x19(:,:,:,:)
       real*8,allocatable::z23(:,:,:,:)
       real*8,allocatable::z24(:,:,:,:)
       real*8,allocatable::x20(:,:,:,:)
       real*8,allocatable::z25(:,:,:,:)
       real*8,allocatable::z26(:,:,:,:)
       real*8,allocatable::z27(:,:,:,:)
       real*8,allocatable::z28(:,:,:,:)
       real*8,allocatable::z29(:,:,:,:)
c
       factor=0
c
!       print*,'t2a'
!       do i=n0+1,n1
!         do j=n0+1,n1
!           do a=n1+1,n3
!             do b=n1+1,n3
!                 write(*,'(e19.12,2x,i2,2x,i2,2x,i2,2x,i2)'),
!     &                 t2a_mc(b,a,j,i),b,a,j,i
!             enddo
!           enddo
!         enddo
!       enddo
!c
!       print*,'t2b'
!       do i=n0+1,n1
!         do j=n0+1,n2
!           do a=n1+1,n3
!             do b=n2+1,n3
!                 write(*,'(e19.12,2x,i2,2x,i2,2x,i2,2x,i2)'),
!     &                 t2b_mc(b,a,j,i),b,a,j,i
!             enddo
!           enddo
!         enddo
!       enddo
!c
!       print*,'t2c'
!       do i=n0+1,n2
!         do j=n0+1,n2
!           do a=n2+1,n3
!             do b=n2+1,n3
!                 write(*,'(e19.12,2x,i2,2x,i2,2x,i2,2x,i2)'),
!     &                 t2c_mc(b,a,j,i),b,a,j,i
!             enddo
!           enddo
!         enddo
!       enddo
c
       allocate(d1(n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2))
       call reorder3124(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n2,n3,n0,n2,intm,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s30(n0+1:n1,n0+1:n1,n2+1:n3,n0+1:n2))
       i1=k2*k4*k1
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s30)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x1(n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n1))
       x1=0.0d0
       call sum4123(n0,n1,n2,n3,n0,n2,n0,n1,x1,s30, 1.000)
       deallocate(s30)
c
       allocate(d1(n0+1:n1,n0+1:n2,n0+1:n2,n0+1:n1))
       call reorder1243(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n0,n2,n0,n2,n0,n1,intm,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s32(n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       i1=k1*k2*k2
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s32)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x2(n0+1:n2,n1+1:n3,n0+1:n2,n0+1:n1))
       x2=0.0d0
       call sum2134(n0,n2,n1,n3,n0,n2,n0,n1,x2,s32,-1.000)
       deallocate(s32)
c
       allocate(d1(n2+1:n3,n0+1:n1,n2+1:n3,n0+1:n1))
       call reorder4123(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n1,n2,n3,n0,n1,intm,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s34(n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n1))
       i1=k1*k4*k1
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s34)
       deallocate(d1)
       deallocate(b2)
c
       call sum3124(n0,n1,n2,n3,n0,n2,n0,n1,x1,s34, 1.000)
       deallocate(s34)
c
       allocate(d1(n1+1:n3,n0+1:n2,n1+1:n3,n0+1:n2))
       call reorder3214(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n2,n1,n3,n0,n2,intm,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s36(n0+1:n1,n0+1:n2,n1+1:n3,n0+1:n2))
       i1=k2*k3*k2
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s36)
       deallocate(d1)
       deallocate(b2)
c
       call sum4123(n0,n2,n1,n3,n0,n2,n0,n1,x2,s36, 1.000)
       deallocate(s36)
c
       allocate(d1(n1+1:n3,n2+1:n3,n2+1:n3,n1+1:n3))
       call reorder3421(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n2,n3,n2,n3,n1,n3,intm,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s38(n0+1:n1,n2+1:n3,n2+1:n3,n1+1:n3))
       i1=k3*k4*k4
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s38)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x3(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n1))
       x3=0.0d0
       call sum4123(n2,n3,n2,n3,n1,n3,n0,n1,x3,s38, 1.000)
       deallocate(s38)
c
       call sumx3241(n0,n3,n2,n3,n2,n3,n1,n3,n0,n1,x3,intm, 1.000)
c
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(z4(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       i1=k1*k3*k4
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,x3,b2,z4)
       deallocate(b2)
c
       call
     & sum3124(n2,n3,n1,n3,n0,n2,n0,n1,v2b,z4, 1.000)
       deallocate(z4)
       deallocate(x3)
c
       allocate(b1(n1+1:n3,n0+1:n1))
       call reorder12(0,n3,0,n3,
     & n1,n3,n0,n1,fockr,b1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(q1(n0+1:n1,n0+1:n1))
       i1=k1
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,b1,b2,q1)
       deallocate(b1)
       deallocate(b2)
c
       allocate(x5(n0+1:n1,n0+1:n1))
       x5=0.0d0
       call sum21(n0,n1,n0,n1,x5,q1, 1.000)
       deallocate(q1)
c
       allocate(b1(n0+1:n1,n1+1:n3))
       call reorder21(0,n3,0,n3,
     & n0,n1,n1,n3,fockr,b1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(q2(n1+1:n3,n1+1:n3))
       i1=k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,b1,b2,q2)
       deallocate(b1)
       deallocate(b2)
c
       allocate(x6(n1+1:n3,n1+1:n3))
       x6=0.0d0
       call sum21(n1,n3,n1,n3,x6,q2,-1.000)
       deallocate(q2)
c
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder2413(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n0,n1,n0,n1,intr,d1)
       allocate(d2(n0+1:n1,n1+1:n3,n2+1:n3,n0+1:n2))
       call reorder4213(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n1,n3,n2,n3,n0,n2,t2b,d2)
       allocate(s42(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       i1=k1*k1
       i2=k2*k4
       i3=k3*k1
       call egemm(i1,i2,i3,d1,d2,s42)
       deallocate(d1)
       deallocate(d2)
c
       call sum2314(n0,n1,n2,n3,n0,n2,n0,n1,x1,s42, 1.000)
       deallocate(s42)
c
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1423(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n0,n1,n0,n1,intr,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(q3(n0+1:n1,n0+1:n1))
       i1=k1*k1
       i3=k3*k1
       call egemm1(i1,i3,d1,b2,q3)
       deallocate(d1)
       deallocate(b2)
c
       x5=x5-q3
       deallocate(q3)
c
       allocate(d1(n1+1:n3,n0+1:n1,n1+1:n3,n1+1:n3))
       call reorder3241(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n1,n3,n1,n3,intr,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s45(n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3))
       i1=k3*k3*k1
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s45)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x9(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       x9=0.0d0
       call sum4123(n0,n1,n1,n3,n1,n3,n0,n1,x9,s45, 1.000)
       deallocate(s45)
c
       allocate(d1(n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3))
       call reorder2341(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n1,n3,n1,n3,intr,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(q4(n1+1:n3,n1+1:n3))
       i1=k3*k3
       i3=k3*k1
       call egemm1(i1,i3,d1,b2,q4)
       deallocate(d1)
       deallocate(b2)
c
       x6=x6-q4
       deallocate(q4)
c
       allocate(d1(n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       call reorder3214(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n2,n0,n1,n0,n2,intm,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s48(n0+1:n1,n0+1:n2,n0+1:n1,n0+1:n2))
       i1=k2*k1*k2
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s48)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x10(n0+1:n2,n0+1:n1,n0+1:n2,n0+1:n1))
       x10=0.0d0
       call sum4123(n0,n2,n0,n1,n0,n2,n0,n1,x10,s48, 1.000)
c
       allocate(d1(n0+1:n1,n0+1:n2,n0+1:n2,n0+1:n1))
       call reorder3241(n0,n1,n0,n2,n0,n1,n0,n2,
     & n0,n1,n0,n2,n0,n2,n0,n1,s48,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s126(n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       i1=k1*k2*k2
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s126)
       deallocate(d1)
       deallocate(b2)
       deallocate(s48)
c
       call sum2134(n0,n2,n1,n3,n0,n2,n0,n1,x2,s126,-1.000)
       deallocate(s126)
c
       allocate(d1(n1+1:n3,n0+1:n1,n2+1:n3,n2+1:n3))
       call reorder3142(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n2,n3,n2,n3,intm,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s50(n0+1:n1,n0+1:n1,n2+1:n3,n2+1:n3))
       i1=k4*k4*k1
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s50)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x11(n0+1:n1,n2+1:n3,n2+1:n3,n0+1:n1))
       x11=0.0d0
       call sum4123(n0,n1,n2,n3,n2,n3,n0,n1,x11,s50, 1.000)
c
       call sumx1342(n0,n3,n0,n1,n2,n3,n2,n3,n0,n1,x11,intm, 1.000)
c
       allocate(d2(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
       call reorder4123(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n2,n3,n1,n3,n0,n2,t2b,d2)
       allocate(z12(n1+1:n3,n0+1:n2,n2+1:n3,n0+1:n1))
       i1=k1*k4
       i2=k2*k3
       i3=k4*k1
       call egemm(i1,i2,i3,x11,d2,z12)
       deallocate(d2)
c
       call
     & sum2314(n2,n3,n1,n3,n0,n2,n0,n1,v2b,z12,-1.000)
       deallocate(z12)
       deallocate(x11)
c
       allocate(d1(n2+1:n3,n0+1:n1,n2+1:n3,n0+1:n1))
       call reorder3241(n0,n1,n0,n1,n2,n3,n2,n3,
     & n2,n3,n0,n1,n2,n3,n0,n1,s50,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s128(n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n1))
       i1=k1*k4*k1
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s128)
       deallocate(d1)
       deallocate(b2)
       deallocate(s50)
c
       call sum3124(n0,n1,n2,n3,n0,n2,n0,n1,x1,s128, 1.000)
       deallocate(s128)
c
       allocate(d1(n0+1:n2,n1+1:n3,n0+1:n1,n0+1:n2))
       call reorder2314(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n1,n3,n0,n1,n0,n2,intm,d1)
       allocate(d2(n0+1:n2,n1+1:n3,n2+1:n3,n0+1:n1))
       call reorder3214(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n1,n3,n2,n3,n0,n1,t2b,d2)
       allocate(s52(n2+1:n3,n0+1:n1,n0+1:n1,n0+1:n2))
       i1=k2*k1
       i2=k1*k4
       i3=k3*k2
       call egemm(i1,i2,i3,d1,d2,s52)
       deallocate(d1)
       deallocate(d2)
c
       call sum2413(n0,n1,n2,n3,n0,n2,n0,n1,x1,s52,-1.000)
       deallocate(s52)
c
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n2,n0+1:n2))
       call reorder1324(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n0,n2,n0,n2,intm,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(q5(n0+1:n2,n0+1:n2))
       i1=k2*k2
       i3=k3*k1
       call egemm1(i1,i3,d1,b2,q5)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x7(n0+1:n2,n0+1:n2))
       x7=0.0d0
       x7=x7+q5
       deallocate(q5)
c
       allocate(d1(n2+1:n3,n1+1:n3,n0+1:n1,n2+1:n3))
       call reorder4312(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n0,n1,n2,n3,intm,d1)
       allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
       allocate(s55(n0+1:n2,n0+1:n1,n0+1:n1,n2+1:n3))
       i1=k4*k1
       i2=k1*k2
       i3=k3*k4
       call egemm(i1,i2,i3,d1,d2,s55)
       deallocate(d1)
       deallocate(d2)
c
       call sum3412(n0,n1,n2,n3,n0,n2,n0,n1,x1,s55, 1.000)
       deallocate(s55)
c
       allocate(d1(n0+1:n1,n1+1:n3,n2+1:n3,n2+1:n3))
       call reorder1342(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n2,n3,n2,n3,intm,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(q6(n2+1:n3,n2+1:n3))
       i1=k4*k4
       i3=k3*k1
       call egemm1(i1,i3,d1,b2,q6)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x8(n2+1:n3,n2+1:n3))
       x8=0.0d0
       x8=x8+q6
       deallocate(q6)
c
       allocate(d1(n0+1:n2,n2+1:n3,n0+1:n1,n0+1:n1))
       call reorder2413(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n0,n1,n0,n1,intm,d1)
       allocate(d2(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
       call reorder3124(n2,n3,n2,n3,n0,n2,n0,n2,
     & n0,n2,n2,n3,n2,n3,n0,n2,t2c,d2)
       allocate(s58(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       i1=k1*k1
       i2=k2*k4
       i3=k4*k2
       call egemm(i1,i2,i3,d1,d2,s58)
       deallocate(d1)
       deallocate(d2)
c
       call sum2314(n0,n1,n2,n3,n0,n2,n0,n1,x1,s58, 1.000)
       deallocate(s58)
c
       allocate(d1(n1+1:n3,n0+1:n2,n2+1:n3,n1+1:n3))
       call reorder3241(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n2,n2,n3,n1,n3,intm,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s60(n0+1:n1,n0+1:n2,n2+1:n3,n1+1:n3))
       i1=k3*k4*k2
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s60)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x14(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       x14=0.0d0
       call sum4123(n0,n2,n2,n3,n1,n3,n0,n1,x14,s60, 1.000)
c
       allocate(d1(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       call reorder3241(n0,n1,n0,n2,n2,n3,n1,n3,
     & n2,n3,n0,n2,n1,n3,n0,n1,s60,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s138(n0+1:n2,n0+1:n2,n1+1:n3,n0+1:n1))
       i1=k1*k3*k2
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s138)
       deallocate(d1)
       deallocate(b2)
       deallocate(s60)
c
       call sum3124(n0,n2,n1,n3,n0,n2,n0,n1,x2,s138, 1.000)
       deallocate(s138)
c
       allocate(d1(n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3))
       call reorder3214(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n0,n1,n1,n3,intr,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s62(n0+1:n1,n0+1:n1,n0+1:n1,n1+1:n3))
       i1=k3*k1*k1
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s62)
       deallocate(d1)
       deallocate(b2)
c
      if(lvl1.eq.'ccsdt'.or.lvl1.eq.'3cc'.or.lvl1.eq.'test')then
      if(dt3diag3.eq.0.and.t1diag3.eq.0)goto 7002
       allocate(x16(n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1))
       x16=0.0d0
      if(t1diag3.eq.0)goto 4001
       factor=t1diag3
       call sum4123(n0,n1,n0,n1,n1,n3,n0,n1,x16,s62,factor)
c
4001  if(dt3diag3.eq.0)goto 7001
       factor=dt3diag3
       call sumx2143(n0,n3,n0,n1,n0,n1,n1,n3,n0,n1,x16,intr,factor)
c
7001   allocate(f2(n0+1:n1,n0+1:n1,n1+1:n3,n2+1:n3,n1+1:n3,n0+1:n2))
       call reorder562134(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n0,n1,n0,n1,n1,n3,n2,n3,n1,n3,n0,n2,t3b,f2)
       allocate(z18(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       i1=k1
       i2=k2*k3*k4
       i3=k3*k1*k1
       call egemm(i1,i2,i3,x16,f2,z18)
       deallocate(f2)
c
       v2b=v2b-0.500*z18
       deallocate(z18)
       deallocate(x16)
7002  endif
c
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder3421(n0,n1,n0,n1,n0,n1,n1,n3,
     & n0,n1,n1,n3,n0,n1,n0,n1,s62,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(q25(n0+1:n1,n0+1:n1))
       i1=k1*k1
       i3=k3*k1
       call egemm1(i1,i3,d1,b2,q25)
       deallocate(d1)
       deallocate(b2)
c
       x5=x5-q25
       deallocate(q25)
c
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder2431(n0,n1,n0,n1,n0,n1,n1,n3,
     & n0,n1,n1,n3,n0,n1,n0,n1,s62,d1)
       allocate(d2(n0+1:n1,n1+1:n3,n2+1:n3,n0+1:n2))
       call reorder4213(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n1,n3,n2,n3,n0,n2,t2b,d2)
       allocate(s130(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       i1=k1*k1
       i2=k2*k4
       i3=k3*k1
       call egemm(i1,i2,i3,d1,d2,s130)
       deallocate(d1)
       deallocate(d2)
       deallocate(s62)
c
       call sum2314(n0,n1,n2,n3,n0,n2,n0,n1,x1,s130, 1.000)
       deallocate(s130)
c
      if(lvl1.eq.'ccsdt'.or.lvl1.eq.'3cc'.or.lvl1.eq.'test')then
      if(t1diag4.eq.0)goto 4002
       allocate(d1(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder2431(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n1,n3,n0,n1,intr,d1)
       allocate(f2(n0+1:n1,n1+1:n3,n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder523146(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n2,n3,n0,n2,n0,n1,t3b,f2)
       allocate(s64(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       i1=k1
       i2=k1*k2*k4
       i3=k3*k3*k1
       call egemm(i1,i2,i3,d1,f2,s64)
       deallocate(d1)
       deallocate(f2)
c
       factor=0.500*t1diag4
       call sum2341(n0,n1,n2,n3,n0,n2,n0,n1,x1,s64,factor)
       deallocate(s64)
4002  endif
c
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n1,n1+1:n3))
       call reorder1324(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n0,n1,n1,n3,intr,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(q7(n0+1:n1,n1+1:n3))
       i1=k3*k1
       i3=k3*k1
       call egemm1(i1,i3,d1,b2,q7)
       deallocate(d1)
       deallocate(b2)
c
      allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(q26(n1+1:n3,n1+1:n3))
       i1=k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,q7,b2,q26)
       deallocate(b2)
       deallocate(q7)
c
      if(lvl1.eq.'ccsdt'.or.lvl1.eq.'3cc'.or.lvl1.eq.'test')then
       allocate(x15(n0+1:n1,n1+1:n3))
       x15=0.0d0
c
      if(t1diag1.eq.0)goto 4003
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n1,n1+1:n3))
      if(t1diag2.eq.0)then
       call reorder1324(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n0,n1,n1,n3,intm,d1)
      else
       call reorder1324(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n0,n1,n1,n3,intr,d1)
      endif
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(q7(n0+1:n1,n1+1:n3))
       i1=k3*k1
       i3=k3*k1
       call egemm1(i1,i3,d1,b2,q7)
       deallocate(d1)
       deallocate(b2)
       x15=x15+t1diag1*q7
       deallocate(q7)
4003  endif
c
       call sum21(n1,n3,n1,n3,x6,q26,-1.000)
       deallocate(q26)
c
       allocate(d1(n1+1:n3,n0+1:n2,n0+1:n1,n2+1:n3))
       call reorder3214(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n2,n0,n1,n2,n3,intm,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s67(n0+1:n1,n0+1:n2,n0+1:n1,n2+1:n3))
       i1=k4*k1*k2
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s67)
       deallocate(d1)
       deallocate(b2)
c
      if(lvl1.eq.'ccsdt'.or.lvl1.eq.'3cc'.or.lvl1.eq.'test')then
      if(dt3diag3.eq.0.and.t1diag3.eq.0)goto 7006
       allocate(x19(n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n1))
       x19=0.0d0
      if(t1diag3.eq.0)goto 4004
       factor=t1diag3
       call sum4123(n0,n2,n0,n1,n2,n3,n0,n1,x19,s67,factor)
c
4004  if(dt3diag3.eq.0)goto 7005
       factor=dt3diag3
       call sumx2143(n0,n3,n0,n2,n0,n1,n2,n3,n0,n1,x19,intm,factor)
c
7005   allocate(f2(n0+1:n2,n0+1:n1,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2))
       call reorder461235(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & n0,n2,n0,n1,n2,n3,n2,n3,n1,n3,n0,n2,t3c,f2)
       allocate(z23(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       i1=k1
       i2=k2*k3*k4
       i3=k4*k1*k2
       call egemm(i1,i2,i3,x19,f2,z23)
       deallocate(f2)
c
       v2b=v2b-z23
       deallocate(z23)
       deallocate(x19)
7006  endif
c
       allocate(d1(n0+1:n2,n2+1:n3,n0+1:n1,n0+1:n1))
       call reorder2431(n0,n1,n0,n2,n0,n1,n2,n3,
     & n0,n2,n2,n3,n0,n1,n0,n1,s67,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(q27(n0+1:n1,n0+1:n1))
       i1=k1*k1
       i3=k4*k2
       call egemm1(i1,i3,d1,b2,q27)
       deallocate(d1)
       deallocate(b2)
c
       x5=x5+q27
       deallocate(q27)
c
       allocate(d1(n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder3421(n0,n1,n0,n2,n0,n1,n2,n3,
     & n0,n1,n2,n3,n0,n2,n0,n1,s67,d1)
       allocate(d2(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
       call reorder4123(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n2,n3,n1,n3,n0,n2,t2b,d2)
       allocate(s142(n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       i1=k1*k2
       i2=k2*k3
       i3=k4*k1
       call egemm(i1,i2,i3,d1,d2,s142)
       deallocate(d1)
       deallocate(d2)
c
       call sum2314(n0,n2,n1,n3,n0,n2,n0,n1,x2,s142,-1.000)
       deallocate(s142)
c
       allocate(d1(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       call reorder4231(n0,n1,n0,n2,n0,n1,n2,n3,
     & n2,n3,n0,n2,n0,n1,n0,n1,s67,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s140(n0+1:n2,n0+1:n2,n0+1:n1,n0+1:n1))
       i1=k1*k1*k2
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s140)
       deallocate(d1)
       deallocate(b2)
c
       call sum3124(n0,n2,n0,n1,n0,n2,n0,n1,x10,s140, 1.000)
c
       allocate(d1(n0+1:n1,n0+1:n2,n0+1:n2,n0+1:n1))
       call reorder3214(n0,n2,n0,n2,n0,n1,n0,n1,
     & n0,n1,n0,n2,n0,n2,n0,n1,s140,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s158(n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       i1=k1*k2*k2
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s158)
       deallocate(d1)
       deallocate(b2)
       deallocate(s140)
c
       call sum2134(n0,n2,n1,n3,n0,n2,n0,n1,x2,s158,-1.000)
       deallocate(s158)
c
       allocate(d1(n0+1:n2,n2+1:n3,n0+1:n1,n0+1:n1))
       call reorder2431(n0,n1,n0,n2,n0,n1,n2,n3,
     & n0,n2,n2,n3,n0,n1,n0,n1,s67,d1)
       allocate(d2(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
       call reorder3124(n2,n3,n2,n3,n0,n2,n0,n2,
     & n0,n2,n2,n3,n2,n3,n0,n2,t2c,d2)
       allocate(s134(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       i1=k1*k1
       i2=k2*k4
       i3=k4*k2
       call egemm(i1,i2,i3,d1,d2,s134)
       deallocate(d1)
       deallocate(d2)
       deallocate(s67)
c
       call sum2314(n0,n1,n2,n3,n0,n2,n0,n1,x1,s134, 1.000)
       deallocate(s134)
c
      if(lvl1.eq.'ccsdt'.or.lvl1.eq.'3cc'.or.lvl1.eq.'test')then
      if(t1diag4.eq.0)goto 4005
       allocate(d1(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder2431(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n1,n3,n0,n1,intm,d1)
       allocate(f2(n0+1:n2,n2+1:n3,n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder413256(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & n0,n2,n2,n3,n1,n3,n2,n3,n0,n2,n0,n1,t3c,f2)
       allocate(s69(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       i1=k1
       i2=k1*k2*k4
       i3=k3*k4*k2
       call egemm(i1,i2,i3,d1,f2,s69)
       deallocate(d1)
       deallocate(f2)
c
       factor=t1diag4
       call sum2341(n0,n1,n2,n3,n0,n2,n0,n1,x1,s69,factor)
       deallocate(s69)
4005  endif
c
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n2,n2+1:n3))
       call reorder1324(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n0,n2,n2,n3,intm,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(q8(n0+1:n2,n2+1:n3))
       i1=k4*k2
       i3=k3*k1
       call egemm1(i1,i3,d1,b2,q8)
       deallocate(d1)
       deallocate(b2)
c
      if(lvl1.eq.'ccsdt'.or.lvl1.eq.'3cc'.or.lvl1.eq.'test')then
       allocate(x18(n0+1:n2,n2+1:n3))
       x18=0.0d0
      if(t1diag1.eq.0)goto 4006
       x18=x18+t1diag1*q8
4006  endif
c
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(q29(n2+1:n3,n2+1:n3))
       i1=k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,q8,b2,q29)
       deallocate(b2)
c
       call sum21(n2,n3,n2,n3,x8,q29,-1.000)
       deallocate(q29)
c
       allocate(b1(n2+1:n3,n0+1:n2))
       call reorder21(n0,n2,n2,n3,
     & n2,n3,n0,n2,q8,b1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(q28(n0+1:n2,n0+1:n2))
       i1=k2
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,b1,b2,q28)
       deallocate(b1)
       deallocate(b2)
       deallocate(q8)
c
       call sum21(n0,n2,n0,n2,x7,q28, 1.000)
       deallocate(q28)
c
       allocate(d1(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       call reorder4213(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n1,n3,n0,n1,intm,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s72(n0+1:n2,n0+1:n2,n1+1:n3,n0+1:n1))
       i1=k1*k3*k2
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s72)
       deallocate(d1)
       deallocate(b2)
c
       call sum3124(n0,n2,n1,n3,n0,n2,n0,n1,x2,s72, 1.000)
       deallocate(s72)
c
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n2,n0+1:n2))
       call reorder1324(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n0,n2,n0,n2,intm,d1)
       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
       allocate(s74(n1+1:n3,n0+1:n1,n0+1:n2,n0+1:n2))
       i1=k2*k2
       i2=k1*k3
       i3=k3*k1
       call egemm(i1,i2,i3,d1,d2,s74)
       deallocate(d1)
       deallocate(d2)
c
       call sum2413(n0,n2,n1,n3,n0,n2,n0,n1,x2,s74, 1.000)
       deallocate(s74)
c
       allocate(d1(n2+1:n3,n0+1:n1,n1+1:n3,n2+1:n3))
       call reorder4132(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n1,n1,n3,n2,n3,intm,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s76(n0+1:n2,n0+1:n1,n1+1:n3,n2+1:n3))
       i1=k4*k3*k1
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s76)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x4(n0+1:n1,n1+1:n3,n2+1:n3,n0+1:n2))
       x4=0.0d0
       call sum4123(n0,n1,n1,n3,n2,n3,n0,n2,x4,s76, 1.000)
       deallocate(s76)
c
       call sumx1324(n0,n3,n0,n1,n1,n3,n2,n3,n0,n2,x4,intm, 1.000)
c
       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
       allocate(z5(n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2))
       i1=k2*k4
       i2=k1*k3
       i3=k3*k1
       call egemm(i1,i2,i3,x4,d2,z5)
       deallocate(d2)
c
       call
     & sum2413(n2,n3,n1,n3,n0,n2,n0,n1,v2b,z5, 1.000)
       deallocate(z5)
       deallocate(x4)
c
       allocate(b1(n2+1:n3,n0+1:n2))
       call reorder12(0,n3,0,n3,
     & n2,n3,n0,n2,fockb,b1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(q9(n0+1:n2,n0+1:n2))
       i1=k2
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,b1,b2,q9)
       deallocate(b1)
       deallocate(b2)
c
       call sum21(n0,n2,n0,n2,x7,q9, 1.000)
       deallocate(q9)
c
       allocate(b1(n0+1:n2,n2+1:n3))
       call reorder21(0,n3,0,n3,
     & n0,n2,n2,n3,fockb,b1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(q10(n2+1:n3,n2+1:n3))
       i1=k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,b1,b2,q10)
       deallocate(b1)
       deallocate(b2)
c
       call sum21(n2,n3,n2,n3,x8,q10,-1.000)
       deallocate(q10)
c
       allocate(d1(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       call reorder4213(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n0,n1,n0,n1,intm,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s80(n0+1:n2,n0+1:n2,n0+1:n1,n0+1:n1))
       i1=k1*k1*k2
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s80)
       deallocate(d1)
       deallocate(b2)
c
       call sum3124(n0,n2,n0,n1,n0,n2,n0,n1,x10,s80, 1.000)
c
       allocate(d1(n0+1:n1,n0+1:n2,n0+1:n2,n0+1:n1))
       call reorder3214(n0,n2,n0,n2,n0,n1,n0,n1,
     & n0,n1,n0,n2,n0,n2,n0,n1,s80,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s136(n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       i1=k1*k2*k2
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s136)
       deallocate(d1)
       deallocate(b2)
       deallocate(s80)
c
       call sum2134(n0,n2,n1,n3,n0,n2,n0,n1,x2,s136,-1.000)
       deallocate(s136)
c
       allocate(d1(n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder1423(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n2,n3,n0,n2,n0,n1,intm,d1)
       allocate(d2(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
       call reorder4123(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n2,n3,n1,n3,n0,n2,t2b,d2)
       allocate(s82(n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       i1=k1*k2
       i2=k2*k3
       i3=k4*k1
       call egemm(i1,i2,i3,d1,d2,s82)
       deallocate(d1)
       deallocate(d2)
c
       call sum2314(n0,n2,n1,n3,n0,n2,n0,n1,x2,s82,-1.000)
       deallocate(s82)
c
       allocate(d1(n0+1:n2,n2+1:n3,n0+1:n1,n0+1:n1))
       call reorder2413(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n0,n1,n0,n1,intm,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(q11(n0+1:n1,n0+1:n1))
       i1=k1*k1
       i3=k4*k2
       call egemm1(i1,i3,d1,b2,q11)
       deallocate(d1)
       deallocate(b2)
c
       x5=x5+q11
       deallocate(q11)
c
       allocate(d1(n2+1:n3,n0+1:n2,n1+1:n3,n1+1:n3))
       call reorder4231(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n1,n3,n1,n3,intm,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s85(n0+1:n2,n0+1:n2,n1+1:n3,n1+1:n3))
       i1=k3*k3*k2
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s85)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x12(n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n2))
       x12=0.0d0
       call sum4123(n0,n2,n1,n3,n1,n3,n0,n2,x12,s85, 1.000)
       deallocate(s85)
c
       allocate(d1(n2+1:n3,n1+1:n3,n0+1:n2,n1+1:n3))
       call reorder4321(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n0,n2,n1,n3,intm,d1)
       allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
       allocate(s87(n0+1:n2,n0+1:n1,n0+1:n2,n1+1:n3))
       i1=k3*k2
       i2=k1*k2
       i3=k3*k4
       call egemm(i1,i2,i3,d1,d2,s87)
       deallocate(d1)
       deallocate(d2)
c
       call sum3412(n0,n2,n1,n3,n0,n2,n0,n1,x2,s87, 1.000)
       deallocate(s87)
c
       allocate(d1(n0+1:n2,n2+1:n3,n1+1:n3,n1+1:n3))
       call reorder2431(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n1,n3,n1,n3,intm,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(q12(n1+1:n3,n1+1:n3))
       i1=k3*k3
       i3=k4*k2
       call egemm1(i1,i3,d1,b2,q12)
       deallocate(d1)
       deallocate(b2)
c
       x6=x6+q12
       deallocate(q12)
c
       allocate(d1(n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder2413(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n0,n2,n0,n2,intb,d1)
       allocate(d2(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n2,n3,n1,n3,n0,n1,t2b,d2)
       allocate(s90(n1+1:n3,n0+1:n1,n0+1:n2,n0+1:n2))
       i1=k2*k2
       i2=k1*k3
       i3=k4*k2
       call egemm(i1,i2,i3,d1,d2,s90)
       deallocate(d1)
       deallocate(d2)
c
       call sum2413(n0,n2,n1,n3,n0,n2,n0,n1,x2,s90, 1.000)
       deallocate(s90)
c
       allocate(d1(n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder1423(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n0,n2,n0,n2,intb,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(q13(n0+1:n2,n0+1:n2))
       i1=k2*k2
       i3=k4*k2
       call egemm1(i1,i3,d1,b2,q13)
       deallocate(d1)
       deallocate(b2)
c
       x7=x7-q13
       deallocate(q13)
c
       allocate(d1(n2+1:n3,n0+1:n2,n2+1:n3,n2+1:n3))
       call reorder3241(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n2,n3,n2,n3,intb,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s93(n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3))
       i1=k4*k4*k2
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s93)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x13(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
       x13=0.0d0
       call sum4123(n0,n2,n2,n3,n2,n3,n0,n2,x13,s93, 1.000)
       deallocate(s93)
c
       call sumx3142(n0,n3,n0,n2,n2,n3,n2,n3,n0,n2,x13,intb, 1.000)
c
       allocate(d2(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n2,n3,n1,n3,n0,n1,t2b,d2)
       allocate(z15(n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2))
       i1=k2*k4
       i2=k1*k3
       i3=k4*k2
       call egemm(i1,i2,i3,x13,d2,z15)
       deallocate(d2)
c
       call
     & sum2413(n2,n3,n1,n3,n0,n2,n0,n1,v2b,z15, 1.000)
       deallocate(z15)
       deallocate(x13)
c
       allocate(d1(n0+1:n2,n2+1:n3,n2+1:n3,n2+1:n3))
       call reorder2341(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n2,n3,n2,n3,intb,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(q14(n2+1:n3,n2+1:n3))
       i1=k4*k4
       i3=k4*k2
       call egemm1(i1,i3,d1,b2,q14)
       deallocate(d1)
       deallocate(b2)
c
       x8=x8-q14
       deallocate(q14)
c
       allocate(d1(n2+1:n3,n0+1:n2,n0+1:n1,n1+1:n3))
       call reorder4213(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n0,n1,n1,n3,intm,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s96(n0+1:n2,n0+1:n2,n0+1:n1,n1+1:n3))
       i1=k3*k1*k2
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s96)
       deallocate(d1)
       deallocate(b2)
c
      if(lvl1.eq.'ccsdt'.or.lvl1.eq.'3cc'.or.lvl1.eq.'test')then
      if(dt3diag3.eq.0.and.t1diag3.eq.0)goto 7004
       allocate(x17(n0+1:n2,n0+1:n1,n1+1:n3,n0+1:n2))
       x17=0.0d0
      if(t1diag3.eq.0)goto 4007
       factor=t1diag3
       call sum4123(n0,n2,n0,n1,n1,n3,n0,n2,x17,s96,factor)
c
4007  if(dt3diag3.eq.0)goto 7003
       factor=dt3diag3
       call sumx2134(n0,n3,n0,n2,n0,n1,n1,n3,n0,n2,x17,intm,factor)
c
7003   allocate(f2(n0+1:n2,n0+1:n1,n1+1:n3,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder452136(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n0,n2,n0,n1,n1,n3,n2,n3,n1,n3,n0,n1,t3b,f2)
       allocate(z20(n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n2))
       i1=k2
       i2=k1*k3*k4
       i3=k3*k1*k2
       call egemm(i1,i2,i3,x17,f2,z20)
       deallocate(f2)
c
       call
     & sum1243(n2,n3,n1,n3,n0,n2,n0,n1,v2b,z20,-1.000)
       deallocate(z20)
       deallocate(x17)
7004  endif
c
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n2,n0+1:n2))
       call reorder3421(n0,n2,n0,n2,n0,n1,n1,n3,
     & n0,n1,n1,n3,n0,n2,n0,n2,s96,d1)
       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
       allocate(s152(n1+1:n3,n0+1:n1,n0+1:n2,n0+1:n2))
       i1=k2*k2
       i2=k1*k3
       i3=k3*k1
       call egemm(i1,i2,i3,d1,d2,s152)
       deallocate(d1)
       deallocate(d2)
c
       call sum2413(n0,n2,n1,n3,n0,n2,n0,n1,x2,s152, 1.000)
       deallocate(s152)
c
       allocate(d1(n0+1:n2,n1+1:n3,n0+1:n1,n0+1:n2))
       call reorder2431(n0,n2,n0,n2,n0,n1,n1,n3,
     & n0,n2,n1,n3,n0,n1,n0,n2,s96,d1)
       allocate(d2(n0+1:n2,n1+1:n3,n2+1:n3,n0+1:n1))
       call reorder3214(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n1,n3,n2,n3,n0,n1,t2b,d2)
       allocate(s145(n2+1:n3,n0+1:n1,n0+1:n1,n0+1:n2))
       i1=k2*k1
       i2=k1*k4
       i3=k3*k2
       call egemm(i1,i2,i3,d1,d2,s145)
       deallocate(d1)
       deallocate(d2)
       deallocate(s96)
c
       call sum2413(n0,n1,n2,n3,n0,n2,n0,n1,x1,s145,-1.000)
       deallocate(s145)
c
       call sumx1243(n0,n3,n0,n1,n2,n3,n0,n2,n0,n1,x1,intm, 1.000)
c
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(z1(n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
       i1=k1*k2*k4
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,x1,b2,z1)
       deallocate(b2)
c
       call
     & sum2134(n2,n3,n1,n3,n0,n2,n0,n1,v2b,z1,-1.000)
       deallocate(z1)
       deallocate(x1)
c
      if(lvl1.eq.'ccsdt'.or.lvl1.eq.'3cc'.or.lvl1.eq.'test')then
      if(t1diag4.eq.0)goto 4008
       allocate(d1(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
       call reorder1432(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n2,n3,n1,n3,n0,n2,intm,d1)
       allocate(f2(n0+1:n1,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder512346(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n0,n1,n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,t3b,f2)
       allocate(s98(n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       i1=k2
       i2=k1*k2*k3
       i3=k3*k4*k1
       call egemm(i1,i2,i3,d1,f2,s98)
       deallocate(d1)
       deallocate(f2)
c
       factor=t1diag4
       call sum2341(n0,n2,n1,n3,n0,n2,n0,n1,x2,s98,factor)
       deallocate(s98)
4008  endif
c
       allocate(d1(n0+1:n2,n2+1:n3,n0+1:n1,n1+1:n3))
       call reorder2413(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n0,n1,n1,n3,intm,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(q15(n0+1:n1,n1+1:n3))
       i1=k3*k1
       i3=k4*k2
       call egemm1(i1,i3,d1,b2,q15)
       deallocate(d1)
       deallocate(b2)
c
      if(lvl1.eq.'ccsdt'.or.lvl1.eq.'3cc'.or.lvl1.eq.'test')then
      if(t1diag1.eq.0)goto 4009
       x15=x15+t1diag1*q15
c
4009   call sumx12(0,n3,n0,n1,n1,n3,x15,fockr, 1.000)
c
       allocate(f2(n0+1:n1,n1+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder521346(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n0,n1,n1,n3,n2,n3,n1,n3,n0,n2,n0,n1,t3b,f2)
       allocate(z17(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       i2=k1*k2*k3*k4
       i3=k3*k1
       call egemm2(i2,i3,x15,f2,z17)
       deallocate(f2)
c
       v2b=v2b+z17
       deallocate(z17)
       deallocate(x15)
      endif
c
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(q30(n1+1:n3,n1+1:n3))
       i1=k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,q15,b2,q30)
       deallocate(b2)
       deallocate(q15)
c
       call sum21(n1,n3,n1,n3,x6,q30,-1.000)
       deallocate(q30)
c
       allocate(d1(n2+1:n3,n0+1:n2,n0+1:n2,n2+1:n3))
       call reorder3214(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n0,n2,n2,n3,intb,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s101(n0+1:n2,n0+1:n2,n0+1:n2,n2+1:n3))
       i1=k4*k2*k2
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s101)
       deallocate(d1)
       deallocate(b2)
c
      if(lvl1.eq.'ccsdt'.or.lvl1.eq.'3cc'.or.lvl1.eq.'test')then
      if(dt3diag3.eq.0.and.t1diag3.eq.0)goto 7008
       allocate(x20(n0+1:n2,n0+1:n2,n2+1:n3,n0+1:n2))
       x20=0.0d0
      if(t1diag3.eq.0)goto 4010
       factor=t1diag3
       call sum4123(n0,n2,n0,n2,n2,n3,n0,n2,x20,s101,factor)
c
4010  if(dt3diag3.eq.0)goto 7007
       factor=dt3diag3
       call sumx2143(n0,n3,n0,n2,n0,n2,n2,n3,n0,n2,x20,intb,factor)
c
7007   allocate(f2(n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder451236(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & n0,n2,n0,n2,n2,n3,n2,n3,n1,n3,n0,n1,t3c,f2)
       allocate(z25(n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n2))
       i1=k2
       i2=k1*k3*k4
       i3=k4*k2*k2
       call egemm(i1,i2,i3,x20,f2,z25)
       deallocate(f2)
c
       call
     & sum1243(n2,n3,n1,n3,n0,n2,n0,n1,v2b,z25,-0.500)
       deallocate(z25)
       deallocate(x20)
7008  endif
c
       allocate(d1(n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder3421(n0,n2,n0,n2,n0,n2,n2,n3,
     & n0,n2,n2,n3,n0,n2,n0,n2,s101,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(q31(n0+1:n2,n0+1:n2))
       i1=k2*k2
       i3=k4*k2
       call egemm1(i1,i3,d1,b2,q31)
       deallocate(d1)
       deallocate(b2)
c
       x7=x7-q31
       deallocate(q31)
c
       allocate(d1(n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder2431(n0,n2,n0,n2,n0,n2,n2,n3,
     & n0,n2,n2,n3,n0,n2,n0,n2,s101,d1)
       allocate(d2(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n2,n3,n1,n3,n0,n1,t2b,d2)
       allocate(s154(n1+1:n3,n0+1:n1,n0+1:n2,n0+1:n2))
       i1=k2*k2
       i2=k1*k3
       i3=k4*k2
       call egemm(i1,i2,i3,d1,d2,s154)
       deallocate(d1)
       deallocate(d2)
       deallocate(s101)
c
       call sum2413(n0,n2,n1,n3,n0,n2,n0,n1,x2,s154, 1.000)
       deallocate(s154)
c
      if(lvl1.eq.'ccsdt'.or.lvl1.eq.'3cc'.or.lvl1.eq.'test')then
      if(t1diag4.eq.0)goto 4011
       allocate(d1(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
       call reorder2431(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n2,n3,n0,n2,intb,d1)
       allocate(f2(n0+1:n2,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder412356(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & n0,n2,n2,n3,n2,n3,n1,n3,n0,n2,n0,n1,t3c,f2)
       allocate(s103(n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       i1=k2
       i2=k1*k2*k3
       i3=k4*k4*k2
       call egemm(i1,i2,i3,d1,f2,s103)
       deallocate(d1)
       deallocate(f2)
c
       factor=0.500*t1diag4
       call sum2341(n0,n2,n1,n3,n0,n2,n0,n1,x2,s103,factor)
       deallocate(s103)
4011  endif
c
       allocate(d1(n0+1:n2,n2+1:n3,n0+1:n2,n2+1:n3))
       call reorder1324(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n0,n2,n2,n3,intb,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(q16(n0+1:n2,n2+1:n3))
       i1=k4*k2
       i3=k4*k2
       call egemm1(i1,i3,d1,b2,q16)
       deallocate(d1)
       deallocate(b2)
c
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(q32(n2+1:n3,n2+1:n3))
       i1=k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,q16,b2,q32)
       deallocate(b2)
       deallocate(q16)
c
      if(lvl1.eq.'ccsdt'.or.lvl1.eq.'3cc'.or.lvl1.eq.'test')then
      if(t1diag1.eq.0)goto 4012
       allocate(d1(n0+1:n2,n2+1:n3,n0+1:n2,n2+1:n3))
      if(t1diag2.eq.0)then
       call reorder1324(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n0,n2,n2,n3,intm,d1)
      else
       call reorder1324(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n0,n2,n2,n3,intb,d1)
      endif
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(q16(n0+1:n2,n2+1:n3))
       i1=k4*k2
       i3=k4*k2
       call egemm1(i1,i3,d1,b2,q16)
       deallocate(d1)
       deallocate(b2)
c
       x18=x18+t1diag1*q16
       deallocate(q16)
c
4012   call sumx12(0,n3,n0,n2,n2,n3,x18,fockb, 1.000)
c
       allocate(f2(n0+1:n2,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder412356(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & n0,n2,n2,n3,n2,n3,n1,n3,n0,n2,n0,n1,t3c,f2)
       allocate(z22(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       i2=k1*k2*k3*k4
       i3=k4*k2
       call egemm2(i2,i3,x18,f2,z22)
       deallocate(f2)
c
       v2b=v2b+z22
       deallocate(z22)
       deallocate(x18)
      endif
c
       call sum21(n2,n3,n2,n3,x8,q32,-1.000)
       deallocate(q32)
c
      if(diag1.eq.0)goto 5001
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n1,n1+1:n3))                 !ilias: acp d1
      if(diag2.eq.0)then
       call reorder1324(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n0,n1,n1,n3,intm,d1)
       else
       call reorder1324(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n0,n1,n1,n3,intr,d1)
      endif
       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
       allocate(s106(n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3))
       i1=k3*k1
       i2=k1*k3
       i3=k3*k1
       call egemm(i1,i2,i3,d1,d2,s106)
       deallocate(d1)
       deallocate(d2)
c
       factor=diag1
       call sum3412(n0,n1,n1,n3,n1,n3,n0,n1,x9,s106,factor)
       deallocate(s106)
       factor=0
c ilias: added the complementary acc-d1_mc term
5001  if(ext_cor.and.diag1.ne.1)then
       print*,'diag1 =/= 1'
       allocate(d1_mc(n0+1:n1,n1+1:n3,n0+1:n1,n1+1:n3))
       call reorder1324(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n0,n1,n1,n3,intr,d1_mc)
       allocate(d2_mc(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a_mc,d2_mc)
       allocate(s106_mc(n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3))
       i1=k3*k1
       i2=k1*k3
       i3=k3*k1
       call egemm(i1,i2,i3,d1_mc,d2_mc,s106_mc)
       deallocate(d1_mc)
       deallocate(d2_mc)
c
       allocate(d1_mc(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3412(n1,n3,n0,n1,n0,n1,n1,n3,
     & n0,n1,n1,n3,n1,n3,n0,n1,s106_mc,d1_mc)
       allocate(d2_mc(n0+1:n1,n1+1:n3,n2+1:n3,n0+1:n2))
       call reorder4213(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n1,n3,n2,n3,n0,n2,t2b_mc,d2_mc)
       allocate(z10_mc(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       i1=k1*k3
       i2=k2*k4
       i3=k3*k1
       call egemm(i1,i2,i3,d1_mc,d2_mc,z10_mc)
       deallocate(s106_mc)
       deallocate(d1_mc)
       deallocate(d2_mc)
c
       factor=(1.0-diag1)
       call
     & sum1324(n2,n3,n1,n3,n0,n2,n0,n1,v2b,z10_mc,factor)
       deallocate(z10_mc)
       factor=0
      endif
c ilias: added the complementary acc-d2_mc term
      if(ext_cor.and.diag1.eq.1.and.diag2.eq.0)then
       print*,'diag2 =/= 1'
       allocate(d1_mc(n0+1:n1,n1+1:n3,n0+1:n1,n1+1:n3))
       call reorder1324(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n0,n1,n1,n3,intr,d1_mc)
       allocate(d2_mc(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a_mc,d2_mc)
       allocate(s106_mc(n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3))
       i1=k3*k1
       i2=k1*k3
       i3=k3*k1
       call egemm(i1,i2,i3,d1_mc,d2_mc,s106_mc)
       deallocate(d1_mc)
       deallocate(d2_mc)
c
       allocate(d1_mc(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3412(n1,n3,n0,n1,n0,n1,n1,n3,
     & n0,n1,n1,n3,n1,n3,n0,n1,s106_mc,d1_mc)
       allocate(d2_mc(n0+1:n1,n1+1:n3,n2+1:n3,n0+1:n2))
       call reorder4213(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n1,n3,n2,n3,n0,n2,t2b_mc,d2_mc)
       allocate(z10_mc(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       i1=k1*k3
       i2=k2*k4
       i3=k3*k1
       call egemm(i1,i2,i3,d1_mc,d2_mc,z10_mc)
       deallocate(s106_mc)
       deallocate(d1_mc)
       deallocate(d2_mc)
c
       factor=diag1
       call
     & sum1324(n2,n3,n1,n3,n0,n2,n0,n1,v2b,z10_mc,factor)
       deallocate(z10_mc)
       factor=0
c
       allocate(d1_mc(n0+1:n1,n1+1:n3,n0+1:n1,n1+1:n3))
       call reorder1324(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n0,n1,n1,n3,intm,d1_mc)
       allocate(d2_mc(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a_mc,d2_mc)
       allocate(s106_mc(n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3))
       i1=k3*k1
       i2=k1*k3
       i3=k3*k1
       call egemm(i1,i2,i3,d1_mc,d2_mc,s106_mc)
       deallocate(d1_mc)
       deallocate(d2_mc)
c
       allocate(d1_mc(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3412(n1,n3,n0,n1,n0,n1,n1,n3,
     & n0,n1,n1,n3,n1,n3,n0,n1,s106_mc,d1_mc)
       allocate(d2_mc(n0+1:n1,n1+1:n3,n2+1:n3,n0+1:n2))
       call reorder4213(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n1,n3,n2,n3,n0,n2,t2b_mc,d2_mc)
       allocate(z10_mc(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       i1=k1*k3
       i2=k2*k4
       i3=k3*k1
       call egemm(i1,i2,i3,d1_mc,d2_mc,z10_mc)
       deallocate(s106_mc)
       deallocate(d1_mc)
       deallocate(d2_mc)
c
       factor=-diag1
       call
     & sum1324(n2,n3,n1,n3,n0,n2,n0,n1,v2b,z10_mc,factor)
       deallocate(z10_mc)
       factor=0
      endif
c
      if(diag4.eq.0)goto 5002
       allocate(d1(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))                 !ilias: acp d4
       call reorder1432(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n1,n3,n0,n1,intr,d1)
       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
       allocate(q17(n0+1:n1,n0+1:n1))
       i1=k1
       i2=k1
       i3=k3*k3*k1
       call egemm(i1,i2,i3,d1,d2,q17)
       deallocate(d1)
       deallocate(d2)
c
       factor=-0.500*diag4
       call sum21(n0,n1,n0,n1,x5,q17,factor)
       deallocate(q17)
       factor=0
c ilias: added the complementary acc-d4_mc term
5002  if(ext_cor.and.diag4.ne.1)then
       print*,'diag4 =/= 1'
       allocate(d1_mc(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder1432(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n1,n3,n0,n1,intr,d1_mc)
       allocate(d2_mc(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a_mc,d2_mc)
       allocate(q17_mc(n0+1:n1,n0+1:n1))
       i1=k1
       i2=k1
       i3=k3*k3*k1
       call egemm(i1,i2,i3,d1_mc,d2_mc,q17_mc)
       deallocate(d1_mc)
       deallocate(d2_mc)
c
       allocate(b1_mc(n0+1:n1,n0+1:n1))
       call reorder21(n0,n1,n0,n1,
     & n0,n1,n0,n1,q17_mc,b1_mc)
       allocate(d2_mc(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
       call reorder4123(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n2,n3,n1,n3,n0,n2,t2b_mc,d2_mc)
       allocate(z6_mc(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       i1=k1
       i2=k2*k3*k4
       i3=k1
       call egemm(i1,i2,i3,b1_mc,d2_mc,z6_mc)
       deallocate(b1_mc)
       deallocate(d2_mc)
       deallocate(q17_mc)
c
       factor=0.500*(1.0-diag4)
       v2b=v2b+factor*z6_mc
       deallocate(z6_mc)
       factor=0
      endif
c
      if(diag3.eq.0)goto 5003
       allocate(d1(n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3))                !ilias: acp d3
       call reorder2134(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n0,n1,n1,n3,n1,n3,intr,d1)
       allocate(d2(n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3))
       call reorder3412(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n0,n1,n1,n3,n1,n3,t2a,d2)
       allocate(q18(n1+1:n3,n1+1:n3))
       i1=k3
       i2=k3
       i3=k3*k1*k1
       call egemm(i1,i2,i3,d1,d2,q18)
       deallocate(d1)
       deallocate(d2)
c
       factor=0.500*diag3
       call sum21(n1,n3,n1,n3,x6,q18,factor)
       deallocate(q18)
       factor=0
c ilias: added the complementary acc-d3_mc term
5003  if(ext_cor.and.diag3.ne.1)then
       print*,'diag3 =/= 1'
       allocate(d1_mc(n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3))
       call reorder2134(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n0,n1,n1,n3,n1,n3,intr,d1_mc)
       allocate(d2_mc(n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3))
       call reorder3412(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n0,n1,n1,n3,n1,n3,t2a_mc,d2_mc)
       allocate(q18_mc(n1+1:n3,n1+1:n3))
       i1=k3
       i2=k3
       i3=k3*k1*k1
       call egemm(i1,i2,i3,d1_mc,d2_mc,q18_mc)
       deallocate(d1_mc)
       deallocate(d2_mc)
c
       allocate(b1_mc(n1+1:n3,n1+1:n3))
       call reorder21(n1,n3,n1,n3,
     & n1,n3,n1,n3,q18_mc,b1_mc)
       allocate(d2_mc(n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder2134(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n2,n3,n0,n2,n0,n1,t2b_mc,d2_mc)
       allocate(z7_mc(n2+1:n3,n0+1:n2,n0+1:n1,n1+1:n3))
       i1=k3
       i2=k1*k2*k4
       i3=k3
       call egemm(i1,i2,i3,b1_mc,d2_mc,z7_mc)
       deallocate(q18_mc)
       deallocate(b1_mc)
       deallocate(d2_mc)
c
       factor=0.500*(1.0-diag3)
       call
     & sum1342(n2,n3,n1,n3,n0,n2,n0,n1,v2b,z7_mc,factor)
       deallocate(z7_mc)
       factor=0
      endif
c
      if(diag1.eq.0)goto 5004
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n2,n2+1:n3))                 !ilias: acp d1
       call reorder1324(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n0,n2,n2,n3,intm,d1)
       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
       allocate(s110(n1+1:n3,n0+1:n1,n0+1:n2,n2+1:n3))
       i1=k4*k2
       i2=k1*k3
       i3=k3*k1
       call egemm(i1,i2,i3,d1,d2,s110)
       deallocate(d1)
       deallocate(d2)
c
       factor=diag1
       call sum3412(n0,n2,n2,n3,n1,n3,n0,n1,x14,s110,factor)
       deallocate(s110)
       factor=0
c ilias: added the complementary acc-d1_mc term
5004  if(ext_cor.and.diag1.ne.1)then
       print*,'diag1 =/= 1'
       allocate(d1_mc(n0+1:n1,n1+1:n3,n0+1:n2,n2+1:n3))
       call reorder1324(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n0,n2,n2,n3,intm,d1_mc)
       allocate(d2_mc(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a_mc,d2_mc)
       allocate(s110_mc(n1+1:n3,n0+1:n1,n0+1:n2,n2+1:n3))
       i1=k4*k2
       i2=k1*k3
       i3=k3*k1
       call egemm(i1,i2,i3,d1_mc,d2_mc,s110_mc)
       deallocate(d1_mc)
       deallocate(d2_mc)
c
       allocate(d1_mc(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder3412(n1,n3,n0,n1,n0,n2,n2,n3,
     & n0,n2,n2,n3,n1,n3,n0,n1,s110_mc,d1_mc)
       allocate(d2_mc(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
       call reorder3124(n2,n3,n2,n3,n0,n2,n0,n2,
     & n0,n2,n2,n3,n2,n3,n0,n2,t2c_mc,d2_mc)
       allocate(z16_mc(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       i1=k1*k3
       i2=k2*k4
       i3=k4*k2
       call egemm(i1,i2,i3,d1_mc,d2_mc,z16_mc)
       deallocate(s110_mc)
       deallocate(d1_mc)
       deallocate(d2_mc)
c
       factor=(1.0-diag1)
       call
     & sum1324(n2,n3,n1,n3,n0,n2,n0,n1,v2b,z16_mc,factor)
       deallocate(z16_mc)
       factor=0
      endif
c
      if(diag3.eq.0)goto 5005
       allocate(d1(n0+1:n2,n0+1:n1,n2+1:n3,n1+1:n3))                 !ilias:acp d3
       call reorder2143(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n0,n1,n2,n3,n1,n3,intm,d1)
       allocate(d2(n0+1:n2,n0+1:n1,n2+1:n3,n1+1:n3))
       call reorder3412(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n0,n1,n2,n3,n1,n3,t2b,d2)
       allocate(q19(n1+1:n3,n1+1:n3))
       i1=k3
       i2=k3
       i3=k4*k1*k2
       call egemm(i1,i2,i3,d1,d2,q19)
       deallocate(d1)
       deallocate(d2)
c
       factor=-diag3
       call sum21(n1,n3,n1,n3,x6,q19,factor)
       deallocate(q19)
       factor=0
c ilias: added the complementary acc-d3_mc term
5005  if(ext_cor.and.diag3.ne.1)then
       print*,'diag3 =/= 1'
       allocate(d1_mc(n0+1:n2,n0+1:n1,n2+1:n3,n1+1:n3))
       call reorder2143(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n0,n1,n2,n3,n1,n3,intm,d1_mc)
       allocate(d2_mc(n0+1:n2,n0+1:n1,n2+1:n3,n1+1:n3))
       call reorder3412(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n0,n1,n2,n3,n1,n3,t2b_mc,d2_mc)
       allocate(q19_mc(n1+1:n3,n1+1:n3))
       i1=k3
       i2=k3
       i3=k4*k1*k2
       call egemm(i1,i2,i3,d1_mc,d2_mc,q19_mc)
       deallocate(d1_mc)
       deallocate(d2_mc)
c
       allocate(b1_mc(n1+1:n3,n1+1:n3))
       call reorder21(n1,n3,n1,n3,
     & n1,n3,n1,n3,q19_mc,b1_mc)
       allocate(d2_mc(n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder2134(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n2,n3,n0,n2,n0,n1,t2b_mc,d2_mc)
       allocate(z7_mc(n2+1:n3,n0+1:n2,n0+1:n1,n1+1:n3))
       i1=k3
       i2=k1*k2*k4
       i3=k3
       call egemm(i1,i2,i3,b1_mc,d2_mc,z7_mc)
       deallocate(q19_mc)
       deallocate(b1_mc)
       deallocate(d2_mc)
c
       factor=-(1.0-diag3)
       call
     & sum1342(n2,n3,n1,n3,n0,n2,n0,n1,v2b,z7_mc,factor)
       deallocate(z7_mc)
       factor=0
      endif
c
       call sumx21(0,n3,n1,n3,n1,n3,x6,fockr, 1.000)
c
       allocate(d2(n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder2134(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n2,n3,n0,n2,n0,n1,t2b,d2)
       allocate(z7(n2+1:n3,n0+1:n2,n0+1:n1,n1+1:n3))
       i1=k3
       i2=k1*k2*k4
       i3=k3
       call egemm(i1,i2,i3,x6,d2,z7)
       deallocate(d2)
c
       call
     & sum1342(n2,n3,n1,n3,n0,n2,n0,n1,v2b,z7, 1.000)
       deallocate(z7)
       deallocate(x6)
c
      if(diag3.eq.0)goto 5006
       allocate(d1(n0+1:n2,n0+1:n1,n1+1:n3,n2+1:n3))                 !ilias: acp d3
       call reorder2134(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n0,n1,n1,n3,n2,n3,intm,d1)
       allocate(d2(n0+1:n2,n0+1:n1,n1+1:n3,n2+1:n3))
       call reorder3421(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n0,n1,n1,n3,n2,n3,t2b,d2)
       allocate(q20(n2+1:n3,n2+1:n3))
       i1=k4
       i2=k4
       i3=k3*k1*k2
       call egemm(i1,i2,i3,d1,d2,q20)
       deallocate(d1)
       deallocate(d2)
c
       factor=-diag3
       call sum21(n2,n3,n2,n3,x8,q20,factor)
       deallocate(q20)
       factor=0
c ilias: added the complementary acc-d3_mc term
5006  if(ext_cor.and.diag3.ne.1)then
       print*,'diag3 =/= 1'
       allocate(d1_mc(n0+1:n2,n0+1:n1,n1+1:n3,n2+1:n3))
       call reorder2134(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n0,n1,n1,n3,n2,n3,intm,d1_mc)
       allocate(d2_mc(n0+1:n2,n0+1:n1,n1+1:n3,n2+1:n3))
       call reorder3421(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n0,n1,n1,n3,n2,n3,t2b_mc,d2_mc)
       allocate(q20_mc(n2+1:n3,n2+1:n3))
       i1=k4
       i2=k4
       i3=k3*k1*k2
       call egemm(i1,i2,i3,d1_mc,d2_mc,q20_mc)
       deallocate(d1_mc)
       deallocate(d2_mc)
c
       allocate(b1_mc(n2+1:n3,n2+1:n3))
       call reorder21(n2,n3,n2,n3,
     & n2,n3,n2,n3,q20_mc,b1_mc)
       allocate(d2_mc(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n0,n1,t2b_mc,d2_mc)
       allocate(z9_mc(n1+1:n3,n0+1:n2,n0+1:n1,n2+1:n3))
       i1=k4
       i2=k1*k2*k3
       i3=k4
       call egemm(i1,i2,i3,b1_mc,d2_mc,z9_mc)
       deallocate(q20_mc)
       deallocate(b1_mc)
       deallocate(d2_mc)
c
       factor=-(1.0-diag3)
       call
     & sum2341(n2,n3,n1,n3,n0,n2,n0,n1,v2b,z9_mc,factor)
       deallocate(z9_mc)
       factor=0
      endif
c
       allocate(d1(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder4321(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n0,n2,n0,n1,intm,d1)
       allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
       allocate(s114(n0+1:n2,n0+1:n1,n0+1:n2,n0+1:n1))
       i1=k1*k2
       i2=k1*k2
       i3=k3*k4
       call egemm(i1,i2,i3,d1,d2,s114)
       deallocate(d1)
       deallocate(d2)
c
      if(diag5.eq.0)goto 5007
       factor=diag5
       call sum3412(n0,n2,n0,n1,n0,n2,n0,n1,x10,s114,factor)        !ilias: acp d5
       factor=0
c ilias: added the complementary acc-d5_mc term
5007  if(ext_cor.and.diag5.ne.1)then
       print*,'diag5 =/= 1'
       allocate(d1_mc(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder4321(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n0,n2,n0,n1,intm,d1_mc)
       allocate(d2_mc(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n0,n1,t2b_mc,d2_mc)
       allocate(s114_mc(n0+1:n2,n0+1:n1,n0+1:n2,n0+1:n1))
       i1=k1*k2
       i2=k1*k2
       i3=k3*k4
       call egemm(i1,i2,i3,d1_mc,d2_mc,s114_mc)
       deallocate(d1_mc)
       deallocate(d2_mc)
c
       allocate(d1_mc(n0+1:n2,n0+1:n1,n0+1:n2,n0+1:n1))
       call reorder3412(n0,n2,n0,n1,n0,n2,n0,n1,
     & n0,n2,n0,n1,n0,n2,n0,n1,s114_mc,d1_mc)
       allocate(d2_mc(n0+1:n2,n0+1:n1,n2+1:n3,n1+1:n3))
       call reorder3412(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n0,n1,n2,n3,n1,n3,t2b_mc,d2_mc)
       allocate(z11_mc(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       i1=k1*k2
       i2=k3*k4
       i3=k1*k2
       call egemm(i1,i2,i3,d1_mc,d2_mc,z11_mc)
       deallocate(s114_mc)
       deallocate(d1_mc)
       deallocate(d2_mc)
c
       factor=(1.0-diag5)
       v2b=v2b+factor*z11_mc
       deallocate(z11_mc)
       factor=0
      endif
c
       call sumx2143(n0,n3,n0,n2,n0,n1,n0,n2,n0,n1,x10,intm, 1.000)
c
       allocate(d2(n0+1:n2,n0+1:n1,n2+1:n3,n1+1:n3))
       call reorder3412(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n0,n1,n2,n3,n1,n3,t2b,d2)
       allocate(z11(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       i1=k1*k2
       i2=k3*k4
       i3=k1*k2
       call egemm(i1,i2,i3,x10,d2,z11)
       deallocate(d2)
c
       v2b=v2b+z11
       deallocate(z11)
       deallocate(x10)
c
       allocate(d1(n0+1:n1,n0+1:n2,n0+1:n2,n0+1:n1))
       call reorder4312(n0,n2,n0,n1,n0,n2,n0,n1,
     & n0,n1,n0,n2,n0,n2,n0,n1,s114,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s148(n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       i1=k1*k2*k2
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s148)
       deallocate(d1)
       deallocate(b2)
       deallocate(s114)
c
       call sum2134(n0,n2,n1,n3,n0,n2,n0,n1,x2,s148,-1.000)
       deallocate(s148)
c
       call sumx2143(n0,n3,n0,n2,n1,n3,n0,n2,n0,n1,x2,intm, 1.000)
c
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(z3(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       i1=k1*k2*k3
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,x2,b2,z3)
       deallocate(b2)
c
       v2b=v2b-z3
       deallocate(z3)
       deallocate(x2)
c
      if(diag4.eq.0)goto 5008
       allocate(d1(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))                 !ilias: acp d4
       call reorder1432(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n2,n3,n1,n3,n0,n2,intm,d1)
       allocate(d2(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
       call reorder4123(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n2,n3,n1,n3,n0,n2,t2b,d2)
       allocate(q21(n0+1:n2,n0+1:n2))
       i1=k2
       i2=k2
       i3=k3*k4*k1
       call egemm(i1,i2,i3,d1,d2,q21)
       deallocate(d1)
       deallocate(d2)
c
       factor=diag4
       call sum21(n0,n2,n0,n2,x7,q21,factor)
       deallocate(q21)
       factor=0
c ilias: added the complementary acc-d4_mc term
5008  if(ext_cor.and.diag4.ne.1)then
       print*,'diag4 =/= 1'
       allocate(d1_mc(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
       call reorder1432(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n2,n3,n1,n3,n0,n2,intm,d1_mc)
       print*,'lol1'
       allocate(d2_mc(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
       call reorder4123(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n2,n3,n1,n3,n0,n2,t2b_mc,d2_mc)
       print*,'lol2'
       allocate(q21_mc(n0+1:n2,n0+1:n2))
       i1=k2
       i2=k2
       i3=k3*k4*k1
       call egemm(i1,i2,i3,d1_mc,d2_mc,q21_mc)
       print*,'lol3'
       deallocate(d1_mc)
       deallocate(d2_mc)
c
       allocate(b1_mc(n0+1:n2,n0+1:n2))
       call reorder21(n0,n2,n0,n2,
     & n0,n2,n0,n2,q21_mc,b1_mc)
       print*,'lol4'
       allocate(d2_mc(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n2,n3,n1,n3,n0,n1,t2b_mc,d2_mc)
       print*,'lol5'
       allocate(z8_mc(n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n2))
       i1=k2
       i2=k1*k3*k4
       i3=k2
       call egemm(i1,i2,i3,b1_mc,d2_mc,z8_mc)
       print*,'lol6'
       deallocate(q21_mc)
       deallocate(b1_mc)
       deallocate(d2_mc)
c
       factor=-(1.0-diag4)
       call
     & sum1243(n2,n3,n1,n3,n0,n2,n0,n1,v2b,z8_mc,factor)
       print*,'lol7'
       deallocate(z8_mc)
       factor=0
      endif
c
      if(diag2.eq.0)goto 5009
       allocate(d1(n0+1:n1,n2+1:n3,n0+1:n2,n1+1:n3))                !ilias: acp d2
       call reorder1423(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n2,n3,n0,n2,n1,n3,intm,d1)
       allocate(d2(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
       call reorder4123(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n2,n3,n1,n3,n0,n2,t2b,d2)
       allocate(s117(n1+1:n3,n0+1:n2,n0+1:n2,n1+1:n3))
       i1=k3*k2
       i2=k2*k3
       i3=k4*k1
       call egemm(i1,i2,i3,d1,d2,s117)
       deallocate(d1)
       deallocate(d2)
c
       factor=-diag2
       call sum3412(n0,n2,n1,n3,n1,n3,n0,n2,x12,s117,factor)
       deallocate(s117)
       factor=0
c ilias: added the complementary acc-d2_mc term
5009  if(ext_cor.and.diag2.ne.1)then
       print*,'diag2 =/= 1'
       allocate(d1_mc(n0+1:n1,n2+1:n3,n0+1:n2,n1+1:n3))
       call reorder1423(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n2,n3,n0,n2,n1,n3,intm,d1_mc)
       allocate(d2_mc(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
       call reorder4123(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n2,n3,n1,n3,n0,n2,t2b_mc,d2_mc)
       allocate(s117_mc(n1+1:n3,n0+1:n2,n0+1:n2,n1+1:n3))
       i1=k3*k2
       i2=k2*k3
       i3=k4*k1
       call egemm(i1,i2,i3,d1_mc,d2_mc,s117_mc)
       deallocate(d1_mc)
       deallocate(d2_mc)
c
       allocate(d1_mc(n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n2))
       call reorder3412(n1,n3,n0,n2,n0,n2,n1,n3,
     & n0,n2,n1,n3,n1,n3,n0,n2,s117_mc,d1_mc)
       allocate(d2_mc(n0+1:n2,n1+1:n3,n2+1:n3,n0+1:n1))
       call reorder3214(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n1,n3,n2,n3,n0,n1,t2b_mc,d2_mc)
       allocate(z13_mc(n2+1:n3,n0+1:n1,n1+1:n3,n0+1:n2))
       i1=k2*k3
       i2=k1*k4
       i3=k3*k2
       call egemm(i1,i2,i3,d1_mc,d2_mc,z13_mc)
       deallocate(s117_mc)
       deallocate(d1_mc)
       deallocate(d2_mc)
c
       factor=(1.0-diag2)
       call
     & sum1423(n2,n3,n1,n3,n0,n2,n0,n1,v2b,z13_mc,factor)
       deallocate(z13_mc)
       factor=0
      endif
c
       call sumx3124(n0,n3,n0,n2,n1,n3,n1,n3,n0,n2,x12,intm, 1.000)
c
       allocate(d2(n0+1:n2,n1+1:n3,n2+1:n3,n0+1:n1))
       call reorder3214(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n1,n3,n2,n3,n0,n1,t2b,d2)
       allocate(z13(n2+1:n3,n0+1:n1,n1+1:n3,n0+1:n2))
       i1=k2*k3
       i2=k1*k4
       i3=k3*k2
       call egemm(i1,i2,i3,x12,d2,z13)
       deallocate(d2)
c
       call
     & sum1423(n2,n3,n1,n3,n0,n2,n0,n1,v2b,z13,-1.000)
       deallocate(z13)
       deallocate(x12)
c
      if(diag1.eq.0)goto 5010
       allocate(d1(n0+1:n2,n2+1:n3,n0+1:n1,n1+1:n3))                 !ilias: acp d1
       call reorder2413(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n0,n1,n1,n3,intm,d1)
       allocate(d2(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n2,n3,n1,n3,n0,n1,t2b,d2)
       allocate(s119(n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3))
       i1=k3*k1
       i2=k1*k3
       i3=k4*k2
       call egemm(i1,i2,i3,d1,d2,s119)
       deallocate(d1)
       deallocate(d2)
c
       factor=diag1
       call sum3412(n0,n1,n1,n3,n1,n3,n0,n1,x9,s119,factor)
       deallocate(s119)
       factor=0
c ilias: added the complementary acc-d1_mc term
5010  if(ext_cor.and.diag1.ne.1)then
       print*,'diag1 =/= 1'
       allocate(d1_mc(n0+1:n2,n2+1:n3,n0+1:n1,n1+1:n3))
       call reorder2413(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n0,n1,n1,n3,intm,d1_mc)
       allocate(d2_mc(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n2,n3,n1,n3,n0,n1,t2b_mc,d2_mc)
       allocate(s119_mc(n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3))
       i1=k3*k1
       i2=k1*k3
       i3=k4*k2
       call egemm(i1,i2,i3,d1_mc,d2_mc,s119_mc)
       deallocate(d1_mc)
       deallocate(d2_mc)
c
       allocate(d1_mc(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3412(n1,n3,n0,n1,n0,n1,n1,n3,
     & n0,n1,n1,n3,n1,n3,n0,n1,s119_mc,d1_mc)
       allocate(d2_mc(n0+1:n1,n1+1:n3,n2+1:n3,n0+1:n2))
       call reorder4213(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n1,n3,n2,n3,n0,n2,t2b_mc,d2_mc)
       allocate(z10_mc(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       i1=k1*k3
       i2=k2*k4
       i3=k3*k1
       call egemm(i1,i2,i3,d1_mc,d2_mc,z10_mc)
       deallocate(s119_mc)
       deallocate(d1_mc)
       deallocate(d2_mc)
c
       factor=(1.0-diag1)
       call
     & sum1324(n2,n3,n1,n3,n0,n2,n0,n1,v2b,z10_mc,factor)
       deallocate(z10_mc)
       factor=0
      endif
c
       call sumx3142(n0,n3,n0,n1,n1,n3,n1,n3,n0,n1,x9,intr, 1.000)
c
       allocate(d2(n0+1:n1,n1+1:n3,n2+1:n3,n0+1:n2))
       call reorder4213(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n1,n3,n2,n3,n0,n2,t2b,d2)
       allocate(z10(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       i1=k1*k3
       i2=k2*k4
       i3=k3*k1
       call egemm(i1,i2,i3,x9,d2,z10)
       deallocate(d2)
c
       call
     & sum1324(n2,n3,n1,n3,n0,n2,n0,n1,v2b,z10, 1.000)
       deallocate(z10)
       deallocate(x9)
c
      if(diag4.eq.0)goto 5011
       allocate(d1(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))                 !ilias: acp d4
       call reorder2431(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n1,n3,n0,n1,intm,d1)
       allocate(d2(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n2,n3,n1,n3,n0,n1,t2b,d2)
       allocate(q22(n0+1:n1,n0+1:n1))
       i1=k1
       i2=k1
       i3=k3*k4*k2
       call egemm(i1,i2,i3,d1,d2,q22)
       deallocate(d1)
       deallocate(d2)
c
       factor=diag4
       call sum21(n0,n1,n0,n1,x5,q22,factor)
       deallocate(q22)
       factor=0
c ilias: added the complementary acc-d4_mc term
5011  if(ext_cor.and.diag4.ne.1)then
       print*,'diag4 =/= 1'
       allocate(d1_mc(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder2431(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n1,n3,n0,n1,intm,d1_mc)
       allocate(d2_mc(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n2,n3,n1,n3,n0,n1,t2b_mc,d2_mc)
       allocate(q22_mc(n0+1:n1,n0+1:n1))
       i1=k1
       i2=k1
       i3=k3*k4*k2
       call egemm(i1,i2,i3,d1_mc,d2_mc,q22_mc)
       deallocate(d1_mc)
       deallocate(d2_mc)
c
       allocate(b1_mc(n0+1:n1,n0+1:n1))
       call reorder21(n0,n1,n0,n1,
     & n0,n1,n0,n1,q22_mc,b1_mc)
       allocate(d2_mc(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
       call reorder4123(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n2,n3,n1,n3,n0,n2,t2b_mc,d2_mc)
       allocate(z6_mc(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       i1=k1
       i2=k2*k3*k4
       i3=k1
       call egemm(i1,i2,i3,b1_mc,d2_mc,z6_mc)
       deallocate(q22_mc)
       deallocate(b1_mc)
       deallocate(d2_mc)
c
       factor=(1.0-diag4)
       v2b=v2b-factor*z6_mc
       deallocate(z6_mc)
       factor=0
      endif
c
       call sumx12(0,n3,n0,n1,n0,n1,x5,fockr, 1.000)
c
       allocate(d2(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
       call reorder4123(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n2,n3,n1,n3,n0,n2,t2b,d2)
       allocate(z6(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       i1=k1
       i2=k2*k3*k4
       i3=k1
       call egemm(i1,i2,i3,x5,d2,z6)
       deallocate(d2)
c
       v2b=v2b-z6
       deallocate(z6)
       deallocate(x5)
c
      if(diag3.eq.0)goto 5012
       allocate(d1(n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3))                !ilias: acp d3
       call reorder2143(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n0,n2,n2,n3,n2,n3,intb,d1)
       allocate(d2(n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3))
       call reorder3412(n2,n3,n2,n3,n0,n2,n0,n2,
     & n0,n2,n0,n2,n2,n3,n2,n3,t2c,d2)
       allocate(q23(n2+1:n3,n2+1:n3))
       i1=k4
       i2=k4
       i3=k4*k2*k2
       call egemm(i1,i2,i3,d1,d2,q23)
       deallocate(d1)
       deallocate(d2)
c
       factor=-0.500*diag3
       call sum21(n2,n3,n2,n3,x8,q23,factor)
       deallocate(q23)
       factor=0
c ilias: added the complementary acc-d3_mc term
5012  if(ext_cor.and.diag3.ne.1)then
       print*,'diag3 =/= 1'
       allocate(d1_mc(n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3))
       call reorder2143(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n0,n2,n2,n3,n2,n3,intb,d1_mc)
       allocate(d2_mc(n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3))
       call reorder3412(n2,n3,n2,n3,n0,n2,n0,n2,
     & n0,n2,n0,n2,n2,n3,n2,n3,t2c_mc,d2_mc)
       allocate(q23_mc(n2+1:n3,n2+1:n3))
       i1=k4
       i2=k4
       i3=k4*k2*k2
       call egemm(i1,i2,i3,d1_mc,d2_mc,q23_mc)
       deallocate(d1_mc)
       deallocate(d2_mc)
c
       allocate(b1_mc(n2+1:n3,n2+1:n3))
       call reorder21(n2,n3,n2,n3,
     & n2,n3,n2,n3,q23_mc,b1_mc)
       allocate(d2_mc(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n0,n1,t2b_mc,d2_mc)
       allocate(z9_mc(n1+1:n3,n0+1:n2,n0+1:n1,n2+1:n3))
       i1=k4
       i2=k1*k2*k3
       i3=k4
       call egemm(i1,i2,i3,b1_mc,d2_mc,z9_mc)
       deallocate(q23_mc)
       deallocate(b1_mc)
       deallocate(d2_mc)
c
       factor=-0.500*(1.0-diag3)
       call
     & sum2341(n2,n3,n1,n3,n0,n2,n0,n1,v2b,z9_mc,factor)
       deallocate(z9_mc)
       factor=0
      endif
c
       call sumx21(0,n3,n2,n3,n2,n3,x8,fockb, 1.000)
c
       allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
       allocate(z9(n1+1:n3,n0+1:n2,n0+1:n1,n2+1:n3))
       i1=k4
       i2=k1*k2*k3
       i3=k4
       call egemm(i1,i2,i3,x8,d2,z9)
       deallocate(d2)
c
       call
     & sum2341(n2,n3,n1,n3,n0,n2,n0,n1,v2b,z9, 1.000)
       deallocate(z9)
       deallocate(x8)
c
      if(diag4.eq.0)goto 5013
       allocate(d1(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))                 !ilias: acp d4
       call reorder2431(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n2,n3,n0,n2,intb,d1)
       allocate(d2(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
       call reorder3124(n2,n3,n2,n3,n0,n2,n0,n2,
     & n0,n2,n2,n3,n2,n3,n0,n2,t2c,d2)
       allocate(q24(n0+1:n2,n0+1:n2))
       i1=k2
       i2=k2
       i3=k4*k4*k2
       call egemm(i1,i2,i3,d1,d2,q24)
       deallocate(d1)
       deallocate(d2)
c
       factor=0.500*diag4
       call sum21(n0,n2,n0,n2,x7,q24,factor)
       deallocate(q24)
       factor=0
c ilias: added the complementary acc-d4_mc term
5013  if(ext_cor.and.diag4.ne.1)then
       print*,'diag4 =/= 1'
       allocate(d1_mc(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
       call reorder2431(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n2,n3,n0,n2,intb,d1_mc)
       allocate(d2_mc(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
       call reorder3124(n2,n3,n2,n3,n0,n2,n0,n2,
     & n0,n2,n2,n3,n2,n3,n0,n2,t2c_mc,d2_mc)
       allocate(q24_mc(n0+1:n2,n0+1:n2))
       i1=k2
       i2=k2
       i3=k4*k4*k2
       call egemm(i1,i2,i3,d1_mc,d2_mc,q24_mc)
       deallocate(d1_mc)
       deallocate(d2_mc)
c
       allocate(b1_mc(n0+1:n2,n0+1:n2))
       call reorder21(n0,n2,n0,n2,
     & n0,n2,n0,n2,q24_mc,b1_mc)
       allocate(d2_mc(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n2,n3,n1,n3,n0,n1,t2b_mc,d2_mc)
       allocate(z8_mc(n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n2))
       i1=k2
       i2=k1*k3*k4
       i3=k2
       call egemm(i1,i2,i3,b1_mc,d2_mc,z8_mc)
       deallocate(q24_mc)
       deallocate(b1_mc)
       deallocate(d2_mc)
c
       factor=-0.500*(1.0-diag4)
       call
     & sum1243(n2,n3,n1,n3,n0,n2,n0,n1,v2b,z8_mc,factor)
       deallocate(z8_mc)
       factor=0
      endif
c
       call sumx12(0,n3,n0,n2,n0,n2,x7,fockb, 1.000)
c
       allocate(d2(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n2,n3,n1,n3,n0,n1,t2b,d2)
       allocate(z8(n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n2))
       i1=k2
       i2=k1*k3*k4
       i3=k2
       call egemm(i1,i2,i3,x7,d2,z8)
       deallocate(d2)
c
       call
     & sum1243(n2,n3,n1,n3,n0,n2,n0,n1,v2b,z8,-1.000)
       deallocate(z8)
       deallocate(x7)
c
      if(diag1.eq.0)goto 5014
       allocate(d1(n0+1:n2,n2+1:n3,n0+1:n2,n2+1:n3))                 !ilias: acp d1
      if(diag2.eq.0)then
       call reorder1324(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n0,n2,n2,n3,intm,d1)
       else
       call reorder1324(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n0,n2,n2,n3,intb,d1)
      endif
       allocate(d2(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n2,n3,n1,n3,n0,n1,t2b,d2)
       allocate(s124(n1+1:n3,n0+1:n1,n0+1:n2,n2+1:n3))
       i1=k4*k2
       i2=k1*k3
       i3=k4*k2
       call egemm(i1,i2,i3,d1,d2,s124)
       deallocate(d1)
       deallocate(d2)
c
       factor=diag1
       call sum3412(n0,n2,n2,n3,n1,n3,n0,n1,x14,s124,factor)
       deallocate(s124)
       factor=0
c ilias: added the complementary acc-d1_mc term
5014  if(ext_cor.and.diag1.ne.1)then
       print*,'diag1 =/= 1'
       allocate(d1_mc(n0+1:n2,n2+1:n3,n0+1:n2,n2+1:n3))
       call reorder1324(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n0,n2,n2,n3,intb,d1_mc)
       allocate(d2_mc(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n2,n3,n1,n3,n0,n1,t2b_mc,d2_mc)
       allocate(s124_mc(n1+1:n3,n0+1:n1,n0+1:n2,n2+1:n3))
       i1=k4*k2
       i2=k1*k3
       i3=k4*k2
       call egemm(i1,i2,i3,d1_mc,d2_mc,s124_mc)
       deallocate(d1_mc)
       deallocate(d2_mc)
c
       allocate(d1_mc(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder3412(n1,n3,n0,n1,n0,n2,n2,n3,
     & n0,n2,n2,n3,n1,n3,n0,n1,s124_mc,d1_mc)
       allocate(d2_mc(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
       call reorder3124(n2,n3,n2,n3,n0,n2,n0,n2,
     & n0,n2,n2,n3,n2,n3,n0,n2,t2c_mc,d2_mc)
       allocate(z16_mc(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       i1=k1*k3
       i2=k2*k4
       i3=k4*k2
       call egemm(i1,i2,i3,d1_mc,d2_mc,z16_mc)
       deallocate(s124_mc)
       deallocate(d1_mc)
       deallocate(d2_mc)
c
       factor=(1.0-diag1)
       call
     & sum1324(n2,n3,n1,n3,n0,n2,n0,n1,v2b,z16_mc,factor)
       deallocate(z16_mc)
       factor=0
      endif
c ilias: added the complementary acc-d2_mc term
      if(ext_cor.and.diag1.eq.1.and.diag2.eq.0)then
       print*,'diag2 =/= 1'
       allocate(d1_mc(n0+1:n2,n2+1:n3,n0+1:n2,n2+1:n3))
       call reorder1324(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n0,n2,n2,n3,intb,d1_mc)
       allocate(d2_mc(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n2,n3,n1,n3,n0,n1,t2b_mc,d2_mc)
       allocate(s124_mc(n1+1:n3,n0+1:n1,n0+1:n2,n2+1:n3))
       i1=k4*k2
       i2=k1*k3
       i3=k4*k2
       call egemm(i1,i2,i3,d1_mc,d2_mc,s124_mc)
       deallocate(d1_mc)
       deallocate(d2_mc)
c
       allocate(d1_mc(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder3412(n1,n3,n0,n1,n0,n2,n2,n3,
     & n0,n2,n2,n3,n1,n3,n0,n1,s124_mc,d1_mc)
       allocate(d2_mc(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
       call reorder3124(n2,n3,n2,n3,n0,n2,n0,n2,
     & n0,n2,n2,n3,n2,n3,n0,n2,t2c_mc,d2_mc)
       allocate(z16_mc(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       i1=k1*k3
       i2=k2*k4
       i3=k4*k2
       call egemm(i1,i2,i3,d1_mc,d2_mc,z16_mc)
       deallocate(s124_mc)
       deallocate(d1_mc)
       deallocate(d2_mc)
c
       factor=diag1
       call
     & sum1324(n2,n3,n1,n3,n0,n2,n0,n1,v2b,z16_mc,factor)
       deallocate(z16_mc)
       factor=0
c
       allocate(d1_mc(n0+1:n2,n2+1:n3,n0+1:n2,n2+1:n3))
       call reorder1324(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n0,n2,n2,n3,intm,d1_mc)
       allocate(d2_mc(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n2,n3,n1,n3,n0,n1,t2b_mc,d2_mc)
       allocate(s124_mc(n1+1:n3,n0+1:n1,n0+1:n2,n2+1:n3))
       i1=k4*k2
       i2=k1*k3
       i3=k4*k2
       call egemm(i1,i2,i3,d1_mc,d2_mc,s124_mc)
       deallocate(d1_mc)
       deallocate(d2_mc)
c
       allocate(d1_mc(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder3412(n1,n3,n0,n1,n0,n2,n2,n3,
     & n0,n2,n2,n3,n1,n3,n0,n1,s124_mc,d1_mc)
       allocate(d2_mc(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
       call reorder3124(n2,n3,n2,n3,n0,n2,n0,n2,
     & n0,n2,n2,n3,n2,n3,n0,n2,t2c_mc,d2_mc)
       allocate(z16_mc(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       i1=k1*k3
       i2=k2*k4
       i3=k4*k2
       call egemm(i1,i2,i3,d1_mc,d2_mc,z16_mc)
       deallocate(s124_mc)
       deallocate(d1_mc)
       deallocate(d2_mc)
c
       factor=-diag1
       call
     & sum1324(n2,n3,n1,n3,n0,n2,n0,n1,v2b,z16_mc,factor)
       deallocate(z16_mc)
       factor=0
      endif
c
       call sumx3142(n0,n3,n0,n2,n2,n3,n1,n3,n0,n1,x14,intm, 1.000)
c
       allocate(d2(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
       call reorder3124(n2,n3,n2,n3,n0,n2,n0,n2,
     & n0,n2,n2,n3,n2,n3,n0,n2,t2c,d2)
       allocate(z16(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       i1=k1*k3
       i2=k2*k4
       i3=k4*k2
       call egemm(i1,i2,i3,x14,d2,z16)
       deallocate(d2)
c
       call
     & sum1324(n2,n3,n1,n3,n0,n2,n0,n1,v2b,z16, 1.000)
       deallocate(z16)
       deallocate(x14)
c
       allocate(d1(n1+1:n3,n2+1:n3,n1+1:n3,n0+1:n2))
       call reorder3214(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n2,n3,n1,n3,n0,n2,intm,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(z2(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
       i1=k2*k3*k4
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,z2)
       deallocate(d1)
       deallocate(b2)
c
       call
     & sum4123(n2,n3,n1,n3,n0,n2,n0,n1,v2b,z2, 1.000)
       deallocate(z2)
c
       allocate(d1(n2+1:n3,n1+1:n3,n2+1:n3,n1+1:n3))
       call reorder4321(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n2,n3,n1,n3,intm,d1)
       allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
       allocate(z14(n0+1:n2,n0+1:n1,n2+1:n3,n1+1:n3))
       i1=k3*k4
       i2=k1*k2
       i3=k3*k4
       call egemm(i1,i2,i3,d1,d2,z14)
       deallocate(d1)
       deallocate(d2)
c
       call
     & sum3412(n2,n3,n1,n3,n0,n2,n0,n1,v2b,z14, 1.000)
       deallocate(z14)
c
      if(lvl1.eq.'ccsdt'.or.lvl1.eq.'3cc'.or.lvl1.eq.'test')then
      if(dt3diag4.eq.0)goto 7009
       allocate(d1(n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3))
       call reorder2431(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n1,n3,n1,n3,intr,d1)
       allocate(f2(n0+1:n1,n1+1:n3,n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder523146(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n2,n3,n0,n2,n0,n1,t3b,f2)
       allocate(z19(n2+1:n3,n0+1:n2,n0+1:n1,n1+1:n3))
       i1=k3
       i2=k1*k2*k4
       i3=k3*k3*k1
       call egemm(i1,i2,i3,d1,f2,z19)
       deallocate(d1)
       deallocate(f2)
c
       factor=0.500*dt3diag4
       call
     & sum1342(n2,n3,n1,n3,n0,n2,n0,n1,v2b,z19,factor)
       deallocate(z19)
c
       allocate(d1(n0+1:n1,n2+1:n3,n1+1:n3,n2+1:n3))
       call reorder1432(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n2,n3,n1,n3,n2,n3,intm,d1)
       allocate(f2(n0+1:n1,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder512346(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n0,n1,n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,t3b,f2)
       allocate(z21(n1+1:n3,n0+1:n2,n0+1:n1,n2+1:n3))
       i1=k4
       i2=k1*k2*k3
       i3=k3*k4*k1
       call egemm(i1,i2,i3,d1,f2,z21)
       deallocate(d1)
       deallocate(f2)
c
       factor=dt3diag4
       call
     & sum2341(n2,n3,n1,n3,n0,n2,n0,n1,v2b,z21,factor)
       deallocate(z21)
c
       allocate(d1(n0+1:n2,n2+1:n3,n1+1:n3,n1+1:n3))
       call reorder2431(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n1,n3,n1,n3,intm,d1)
       allocate(f2(n0+1:n2,n2+1:n3,n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder413256(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & n0,n2,n2,n3,n1,n3,n2,n3,n0,n2,n0,n1,t3c,f2)
       allocate(z24(n2+1:n3,n0+1:n2,n0+1:n1,n1+1:n3))
       i1=k3
       i2=k1*k2*k4
       i3=k3*k4*k2
       call egemm(i1,i2,i3,d1,f2,z24)
       deallocate(d1)
       deallocate(f2)
c
       factor=dt3diag4
       call
     & sum1342(n2,n3,n1,n3,n0,n2,n0,n1,v2b,z24,factor)
       deallocate(z24)
c
       allocate(d1(n0+1:n2,n2+1:n3,n2+1:n3,n2+1:n3))
       call reorder2431(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n2,n3,n2,n3,intb,d1)
       allocate(f2(n0+1:n2,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder412356(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & n0,n2,n2,n3,n2,n3,n1,n3,n0,n2,n0,n1,t3c,f2)
       allocate(z26(n1+1:n3,n0+1:n2,n0+1:n1,n2+1:n3))
       i1=k4
       i2=k1*k2*k3
       i3=k4*k4*k2
       call egemm(i1,i2,i3,d1,f2,z26)
       deallocate(d1)
       deallocate(f2)
c
       factor=0.500*dt3diag4
       call
     & sum2341(n2,n3,n1,n3,n0,n2,n0,n1,v2b,z26,factor)
       deallocate(z26)
7009  endif
c
!        goto 601
!       allocate(d1(n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3))
!       call reorder2143(n0,n3,n0,n3,n0,n3,n0,n3,
!     & n0,n1,n0,n1,n1,n3,n1,n3,intr,d1)
!a!       rewind(tb)
!a!       allocate(t4b(k4*k3*k3*k3,k2*k1*k1*k1))
!a!       read(tb)t4b
!a!       i2=k1*k2*k3*k4
!a!       i1=k1*k1*k3*k3
!a!       allocate(b2(i1,i2))
!a!       allocate(z27(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
!a!       allocate(ind1(n2+1:n3,n1+1:n3,n1+1:n3,n1+1:n3))
!a!       allocate(ind2(n0+1:n2,n0+1:n1,n0+1:n1,n0+1:n1))
!a!       i1=0
!a!       ind1=0
!a!       ind2=0
!a!       do a=n1+1,n3
!a!        do e=n1+1,n3
!a!         do f=n1+1,n3
!a!          do b=n2+1,n3
!a!           i1=i1+1
!a!           ind1(b,f,e,a)=i1
!a!       enddo;enddo;enddo;enddo
!a!       i1=0
!a!       do i=n0+1,n1
!a!        do m=n0+1,n1
!a!         do n=n0+1,n1
!a!          do j=n0+1,n2
!a!           i1=i1+1
!a!           ind2(j,n,m,i)=i1
!a!       enddo;enddo;enddo;enddo
!a!       i2=0
!a!       do i=n0+1,n1
!a!        do j=n0+1,n2
!a!         do a=n1+1,n3
!a!          do b=n2+1,n3
!a!           i2=i2+1
!a!           i1=0
!a!           do f=n1+1,n3
!a!            do e=n1+1,n3
!a!             do n=n0+1,n1
!a!              do m=n0+1,n1
!a!              i1=i1+1
!a!              b2(i1,i2)=t4b(ind1(b,f,e,a),ind2(j,n,m,i))
!a!           enddo;enddo;enddo;enddo
!a!!              call egemm2(1,i1,d1,b2(1,i2),z27(b,a,j,i))
!a!              call egemm2(1,i1,intr(n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3),
!a!     & b2(1,i2),z27(b,a,j,i))
!a!              v2b(b,a,j,i)=v2b(b,a,j,i)+0.250*z27(b,a,j,i)
!a!       enddo;enddo;enddo;enddo
!a!       deallocate(t4b)
!a!       deallocate(ind1)
!a!       deallocate(ind2)
!a!!       deallocate(d1)
!a!       deallocate(b2)
!a!       deallocate(z27)
c
      if(lvlq.eq.'ccsdtq'.or.lvlq.eq.'4cc')then
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       allocate(t4b(n2+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,                !ilias: if no quadruples comment out the following 21 lines
     & n0+1:n2,n0+1:n1,n0+1:n1,n0+1:n1))
       rewind(tb)
       read(tb)t4b
       allocate(d1(n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3))
       call reorder2143(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n0,n1,n1,n3,n1,n3,intr,d1)
       allocate(h2(n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3,n2+1:n3,n1+1:n3,
     & n0+1:n2,n0+1:n1))
       call reorder67231458(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n0,n1,n0,n1,n0,n1,n1,n3,n1,n3,n2,n3,n1,n3,n0,n2,n0,n1,t4b,h2)
       allocate(z27(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       i2=k1*k2*k3*k4
       i3=k3*k3*k1*k1
       call egemm2(i2,i3,d1,h2,z27)
       deallocate(d1)
       deallocate(h2)
       deallocate(t4b)
c
       v2b=v2b+0.250*z27
       deallocate(z27)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c
!       allocate(d1(n0+1:n2,n0+1:n1,n2+1:n3,n1+1:n3))
!       call reorder2143(n0,n3,n0,n3,n0,n3,n0,n3,
!     & n0,n2,n0,n1,n2,n3,n1,n3,intm,d1)
!a!       rewind(tc)
!a!       allocate(t4c(k4*k4*k3*k3,k2*k2*k1*k1))
!a!       read(tc)t4c
!a!       i2=k1*k2*k3*k4
!a!       i1=k1*k2*k3*k4
!a!       allocate(b2(i1,i2))
!a!       allocate(z28(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
!a!       allocate(ind1(n2+1:n3,n2+1:n3,n1+1:n3,n1+1:n3))
!a!       allocate(ind2(n0+1:n2,n0+1:n2,n0+1:n1,n0+1:n1))
!a!       i1=0
!a!       ind1=0
!a!       ind2=0
!a!       do a=n1+1,n3
!a!        do e=n1+1,n3
!a!         do b=n2+1,n3
!a!          do f=n2+1,n3
!a!           i1=i1+1
!a!           ind1(f,b,e,a)=i1
!a!       enddo;enddo;enddo;enddo
!a!       i1=0
!a!       do i=n0+1,n1
!a!        do m=n0+1,n1
!a!         do j=n0+1,n2
!a!          do n=n0+1,n2
!a!           i1=i1+1
!a!           ind2(n,j,m,i)=i1
!a!       enddo;enddo;enddo;enddo
!a!       i2=0
!a!       do i=n0+1,n1
!a!        do j=n0+1,n2
!a!         do a=n1+1,n3
!a!          do b=n2+1,n3
!a!           i2=i2+1
!a!           i1=0
!a!           do f=n2+1,n3
!a!            do e=n1+1,n3
!a!             do n=n0+1,n2
!a!              do m=n0+1,n1
!a!              i1=i1+1
!a!              b2(i1,i2)=t4c(ind1(f,b,e,a),ind2(n,j,m,i))
!a!           enddo;enddo;enddo;enddo
!a!!              call egemm2(1,i1,d1,b2(1,i2),z28(b,a,j,i))
!a!              call egemm2(1,i1,intm(n0+1:n1,n0+1:n2,n1+1:n3,n2+1:n3),
!a!     & b2(1,i2),z28(b,a,j,i))
!a!              v2b(b,a,j,i)=v2b(b,a,j,i)+z28(b,a,j,i)
!a!       enddo;enddo;enddo;enddo
!a!       deallocate(t4c)
!a!       deallocate(ind1)
!a!       deallocate(ind2)
!a!!       deallocate(d1)
!a!       deallocate(b2)
!a!       deallocate(z28)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       allocate(t4c(n2+1:n3,n2+1:n3,n1+1:n3,n1+1:n3,                !ilias: if no quadruples comment out the following 21 lines
     & n0+1:n2,n0+1:n2,n0+1:n1,n0+1:n1))
       rewind(tc)
       read(tc)t4c
       allocate(d1(n0+1:n2,n0+1:n1,n2+1:n3,n1+1:n3))
       call reorder2143(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n0,n1,n2,n3,n1,n3,intm,d1)
       allocate(h2(n0+1:n2,n0+1:n1,n2+1:n3,n1+1:n3,n2+1:n3,n1+1:n3,
     & n0+1:n2,n0+1:n1))
       call reorder57132468(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & n0,n1,n0,n2,n0,n1,n2,n3,n1,n3,n2,n3,n1,n3,n0,n2,n0,n1,t4c,h2)
       allocate(z28(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       i2=k1*k2*k3*k4
       i3=k3*k4*k1*k2
       call egemm2(i2,i3,d1,h2,z28)
       deallocate(d1)
       deallocate(h2)
       deallocate(t4c)
c
       v2b=v2b+z28
       deallocate(z28)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c
!       allocate(d1(n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3))
!       call reorder2143(n0,n3,n0,n3,n0,n3,n0,n3,
!     & n0,n2,n0,n2,n2,n3,n2,n3,intb,d1)
!a!       rewind(td)
!a!       allocate(t4d(k4*k4*k4*k3,k2*k2*k2*k1))
!a!       read(td)t4d
!a!       i2=k1*k2*k3*k4
!a!       i1=k2*k2*k4*k4
!a!       allocate(b2(i1,i2))
!a!       allocate(z29(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
!a!       allocate(ind1(n2+1:n3,n2+1:n3,n2+1:n3,n1+1:n3))
!a!       allocate(ind2(n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n1))
!a!       i1=0
!a!       ind1=0
!a!       ind2=0
!a!       do a=n1+1,n3
!a!        do b=n2+1,n3
!a!         do e=n2+1,n3
!a!          do f=n2+1,n3
!a!           i1=i1+1
!a!           ind1(f,e,b,a)=i1
!a!       enddo;enddo;enddo;enddo
!a!       i1=0
!a!       do i=n0+1,n1
!a!        do j=n0+1,n2
!a!         do m=n0+1,n2
!a!          do n=n0+1,n2
!a!           i1=i1+1
!a!           ind2(n,m,j,i)=i1
!a!       enddo;enddo;enddo;enddo
!a!       i2=0
!a!       do i=n0+1,n1
!a!        do j=n0+1,n2
!a!         do a=n1+1,n3
!a!          do b=n2+1,n3
!a!           i2=i2+1
!a!           i1=0
!a!           do f=n2+1,n3
!a!            do e=n2+1,n3
!a!             do n=n0+1,n2
!a!              do m=n0+1,n2
!a!              i1=i1+1
!a!              b2(i1,i2)=t4d(ind1(f,e,b,a),ind2(n,m,j,i))
!a!           enddo;enddo;enddo;enddo
!a!!              call egemm2(1,i1,d1,b2(1,i2),z29(b,a,j,i))
!a!              call egemm2(1,i1,intb(n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3),
!a!     & b2(1,i2),z29(b,a,j,i))
!a!              v2b(b,a,j,i)=v2b(b,a,j,i)+0.250*z29(b,a,j,i)
!a!       enddo;enddo;enddo;enddo
!a!       deallocate(t4d)
!a!       deallocate(ind1)
!a!       deallocate(ind2)
!a!!       deallocate(d1)
!a!       deallocate(b2)
!a!       deallocate(z29)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       allocate(t4d(n2+1:n3,n2+1:n3,n2+1:n3,n1+1:n3,                !ilias: if no quadruples comment out the following 21 lines
     & n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n1))
       rewind(td)
       read(td)t4d
       allocate(d1(n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3))
       call reorder2143(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n0,n2,n2,n3,n2,n3,intb,d1)
       allocate(h2(n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3,n2+1:n3,n1+1:n3,
     & n0+1:n2,n0+1:n1))
       call reorder56123478(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,
     & n0,n1,n0,n2,n0,n2,n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n1,t4d,h2)
       allocate(z29(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       i2=k1*k2*k3*k4
       i3=k4*k4*k2*k2
       call egemm2(i2,i3,d1,h2,z29)
       deallocate(d1)
       deallocate(h2)
       deallocate(t4d)
c
       v2b=v2b+0.250*z29
       deallocate(z29)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c
      endif
       do i=n0+1,n1
       do j=n0+1,n2
       do a=n1+1,n3
       do b=n2+1,n3
         coeleft=fockb(b,b)
     &          +fockr(a,a)
     &          -fockb(j,j)
     &          -fockr(i,i)
     &          +shift
         t2b(b,a,j,i)=t2b(b,a,j,i)-v2b(b,a,j,i)/coeleft
       enddo
       enddo
       enddo
       enddo
c
       end
