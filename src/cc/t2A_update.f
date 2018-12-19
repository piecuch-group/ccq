       subroutine t2a_update(n0,n1,n2,n3,k1,k2,k3,k4,lvl_t,lvl_q,shift
     & ,v2a,ext_cor,fockr,fockb,intr,intb,intm,
     & diag1,diag2,diag3,diag4,diag5,
     & t1diag1,t1diag2,t1diag3,t1diag4,
     & dt3diag3,dt3diag4,
     & t1a,t1b,t2a,t2b,t2c,t3a,t3b,t3c,t3d,
     & t2a_mc,t2b_mc,t2c_mc)
!     & t4a,t4b,t4c,t4d,t4e)
c
       integer a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p
       logical lvl_t, lvl_q
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
!      real*8 t4b(n2+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,
!    & n0+1:n2,n0+1:n1,n0+1:n1,n0+1:n1)
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
       real*8 v2a(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1)
c
       real*8,allocatable::t4a(:,:,:,:,:,:,:,:)                       !ilias: if no quadruples comment out the next 6 lines
       real*8,allocatable::t4b(:,:,:,:,:,:,:,:)
       real*8,allocatable::t4c(:,:,:,:,:,:,:,:)
c
       integer ta,tb,tc,td,te
       parameter(ta=29,tb=30,tc=31,td=32,te=33)
c
!       integer i1,i2
!       integer,allocatable::ind1(:,:,:,:)
!       integer,allocatable::ind2(:,:,:,:)
c
       real*8,allocatable::b1(:,:)
       real*8,allocatable::b1_mc(:,:)
       real*8,allocatable::b2(:,:)
       real*8,allocatable::d1(:,:,:,:)
       real*8,allocatable::d2(:,:,:,:)
       real*8,allocatable::d1_mc(:,:,:,:)
       real*8,allocatable::d2_mc(:,:,:,:)
       real*8,allocatable::f2(:,:,:,:,:,:)
       real*8,allocatable::h2(:,:,:,:,:,:,:,:)
c
       real*8,allocatable::s18(:,:,:,:)
       real*8,allocatable::s20(:,:,:,:)
       real*8,allocatable::s22(:,:,:,:)
       real*8,allocatable::q1(:,:)
       real*8,allocatable::q2(:,:)
       real*8,allocatable::s26(:,:,:,:)
       real*8,allocatable::s66(:,:,:,:)
       real*8,allocatable::s28(:,:,:,:)
       real*8,allocatable::q3(:,:)
       real*8,allocatable::s31(:,:,:,:)
       real*8,allocatable::s68(:,:,:,:)
       real*8,allocatable::s33(:,:,:,:)
       real*8,allocatable::q4(:,:)
       real*8,allocatable::s36(:,:,:,:)
       real*8,allocatable::s38(:,:,:,:)
       real*8,allocatable::s40(:,:,:,:)
       real*8,allocatable::q15(:,:)
       real*8,allocatable::s72(:,:,:,:)
       real*8,allocatable::s70(:,:,:,:)
       real*8,allocatable::s82(:,:,:,:)
       real*8,allocatable::s42(:,:,:,:)
       real*8,allocatable::q5(:,:)
       real*8,allocatable::q16(:,:)
       real*8,allocatable::s45(:,:,:,:)
       real*8,allocatable::q17(:,:)
       real*8,allocatable::s78(:,:,:,:)
       real*8,allocatable::s47(:,:,:,:)
       real*8,allocatable::q6(:,:)
       real*8,allocatable::q7(:,:)
       real*8,allocatable::q8(:,:)
       real*8,allocatable::q9(:,:)
       real*8,allocatable::q18(:,:)
       real*8,allocatable::q10(:,:)
       real*8,allocatable::q11(:,:)
       real*8,allocatable::q11_mc(:,:)
       real*8,allocatable::s55(:,:,:,:)
       real*8,allocatable::s55_mc(:,:,:,:)
       real*8,allocatable::s75(:,:,:,:)
       real*8,allocatable::q12(:,:)
       real*8,allocatable::q12_mc(:,:)
       real*8,allocatable::s58(:,:,:,:)
       real*8,allocatable::s58_mc(:,:,:,:)
       real*8,allocatable::q13(:,:)
       real*8,allocatable::q13_mc(:,:)
       real*8,allocatable::q14(:,:)
       real*8,allocatable::q14_mc(:,:)
       real*8,allocatable::s62(:,:,:,:)
       real*8,allocatable::s62_mc(:,:,:,:)
       real*8,allocatable::s64(:,:,:,:)
       real*8,allocatable::s64_mc(:,:,:,:)
       real*8,allocatable::x1(:,:,:,:)
       real*8,allocatable::z1(:,:,:,:)
       real*8,allocatable::z2(:,:,:,:)
       real*8,allocatable::x2(:,:)
       real*8,allocatable::z3(:,:,:,:)
       real*8,allocatable::z3_mc(:,:,:,:)
       real*8,allocatable::x3(:,:)
       real*8,allocatable::z4(:,:,:,:)
       real*8,allocatable::z4_mc(:,:,:,:)
       real*8,allocatable::x4(:,:,:,:)
       real*8,allocatable::z5(:,:,:,:)
       real*8,allocatable::z5_mc(:,:,:,:)
       real*8,allocatable::x5(:,:,:,:)
       real*8,allocatable::z6(:,:,:,:)
       real*8,allocatable::z7(:,:,:,:)
       real*8,allocatable::x6(:,:,:,:)
       real*8,allocatable::z8(:,:,:,:)
       real*8,allocatable::z8_mc(:,:,:,:)
       real*8,allocatable::x7(:,:)
       real*8,allocatable::z9(:,:,:,:)
       real*8,allocatable::x8(:,:,:,:)
       real*8,allocatable::z10(:,:,:,:)
       real*8,allocatable::z11(:,:,:,:)
       real*8,allocatable::x9(:,:)
       real*8,allocatable::z12(:,:,:,:)
       real*8,allocatable::x10(:,:,:,:)
       real*8,allocatable::z13(:,:,:,:)
       real*8,allocatable::z14(:,:,:,:)
       real*8,allocatable::z15(:,:,:,:)
       real*8,allocatable::z16(:,:,:,:)
       real*8,allocatable::z17(:,:,:,:)
       real*8,allocatable::x11(:,:,:,:)
       real*8,allocatable::z19(:,:,:,:)
       real*8,allocatable::x12(:,:,:,:)
       real*8,allocatable::z21(:,:,:,:)
       real*8,allocatable::z23(:,:,:,:)
       real*8,allocatable::z67(:,:,:,:)
       real*8,allocatable::z27(:,:,:,:)
       real*8,allocatable::z54(:,:,:,:)
       real*8,allocatable::z54_mc(:,:,:,:)
       real*8,allocatable::z59(:,:,:,:)
       real*8,allocatable::z59_mc(:,:,:,:)
       real*8,allocatable::z65(:,:,:,:)
       real*8,allocatable::z65_mc(:,:,:,:)
c
       factor=0
c
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
       allocate(d1(n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       call reorder1243(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n0,n1,n0,n1,n0,n1,intr,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s18(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1*k1
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s18)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x11(n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       x11=0.0d0
       call sum2134(n0,n1,n1,n3,n0,n1,n0,n1,x11,s18, 1.000)
       deallocate(s18)
c
       allocate(d1(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       call reorder4213(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n1,n3,n0,n1,intr,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s20(n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1))
       i1=k1*k3*k1
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s20)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x12(n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       x12=0.0d0
       call sum3124(n0,n1,n1,n3,n0,n1,n0,n1,x12,s20, 1.000)
       deallocate(s20)
c
       allocate(d1(n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3))
       call reorder3421(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n1,n3,n1,n3,n1,n3,intr,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s22(n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3))
       i1=k3*k3*k3
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s22)
       deallocate(d1)
       deallocate(b2)
c
       allocate(d1(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder2341(n0,n1,n1,n3,n1,n3,n1,n3,
     & n1,n3,n1,n3,n1,n3,n0,n1,s22,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(z23(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       i1=k1*k3*k3
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,z23)
       deallocate(d1)
       deallocate(b2)
c
       call
     & sum3124(n1,n3,n1,n3,n0,n1,n0,n1,v2a,z23, 1.000)
       deallocate(z23)
       deallocate(s22)
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
       allocate(x2(n0+1:n1,n0+1:n1))
       x2=0.0d0
       call sum21(n0,n1,n0,n1,x2,q1, 1.000)
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
       allocate(x3(n1+1:n3,n1+1:n3))
       x3=0.0d0
       call sum21(n1,n3,n1,n3,x3,q2,-1.000)
       deallocate(q2)
c
       allocate(d1(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       call reorder4213(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n0,n1,n0,n1,intr,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s26(n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1*k1
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s26)
       deallocate(d1)
       deallocate(b2)
c
       allocate(d1(n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       call reorder2314(n0,n1,n0,n1,n0,n1,n0,n1,
     & n0,n1,n0,n1,n0,n1,n0,n1,s26,d1)
       allocate(d2(n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3))
       call reorder3412(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n0,n1,n1,n3,n1,n3,t2a,d2)
       allocate(z27(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       i1=k1*k1
       i2=k3*k3
       i3=k1*k1
       call egemm(i1,i2,i3,d1,d2,z27)
       deallocate(d1)
       deallocate(d2)
c
       v2a=v2a+0.500*z27
       call
     & sum1243(n1,n3,n1,n3,n0,n1,n0,n1,v2a,z27,-0.500)
       deallocate(z27)
c
       allocate(d1(n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       call reorder3214(n0,n1,n0,n1,n0,n1,n0,n1,
     & n0,n1,n0,n1,n0,n1,n0,n1,s26,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s66(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1*k1
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s66)
       deallocate(d1)
       deallocate(b2)
       deallocate(s26)
c
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder2134(n1,n3,n0,n1,n0,n1,n0,n1,
     & n0,n1,n1,n3,n0,n1,n0,n1,s66,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(z67(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       i1=k1*k1*k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,z67)
       deallocate(d1)
       deallocate(b2)
c
       v2a=v2a+z67
       call
     & sum1243(n1,n3,n1,n3,n0,n1,n0,n1,v2a,z67,-1.000)
       deallocate(z67)
       deallocate(s66)
c
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder2413(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n0,n1,n0,n1,intr,d1)
       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
       allocate(s28(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1
       i2=k1*k3
       i3=k3*k1
       call egemm(i1,i2,i3,d1,d2,s28)
       deallocate(d1)
       deallocate(d2)
c
       call sum2314(n0,n1,n1,n3,n0,n1,n0,n1,x12,s28,-1.000)
       deallocate(s28)
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
       x2=x2-q3
       deallocate(q3)
c
       allocate(d1(n1+1:n3,n0+1:n1,n1+1:n3,n1+1:n3))
       call reorder3241(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n1,n3,n1,n3,intr,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s31(n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3))
       i1=k3*k3*k1
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s31)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x5(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       x5=0.0d0
       call sum4123(n0,n1,n1,n3,n1,n3,n0,n1,x5,s31, 1.000)
c
       call sumx3142(n0,n3,n0,n1,n1,n3,n1,n3,n0,n1,x5,intr, 1.000)
c
       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
       allocate(z6(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       i1=k1*k3
       i2=k1*k3
       i3=k3*k1
       call egemm(i1,i2,i3,x5,d2,z6)
       deallocate(d2)
c
       call
     & sum2314(n1,n3,n1,n3,n0,n1,n0,n1,v2a,z6,-1.000)
       call
     & sum1324(n1,n3,n1,n3,n0,n1,n0,n1,v2a,z6, 1.000)
       call
     & sum2413(n1,n3,n1,n3,n0,n1,n0,n1,v2a,z6, 1.000)
       call
     & sum1423(n1,n3,n1,n3,n0,n1,n0,n1,v2a,z6,-1.000)
       deallocate(z6)
       deallocate(x5)
c
       allocate(d1(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       call reorder3241(n0,n1,n0,n1,n1,n3,n1,n3,
     & n1,n3,n0,n1,n1,n3,n0,n1,s31,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s68(n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1))
       i1=k1*k3*k1
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s68)
       deallocate(d1)
       deallocate(b2)
       deallocate(s31)
c
       allocate(x1(n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       x1=0.0d0
       call sum3124(n0,n1,n1,n3,n0,n1,n0,n1,x1,s68, 1.000)
       deallocate(s68)
c
       allocate(d1(n1+1:n3,n1+1:n3,n0+1:n1,n1+1:n3))
       call reorder4321(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n1,n3,n0,n1,n1,n3,intr,d1)
       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
       allocate(s33(n0+1:n1,n0+1:n1,n0+1:n1,n1+1:n3))
       i1=k3*k1
       i2=k1*k1
       i3=k3*k3
       call egemm(i1,i2,i3,d1,d2,s33)
       deallocate(d1)
       deallocate(d2)
c
       call sum3412(n0,n1,n1,n3,n0,n1,n0,n1,x1,s33, 0.500)
       deallocate(s33)
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
       x3=x3-q4
       deallocate(q4)
c
       allocate(d1(n0+1:n2,n2+1:n3,n0+1:n1,n0+1:n1))
       call reorder2413(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n0,n1,n0,n1,intm,d1)
       allocate(d2(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n2,n3,n1,n3,n0,n1,t2b,d2)
       allocate(s36(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1
       i2=k1*k3
       i3=k4*k2
       call egemm(i1,i2,i3,d1,d2,s36)
       deallocate(d1)
       deallocate(d2)
c
       call sum2314(n0,n1,n1,n3,n0,n1,n0,n1,x12,s36,-1.000)
       deallocate(s36)
c
       allocate(d1(n1+1:n3,n0+1:n2,n2+1:n3,n1+1:n3))
       call reorder3241(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n2,n2,n3,n1,n3,intm,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s38(n0+1:n1,n0+1:n2,n2+1:n3,n1+1:n3))
       i1=k3*k4*k2
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s38)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x6(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       x6=0.0d0
       call sum4123(n0,n2,n2,n3,n1,n3,n0,n1,x6,s38, 1.000)
       deallocate(s38)
c
       allocate(d1(n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3))
       call reorder3214(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n0,n1,n1,n3,intr,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s40(n0+1:n1,n0+1:n1,n0+1:n1,n1+1:n3))
       i1=k3*k1*k1
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s40)
       deallocate(d1)
       deallocate(b2)
c
      if(lvl_t)then
      if(dt3diag3.eq.0.and.t1diag3.eq.0)goto 7002
       allocate(x8(n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1))
       x8=0.0d0
      if(t1diag3.eq.0)goto 4001
       factor=t1diag3
       call sum4123(n0,n1,n0,n1,n1,n3,n0,n1,x8,s40,factor)
c
4001  if(dt3diag3.eq.0)goto 7001
       factor=dt3diag3
       call sumx2143(n0,n3,n0,n1,n0,n1,n1,n3,n0,n1,x8,intr,factor)
c
7001   allocate(f2(n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder451236(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,
     & n0,n1,n0,n1,n1,n3,n1,n3,n1,n3,n0,n1,t3a,f2)
       allocate(z10(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       i1=k1
       i2=k1*k3*k3
       i3=k3*k1*k1
       call egemm(i1,i2,i3,x8,f2,z10)
       deallocate(f2)
c
       v2a=v2a+0.500*z10
       call
     & sum1243(n1,n3,n1,n3,n0,n1,n0,n1,v2a,z10,-0.500)
       deallocate(z10)
       deallocate(x8)
7002  endif
c
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder3421(n0,n1,n0,n1,n0,n1,n1,n3,
     & n0,n1,n1,n3,n0,n1,n0,n1,s40,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(q15(n0+1:n1,n0+1:n1))
       i1=k1*k1
       i3=k3*k1
       call egemm1(i1,i3,d1,b2,q15)
       deallocate(d1)
       deallocate(b2)
c
       x2=x2-q15
       deallocate(q15)
c
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder2431(n0,n1,n0,n1,n0,n1,n1,n3,
     & n0,n1,n1,n3,n0,n1,n0,n1,s40,d1)
       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
       allocate(s72(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1
       i2=k1*k3
       i3=k3*k1
       call egemm(i1,i2,i3,d1,d2,s72)
       deallocate(d1)
       deallocate(d2)
c
       call sum2314(n0,n1,n1,n3,n0,n1,n0,n1,x12,s72,-1.000)
       deallocate(s72)
c
       allocate(d1(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       call reorder4231(n0,n1,n0,n1,n0,n1,n1,n3,
     & n1,n3,n0,n1,n0,n1,n0,n1,s40,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s70(n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1*k1
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s70)
       deallocate(d1)
       deallocate(b2)
       deallocate(s40)
c
       allocate(x4(n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       x4=0.0d0
       call sum3124(n0,n1,n0,n1,n0,n1,n0,n1,x4,s70, 1.000)
c
       allocate(d1(n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       call reorder3214(n0,n1,n0,n1,n0,n1,n0,n1,
     & n0,n1,n0,n1,n0,n1,n0,n1,s70,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s82(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1*k1
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s82)
       deallocate(d1)
       deallocate(b2)
       deallocate(s70)
c
       call sum2134(n0,n1,n1,n3,n0,n1,n0,n1,x11,s82, 1.000)
       deallocate(s82)
c
      if(lvl_t)then
      if(t1diag4.eq.0)goto 4002
       allocate(d1(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder2431(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n1,n3,n0,n1,intr,d1)
       allocate(f2(n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder412356(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,t3a,f2)
       allocate(s42(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1
       i2=k1*k1*k3
       i3=k3*k3*k1
       call egemm(i1,i2,i3,d1,f2,s42)
       deallocate(d1)
       deallocate(f2)
c
       factor=0.500*t1diag4
       call sum2341(n0,n1,n1,n3,n0,n1,n0,n1,x1,s42,factor)
       deallocate(s42)
4002  endif
c
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n1,n1+1:n3))
       call reorder1324(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n0,n1,n1,n3,intr,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(q5(n0+1:n1,n1+1:n3))
       i1=k3*k1
       i3=k3*k1
       call egemm1(i1,i3,d1,b2,q5)
       deallocate(d1)
       deallocate(b2)
c
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(q16(n1+1:n3,n1+1:n3))
       i1=k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,q5,b2,q16)
       deallocate(b2)
       deallocate(q5)
c
      if(lvl_t)then
       allocate(x7(n0+1:n1,n1+1:n3))
       x7=0.0d0
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
       allocate(q5(n0+1:n1,n1+1:n3))
       i1=k3*k1
       i3=k3*k1
       call egemm1(i1,i3,d1,b2,q5)
       deallocate(d1)
       deallocate(b2)
c
       x7=x7+t1diag1*q5
       deallocate(q5)
4003  endif
c
       call sum21(n1,n3,n1,n3,x3,q16,-1.000)
       deallocate(q16)
c
       allocate(d1(n1+1:n3,n0+1:n2,n0+1:n1,n2+1:n3))
       call reorder3214(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n2,n0,n1,n2,n3,intm,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s45(n0+1:n1,n0+1:n2,n0+1:n1,n2+1:n3))
       i1=k4*k1*k2
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s45)
       deallocate(d1)
       deallocate(b2)
c
      if(lvl_t)then
      if(dt3diag3.eq.0.and.t1diag3.eq.0)goto 7004
       allocate(x10(n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n1))
       x10=0.0d0
      if(t1diag3.eq.0)goto 4004
       factor=t1diag3
       call sum4123(n0,n2,n0,n1,n2,n3,n0,n1,x10,s45,factor)
c
4004  if(dt3diag3.eq.0)goto 7003
       factor=dt3diag3
       call sumx2143(n0,n3,n0,n2,n0,n1,n2,n3,n0,n1,x10,intm,factor)
c
7003   allocate(f2(n0+1:n2,n0+1:n1,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder451236(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n0,n2,n0,n1,n2,n3,n1,n3,n1,n3,n0,n1,t3b,f2)
       allocate(z13(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       i1=k1
       i2=k1*k3*k3
       i3=k4*k1*k2
       call egemm(i1,i2,i3,x10,f2,z13)
       deallocate(f2)
c
       v2a=v2a+z13
       call
     & sum1243(n1,n3,n1,n3,n0,n1,n0,n1,v2a,z13,-1.000)
       deallocate(z13)
       deallocate(x10)
7004  endif
c
       allocate(d1(n0+1:n2,n2+1:n3,n0+1:n1,n0+1:n1))
       call reorder2431(n0,n1,n0,n2,n0,n1,n2,n3,
     & n0,n2,n2,n3,n0,n1,n0,n1,s45,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(q17(n0+1:n1,n0+1:n1))
       i1=k1*k1
       i3=k4*k2
       call egemm1(i1,i3,d1,b2,q17)
       deallocate(d1)
       deallocate(b2)
c
       x2=x2+q17
       deallocate(q17)
c
       allocate(d1(n0+1:n2,n2+1:n3,n0+1:n1,n0+1:n1))
       call reorder2431(n0,n1,n0,n2,n0,n1,n2,n3,
     & n0,n2,n2,n3,n0,n1,n0,n1,s45,d1)
       allocate(d2(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n2,n3,n1,n3,n0,n1,t2b,d2)
       allocate(s78(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1
       i2=k1*k3
       i3=k4*k2
       call egemm(i1,i2,i3,d1,d2,s78)
       deallocate(d1)
       deallocate(d2)
       deallocate(s45)
c
       call sum2314(n0,n1,n1,n3,n0,n1,n0,n1,x12,s78,-1.000)
       deallocate(s78)
c
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(z21(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       i1=k1*k1*k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,x12,b2,z21)
       deallocate(b2)
c
       call
     & sum2134(n1,n3,n1,n3,n0,n1,n0,n1,v2a,z21, 1.000)
       v2a=v2a-z21
       call
     & sum2143(n1,n3,n1,n3,n0,n1,n0,n1,v2a,z21,-1.000)
       call
     & sum1243(n1,n3,n1,n3,n0,n1,n0,n1,v2a,z21, 1.000)
       deallocate(z21)
       deallocate(x12)
c
      if(lvl_t)then
      if(t1diag4.eq.0)goto 4005
       allocate(d1(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder2431(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n1,n3,n0,n1,intm,d1)
       allocate(f2(n0+1:n2,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder412356(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n0,n2,n2,n3,n1,n3,n1,n3,n0,n1,n0,n1,t3b,f2)
       allocate(s47(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1
       i2=k1*k1*k3
       i3=k3*k4*k2
       call egemm(i1,i2,i3,d1,f2,s47)
       deallocate(d1)
       deallocate(f2)
c
       factor=t1diag4
       call sum2341(n0,n1,n1,n3,n0,n1,n0,n1,x1,s47,factor)
       deallocate(s47)
4005  endif
c
       call sumx2143(n0,n3,n0,n1,n1,n3,n0,n1,n0,n1,x1,intr, 1.000)
c
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(z1(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       i1=k1*k1*k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,x1,b2,z1)
       deallocate(b2)
c
       call
     & sum2134(n1,n3,n1,n3,n0,n1,n0,n1,v2a,z1, 1.000)
       v2a=v2a-z1
       deallocate(z1)
       deallocate(x1)
c
      if(lvl_t)then
       allocate(x9(n0+1:n2,n2+1:n3))
       x9=0.0d0
c
      if(t1diag1.eq.0)goto 4006
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n2,n2+1:n3))
       call reorder1324(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n0,n2,n2,n3,intm,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(q6(n0+1:n2,n2+1:n3))
       i1=k4*k2
       i3=k3*k1
       call egemm1(i1,i3,d1,b2,q6)
       deallocate(d1)
       deallocate(b2)
c
       x9=x9+t1diag1*q6
       deallocate(q6)
4006  endif
c
       allocate(d1(n0+1:n2,n2+1:n3,n0+1:n1,n0+1:n1))
       call reorder2413(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n0,n1,n0,n1,intm,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(q7(n0+1:n1,n0+1:n1))
       i1=k1*k1
       i3=k4*k2
       call egemm1(i1,i3,d1,b2,q7)
       deallocate(d1)
       deallocate(b2)
c
       x2=x2+q7
       deallocate(q7)
c
       allocate(d1(n0+1:n2,n2+1:n3,n1+1:n3,n1+1:n3))
       call reorder2431(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n1,n3,n1,n3,intm,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(q8(n1+1:n3,n1+1:n3))
       i1=k3*k3
       i3=k4*k2
       call egemm1(i1,i3,d1,b2,q8)
       deallocate(d1)
       deallocate(b2)
c
       x3=x3+q8
       deallocate(q8)
c
       allocate(d1(n0+1:n2,n2+1:n3,n0+1:n1,n1+1:n3))
       call reorder2413(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n0,n1,n1,n3,intm,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(q9(n0+1:n1,n1+1:n3))
       i1=k3*k1
       i3=k4*k2
       call egemm1(i1,i3,d1,b2,q9)
       deallocate(d1)
       deallocate(b2)
c
      if(lvl_t)then
      if(t1diag1.eq.0)goto 4007
       x7=x7+t1diag1*q9
c
4007   call sumx12(0,n3,n0,n1,n1,n3,x7,fockr, 1.000)
c
       allocate(f2(n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder412356(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,t3a,f2)
       allocate(z9(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       i2=k1*k1*k3*k3
       i3=k3*k1
       call egemm2(i2,i3,x7,f2,z9)
       deallocate(f2)
c
       v2a=v2a+z9
       deallocate(z9)
       deallocate(x7)
      endif
c
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(q18(n1+1:n3,n1+1:n3))
       i1=k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,q9,b2,q18)
       deallocate(b2)
       deallocate(q9)
c
       call sum21(n1,n3,n1,n3,x3,q18,-1.000)
       deallocate(q18)
c
      if(lvl_t)then
      if(t1diag1.eq.0)goto 4008
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
       allocate(q10(n0+1:n2,n2+1:n3))
       i1=k4*k2
       i3=k4*k2
       call egemm1(i1,i3,d1,b2,q10)
       deallocate(d1)
       deallocate(b2)
c
       x9=x9+t1diag1*q10
       deallocate(q10)
c
4008   call sumx12(0,n3,n0,n2,n2,n3,x9,fockb, 1.000)
c
       allocate(f2(n0+1:n2,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder412356(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n0,n2,n2,n3,n1,n3,n1,n3,n0,n1,n0,n1,t3b,f2)
       allocate(z12(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       i2=k1*k1*k3*k3
       i3=k4*k2
       call egemm2(i2,i3,x9,f2,z12)
       deallocate(f2)
c
       v2a=v2a+z12
       deallocate(z12)
       deallocate(x9)
      endif
c
      if(diag3.eq.0)goto 5001
       allocate(d1(n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3))                !ilias: acp d3
       call reorder2134(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n0,n1,n1,n3,n1,n3,intr,d1)
       allocate(d2(n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3))
       call reorder3412(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n0,n1,n1,n3,n1,n3,t2a,d2)
       allocate(q11(n1+1:n3,n1+1:n3))
       i1=k3
       i2=k3
       i3=k3*k1*k1
       call egemm(i1,i2,i3,d1,d2,q11)
       deallocate(d1)
       deallocate(d2)
c
       allocate(b1(n1+1:n3,n1+1:n3))
       call reorder21(n1,n3,n1,n3,
     & n1,n3,n1,n3,q11,b1)
       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
       allocate(z54(n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3))
       i1=k3
       i2=k1*k1*k3
       i3=k3
       call egemm(i1,i2,i3,b1,d2,z54)
       deallocate(b1)
       deallocate(d2)
c
       factor=-0.500*diag3
       call
     & sum1342(n1,n3,n1,n3,n0,n1,n0,n1,v2a,z54,factor)
       factor=-factor
       call
     & sum2341(n1,n3,n1,n3,n0,n1,n0,n1,v2a,z54,factor)
       deallocate(z54)
       deallocate(q11)
       factor=0
c ilias: added the complementary acc-d3_mc term
5001  if(ext_cor.and.diag3.ne.1)then
       allocate(d1_mc(n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3))
       call reorder2134(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n0,n1,n1,n3,n1,n3,intr,d1_mc)
       allocate(d2_mc(n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3))
       call reorder3412(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n0,n1,n1,n3,n1,n3,t2a_mc,d2_mc)
       allocate(q11_mc(n1+1:n3,n1+1:n3))
       i1=k3
       i2=k3
       i3=k3*k1*k1
       call egemm(i1,i2,i3,d1_mc,d2_mc,q11_mc)
       deallocate(d1_mc)
       deallocate(d2_mc)
c
       allocate(b1_mc(n1+1:n3,n1+1:n3))
       call reorder21(n1,n3,n1,n3,
     & n1,n3,n1,n3,q11_mc,b1_mc)
       allocate(d2_mc(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n0,n1,t2a_mc,d2_mc)
       allocate(z54_mc(n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3))
       i1=k3
       i2=k1*k1*k3
       i3=k3
       call egemm(i1,i2,i3,b1_mc,d2_mc,z54_mc)
       deallocate(q11_mc)
       deallocate(b1_mc)
       deallocate(d2_mc)
c
       factor=-0.500*(1.0-diag3)
       call
     & sum1342(n1,n3,n1,n3,n0,n1,n0,n1,v2a,z54_mc,factor)
       factor=-factor
       call
     & sum2341(n1,n3,n1,n3,n0,n1,n0,n1,v2a,z54_mc,factor)
       deallocate(z54_mc)
       factor=0
      endif
c
       allocate(d1(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder4321(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n1,n3,n0,n1,n0,n1,intr,d1)
       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
       allocate(s55(n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1
       i2=k1*k1
       i3=k3*k3
       call egemm(i1,i2,i3,d1,d2,s55)
       deallocate(d1)
       deallocate(d2)
c
      if(diag5.eq.0)goto 5002
       factor=0.500*diag5
       call sum3412(n0,n1,n0,n1,n0,n1,n0,n1,x4,s55,factor)          !ilias: acp d5
       factor=0
c ilias: added the complementary acc-d5_mc term
5002  if(ext_cor.and.diag5.ne.1)then
       allocate(d1_mc(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder4321(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n1,n3,n0,n1,n0,n1,intr,d1_mc)
       allocate(d2_mc(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n0,n1,t2a_mc,d2_mc)
       allocate(s55_mc(n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1
       i2=k1*k1
       i3=k3*k3
       call egemm(i1,i2,i3,d1_mc,d2_mc,s55_mc)
       deallocate(d1_mc)
       deallocate(d2_mc)
c
       allocate(d1_mc(n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       call reorder3412(n0,n1,n0,n1,n0,n1,n0,n1,
     & n0,n1,n0,n1,n0,n1,n0,n1,s55_mc,d1_mc)
       allocate(d2_mc(n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3))
       call reorder3412(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n0,n1,n1,n3,n1,n3,t2a_mc,d2_mc)
       allocate(z5_mc(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       i1=k1*k1
       i2=k3*k3
       i3=k1*k1
       call egemm(i1,i2,i3,d1_mc,d2_mc,z5_mc)
       deallocate(s55_mc)
       deallocate(d1_mc)
       deallocate(d2_mc)
c
       v2a=v2a+0.250*(1.0-diag5)*z5_mc
       deallocate(z5_mc)
       factor=0
      endif
       call sumx2143(n0,n3,n0,n1,n0,n1,n0,n1,n0,n1,x4,intr, 1.000)
c
       allocate(d2(n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3))
       call reorder3412(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n0,n1,n1,n3,n1,n3,t2a,d2)
       allocate(z5(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       i1=k1*k1
       i2=k3*k3
       i3=k1*k1
       call egemm(i1,i2,i3,x4,d2,z5)
       deallocate(d2)
c
       v2a=v2a+0.500*z5
       deallocate(z5)
       deallocate(x4)
c
       allocate(d1(n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       call reorder4312(n0,n1,n0,n1,n0,n1,n0,n1,
     & n0,n1,n0,n1,n0,n1,n0,n1,s55,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s75(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1*k1
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s75)
       deallocate(d1)
       deallocate(b2)
       deallocate(s55)
c
       call sum2134(n0,n1,n1,n3,n0,n1,n0,n1,x11,s75, 0.500)
       deallocate(s75)
c
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(z19(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       i1=k1*k1*k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,x11,b2,z19)
       deallocate(b2)
c
       v2a=v2a+z19
       deallocate(z19)
       deallocate(x11)
c
      if(diag4.eq.0)goto 5003
       allocate(d1(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))                 !ilias: acp d4
       call reorder2431(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n1,n3,n0,n1,intr,d1)
       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
       allocate(q12(n0+1:n1,n0+1:n1))
       i1=k1
       i2=k1
       i3=k3*k3*k1
       call egemm(i1,i2,i3,d1,d2,q12)
       deallocate(d1)
       deallocate(d2)
c
       factor=0.500*diag4
       call sum21(n0,n1,n0,n1,x2,q12,factor)
       deallocate(q12)
       factor=0
c ilias: added the complementary acc-d4_mc term
5003  if(ext_cor.and.diag4.ne.1)then
       allocate(d1_mc(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder2431(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n1,n3,n0,n1,intr,d1_mc)
       allocate(d2_mc(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a_mc,d2_mc)
       allocate(q12_mc(n0+1:n1,n0+1:n1))
       i1=k1
       i2=k1
       i3=k3*k3*k1
       call egemm(i1,i2,i3,d1_mc,d2_mc,q12_mc)
       deallocate(d1_mc)
       deallocate(d2_mc)
c
       allocate(b1_mc(n0+1:n1,n0+1:n1))
       call reorder21(n0,n1,n0,n1,
     & n0,n1,n0,n1,q12_mc,b1_mc)
       allocate(d2_mc(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a_mc,d2_mc)
       allocate(z3_mc(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       i1=k1
       i2=k3*k3*k1
       i3=k1
       call egemm(i1,i2,i3,b1_mc,d2_mc,z3_mc)
       deallocate(q12_mc)
       deallocate(b1_mc)
       deallocate(d2_mc)
c
       factor=0.500*(1.0-diag4)
       v2a=v2a+factor*z3_mc
       factor=-factor
       call
     & sum1243(n1,n3,n1,n3,n0,n1,n0,n1,v2a,z3_mc,factor)
       deallocate(z3_mc)
       factor=0
      endif
c
      if(diag1.eq.0)goto 5004
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
       allocate(s58(n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3))
       i1=k3*k1
       i2=k1*k3
       i3=k3*k1
       call egemm(i1,i2,i3,d1,d2,s58)
       deallocate(d1)
       deallocate(d2)
c
       allocate(d1(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))                 !ilias: acp d1
       call reorder3412(n1,n3,n0,n1,n0,n1,n1,n3,
     & n0,n1,n1,n3,n1,n3,n0,n1,s58,d1)
       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
       allocate(z59(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       i1=k1*k3
       i2=k1*k3
       i3=k3*k1
       call egemm(i1,i2,i3,d1,d2,z59)
       deallocate(d1)
       deallocate(d2)
c
       factor=-diag1
       call
     & sum1423(n1,n3,n1,n3,n0,n1,n0,n1,v2a,z59,factor)               !ilias: acp d1
       factor=-factor
       call
     & sum1324(n1,n3,n1,n3,n0,n1,n0,n1,v2a,z59,factor)
       deallocate(z59)
       deallocate(s58)
       factor=0
c ilias: added the complementary acc-d1_mc term
5004  if(ext_cor.and.diag1.ne.1)then
       allocate(d1_mc(n0+1:n1,n1+1:n3,n0+1:n1,n1+1:n3))
       call reorder1324(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n0,n1,n1,n3,intr,d1_mc)
       allocate(d2_mc(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a_mc,d2_mc)
       allocate(s58_mc(n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3))
       i1=k3*k1
       i2=k1*k3
       i3=k3*k1
       call egemm(i1,i2,i3,d1_mc,d2_mc,s58_mc)
       deallocate(d1_mc)
       deallocate(d2_mc)
c
       allocate(d1_mc(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3412(n1,n3,n0,n1,n0,n1,n1,n3,
     & n0,n1,n1,n3,n1,n3,n0,n1,s58_mc,d1_mc)
       allocate(d2_mc(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a_mc,d2_mc)
       allocate(z59_mc(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       i1=k1*k3
       i2=k1*k3
       i3=k3*k1
       call egemm(i1,i2,i3,d1_mc,d2_mc,z59_mc)
       deallocate(s58_mc)
       deallocate(d1_mc)
       deallocate(d2_mc)
c
       factor=-(1.0-diag1)
       call
     & sum1423(n1,n3,n1,n3,n0,n1,n0,n1,v2a,z59_mc,factor)
       factor=-factor
       call
     & sum1324(n1,n3,n1,n3,n0,n1,n0,n1,v2a,z59_mc,factor)
       deallocate(z59_mc)
       factor=0
      endif
c ilias: added the complementary acc-d2_mc term
      if(ext_cor.and.diag1.eq.1.and.diag2.eq.0)then
       allocate(d1_mc(n0+1:n1,n1+1:n3,n0+1:n1,n1+1:n3))
       call reorder1324(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n0,n1,n1,n3,intr,d1_mc)
       allocate(d2_mc(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a_mc,d2_mc)
       allocate(s58_mc(n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3))
       i1=k3*k1
       i2=k1*k3
       i3=k3*k1
       call egemm(i1,i2,i3,d1_mc,d2_mc,s58_mc)
       deallocate(d1_mc)
       deallocate(d2_mc)
c
       allocate(d1_mc(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3412(n1,n3,n0,n1,n0,n1,n1,n3,
     & n0,n1,n1,n3,n1,n3,n0,n1,s58_mc,d1_mc)
       allocate(d2_mc(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a_mc,d2_mc)
       allocate(z59_mc(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       i1=k1*k3
       i2=k1*k3
       i3=k3*k1
       call egemm(i1,i2,i3,d1_mc,d2_mc,z59_mc)
       deallocate(s58_mc)
       deallocate(d1_mc)
       deallocate(d2_mc)
c
       factor=-diag1
       call
     & sum1423(n1,n3,n1,n3,n0,n1,n0,n1,v2a,z59_mc,factor)
       factor=-factor
       call
     & sum1324(n1,n3,n1,n3,n0,n1,n0,n1,v2a,z59_mc,factor)
       deallocate(z59_mc)
       factor=0
c
       allocate(d1_mc(n0+1:n1,n1+1:n3,n0+1:n1,n1+1:n3))
       call reorder1324(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n0,n1,n1,n3,intm,d1_mc)
       allocate(d2_mc(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a_mc,d2_mc)
       allocate(s58_mc(n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3))
       i1=k3*k1
       i2=k1*k3
       i3=k3*k1
       call egemm(i1,i2,i3,d1_mc,d2_mc,s58_mc)
       deallocate(d1_mc)
       deallocate(d2_mc)
c
       allocate(d1_mc(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3412(n1,n3,n0,n1,n0,n1,n1,n3,
     & n0,n1,n1,n3,n1,n3,n0,n1,s58_mc,d1_mc)
       allocate(d2_mc(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a_mc,d2_mc)
       allocate(z59_mc(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       i1=k1*k3
       i2=k1*k3
       i3=k3*k1
       call egemm(i1,i2,i3,d1_mc,d2_mc,z59_mc)
       deallocate(s58_mc)
       deallocate(d1_mc)
       deallocate(d2_mc)
c
       factor=diag1
       call
     & sum1423(n1,n3,n1,n3,n0,n1,n0,n1,v2a,z59_mc,factor)
       factor=-factor
       call
     & sum1324(n1,n3,n1,n3,n0,n1,n0,n1,v2a,z59_mc,factor)
       deallocate(z59_mc)
       factor=0
      endif
c
      if(diag3.eq.0)goto 5005
       allocate(d1(n0+1:n2,n0+1:n1,n2+1:n3,n1+1:n3))                 !ilias: acp d3
       call reorder2143(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n0,n1,n2,n3,n1,n3,intm,d1)
       allocate(d2(n0+1:n2,n0+1:n1,n2+1:n3,n1+1:n3))
       call reorder3412(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n0,n1,n2,n3,n1,n3,t2b,d2)
       allocate(q13(n1+1:n3,n1+1:n3))
       i1=k3
       i2=k3
       i3=k4*k1*k2
       call egemm(i1,i2,i3,d1,d2,q13)
       deallocate(d1)
       deallocate(d2)
c
       factor=-diag3
       call sum21(n1,n3,n1,n3,x3,q13,factor)
       deallocate(q13)
       factor=0
c ilias: added the complementary acc-d3_mc term
5005  if(ext_cor.and.diag3.ne.1)then
       allocate(d1_mc(n0+1:n2,n0+1:n1,n2+1:n3,n1+1:n3))
       call reorder2143(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n0,n1,n2,n3,n1,n3,intm,d1_mc)
       allocate(d2_mc(n0+1:n2,n0+1:n1,n2+1:n3,n1+1:n3))
       call reorder3412(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n0,n1,n2,n3,n1,n3,t2b_mc,d2_mc)
       allocate(q13_mc(n1+1:n3,n1+1:n3))
       i1=k3
       i2=k3
       i3=k4*k1*k2
       call egemm(i1,i2,i3,d1_mc,d2_mc,q13_mc)
       deallocate(d1_mc)
       deallocate(d2_mc)
c
       allocate(b1_mc(n1+1:n3,n1+1:n3))
       call reorder21(n1,n3,n1,n3,
     & n1,n3,n1,n3,q13_mc,b1_mc)
       allocate(d2_mc(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n0,n1,t2a_mc,d2_mc)
       allocate(z4_mc(n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3))
       i1=k3
       i2=k1*k1*k3
       i3=k3
       call egemm(i1,i2,i3,b1_mc,d2_mc,z4_mc)
       deallocate(q13_mc)
       deallocate(b1_mc)
       deallocate(d2_mc)
c
       factor=-(1.0-diag3)
       call sum2341(n1,n3,n1,n3,n0,n1,n0,n1,v2a,z4_mc,factor)
       factor=-factor
       call sum1342(n1,n3,n1,n3,n0,n1,n0,n1,v2a,z4_mc,factor)
       factor=0
       deallocate(z4_mc)
      endif
c
       call sumx21(0,n3,n1,n3,n1,n3,x3,fockr, 1.000)
c
       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
       allocate(z4(n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3))
       i1=k3
       i2=k1*k1*k3
       i3=k3
       call egemm(i1,i2,i3,x3,d2,z4)
       deallocate(d2)
c
       call
     & sum2341(n1,n3,n1,n3,n0,n1,n0,n1,v2a,z4, 1.000)
       call
     & sum1342(n1,n3,n1,n3,n0,n1,n0,n1,v2a,z4,-1.000)
       deallocate(z4)
       deallocate(x3)
c
      if(diag4.eq.0)goto 5006
       allocate(d1(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))                 !ilias: acp d4
       call reorder2431(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n1,n3,n0,n1,intm,d1)
       allocate(d2(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n2,n3,n1,n3,n0,n1,t2b,d2)
       allocate(q14(n0+1:n1,n0+1:n1))
       i1=k1
       i2=k1
       i3=k3*k4*k2
       call egemm(i1,i2,i3,d1,d2,q14)
       deallocate(d1)
       deallocate(d2)
c
       factor=diag4
       call sum21(n0,n1,n0,n1,x2,q14,factor)
       deallocate(q14)
       factor=0
c ilias: added the complementary acc-d4_mc term
5006  if(ext_cor.and.diag4.ne.1)then
       allocate(d1_mc(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder2431(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n1,n3,n0,n1,intm,d1_mc)
       allocate(d2_mc(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n2,n3,n1,n3,n0,n1,t2b_mc,d2_mc)
       allocate(q14_mc(n0+1:n1,n0+1:n1))
       i1=k1
       i2=k1
       i3=k3*k4*k2
       call egemm(i1,i2,i3,d1_mc,d2_mc,q14_mc)
       deallocate(d1_mc)
       deallocate(d2_mc)
c
       allocate(b1_mc(n0+1:n1,n0+1:n1))
       call reorder21(n0,n1,n0,n1,
     & n0,n1,n0,n1,q14_mc,b1_mc)
       allocate(d2_mc(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a_mc,d2_mc)
       allocate(z3_mc(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       i1=k1
       i2=k1*k3*k3
       i3=k1
       call egemm(i1,i2,i3,b1_mc,d2_mc,z3_mc)
       deallocate(q14_mc)
       deallocate(b1_mc)
       deallocate(d2_mc)
c
       factor=(1.0-diag4)
       v2a=v2a+factor*z3_mc
       factor=-factor
       call
     & sum1243(n1,n3,n1,n3,n0,n1,n0,n1,v2a,z3_mc,factor)
       deallocate(z3_mc)
       factor=0
      endif
c
       call sumx12(0,n3,n0,n1,n0,n1,x2,fockr, 1.000)
c
       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
       allocate(z3(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       i1=k1
       i2=k1*k3*k3
       i3=k1
       call egemm(i1,i2,i3,x2,d2,z3)
       deallocate(d2)
c
       v2a=v2a+z3
       call
     & sum1243(n1,n3,n1,n3,n0,n1,n0,n1,v2a,z3,-1.000)
       deallocate(z3)
       deallocate(x2)
c
      if(diag1.eq.0)goto 5007
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n2,n2+1:n3))                 !ilias: acp d1
       call reorder1324(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n0,n2,n2,n3,intm,d1)
       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
       allocate(s62(n1+1:n3,n0+1:n1,n0+1:n2,n2+1:n3))
       i1=k4*k2
       i2=k1*k3
       i3=k3*k1
       call egemm(i1,i2,i3,d1,d2,s62)
       deallocate(d1)
       deallocate(d2)
c
       factor=diag1
       call sum3412(n0,n2,n2,n3,n1,n3,n0,n1,x6,s62,factor)           !ilias: acp d1
       deallocate(s62)
       factor=0
c ilias: added the complementary acc-d1_mc term
5007  if(ext_cor.and.diag1.ne.1)then
       allocate(d1_mc(n0+1:n1,n1+1:n3,n0+1:n2,n2+1:n3))
       call reorder1324(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n0,n2,n2,n3,intm,d1_mc)
       allocate(d2_mc(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a_mc,d2_mc)
       allocate(s62_mc(n1+1:n3,n0+1:n1,n0+1:n2,n2+1:n3))
       i1=k4*k2
       i2=k1*k3
       i3=k3*k1
       call egemm(i1,i2,i3,d1_mc,d2_mc,s62_mc)
       deallocate(d1_mc)
       deallocate(d2_mc)
c
       allocate(d1_mc(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder3412(n1,n3,n0,n1,n0,n2,n2,n3,
     & n0,n2,n2,n3,n1,n3,n0,n1,s62_mc,d1_mc)
       allocate(d2_mc(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n2,n3,n1,n3,n0,n1,t2b_mc,d2_mc)
       allocate(z8_mc(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       i1=k1*k3
       i2=k1*k3
       i3=k4*k2
       call egemm(i1,i2,i3,d1_mc,d2_mc,z8_mc)
       deallocate(s62_mc)
       deallocate(d1_mc)
       deallocate(d2_mc)
c
       factor=-(1.0-diag1)
       call sum2314(n1,n3,n1,n3,n0,n1,n0,n1,v2a,z8_mc,factor)
       factor=-factor
       call sum1324(n1,n3,n1,n3,n0,n1,n0,n1,v2a,z8_mc,factor)
       call sum2413(n1,n3,n1,n3,n0,n1,n0,n1,v2a,z8_mc,factor)
       factor=-factor
       call sum1423(n1,n3,n1,n3,n0,n1,n0,n1,v2a,z8_mc,factor)
       deallocate(z8_mc)
       factor=0
      endif
c
       call sumx3142(n0,n3,n0,n2,n2,n3,n1,n3,n0,n1,x6,intm, 1.000)
c
       allocate(d2(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n2,n3,n1,n3,n0,n1,t2b,d2)
       allocate(z8(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       i1=k1*k3
       i2=k1*k3
       i3=k4*k2
       call egemm(i1,i2,i3,x6,d2,z8)
       deallocate(d2)
c
       call
     & sum2314(n1,n3,n1,n3,n0,n1,n0,n1,v2a,z8,-1.000)
       call
     & sum1324(n1,n3,n1,n3,n0,n1,n0,n1,v2a,z8, 1.000)
       call
     & sum2413(n1,n3,n1,n3,n0,n1,n0,n1,v2a,z8, 1.000)
       call
     & sum1423(n1,n3,n1,n3,n0,n1,n0,n1,v2a,z8,-1.000)
       deallocate(z8)
       deallocate(x6)
c
      if(diag1.eq.0)goto 5008
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
       allocate(s64(n1+1:n3,n0+1:n1,n0+1:n2,n2+1:n3))
       i1=k4*k2
       i2=k1*k3
       i3=k4*k2
       call egemm(i1,i2,i3,d1,d2,s64)
       deallocate(d1)
       deallocate(d2)
c
       allocate(d1(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))                 !ilias: acp d1
       call reorder3412(n1,n3,n0,n1,n0,n2,n2,n3,
     & n0,n2,n2,n3,n1,n3,n0,n1,s64,d1)
       allocate(d2(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n2,n3,n1,n3,n0,n1,t2b,d2)
       allocate(z65(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       i1=k1*k3
       i2=k1*k3
       i3=k4*k2
       call egemm(i1,i2,i3,d1,d2,z65)
       deallocate(d1)
       deallocate(d2)
c
       factor=-diag1
       call                                                          !ilias: acp d1
     & sum1423(n1,n3,n1,n3,n0,n1,n0,n1,v2a,z65,factor)
       factor=-factor
       call
     & sum1324(n1,n3,n1,n3,n0,n1,n0,n1,v2a,z65,factor)
       deallocate(z65)
       deallocate(s64)
       factor=0
c ilias: added the complementary acc-d1_mc term
5008  if(ext_cor.and.diag1.ne.1)then
       allocate(d1_mc(n0+1:n2,n2+1:n3,n0+1:n2,n2+1:n3))
       call reorder1324(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n0,n2,n2,n3,intb,d1_mc)
       allocate(d2_mc(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n2,n3,n1,n3,n0,n1,t2b_mc,d2_mc)
       allocate(s64_mc(n1+1:n3,n0+1:n1,n0+1:n2,n2+1:n3))
       i1=k4*k2
       i2=k1*k3
       i3=k4*k2
       call egemm(i1,i2,i3,d1_mc,d2_mc,s64_mc)
       deallocate(d1_mc)
       deallocate(d2_mc)
c
       allocate(d1_mc(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder3412(n1,n3,n0,n1,n0,n2,n2,n3,
     & n0,n2,n2,n3,n1,n3,n0,n1,s64_mc,d1_mc)
       allocate(d2_mc(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n2,n3,n1,n3,n0,n1,t2b_mc,d2_mc)
       allocate(z65_mc(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       i1=k1*k3
       i2=k1*k3
       i3=k4*k2
       call egemm(i1,i2,i3,d1_mc,d2_mc,z65_mc)
       deallocate(s64_mc)
       deallocate(d1_mc)
       deallocate(d2_mc)
c
       factor=-(1.0-diag1)
       call
     & sum1423(n1,n3,n1,n3,n0,n1,n0,n1,v2a,z65_mc,factor)
       factor=-factor
       call
     & sum1324(n1,n3,n1,n3,n0,n1,n0,n1,v2a,z65_mc,factor)
       deallocate(z65_mc)
       factor=0
      endif
c ilias: added the complementary acc-d2_mc term
      if(ext_cor.and.diag1.eq.1.and.diag2.eq.0)then
       allocate(d1_mc(n0+1:n2,n2+1:n3,n0+1:n2,n2+1:n3))
       call reorder1324(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n0,n2,n2,n3,intb,d1_mc)
       allocate(d2_mc(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n2,n3,n1,n3,n0,n1,t2b_mc,d2_mc)
       allocate(s64_mc(n1+1:n3,n0+1:n1,n0+1:n2,n2+1:n3))
       i1=k4*k2
       i2=k1*k3
       i3=k4*k2
       call egemm(i1,i2,i3,d1_mc,d2_mc,s64_mc)
       deallocate(d1_mc)
       deallocate(d2_mc)
c
       allocate(d1_mc(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder3412(n1,n3,n0,n1,n0,n2,n2,n3,
     & n0,n2,n2,n3,n1,n3,n0,n1,s64_mc,d1_mc)
       allocate(d2_mc(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n2,n3,n1,n3,n0,n1,t2b_mc,d2_mc)
       allocate(z65_mc(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       i1=k1*k3
       i2=k1*k3
       i3=k4*k2
       call egemm(i1,i2,i3,d1_mc,d2_mc,z65_mc)
       deallocate(s64_mc)
       deallocate(d1_mc)
       deallocate(d2_mc)
c
       factor=-diag1
       call
     & sum1423(n1,n3,n1,n3,n0,n1,n0,n1,v2a,z65_mc,factor)
       factor=-factor
       call
     & sum1324(n1,n3,n1,n3,n0,n1,n0,n1,v2a,z65_mc,factor)
       deallocate(z65_mc)
       factor=0
c
       allocate(d1_mc(n0+1:n2,n2+1:n3,n0+1:n2,n2+1:n3))
       call reorder1324(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n0,n2,n2,n3,intm,d1_mc)
       allocate(d2_mc(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n2,n3,n1,n3,n0,n1,t2b_mc,d2_mc)
       allocate(s64_mc(n1+1:n3,n0+1:n1,n0+1:n2,n2+1:n3))
       i1=k4*k2
       i2=k1*k3
       i3=k4*k2
       call egemm(i1,i2,i3,d1_mc,d2_mc,s64_mc)
       deallocate(d1_mc)
       deallocate(d2_mc)
c
       allocate(d1_mc(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder3412(n1,n3,n0,n1,n0,n2,n2,n3,
     & n0,n2,n2,n3,n1,n3,n0,n1,s64_mc,d1_mc)
       allocate(d2_mc(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n2,n3,n1,n3,n0,n1,t2b_mc,d2_mc)
       allocate(z65_mc(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       i1=k1*k3
       i2=k1*k3
       i3=k4*k2
       call egemm(i1,i2,i3,d1_mc,d2_mc,z65_mc)
       deallocate(s64_mc)
       deallocate(d1_mc)
       deallocate(d2_mc)
c
       factor=diag1
       call
     & sum1423(n1,n3,n1,n3,n0,n1,n0,n1,v2a,z65_mc,factor)
       factor=-factor
       call
     & sum1324(n1,n3,n1,n3,n0,n1,n0,n1,v2a,z65_mc,factor)
       deallocate(z65_mc)
       factor=0
      endif
c
       allocate(d1(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder4213(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n1,n3,n1,n3,n0,n1,intr,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(z2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       i1=k1*k3*k3
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,z2)
       deallocate(d1)
       deallocate(b2)
c
       call
     & sum3124(n1,n3,n1,n3,n0,n1,n0,n1,v2a,z2, 1.000)
       call
     & sum4123(n1,n3,n1,n3,n0,n1,n0,n1,v2a,z2,-1.000)
       deallocate(z2)
c
       allocate(d1(n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3))
       call reorder4321(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n1,n3,n1,n3,n1,n3,intr,d1)
       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
       allocate(z7(n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3))
       i1=k3*k3
       i2=k1*k1
       i3=k3*k3
       call egemm(i1,i2,i3,d1,d2,z7)
       deallocate(d1)
       deallocate(d2)
c
       call
     & sum3412(n1,n3,n1,n3,n0,n1,n0,n1,v2a,z7, 0.500)
       deallocate(z7)
c
      if(lvl_t)then
      if(dt3diag4.eq.0)goto 7005
       allocate(d1(n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3))
       call reorder2431(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n1,n3,n1,n3,intr,d1)
       allocate(f2(n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder412356(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,t3a,f2)
       allocate(z11(n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3))
       i1=k3
       i2=k1*k1*k3
       i3=k3*k3*k1
       call egemm(i1,i2,i3,d1,f2,z11)
       deallocate(d1)
       deallocate(f2)
c
       factor=0.500*dt3diag4
       call
     & sum2341(n1,n3,n1,n3,n0,n1,n0,n1,v2a,z11,factor)
       factor=-factor
       call
     & sum1342(n1,n3,n1,n3,n0,n1,n0,n1,v2a,z11,factor)
       deallocate(z11)
c
       allocate(d1(n0+1:n2,n2+1:n3,n1+1:n3,n1+1:n3))
       call reorder2431(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n1,n3,n1,n3,intm,d1)
       allocate(f2(n0+1:n2,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder412356(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n0,n2,n2,n3,n1,n3,n1,n3,n0,n1,n0,n1,t3b,f2)
       allocate(z14(n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3))
       i1=k3
       i2=k1*k1*k3
       i3=k3*k4*k2
       call egemm(i1,i2,i3,d1,f2,z14)
       deallocate(d1)
       deallocate(f2)
c
       factor=dt3diag4
       call
     & sum2341(n1,n3,n1,n3,n0,n1,n0,n1,v2a,z14,factor)
       factor=-factor
       call
     & sum1342(n1,n3,n1,n3,n0,n1,n0,n1,v2a,z14,factor)
       deallocate(z14)
7005  endif
c
!	goto 501
!       allocate(d1(n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3))
!       call reorder2143(n0,n3,n0,n3,n0,n3,n0,n3,
!     & n0,n1,n0,n1,n1,n3,n1,n3,intr,d1)
!a!       rewind(ta)
!a!       allocate(t4a(k3*k3*k3*k3,k1*k1*k1*k1))
!a!       read(ta)t4a
!a!       i2=(k1*(k1-1)*k3*(k3-1))/4
!a!       i1=k1*k1*k3*k3
!a!       allocate(b2(i1,i2))
!a!       allocate(z15(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!a!       allocate(ind1(n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3))
!a!       allocate(ind2(n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
!a!       i1=0
!a!       ind1=0
!a!       ind2=0
!a!       do a=n1+1,n3
!a!        do b=n1+1,n3
!a!         do e=n1+1,n3
!a!          do f=n1+1,n3
!a!           i1=i1+1
!a!           ind1(f,e,b,a)=i1
!a!       enddo;enddo;enddo;enddo
!a!       i1=0
!a!       do i=n0+1,n1
!a!        do j=n0+1,n1
!a!         do m=n0+1,n1
!a!          do n=n0+1,n1
!a!           i1=i1+1
!a!           ind2(n,m,j,i)=i1
!a!       enddo;enddo;enddo;enddo
!a!       i2=0
!a!       do i=n0+1,n1
!a!        do j=i+1,n1
!a!         do a=n1+1,n3
!a!          do b=a+1,n3
!a!           i2=i2+1
!a!           i1=0
!a!           do f=n1+1,n3
!a!            do e=n1+1,n3
!a!             do n=n0+1,n1
!a!              do m=n0+1,n1
!a!              i1=i1+1
!a!              b2(i1,i2)=t4a(ind1(f,e,b,a),ind2(n,m,j,i))
!a!           enddo;enddo;enddo;enddo
!a!!              call egemm2(1,i1,d1,b2(1,i2),z15(b,a,j,i))
!a!              call egemm2(1,i1,intr(n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3),
!a!     & b2(1,i2),z15(b,a,j,i))
!a!              v2a(b,a,j,i)=v2a(b,a,j,i)+0.250*z15(b,a,j,i)
!a!       enddo;enddo;enddo;enddo
!a!       deallocate(t4a)
!a!       deallocate(ind1)
!a!       deallocate(ind2)
!a!!       deallocate(d1)
!a!       deallocate(b2)
!a!       deallocate(z15)
!
!c
      if (lvl_q) then
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       allocate(t4a(n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,                !ilias: if no quadruples comment out the following 21 lines
     & n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       rewind(ta)
       read(ta)t4a
       allocate(d1(n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3))
       call reorder2143(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n0,n1,n1,n3,n1,n3,intr,d1)
       allocate(h2(n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,
     & n0+1:n1,n0+1:n1))
       call reorder56123478(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,
     & n0,n1,n0,n1,n0,n1,n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,t4a,h2)
       allocate(z15(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       i2=k1*k1*k3*k3
       i3=k3*k3*k1*k1
       call egemm2(i2,i3,d1,h2,z15)
       deallocate(d1)
       deallocate(h2)
       deallocate(t4a)
c
       v2a=v2a+0.250*z15
       deallocate(z15)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!c
!a!!       allocate(d1(n0+1:n2,n0+1:n1,n2+1:n3,n1+1:n3))
!a!!       call reorder2143(n0,n3,n0,n3,n0,n3,n0,n3,
!a!!     & n0,n2,n0,n1,n2,n3,n1,n3,intm,d1)
!a!       rewind(tb)
!a!       allocate(t4b(k4*k3*k3*k3,k2*k1*k1*k1))
!a!       read(tb)t4b
!a!       i2=(k1*(k1-1)*k3*(k3-1))/4
!a!       i1=k1*k2*k3*k4
!a!       allocate(b2(i1,i2))
!a!       allocate(z16(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!a!       allocate(ind1(n2+1:n3,n1+1:n3,n1+1:n3,n1+1:n3))
!a!       allocate(ind2(n0+1:n2,n0+1:n1,n0+1:n1,n0+1:n1))
!a!       i1=0
!a!       ind1=0
!a!       ind2=0
!a!       do a=n1+1,n3
!a!        do b=n1+1,n3
!a!         do e=n1+1,n3
!a!          do f=n2+1,n3
!a!           i1=i1+1
!a!           ind1(f,e,b,a)=i1
!a!       enddo;enddo;enddo;enddo
!a!       i1=0
!a!       do i=n0+1,n1
!a!        do j=n0+1,n1
!a!         do m=n0+1,n1
!a!          do n=n0+1,n2
!a!           i1=i1+1
!a!           ind2(n,m,j,i)=i1
!a!       enddo;enddo;enddo;enddo
!a!       i2=0
!a!       do i=n0+1,n1
!a!        do j=i+1,n1
!a!         do a=n1+1,n3
!a!          do b=a+1,n3
!a!           i2=i2+1
!a!           i1=0
!a!           do f=n2+1,n3
!a!            do e=n1+1,n3
!a!             do n=n0+1,n2
!a!              do m=n0+1,n1
!a!              i1=i1+1
!a!              b2(i1,i2)=t4b(ind1(f,e,b,a),ind2(n,m,j,i))
!a!           enddo;enddo;enddo;enddo
!a!!              call egemm2(1,i1,d1,b2(1,i2),z16(b,a,j,i))
!a!              call egemm2(1,i1,intm(n0+1:n1,n0+1:n2,n1+1:n3,n2+1:n3),
!a!     & b2(1,i2),z16(b,a,j,i))
!a!              v2a(b,a,j,i)=v2a(b,a,j,i)+z16(b,a,j,i)
!a!       enddo;enddo;enddo;enddo
!a!       deallocate(t4b)
!a!       deallocate(ind1)
!a!       deallocate(ind2)
!a!!       deallocate(d1)
!a!       deallocate(b2)
!a!       deallocate(z16)
!c
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       allocate(t4b(n2+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,                !ilias: if no quadruples comment out the following 21 lines
     & n0+1:n2,n0+1:n1,n0+1:n1,n0+1:n1))
       rewind(tb)
       read(tb)t4b
       allocate(d1(n0+1:n2,n0+1:n1,n2+1:n3,n1+1:n3))
       call reorder2143(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n0,n1,n2,n3,n1,n3,intm,d1)
       allocate(h2(n0+1:n2,n0+1:n1,n2+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,
     & n0+1:n1,n0+1:n1))
       call reorder56123478(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n0,n1,n0,n2,n0,n1,n2,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,t4b,h2)
       allocate(z16(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       i2=k1*k1*k3*k3
       i3=k3*k4*k1*k2
       call egemm2(i2,i3,d1,h2,z16)
       deallocate(d1)
       deallocate(h2)
       deallocate(t4b)
c
       v2a=v2a+z16
       deallocate(z16)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!c
!       allocate(d1(n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3))
!       call reorder2143(n0,n3,n0,n3,n0,n3,n0,n3,
!     & n0,n2,n0,n2,n2,n3,n2,n3,intb,d1)
!a!       rewind(tc)
!a!       allocate(t4c(k4*k4*k3*k3,k2*k2*k1*k1))
!a!       read(tc)t4c
!a!       i2=(k1*(k1-1)*k3*(k3-1))/4
!a!       i1=k2*k2*k4*k4
!a!       allocate(b2(i1,i2))
!a!       allocate(z17(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!a!       allocate(ind1(n2+1:n3,n2+1:n3,n1+1:n3,n1+1:n3))
!a!       allocate(ind2(n0+1:n2,n0+1:n2,n0+1:n1,n0+1:n1))
!a!       i1=0
!a!       ind1=0
!a!       ind2=0
!a!       do a=n1+1,n3
!a!        do b=n1+1,n3
!a!         do e=n2+1,n3
!a!          do f=n2+1,n3
!a!           i1=i1+1
!a!           ind1(f,e,b,a)=i1
!a!       enddo;enddo;enddo;enddo
!a!       i1=0
!a!       do i=n0+1,n1
!a!        do j=n0+1,n1
!a!         do m=n0+1,n2
!a!          do n=n0+1,n2
!a!           i1=i1+1
!a!           ind2(n,m,j,i)=i1
!a!       enddo;enddo;enddo;enddo
!a!       i2=0
!a!       do i=n0+1,n1
!a!        do j=i+1,n1
!a!         do a=n1+1,n3
!a!          do b=a+1,n3
!a!           i2=i2+1
!a!           i1=0
!a!           do f=n2+1,n3
!a!            do e=n2+1,n3
!a!             do n=n0+1,n2
!a!              do m=n0+1,n2
!a!              i1=i1+1
!a!              b2(i1,i2)=t4c(ind1(f,e,b,a),ind2(n,m,j,i))
!a!           enddo;enddo;enddo;enddo
!a!!              call egemm2(1,i1,d1,b2(1,i2),z17(b,a,j,i))
!a!              call egemm2(1,i1,intb(n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3),
!a!     & b2(1,i2),z17(b,a,j,i))
!a!              v2a(b,a,j,i)=v2a(b,a,j,i)+0.250*z17(b,a,j,i)
!a!       enddo;enddo;enddo;enddo
!a!       deallocate(t4c)
!a!       deallocate(ind1)
!a!       deallocate(ind2)
!a!!       deallocate(d1)
!a!       deallocate(b2)
!a!       deallocate(z17)
!c
!       v2a=v2a+0.250*z17
!       deallocate(z17)
!c
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       allocate(t4c(n2+1:n3,n2+1:n3,n1+1:n3,n1+1:n3,                !ilias: if no quadruples comment out the next 20 lines
     & n0+1:n2,n0+1:n2,n0+1:n1,n0+1:n1))
       rewind(tc)
       read(tc)t4c
       allocate(d1(n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3))
       call reorder2143(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n0,n2,n2,n3,n2,n3,intb,d1)
       allocate(h2(n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3,n1+1:n3,n1+1:n3,
     & n0+1:n1,n0+1:n1))
       call reorder56123478(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & n0,n1,n0,n2,n0,n2,n2,n3,n2,n3,n1,n3,n1,n3,n0,n1,n0,n1,t4c,h2)
       allocate(z17(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       i2=k1*k1*k3*k3
       i3=k4*k4*k2*k2
       call egemm2(i2,i3,d1,h2,z17)
       deallocate(d1)
       deallocate(h2)
c
       v2a=v2a+0.250*z17
       deallocate(z17)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      endif
       do i=n0+1,n1-1
       do j=i+1,n1
       do a=n1+1,n3-1
       do b=a+1,n3
         coeleft=fockr(b,b)
     &          +fockr(a,a)
     &          -fockr(j,j)
     &          -fockr(i,i)
     &          +shift
         t2a(b,a,j,i)=t2a(b,a,j,i)-v2a(b,a,j,i)/coeleft
         t2a(b,a,i,j)=-t2a(b,a,j,i)
         t2a(a,b,j,i)=-t2a(b,a,j,i)
         t2a(a,b,i,j)= t2a(b,a,j,i)
       enddo
       enddo
       enddo
       enddo
c
       end
