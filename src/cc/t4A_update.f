       subroutine t4a_update(n0,n1,n2,n3,k1,k2,k3,k4,shift,
     & fockr,fockb,intr,intb,intm,t1a,t1b,t2a,t2b,t2c,t3a,t3b,t3c,t3d,
     & iactocca,iactunoa,iactindq)
!    & t4a,t4b,t4c,t4d,t4e)
c
       integer a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p
       integer iactocca,iactunoa,iactindq
       integer iocca,iunoa
       integer, allocatable:: indocc(:,:,:,:)
       integer, allocatable:: indunocc(:,:,:,:)
!       integer indocc(n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1)
!       integer indunocc(n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3)
       real*8 shift,pp,coeleft,timt1,timt2,time1,time2
       real*8 sum
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
c
!   real*8 t4a(n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,
! & n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1)
!   real*8 t4b(n2+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,
! & n0+1:n2,n0+1:n1,n0+1:n1,n0+1:n1)
!   real*8 t4c(n2+1:n3,n2+1:n3,n1+1:n3,n1+1:n3,
! & n0+1:n2,n0+1:n2,n0+1:n1,n0+1:n1)
!   real*8 t4d(n2+1:n3,n2+1:n3,n2+1:n3,n1+1:n3,
! & n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n1)
!   real*8 t4e(n2+1:n3,n2+1:n3,n2+1:n3,n2+1:n3,
! & n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n2)
!   real*8 v4a(k3*k3*k3*k3,k1*k1*k1*k1)
c
       real*8,allocatable::t4a(:,:,:,:,:,:,:,:)
       real*8,allocatable::t4b(:,:,:,:,:,:,:,:)
!   real*8,allocatable::t4c(:,:,:,:,:,:,:,:)
!   real*8,allocatable::t4d(:,:,:,:,:,:,:,:)
!   real*8,allocatable::t4e(:,:,:,:,:,:,:,:)
       real*8,allocatable::v4a(:,:,:,:,:,:,:,:)
c
       integer ta,tb,tc,td,te
       parameter(ta=29,tb=30,tc=31,td=32,te=33)
c
       real*8,allocatable::b1(:,:)
       real*8,allocatable::b2(:,:)
       real*8,allocatable::d1(:,:,:,:)
       real*8,allocatable::d2(:,:,:,:)
       real*8,allocatable::f1(:,:,:,:,:,:)
       real*8,allocatable::f2(:,:,:,:,:,:)
       real*8,allocatable::h2(:,:,:,:,:,:,:,:)
c
       real*8,allocatable::s1(:,:,:,:)
       real*8,allocatable::s2(:,:,:,:)
       real*8,allocatable::s3(:,:,:,:)
       real*8,allocatable::s4(:,:,:,:)
       real*8,allocatable::q1(:,:)
       real*8,allocatable::q2(:,:)
       real*8,allocatable::s5(:,:,:,:)
       real*8,allocatable::u18(:,:,:,:,:,:)
       real*8,allocatable::s29(:,:,:,:)
       real*8,allocatable::s6(:,:,:,:)
       real*8,allocatable::u19(:,:,:,:,:,:)
       real*8,allocatable::s30(:,:,:,:)
       real*8,allocatable::q3(:,:)
       real*8,allocatable::s7(:,:,:,:)
       real*8,allocatable::u20(:,:,:,:,:,:)
       real*8,allocatable::s32(:,:,:,:)
       real*8,allocatable::s31(:,:,:,:)
       real*8,allocatable::s8(:,:,:,:)
       real*8,allocatable::u21(:,:,:,:,:,:)
       real*8,allocatable::q4(:,:)
       real*8,allocatable::s9(:,:,:,:)
       real*8,allocatable::s10(:,:,:,:)
       real*8,allocatable::q5(:,:)
       real*8,allocatable::q6(:,:)
       real*8,allocatable::u1(:,:,:,:,:,:)
       real*8,allocatable::u2(:,:,:,:,:,:)
       real*8,allocatable::u3(:,:,:,:,:,:)
       real*8,allocatable::s11(:,:,:,:)
       real*8,allocatable::s12(:,:,:,:)
       real*8,allocatable::u4(:,:,:,:,:,:)
       real*8,allocatable::u5(:,:,:,:,:,:)
       real*8,allocatable::s13(:,:,:,:)
       real*8,allocatable::s14(:,:,:,:)
       real*8,allocatable::u6(:,:,:,:,:,:)
       real*8,allocatable::s15(:,:,:,:)
       real*8,allocatable::u7(:,:,:,:,:,:)
       real*8,allocatable::s16(:,:,:,:)
       real*8,allocatable::u8(:,:,:,:,:,:)
       real*8,allocatable::u9(:,:,:,:,:,:)
       real*8,allocatable::u10(:,:,:,:,:,:)
       real*8,allocatable::u30(:,:,:,:,:,:)
       real*8,allocatable::u29(:,:,:,:,:,:)
       real*8,allocatable::u28(:,:,:,:,:,:)
       real*8,allocatable::s43(:,:,:,:)
       real*8,allocatable::u24(:,:,:,:,:,:)
       real*8,allocatable::u35(:,:,:,:,:,:)
       real*8,allocatable::u22(:,:,:,:,:,:)
       real*8,allocatable::u34(:,:,:,:,:,:)
       real*8,allocatable::s17(:,:,:,:)
       real*8,allocatable::u31(:,:,:,:,:,:)
       real*8,allocatable::s42(:,:,:,:)
       real*8,allocatable::u11(:,:,:,:,:,:)
       real*8,allocatable::s18(:,:,:,:)
       real*8,allocatable::s44(:,:,:,:)
       real*8,allocatable::s40(:,:,:,:)
       real*8,allocatable::q7(:,:)
       real*8,allocatable::s19(:,:,:,:)
       real*8,allocatable::s41(:,:,:,:)
       real*8,allocatable::q8(:,:)
       real*8,allocatable::u12(:,:,:,:,:,:)
       real*8,allocatable::u32(:,:,:,:,:,:)
       real*8,allocatable::s48(:,:,:,:)
       real*8,allocatable::u27(:,:,:,:,:,:)
       real*8,allocatable::u13(:,:,:,:,:,:)
       real*8,allocatable::s20(:,:,:,:)
       real*8,allocatable::s21(:,:,:,:)
       real*8,allocatable::s22(:,:,:,:)
       real*8,allocatable::s23(:,:,:,:)
       real*8,allocatable::s47(:,:,:,:)
       real*8,allocatable::s46(:,:,:,:)
       real*8,allocatable::q9(:,:)
       real*8,allocatable::q10(:,:)
       real*8,allocatable::s24(:,:,:,:)
       real*8,allocatable::s25(:,:,:,:)
       real*8,allocatable::u14(:,:,:,:,:,:)
       real*8,allocatable::u25(:,:,:,:,:,:)
       real*8,allocatable::s26(:,:,:,:)
       real*8,allocatable::u15(:,:,:,:,:,:)
       real*8,allocatable::u23(:,:,:,:,:,:)
       real*8,allocatable::s27(:,:,:,:)
       real*8,allocatable::s28(:,:,:,:)
       real*8,allocatable::u16(:,:,:,:,:,:)
       real*8,allocatable::u17(:,:,:,:,:,:)
       real*8,allocatable::s33(:,:,:,:)
       real*8,allocatable::q11(:,:)
       real*8,allocatable::s35(:,:,:,:)
       real*8,allocatable::s51(:,:,:,:)
       real*8,allocatable::s34(:,:,:,:)
       real*8,allocatable::u33(:,:,:,:,:,:)
       real*8,allocatable::s50(:,:,:,:)
       real*8,allocatable::s36(:,:,:,:)
       real*8,allocatable::q12(:,:)
       real*8,allocatable::s37(:,:,:,:)
       real*8,allocatable::s38(:,:,:,:)
       real*8,allocatable::u26(:,:,:,:,:,:)
       real*8,allocatable::q13(:,:)
       real*8,allocatable::s39(:,:,:,:)
       real*8,allocatable::q14(:,:)
       real*8,allocatable::s49(:,:,:,:)
       real*8,allocatable::q15(:,:)
       real*8,allocatable::q16(:,:)
       real*8,allocatable::s45(:,:,:,:)
       real*8,allocatable::x1(:,:,:,:)
       real*8,allocatable::z1(:,:,:,:,:,:,:,:)
       real*8,allocatable::x2(:,:,:,:)
       real*8,allocatable::z2(:,:,:,:,:,:,:,:)
       real*8,allocatable::x3(:,:)
       real*8,allocatable::z3(:,:,:,:,:,:,:,:)
       real*8,allocatable::x4(:,:)
       real*8,allocatable::z4(:,:,:,:,:,:,:,:)
       real*8,allocatable::x5(:,:,:,:)
       real*8,allocatable::z5(:,:,:,:,:,:,:,:)
       real*8,allocatable::x6(:,:,:,:)
       real*8,allocatable::z6(:,:,:,:,:,:,:,:)
       real*8,allocatable::x7(:,:,:,:)
       real*8,allocatable::z7(:,:,:,:,:,:,:,:)
       real*8,allocatable::x8(:,:,:,:)
       real*8,allocatable::z8(:,:,:,:,:,:,:,:)
       real*8,allocatable::x9(:,:,:,:)
       real*8,allocatable::z10(:,:,:,:,:,:,:,:)
       real*8,allocatable::x10(:,:,:,:)
       real*8,allocatable::z11(:,:,:,:,:,:,:,:)
       real*8,allocatable::z76(:,:,:,:,:,:,:,:)
       real*8,allocatable::z15(:,:,:,:,:,:,:,:)
       real*8,allocatable::z77(:,:,:,:,:,:,:,:)
       real*8,allocatable::z78(:,:,:,:,:,:,:,:)
       real*8,allocatable::z79(:,:,:,:,:,:,:,:)
       real*8,allocatable::z19(:,:,:,:,:,:,:,:)
       real*8,allocatable::z25(:,:,:,:,:,:,:,:)
       real*8,allocatable::z26(:,:,:,:,:,:,:,:)
       real*8,allocatable::z27(:,:,:,:,:,:,:,:)
       real*8,allocatable::z30(:,:,:,:,:,:,:,:)
       real*8,allocatable::z31(:,:,:,:,:,:,:,:)
       real*8,allocatable::z34(:,:,:,:,:,:,:,:)
       real*8,allocatable::z36(:,:,:,:,:,:,:,:)
       real*8,allocatable::z38(:,:,:,:,:,:,:,:)
       real*8,allocatable::z39(:,:,:,:,:,:,:,:)
       real*8,allocatable::z98(:,:,:,:,:,:,:,:)
       real*8,allocatable::z97(:,:,:,:,:,:,:,:)
       real*8,allocatable::z96(:,:,:,:,:,:,:,:)
       real*8,allocatable::z105(:,:,:,:,:,:,:,:)
       real*8,allocatable::z84(:,:,:,:,:,:,:,:)
       real*8,allocatable::z104(:,:,:,:,:,:,:,:)
       real*8,allocatable::z80(:,:,:,:,:,:,:,:)
       real*8,allocatable::z40(:,:,:,:,:,:,:,:)
       real*8,allocatable::z99(:,:,:,:,:,:,:,:)
       real*8,allocatable::z42(:,:,:,:,:,:,:,:)
       real*8,allocatable::z100(:,:,:,:,:,:,:,:)
       real*8,allocatable::z91(:,:,:,:,:,:,:,:)
       real*8,allocatable::z47(:,:,:,:,:,:,:,:)
       real*8,allocatable::z48(:,:,:,:,:,:,:,:)
       real*8,allocatable::z56(:,:,:,:,:,:,:,:)
       real*8,allocatable::z87(:,:,:,:,:,:,:,:)
       real*8,allocatable::z57(:,:,:,:,:,:,:,:)
       real*8,allocatable::z58(:,:,:,:,:,:,:,:)
       real*8,allocatable::z81(:,:,:,:,:,:,:,:)
       real*8,allocatable::z59(:,:,:,:,:,:,:,:)
       real*8,allocatable::z60(:,:,:,:,:,:,:,:)
       real*8,allocatable::z61(:,:,:,:,:,:,:,:)
       real*8,allocatable::z62(:,:,:,:,:,:,:,:)
       real*8,allocatable::z63(:,:,:,:,:,:,:,:)
       real*8,allocatable::z103(:,:,:,:,:,:,:,:)
       real*8,allocatable::z90(:,:,:,:,:,:,:,:)
c
       allocate(t4a(n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,
     & n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       allocate(t4b(n2+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,
     & n0+1:n2,n0+1:n1,n0+1:n1,n0+1:n1))
!   allocate(t4c(n2+1:n3,n2+1:n3,n1+1:n3,n1+1:n3,
! & n0+1:n2,n0+1:n2,n0+1:n1,n0+1:n1))
!   allocate(t4d(n2+1:n3,n2+1:n3,n2+1:n3,n1+1:n3,
! & n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n1))
!   allocate(t4e(n2+1:n3,n2+1:n3,n2+1:n3,n2+1:n3,
! & n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n2))
       allocate(v4a(n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,
     & n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
c
       rewind(ta)
       rewind(tb)
!   rewind(tc)
!   rewind(td)
!   rewind(te)
       read(ta)t4a
       read(tb)t4b
!   read(tc)t4c
!   read(td)t4d
!   read(te)t4e
c
       allocate(indocc(n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       allocate(indunocc(n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3))
       indocc=0
       indunocc=0
       do i=n0+1,n1-3;do j=i+1,n1-2;do k=j+1,n1-1;do l=k+1,n1
        iocca=0
        if(i.gt.(n1-iactocca))iocca=iocca+1
        if(j.gt.(n1-iactocca))iocca=iocca+1
        if(k.gt.(n1-iactocca))iocca=iocca+1
        if(l.gt.(n1-iactocca))iocca=iocca+1
        if(iocca.lt.iactindq)indocc(l,k,j,i)=1
       enddo;enddo;enddo;enddo
          do a=n1+1,n3-3;do b=a+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
           iunoa=0
           if(a.lt.(n1+iactunoa+1))iunoa=iunoa+1
           if(b.lt.(n1+iactunoa+1))iunoa=iunoa+1
           if(c.lt.(n1+iactunoa+1))iunoa=iunoa+1
           if(d.lt.(n1+iactunoa+1))iunoa=iunoa+1
           if(iunoa.lt.iactindq)indunocc(d,c,b,a)=1
          enddo;enddo;enddo;enddo
c
       v4a=0.0d0
c
       allocate(d1(n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       call reorder1243(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n0,n1,n0,n1,n0,n1,intr,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s1(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1*k1
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s1)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x1(n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       x1=0.0d0
       call sum2134(n0,n1,n1,n3,n0,n1,n0,n1,x1,s1,-1.000)
       deallocate(s1)
c
       allocate(d1(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       call reorder4213(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n1,n3,n0,n1,intr,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s2(n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1))
       i1=k1*k3*k1
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s2)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x9(n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       x9=0.0d0
       call sum3124(n0,n1,n1,n3,n0,n1,n0,n1,x9,s2, 1.000)
       deallocate(s2)
c
       allocate(d1(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder2413(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n1,n3,n0,n1,intr,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s3(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
       i1=k1*k3*k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s3)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x10(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
       x10=0.0d0
       call sum3124(n1,n3,n1,n3,n1,n3,n0,n1,x10,s3, 1.000)
       deallocate(s3)
c
       allocate(d1(n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3))
       call reorder3421(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n1,n3,n1,n3,n1,n3,intr,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s4(n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3))
       i1=k3*k3*k3
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s4)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x2(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
       x2=0.0d0
       call sum4123(n1,n3,n1,n3,n1,n3,n0,n1,x2,s4, 1.000)
       deallocate(s4)
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
       allocate(x3(n0+1:n1,n0+1:n1))
       x3=0.0d0
       call sum21(n0,n1,n0,n1,x3,q1, 1.000)
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
       allocate(x4(n1+1:n3,n1+1:n3))
       x4=0.0d0
       call sum21(n1,n3,n1,n3,x4,q2,-1.000)
       deallocate(q2)
c
       allocate(d1(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       call reorder4132(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n0,n1,n0,n1,intr,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s5(n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1*k1
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s5)
       deallocate(d1)
       deallocate(b2)
c
!       allocate(d1(n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
!       call reorder4213(n0,n1,n0,n1,n0,n1,n0,n1,
!     & n0,n1,n0,n1,n0,n1,n0,n1,s5,d1)
!       allocate(h2(n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!       call reorder56123478(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,
!     & n0,n1,n0,n1,n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,t4a,h2)
!       allocate(z15(n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
!       i1=k1*k1
!       i2=k1*k1*k3*k3*k3*k3
!       i3=k1*k1
!       call egemm(i1,i2,i3,d1,h2,z15)
!       deallocate(d1)
!       deallocate(h2)
c
       do i=n0+1,n1-3;do j=i+1,n1-2;do k=j+1,n1-1;do l=k+1,n1
        if(indocc(l,k,j,i).eq.1)cycle
         do a=n1+1,n3-3;do b=a+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
          if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1;do n=n0+1,n1
             sum=sum
     &     + (s5(j,m,i,n)*t4a(d,c,b,a,n,m,l,k)      ! lkji
     &     - s5(k,m,i,n)*t4a(d,c,b,a,n,m,l,j)       ! ljki (-0.500)
     &     + s5(l,m,i,n)*t4a(d,c,b,a,n,m,k,j)       ! kjli (+0.500)
     &     - s5(i,m,j,n)*t4a(d,c,b,a,n,m,l,k)       ! lkij (-0.500)
     &     + s5(i,m,k,n)*t4a(d,c,b,a,n,m,l,j)       ! ljik (+0.500)
     &     - s5(i,m,l,n)*t4a(d,c,b,a,n,m,k,j)       ! kjil (-0.500)
     &     + s5(k,m,j,n)*t4a(d,c,b,a,n,m,l,i)       ! likj (+0.500)
     &     - s5(l,m,j,n)*t4a(d,c,b,a,n,m,k,i)       ! kilj (-0.500)
     &     - s5(j,m,k,n)*t4a(d,c,b,a,n,m,l,i)       ! lijk (-0.500)
     &     + s5(j,m,l,n)*t4a(d,c,b,a,n,m,k,i)       ! kijl (+0.500)
     &     + s5(l,m,k,n)*t4a(d,c,b,a,n,m,j,i)       ! jilk (+0.500)
     &     - s5(k,m,l,n)*t4a(d,c,b,a,n,m,j,i))/2.0d0! jikl (-0.500)
             enddo;enddo
             v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       v4a=v4a+0.500*z15
!       call
!     & sum12345768(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z15,-0.500)
!       call
!     & sum12346758(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z15, 0.500)
!       call
!     & sum12345687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z15,-0.500)
!       call
!     & sum12345786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z15, 0.500)
!       call
!     & sum12346785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z15,-0.500)
!       call
!     & sum12345867(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z15, 0.500)
!       call
!     & sum12346857(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z15,-0.500)
!       call
!     & sum12345876(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z15,-0.500)
!       call
!     & sum12346875(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z15, 0.500)
!       call
!     & sum12347856(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z15, 0.500)
!       call
!     & sum12347865(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z15,-0.500)
c
!       do i=n0+1,n1-3
!       do j=i+1,n1-2
!       do k=j+1,n1-1
!       do l=k+1,n1
!       do a=n1+1,n3-3
!       do b=a+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!
!       v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)
!     & +(z15(d,c,b,a,l,k,j,i)
!     & -z15(d,c,b,a,l,j,k,i)       ! 12345768 (-0.500)
!     & +z15(d,c,b,a,k,j,l,i)       ! 12346758 (+0.500)
!     & -z15(d,c,b,a,l,k,i,j)       ! 12345687 (-0.500)
!     & +z15(d,c,b,a,l,j,i,k)       ! 12345786 (+0.500)
!     & -z15(d,c,b,a,k,j,i,l)       ! 12346785 (-0.500)
!     & +z15(d,c,b,a,l,i,k,j)       ! 12345867 (+0.500)
!     & -z15(d,c,b,a,k,i,l,j)       ! 12346857 (-0.500)
!     & -z15(d,c,b,a,l,i,j,k)       ! 12345876 (-0.500)
!     & +z15(d,c,b,a,k,i,j,l)       ! 12346875 (+0.500)
!     & +z15(d,c,b,a,j,i,l,k)       ! 12347856 (+0.500)
!     & -z15(d,c,b,a,j,i,k,l))/2.0d0! 12347865 (-0.500)
!
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z15)
c
       allocate(d1(n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       call reorder2413(n0,n1,n0,n1,n0,n1,n0,n1,
     & n0,n1,n0,n1,n0,n1,n0,n1,s5,d1)
       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
       allocate(u18(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1*k1
       i2=k1*k3*k3
       i3=k1
       call egemm(i1,i2,i3,d1,d2,u18)
       deallocate(d1)
       deallocate(d2)
c
!       allocate(f1(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
!       call reorder412356(n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,u18,f1)
!       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
!       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
!       allocate(z76(n1+1:n3,n1+1:n3,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
!       i1=k1*k1*k1*k3*k3
!       i2=k1*k3*k3
!       i3=k1
!       call egemm(i1,i2,i3,f1,d2,z76)
!       deallocate(f1)
!       deallocate(d2)
c
       do i=n0+1,n1-3;do j=i+1,n1-2;do k=j+1,n1-1;do l=k+1,n1
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-3;do b=a+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
          if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do n=n0+1,n1
             sum=sum
     &     + u18(b,a,l,n,j,i)*t2a(d,c,n,k)       !balnjidcnk      (+1.000)
     &     - u18(c,a,l,n,j,i)*t2a(d,b,n,k)       !calnjidbnk      (-1.000)
     &     - u18(d,a,k,n,j,i)*t2a(c,b,n,l)       !daknjicbnl      (-1.000)
     &     + u18(d,a,l,n,j,i)*t2a(c,b,n,k)       !dalnjicbnk      (+1.000)
     &     + u18(c,a,k,n,j,i)*t2a(d,b,n,l)       !caknjidbnl      (+1.000)
     &     - u18(b,a,k,n,j,i)*t2a(d,c,n,l)       !baknjidcnl      (-1.000)
     &     - u18(b,a,l,n,k,i)*t2a(d,c,n,j)       !balnkidcnj      (-1.000)
     &     + u18(c,a,l,n,k,i)*t2a(d,b,n,j)       !calnkidbnj      (+1.000)
     &     + u18(d,a,j,n,k,i)*t2a(c,b,n,l)       !dajnkicbnl      (+1.000)
     &     - u18(d,a,l,n,k,i)*t2a(c,b,n,j)       !dalnkicbnj      (-1.000)
     &     - u18(c,a,j,n,k,i)*t2a(d,b,n,l)       !cajnkidbnl      (-1.000)
     &     + u18(b,a,j,n,k,i)*t2a(d,c,n,l)       !bajnkidcnl      (+1.000)
     &     + u18(b,a,k,n,l,i)*t2a(d,c,n,j)       !baknlidcnj      (+1.000)
     &     - u18(c,a,k,n,l,i)*t2a(d,b,n,j)       !caknlidbnj      (-1.000)
     &     - u18(d,a,j,n,l,i)*t2a(c,b,n,k)       !dajnlicbnk      (-1.000)
     &     + u18(d,a,k,n,l,i)*t2a(c,b,n,j)       !daknlicbnj      (+1.000)
     &     + u18(c,a,j,n,l,i)*t2a(d,b,n,k)       !cajnlidbnk      (+1.000)
     &     - u18(b,a,j,n,l,i)*t2a(d,c,n,k)       !bajnlidcnk      (-1.000)
     &     - u18(b,a,l,n,i,j)*t2a(d,c,n,k)       !balnijdcnk      (-1.000)
     &     + u18(c,a,l,n,i,j)*t2a(d,b,n,k)       !calnijdbnk      (+1.000)
     &     + u18(d,a,k,n,i,j)*t2a(c,b,n,l)       !daknijcbnl      (+1.000)
     &     - u18(d,a,l,n,i,j)*t2a(c,b,n,k)       !dalnijcbnk      (-1.000)
     &     - u18(c,a,k,n,i,j)*t2a(d,b,n,l)       !caknijdbnl      (-1.000)
     &     + u18(b,a,k,n,i,j)*t2a(d,c,n,l)       !baknijdcnl      (+1.000)
     &     + u18(b,a,l,n,i,k)*t2a(d,c,n,j)       !balnikdcnj      (+1.000)
     &     - u18(c,a,l,n,i,k)*t2a(d,b,n,j)       !calnikdbnj      (-1.000)
     &     - u18(d,a,j,n,i,k)*t2a(c,b,n,l)       !dajnikcbnl      (-1.000)
     &     + u18(d,a,l,n,i,k)*t2a(c,b,n,j)       !dalnikcbnj      (+1.000)
     &     + u18(c,a,j,n,i,k)*t2a(d,b,n,l)       !cajnikdbnl      (+1.000)
     &     - u18(b,a,j,n,i,k)*t2a(d,c,n,l)       !bajnikdcnl      (-1.000)
     &     - u18(b,a,k,n,i,l)*t2a(d,c,n,j)       !baknildcnj      (-1.000)
     &     + u18(c,a,k,n,i,l)*t2a(d,b,n,j)       !caknildbnj      (+1.000)
     &     + u18(d,a,j,n,i,l)*t2a(c,b,n,k)       !dajnilcbnk      (+1.000)
     &     - u18(d,a,k,n,i,l)*t2a(c,b,n,j)       !daknilcbnj      (-1.000)
     &     - u18(c,a,j,n,i,l)*t2a(d,b,n,k)       !cajnildbnk      (-1.000)
     &     + u18(b,a,j,n,i,l)*t2a(d,c,n,k)       !bajnildcnk      (+1.000)
     &     + u18(b,a,l,n,k,j)*t2a(d,c,n,i)       !balnkjdcni      (+1.000)
     &     - u18(c,a,l,n,k,j)*t2a(d,b,n,i)       !calnkjdbni      (-1.000)
     &     - u18(d,a,i,n,k,j)*t2a(c,b,n,l)       !dainkjcbnl      (-1.000)
     &     + u18(d,a,l,n,k,j)*t2a(c,b,n,i)       !dalnkjcbni      (+1.000)
     &     + u18(c,a,i,n,k,j)*t2a(d,b,n,l)       !cainkjdbnl      (+1.000)
     &     - u18(b,a,i,n,k,j)*t2a(d,c,n,l)       !bainkjdcnl      (-1.000)
     &     - u18(b,a,k,n,l,j)*t2a(d,c,n,i)       !baknljdcni      (-1.000)
     &     + u18(c,a,k,n,l,j)*t2a(d,b,n,i)       !caknljdbni      (+1.000)
     &     + u18(d,a,i,n,l,j)*t2a(c,b,n,k)       !dainljcbnk      (+1.000)
     &     - u18(d,a,k,n,l,j)*t2a(c,b,n,i)       !daknljcbni      (-1.000)
     &     - u18(c,a,i,n,l,j)*t2a(d,b,n,k)       !cainljdbnk      (-1.000)
     &     + u18(b,a,i,n,l,j)*t2a(d,c,n,k)       !bainljdcnk      (+1.000)
     &     - u18(b,a,l,n,j,k)*t2a(d,c,n,i)       !balnjkdcni      (-1.000)
     &     + u18(c,a,l,n,j,k)*t2a(d,b,n,i)       !calnjkdbni      (+1.000)
     &     + u18(d,a,i,n,j,k)*t2a(c,b,n,l)       !dainjkcbnl      (+1.000)
     &     - u18(d,a,l,n,j,k)*t2a(c,b,n,i)       !dalnjkcbni      (-1.000)
     &     - u18(c,a,i,n,j,k)*t2a(d,b,n,l)       !cainjkdbnl      (-1.000)
     &     + u18(b,a,i,n,j,k)*t2a(d,c,n,l)       !bainjkdcnl      (+1.000)
     &     + u18(b,a,k,n,j,l)*t2a(d,c,n,i)       !baknjldcni      (+1.000)
     &     - u18(c,a,k,n,j,l)*t2a(d,b,n,i)       !caknjldbni      (-1.000)
     &     - u18(d,a,i,n,j,l)*t2a(c,b,n,k)       !dainjlcbnk      (-1.000)
     &     + u18(d,a,k,n,j,l)*t2a(c,b,n,i)       !daknjlcbni      (+1.000)
     &     + u18(c,a,i,n,j,l)*t2a(d,b,n,k)       !cainjldbnk      (+1.000)
     &     - u18(b,a,i,n,j,l)*t2a(d,c,n,k)       !bainjldcnk      (-1.000)
     &     + u18(b,a,j,n,l,k)*t2a(d,c,n,i)       !bajnlkdcni      (+1.000)
     &     - u18(c,a,j,n,l,k)*t2a(d,b,n,i)       !cajnlkdbni      (-1.000)
     &     - u18(d,a,i,n,l,k)*t2a(c,b,n,j)       !dainlkcbnj      (-1.000)
     &     + u18(d,a,j,n,l,k)*t2a(c,b,n,i)       !dajnlkcbni      (+1.000)
     &     + u18(c,a,i,n,l,k)*t2a(d,b,n,j)       !cainlkdbnj      (+1.000)
     &     - u18(b,a,i,n,l,k)*t2a(d,c,n,j)       !bainlkdcnj      (-1.000)
     &     - u18(b,a,j,n,k,l)*t2a(d,c,n,i)       !bajnkldcni      (-1.000)
     &     + u18(c,a,j,n,k,l)*t2a(d,b,n,i)       !cajnkldbni      (+1.000)
     &     + u18(d,a,i,n,k,l)*t2a(c,b,n,j)       !dainklcbnj      (+1.000)
     &     - u18(d,a,j,n,k,l)*t2a(c,b,n,i)       !dajnklcbni      (-1.000)
     &     - u18(c,a,i,n,k,l)*t2a(d,b,n,j)       !cainkldbnj      (-1.000)
     &     + u18(b,a,i,n,k,l)*t2a(d,c,n,j)       !bainkldcnj      (+1.000)             
             enddo
             v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call
!     & sum12634578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z76, 1.000)
!       call
!     & sum13624578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z76,-1.000)
!       call
!     & sum23514678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z76,-1.000)
!       call
!     & sum23614578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z76, 1.000)
!       call
!     & sum13524678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z76, 1.000)
!       call
!     & sum12534678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z76,-1.000)
!       call
!     & sum12734568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z76,-1.000)
!       call
!     & sum13724568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z76, 1.000)
!       call
!     & sum23514768(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z76, 1.000)
!       call
!     & sum23714568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z76,-1.000)
!       call
!     & sum13524768(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z76,-1.000)
!       call
!     & sum12534768(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z76, 1.000)
!       call
!     & sum12734658(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z76, 1.000)
!       call
!     & sum13724658(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z76,-1.000)
!       call
!     & sum23614758(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z76,-1.000)
!       call
!     & sum23714658(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z76, 1.000)
!       call
!     & sum13624758(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z76, 1.000)
!       call
!     & sum12634758(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z76,-1.000)
!       call
!     & sum12634587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z76,-1.000)
!       call
!     & sum13624587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z76, 1.000)
!       call
!     & sum23514687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z76, 1.000)
!       call
!     & sum23614587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z76,-1.000)
!       call
!     & sum13524687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z76,-1.000)
!       call
!     & sum12534687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z76, 1.000)
!       call
!     & sum12734586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z76, 1.000)
!       call
!     & sum13724586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z76,-1.000)
!       call
!     & sum23514786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z76,-1.000)
!       call
!     & sum23714586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z76, 1.000)
!       call
!     & sum13524786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z76, 1.000)
!       call
!     & sum12534786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z76,-1.000)
!       call
!     & sum12734685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z76,-1.000)
!       call
!     & sum13724685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z76, 1.000)
!       call
!     & sum23614785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z76, 1.000)
!       call
!     & sum23714685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z76,-1.000)
!       call
!     & sum13624785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z76,-1.000)
!       call
!     & sum12634785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z76, 1.000)
!       call
!     & sum12834567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z76, 1.000)
!       call
!     & sum13824567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z76,-1.000)
!       call
!     & sum23514867(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z76,-1.000)
!       call
!     & sum23814567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z76, 1.000)
!       call
!     & sum13524867(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z76, 1.000)
!       call
!     & sum12534867(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z76,-1.000)
!       call
!     & sum12834657(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z76,-1.000)
!       call
!     & sum13824657(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z76, 1.000)
!       call
!     & sum23614857(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z76, 1.000)
!       call
!     & sum23814657(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z76,-1.000)
!       call
!     & sum13624857(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z76,-1.000)
!       call
!     & sum12634857(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z76, 1.000)
!       call
!     & sum12834576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z76,-1.000)
!       call
!     & sum13824576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z76, 1.000)
!       call
!     & sum23514876(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z76, 1.000)
!       call
!     & sum23814576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z76,-1.000)
!       call
!     & sum13524876(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z76,-1.000)
!       call
!     & sum12534876(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z76, 1.000)
!       call
!     & sum12834675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z76, 1.000)
!       call
!     & sum13824675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z76,-1.000)
!       call
!     & sum23614875(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z76,-1.000)
!       call
!     & sum23814675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z76, 1.000)
!       call
!     & sum13624875(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z76, 1.000)
!       call
!     & sum12634875(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z76,-1.000)
!       call
!     & sum12834756(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z76, 1.000)
!       call
!     & sum13824756(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z76,-1.000)
!       call
!     & sum23714856(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z76,-1.000)
!       call
!     & sum23814756(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z76, 1.000)
!       call
!     & sum13724856(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z76, 1.000)
!       call
!     & sum12734856(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z76,-1.000)
!       call
!     & sum12834765(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z76,-1.000)
!       call
!     & sum13824765(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z76, 1.000)
!       call
!     & sum23714865(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z76, 1.000)
!       call
!     & sum23814765(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z76,-1.000)
!       call
!     & sum13724865(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z76,-1.000)
!       call
!     & sum12734865(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z76, 1.000)
c
!       do i=n0+1,n1-3
!       do j=i+1,n1-2
!       do k=j+1,n1-1
!       do l=k+1,n1
!       do a=n1+1,n3-3
!       do b=a+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)
!     & +z76(d,c,k,b,a,l,j,i)       ! 12634578( 1.000)
!     & -z76(d,b,k,c,a,l,j,i)       ! 13624578(-1.000)
!     & -z76(c,b,l,d,a,k,j,i)       ! 23514678(-1.000)
!     & +z76(c,b,k,d,a,l,j,i)       ! 23614578( 1.000)
!     & +z76(d,b,l,c,a,k,j,i)       ! 13524678( 1.000)
!     & -z76(d,c,l,b,a,k,j,i)       ! 12534678(-1.000)
!     & -z76(d,c,j,b,a,l,k,i)       ! 12734568(-1.000)
!     & +z76(d,b,j,c,a,l,k,i)       ! 13724568( 1.000)
!     & +z76(c,b,l,d,a,j,k,i)       ! 23514768( 1.000)
!     & -z76(c,b,j,d,a,l,k,i)       ! 23714568(-1.000)
!     & -z76(d,b,l,c,a,j,k,i)       ! 13524768(-1.000)
!     & +z76(d,c,l,b,a,j,k,i)       ! 12534768( 1.000)
!     & +z76(d,c,j,b,a,k,l,i)       ! 12734658( 1.000)
!     & -z76(d,b,j,c,a,k,l,i)       ! 13724658(-1.000)
!     & -z76(c,b,k,d,a,j,l,i)       ! 23614758(-1.000)
!     & +z76(c,b,j,d,a,k,l,i)       ! 23714658( 1.000)
!     & +z76(d,b,k,c,a,j,l,i)       ! 13624758( 1.000)
!     & -z76(d,c,k,b,a,j,l,i)       ! 12634758(-1.000)
!     & -z76(d,c,k,b,a,l,i,j)       ! 12634587(-1.000)
!     & +z76(d,b,k,c,a,l,i,j)       ! 13624587( 1.000)
!     & +z76(c,b,l,d,a,k,i,j)       ! 23514687( 1.000)
!     & -z76(c,b,k,d,a,l,i,j)       ! 23614587(-1.000)
!     & -z76(d,b,l,c,a,k,i,j)       ! 13524687(-1.000)
!     & +z76(d,c,l,b,a,k,i,j)       ! 12534687( 1.000)
!     & +z76(d,c,j,b,a,l,i,k)       ! 12734586( 1.000)
!     & -z76(d,b,j,c,a,l,i,k)       ! 13724586(-1.000)
!     & -z76(c,b,l,d,a,j,i,k)       ! 23514786(-1.000)
!     & +z76(c,b,j,d,a,l,i,k)       ! 23714586( 1.000)
!     & +z76(d,b,l,c,a,j,i,k)       ! 13524786( 1.000)
!     & -z76(d,c,l,b,a,j,i,k)       ! 12534786(-1.000)
!     & -z76(d,c,j,b,a,k,i,l)       ! 12734685(-1.000)
!     & +z76(d,b,j,c,a,k,i,l)       ! 13724685( 1.000)
!     & +z76(c,b,k,d,a,j,i,l)       ! 23614785( 1.000)
!     & -z76(c,b,j,d,a,k,i,l)       ! 23714685(-1.000)
!     & -z76(d,b,k,c,a,j,i,l)       ! 13624785(-1.000)
!     & +z76(d,c,k,b,a,j,i,l)       ! 12634785( 1.000)
!     & +z76(d,c,i,b,a,l,k,j)       ! 12834567( 1.000)
!     & -z76(d,b,i,c,a,l,k,j)       ! 13824567(-1.000)
!     & -z76(c,b,l,d,a,i,k,j)       ! 23514867(-1.000)
!     & +z76(c,b,i,d,a,l,k,j)       ! 23814567( 1.000)
!     & +z76(d,b,l,c,a,i,k,j)       ! 13524867( 1.000)
!     & -z76(d,c,l,b,a,i,k,j)       ! 12534867(-1.000)
!     & -z76(d,c,i,b,a,k,l,j)       ! 12834657(-1.000)
!     & +z76(d,b,i,c,a,k,l,j)       ! 13824657( 1.000)
!     & +z76(c,b,k,d,a,i,l,j)       ! 23614857( 1.000)
!     & -z76(c,b,i,d,a,k,l,j)       ! 23814657(-1.000)
!     & -z76(d,b,k,c,a,i,l,j)       ! 13624857(-1.000)
!     & +z76(d,c,k,b,a,i,l,j)       ! 12634857( 1.000)
!     & -z76(d,c,i,b,a,l,j,k)       ! 12834576(-1.000)
!     & +z76(d,b,i,c,a,l,j,k)       ! 13824576( 1.000)
!     & +z76(c,b,l,d,a,i,j,k)       ! 23514876( 1.000)
!     & -z76(c,b,i,d,a,l,j,k)       ! 23814576(-1.000)
!     & -z76(d,b,l,c,a,i,j,k)       ! 13524876(-1.000)
!     & +z76(d,c,l,b,a,i,j,k)       ! 12534876( 1.000)
!     & +z76(d,c,i,b,a,k,j,l)       ! 12834675( 1.000)
!     & -z76(d,b,i,c,a,k,j,l)       ! 13824675(-1.000)
!     & -z76(c,b,k,d,a,i,j,l)       ! 23614875(-1.000)
!     & +z76(c,b,i,d,a,k,j,l)       ! 23814675( 1.000)
!     & +z76(d,b,k,c,a,i,j,l)       ! 13624875( 1.000)
!     & -z76(d,c,k,b,a,i,j,l)       ! 12634875(-1.000)
!     & +z76(d,c,i,b,a,j,l,k)       ! 12834756( 1.000)
!     & -z76(d,b,i,c,a,j,l,k)       ! 13824756(-1.000)
!     & -z76(c,b,j,d,a,i,l,k)       ! 23714856(-1.000)
!     & +z76(c,b,i,d,a,j,l,k)       ! 23814756( 1.000)
!     & +z76(d,b,j,c,a,i,l,k)       ! 13724856( 1.000)
!     & -z76(d,c,j,b,a,i,l,k)       ! 12734856(-1.000)
!     & -z76(d,c,i,b,a,j,k,l)       ! 12834765(-1.000)
!     & +z76(d,b,i,c,a,j,k,l)       ! 13824765( 1.000)
!     & +z76(c,b,j,d,a,i,k,l)       ! 23714865( 1.000)
!     & -z76(c,b,i,d,a,j,k,l)       ! 23814765(-1.000)
!     & -z76(d,b,j,c,a,i,k,l)       ! 13724865(-1.000)
!     & +z76(d,c,j,b,a,i,k,l)       ! 12734865( 1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z76)
       deallocate(u18)
c
       allocate(d1(n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       call reorder2413(n0,n1,n0,n1,n0,n1,n0,n1,
     & n0,n1,n0,n1,n0,n1,n0,n1,s5,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s29(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1*k1
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s29)
       deallocate(d1)
       deallocate(b2)
       deallocate(s5)
c
       call sum2134(n0,n1,n1,n3,n0,n1,n0,n1,x9,s29,-1.000)
       deallocate(s29)
c
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1432(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n0,n1,n0,n1,intr,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s6(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       i1=k1*k1*k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s6)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x6(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       x6=0.0d0
       call sum3241(n0,n1,n1,n3,n1,n3,n0,n1,x6,s6,-1.000)
c
       allocate(d1(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       call reorder2413(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n0,n1,n1,n3,n0,n1,s6,d1)
       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
       allocate(u19(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1))
       i1=k1*k3*k1
       i2=k1*k1*k3
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u19)
       deallocate(d1)
       deallocate(d2)
c
!       allocate(f1(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
!       call reorder451236(n1,n3,n0,n1,n0,n1,n0,n1,n1,n3,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,u19,f1)
!       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
!       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
!       allocate(z77(n1+1:n3,n1+1:n3,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
!       i1=k1*k1*k1*k3*k3
!       i2=k1*k3*k3
!       i3=k1
!       call egemm(i1,i2,i3,f1,d2,z77)
!       deallocate(f1)
!       deallocate(d2)
!c
       do i=n0+1,n1-3;do j=i+1,n1-2;do k=j+1,n1-1;do l=k+1,n1
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-3;do b=a+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
          if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do n=n0+1,n1
             sum=sum
     &     + u19(c,k,j,n,d,i)*t2a(b,a,n,l)       !ckjndibanl      (+1.000)
     &     - u19(b,k,j,n,d,i)*t2a(c,a,n,l)       !bkjndicanl      (-1.000)
     &     + u19(a,k,j,n,d,i)*t2a(c,b,n,l)       !akjndicbnl      (+1.000)
     &     - u19(d,k,j,n,c,i)*t2a(b,a,n,l)       !dkjncibanl      (-1.000)
     &     + u19(d,k,j,n,b,i)*t2a(c,a,n,l)       !dkjnbicanl      (+1.000)
     &     - u19(d,k,j,n,a,i)*t2a(c,b,n,l)       !dkjnaicbnl      (-1.000)
     &     + u19(b,k,j,n,c,i)*t2a(d,a,n,l)       !bkjncidanl      (+1.000)
     &     - u19(a,k,j,n,c,i)*t2a(d,b,n,l)       !akjncidbnl      (-1.000)
     &     - u19(c,k,j,n,b,i)*t2a(d,a,n,l)       !ckjnbidanl      (-1.000)
     &     + u19(c,k,j,n,a,i)*t2a(d,b,n,l)       !ckjnaidbnl      (+1.000)
     &     + u19(a,k,j,n,b,i)*t2a(d,c,n,l)       !akjnbidcnl      (+1.000)
     &     - u19(b,k,j,n,a,i)*t2a(d,c,n,l)       !bkjnaidcnl      (-1.000)
     &     - u19(c,l,j,n,d,i)*t2a(b,a,n,k)       !cljndibank      (-1.000)
     &     + u19(b,l,j,n,d,i)*t2a(c,a,n,k)       !bljndicank      (+1.000)
     &     - u19(a,l,j,n,d,i)*t2a(c,b,n,k)       !aljndicbnk      (-1.000)
     &     + u19(d,l,j,n,c,i)*t2a(b,a,n,k)       !dljncibank      (+1.000)
     &     - u19(d,l,j,n,b,i)*t2a(c,a,n,k)       !dljnbicank      (-1.000)
     &     + u19(d,l,j,n,a,i)*t2a(c,b,n,k)       !dljnaicbnk      (+1.000)
     &     - u19(b,l,j,n,c,i)*t2a(d,a,n,k)       !bljncidank      (-1.000)
     &     + u19(a,l,j,n,c,i)*t2a(d,b,n,k)       !aljncidbnk      (+1.000)
     &     + u19(c,l,j,n,b,i)*t2a(d,a,n,k)       !cljnbidank      (+1.000)
     &     - u19(c,l,j,n,a,i)*t2a(d,b,n,k)       !cljnaidbnk      (-1.000)
     &     - u19(a,l,j,n,b,i)*t2a(d,c,n,k)       !aljnbidcnk      (-1.000)
     &     + u19(b,l,j,n,a,i)*t2a(d,c,n,k)       !bljnaidcnk      (+1.000)
     &     + u19(a,l,k,n,d,i)*t2a(c,b,n,j)       !alkndicbnj      (+1.000)
     &     - u19(b,l,k,n,d,i)*t2a(c,a,n,j)       !blkndicanj      (-1.000)
     &     + u19(c,l,k,n,d,i)*t2a(b,a,n,j)       !clkndibanj      (+1.000)
     &     - u19(a,l,k,n,c,i)*t2a(d,b,n,j)       !alkncidbnj      (-1.000)
     &     + u19(b,l,k,n,c,i)*t2a(d,a,n,j)       !blkncidanj      (+1.000)
     &     + u19(a,l,k,n,b,i)*t2a(d,c,n,j)       !alknbidcnj      (+1.000)
     &     - u19(b,l,k,n,a,i)*t2a(d,c,n,j)       !blknaidcnj      (-1.000)
     &     - u19(c,l,k,n,b,i)*t2a(d,a,n,j)       !clknbidanj      (-1.000)
     &     + u19(c,l,k,n,a,i)*t2a(d,b,n,j)       !clknaidbnj      (+1.000)
     &     - u19(d,l,k,n,c,i)*t2a(b,a,n,j)       !dlkncibanj      (-1.000)
     &     + u19(d,l,k,n,b,i)*t2a(c,a,n,j)       !dlknbicanj      (+1.000)
     &     - u19(d,l,k,n,a,i)*t2a(c,b,n,j)       !dlknaicbnj      (-1.000)
     &     - u19(c,k,i,n,d,j)*t2a(b,a,n,l)       !ckindjbanl      (-1.000)
     &     + u19(b,k,i,n,d,j)*t2a(c,a,n,l)       !bkindjcanl      (+1.000)
     &     - u19(a,k,i,n,d,j)*t2a(c,b,n,l)       !akindjcbnl      (-1.000)
     &     + u19(d,k,i,n,c,j)*t2a(b,a,n,l)       !dkincjbanl      (+1.000)
     &     - u19(d,k,i,n,b,j)*t2a(c,a,n,l)       !dkinbjcanl      (-1.000)
     &     + u19(d,k,i,n,a,j)*t2a(c,b,n,l)       !dkinajcbnl      (+1.000)
     &     - u19(b,k,i,n,c,j)*t2a(d,a,n,l)       !bkincjdanl      (-1.000)
     &     + u19(a,k,i,n,c,j)*t2a(d,b,n,l)       !akincjdbnl      (+1.000)
     &     + u19(c,k,i,n,b,j)*t2a(d,a,n,l)       !ckinbjdanl      (+1.000)
     &     - u19(c,k,i,n,a,j)*t2a(d,b,n,l)       !ckinajdbnl      (-1.000)
     &     - u19(a,k,i,n,b,j)*t2a(d,c,n,l)       !akinbjdcnl      (-1.000)
     &     + u19(b,k,i,n,a,j)*t2a(d,c,n,l)       !bkinajdcnl      (+1.000)
     &     + u19(c,l,i,n,d,j)*t2a(b,a,n,k)       !clindjbank      (+1.000)
     &     - u19(b,l,i,n,d,j)*t2a(c,a,n,k)       !blindjcank      (-1.000)
     &     + u19(a,l,i,n,d,j)*t2a(c,b,n,k)       !alindjcbnk      (+1.000)
     &     - u19(d,l,i,n,c,j)*t2a(b,a,n,k)       !dlincjbank      (-1.000)
     &     + u19(d,l,i,n,b,j)*t2a(c,a,n,k)       !dlinbjcank      (+1.000)
     &     - u19(d,l,i,n,a,j)*t2a(c,b,n,k)       !dlinajcbnk      (-1.000)
     &     + u19(b,l,i,n,c,j)*t2a(d,a,n,k)       !blincjdank      (+1.000)
     &     - u19(a,l,i,n,c,j)*t2a(d,b,n,k)       !alincjdbnk      (-1.000)
     &     - u19(c,l,i,n,b,j)*t2a(d,a,n,k)       !clinbjdank      (-1.000)
     &     + u19(c,l,i,n,a,j)*t2a(d,b,n,k)       !clinajdbnk      (+1.000)
     &     + u19(a,l,i,n,b,j)*t2a(d,c,n,k)       !alinbjdcnk      (+1.000)
     &     - u19(b,l,i,n,a,j)*t2a(d,c,n,k)       !blinajdcnk      (-1.000)
     &     - u19(a,l,k,n,d,j)*t2a(c,b,n,i)       !alkndjcbni      (-1.000)
     &     + u19(b,l,k,n,d,j)*t2a(c,a,n,i)       !blkndjcani      (+1.000)
     &     - u19(c,l,k,n,d,j)*t2a(b,a,n,i)       !clkndjbani      (-1.000)
     &     + u19(a,l,k,n,c,j)*t2a(d,b,n,i)       !alkncjdbni      (+1.000)
     &     - u19(b,l,k,n,c,j)*t2a(d,a,n,i)       !blkncjdani      (-1.000)
     &     - u19(a,l,k,n,b,j)*t2a(d,c,n,i)       !alknbjdcni      (-1.000)
     &     + u19(b,l,k,n,a,j)*t2a(d,c,n,i)       !blknajdcni      (+1.000)
     &     + u19(c,l,k,n,b,j)*t2a(d,a,n,i)       !clknbjdani      (+1.000)
     &     - u19(c,l,k,n,a,j)*t2a(d,b,n,i)       !clknajdbni      (-1.000)
     &     + u19(d,l,k,n,c,j)*t2a(b,a,n,i)       !dlkncjbani      (+1.000)
     &     - u19(d,l,k,n,b,j)*t2a(c,a,n,i)       !dlknbjcani      (-1.000)
     &     + u19(d,l,k,n,a,j)*t2a(c,b,n,i)       !dlknajcbni      (+1.000)
     &     + u19(c,j,i,n,d,k)*t2a(b,a,n,l)       !cjindkbanl      (+1.000)
     &     - u19(b,j,i,n,d,k)*t2a(c,a,n,l)       !bjindkcanl      (-1.000)
     &     + u19(a,j,i,n,d,k)*t2a(c,b,n,l)       !ajindkcbnl      (+1.000)
     &     - u19(d,j,i,n,c,k)*t2a(b,a,n,l)       !djinckbanl      (-1.000)
     &     + u19(d,j,i,n,b,k)*t2a(c,a,n,l)       !djinbkcanl      (+1.000)
     &     - u19(d,j,i,n,a,k)*t2a(c,b,n,l)       !djinakcbnl      (-1.000)
     &     + u19(b,j,i,n,c,k)*t2a(d,a,n,l)       !bjinckdanl      (+1.000)
     &     - u19(a,j,i,n,c,k)*t2a(d,b,n,l)       !ajinckdbnl      (-1.000)
     &     - u19(c,j,i,n,b,k)*t2a(d,a,n,l)       !cjinbkdanl      (-1.000)
     &     + u19(c,j,i,n,a,k)*t2a(d,b,n,l)       !cjinakdbnl      (+1.000)
     &     + u19(a,j,i,n,b,k)*t2a(d,c,n,l)       !ajinbkdcnl      (+1.000)
     &     - u19(b,j,i,n,a,k)*t2a(d,c,n,l)       !bjinakdcnl      (-1.000)
     &     - u19(c,j,i,n,d,l)*t2a(b,a,n,k)       !cjindlbank      (-1.000)
     &     + u19(b,j,i,n,d,l)*t2a(c,a,n,k)       !bjindlcank      (+1.000)
     &     - u19(a,j,i,n,d,l)*t2a(c,b,n,k)       !ajindlcbnk      (-1.000)
     &     + u19(d,j,i,n,c,l)*t2a(b,a,n,k)       !djinclbank      (+1.000)
     &     - u19(d,j,i,n,b,l)*t2a(c,a,n,k)       !djinblcank      (-1.000)
     &     + u19(d,j,i,n,a,l)*t2a(c,b,n,k)       !djinalcbnk      (+1.000)
     &     - u19(b,j,i,n,c,l)*t2a(d,a,n,k)       !bjincldank      (-1.000)
     &     + u19(a,j,i,n,c,l)*t2a(d,b,n,k)       !ajincldbnk      (+1.000)
     &     + u19(c,j,i,n,b,l)*t2a(d,a,n,k)       !cjinbldank      (+1.000)
     &     - u19(c,j,i,n,a,l)*t2a(d,b,n,k)       !cjinaldbnk      (-1.000)
     &     - u19(a,j,i,n,b,l)*t2a(d,c,n,k)       !ajinbldcnk      (-1.000)
     &     + u19(b,j,i,n,a,l)*t2a(d,c,n,k)       !bjinaldcnk      (+1.000)
     &     - u19(c,l,i,n,d,k)*t2a(b,a,n,j)       !clindkbanj      (-1.000)
     &     + u19(b,l,i,n,d,k)*t2a(c,a,n,j)       !blindkcanj      (+1.000)
     &     - u19(a,l,i,n,d,k)*t2a(c,b,n,j)       !alindkcbnj      (-1.000)
     &     + u19(d,l,i,n,c,k)*t2a(b,a,n,j)       !dlinckbanj      (+1.000)
     &     - u19(d,l,i,n,b,k)*t2a(c,a,n,j)       !dlinbkcanj      (-1.000)
     &     + u19(d,l,i,n,a,k)*t2a(c,b,n,j)       !dlinakcbnj      (+1.000)
     &     - u19(b,l,i,n,c,k)*t2a(d,a,n,j)       !blinckdanj      (-1.000)
     &     + u19(a,l,i,n,c,k)*t2a(d,b,n,j)       !alinckdbnj      (+1.000)
     &     + u19(c,l,i,n,b,k)*t2a(d,a,n,j)       !clinbkdanj      (+1.000)
     &     - u19(c,l,i,n,a,k)*t2a(d,b,n,j)       !clinakdbnj      (-1.000)
     &     - u19(a,l,i,n,b,k)*t2a(d,c,n,j)       !alinbkdcnj      (-1.000)
     &     + u19(b,l,i,n,a,k)*t2a(d,c,n,j)       !blinakdcnj      (+1.000)
     &     + u19(a,l,j,n,d,k)*t2a(c,b,n,i)       !aljndkcbni      (+1.000)
     &     - u19(b,l,j,n,d,k)*t2a(c,a,n,i)       !bljndkcani      (-1.000)
     &     + u19(c,l,j,n,d,k)*t2a(b,a,n,i)       !cljndkbani      (+1.000)
     &     - u19(a,l,j,n,c,k)*t2a(d,b,n,i)       !aljnckdbni      (-1.000)
     &     + u19(b,l,j,n,c,k)*t2a(d,a,n,i)       !bljnckdani      (+1.000)
     &     + u19(a,l,j,n,b,k)*t2a(d,c,n,i)       !aljnbkdcni      (+1.000)
     &     - u19(b,l,j,n,a,k)*t2a(d,c,n,i)       !bljnakdcni      (-1.000)
     &     - u19(c,l,j,n,b,k)*t2a(d,a,n,i)       !cljnbkdani      (-1.000)
     &     + u19(c,l,j,n,a,k)*t2a(d,b,n,i)       !cljnakdbni      (+1.000)
     &     - u19(d,l,j,n,c,k)*t2a(b,a,n,i)       !dljnckbani      (-1.000)
     &     + u19(d,l,j,n,b,k)*t2a(c,a,n,i)       !dljnbkcani      (+1.000)
     &     - u19(d,l,j,n,a,k)*t2a(c,b,n,i)       !dljnakcbni      (-1.000)
     &     + u19(c,k,i,n,d,l)*t2a(b,a,n,j)       !ckindlbanj      (+1.000)
     &     - u19(b,k,i,n,d,l)*t2a(c,a,n,j)       !bkindlcanj      (-1.000)
     &     + u19(a,k,i,n,d,l)*t2a(c,b,n,j)       !akindlcbnj      (+1.000)
     &     - u19(d,k,i,n,c,l)*t2a(b,a,n,j)       !dkinclbanj      (-1.000)
     &     + u19(d,k,i,n,b,l)*t2a(c,a,n,j)       !dkinblcanj      (+1.000)
     &     - u19(d,k,i,n,a,l)*t2a(c,b,n,j)       !dkinalcbnj      (-1.000)
     &     + u19(b,k,i,n,c,l)*t2a(d,a,n,j)       !bkincldanj      (+1.000)
     &     - u19(a,k,i,n,c,l)*t2a(d,b,n,j)       !akincldbnj      (-1.000)
     &     - u19(c,k,i,n,b,l)*t2a(d,a,n,j)       !ckinbldanj      (-1.000)
     &     + u19(c,k,i,n,a,l)*t2a(d,b,n,j)       !ckinaldbnj      (+1.000)
     &     + u19(a,k,i,n,b,l)*t2a(d,c,n,j)       !akinbldcnj      (+1.000)
     &     - u19(b,k,i,n,a,l)*t2a(d,c,n,j)       !bkinaldcnj      (-1.000)
     &     - u19(a,k,j,n,d,l)*t2a(c,b,n,i)       !akjndlcbni      (-1.000)
     &     + u19(b,k,j,n,d,l)*t2a(c,a,n,i)       !bkjndlcani      (+1.000)
     &     - u19(c,k,j,n,d,l)*t2a(b,a,n,i)       !ckjndlbani      (-1.000)
     &     + u19(a,k,j,n,c,l)*t2a(d,b,n,i)       !akjncldbni      (+1.000)
     &     - u19(b,k,j,n,c,l)*t2a(d,a,n,i)       !bkjncldani      (-1.000)
     &     - u19(a,k,j,n,b,l)*t2a(d,c,n,i)       !akjnbldcni      (-1.000)
     &     + u19(b,k,j,n,a,l)*t2a(d,c,n,i)       !bkjnaldcni      (+1.000)
     &     + u19(c,k,j,n,b,l)*t2a(d,a,n,i)       !ckjnbldani      (+1.000)
     &     - u19(c,k,j,n,a,l)*t2a(d,b,n,i)       !ckjnaldbni      (-1.000)
     &     + u19(d,k,j,n,c,l)*t2a(b,a,n,i)       !dkjnclbani      (+1.000)
     &     - u19(d,k,j,n,b,l)*t2a(c,a,n,i)       !dkjnblcani      (-1.000)
     &     + u19(d,k,j,n,a,l)*t2a(c,b,n,i)       !dkjnalcbni      (+1.000)
             enddo
             v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call
!     & sum34512678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77, 1.000)
!       call
!     & sum24513678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77,-1.000)
!       call
!     & sum23514678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77, 1.000)
!       call
!     & sum34521678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77,-1.000)
!       call
!     & sum24531678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77, 1.000)
!       call
!     & sum23541678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77,-1.000)
!       call
!     & sum14523678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77, 1.000)
!       call
!     & sum13524678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77,-1.000)
!       call
!     & sum14532678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77,-1.000)
!       call
!     & sum13542678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77, 1.000)
!       call
!     & sum12534678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77, 1.000)
!       call
!     & sum12543678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77,-1.000)
!       call
!     & sum34612578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77,-1.000)
!       call
!     & sum24613578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77, 1.000)
!       call
!     & sum23614578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77,-1.000)
!       call
!     & sum34621578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77, 1.000)
!       call
!     & sum24631578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77,-1.000)
!       call
!     & sum23641578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77, 1.000)
!       call
!     & sum14623578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77,-1.000)
!       call
!     & sum13624578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77, 1.000)
!       call
!     & sum14632578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77, 1.000)
!       call
!     & sum13642578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77,-1.000)
!       call
!     & sum12634578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77,-1.000)
!       call
!     & sum12643578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77, 1.000)
!       call
!     & sum23714568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77, 1.000)
!       call
!     & sum24713568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77,-1.000)
!       call
!     & sum34712568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77, 1.000)
!       call
!     & sum13724568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77,-1.000)
!       call
!     & sum14723568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77, 1.000)
!       call
!     & sum12734568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77, 1.000)
!       call
!     & sum12743568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77,-1.000)
!       call
!     & sum14732568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77,-1.000)
!       call
!     & sum13742568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77, 1.000)
!       call
!     & sum34721568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77,-1.000)
!       call
!     & sum24731568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77, 1.000)
!       call
!     & sum23741568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77,-1.000)
!       call
!     & sum34512687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77,-1.000)
!       call
!     & sum24513687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77, 1.000)
!       call
!     & sum23514687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77,-1.000)
!       call
!     & sum34521687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77, 1.000)
!       call
!     & sum24531687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77,-1.000)
!       call
!     & sum23541687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77, 1.000)
!       call
!     & sum14523687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77,-1.000)
!       call
!     & sum13524687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77, 1.000)
!       call
!     & sum14532687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77, 1.000)
!       call
!     & sum13542687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77,-1.000)
!       call
!     & sum12534687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77,-1.000)
!       call
!     & sum12543687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77, 1.000)
!       call
!     & sum34612587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77, 1.000)
!       call
!     & sum24613587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77,-1.000)
!       call
!     & sum23614587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77, 1.000)
!       call
!     & sum34621587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77,-1.000)
!       call
!     & sum24631587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77, 1.000)
!       call
!     & sum23641587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77,-1.000)
!       call
!     & sum14623587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77, 1.000)
!       call
!     & sum13624587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77,-1.000)
!       call
!     & sum14632587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77,-1.000)
!       call
!     & sum13642587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77, 1.000)
!       call
!     & sum12634587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77, 1.000)
!       call
!     & sum12643587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77,-1.000)
!       call
!     & sum23814567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77,-1.000)
!       call
!     & sum24813567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77, 1.000)
!       call
!     & sum34812567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77,-1.000)
!       call
!     & sum13824567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77, 1.000)
!       call
!     & sum14823567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77,-1.000)
!       call
!     & sum12834567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77,-1.000)
!       call
!     & sum12843567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77, 1.000)
!       call
!     & sum14832567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77, 1.000)
!       call
!     & sum13842567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77,-1.000)
!       call
!     & sum34821567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77, 1.000)
!       call
!     & sum24831567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77,-1.000)
!       call
!     & sum23841567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77, 1.000)
!       call
!     & sum34512786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77, 1.000)
!       call
!     & sum24513786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77,-1.000)
!       call
!     & sum23514786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77, 1.000)
!       call
!     & sum34521786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77,-1.000)
!       call
!     & sum24531786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77, 1.000)
!       call
!     & sum23541786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77,-1.000)
!       call
!     & sum14523786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77, 1.000)
!       call
!     & sum13524786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77,-1.000)
!       call
!     & sum14532786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77,-1.000)
!       call
!     & sum13542786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77, 1.000)
!       call
!     & sum12534786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77, 1.000)
!       call
!     & sum12543786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77,-1.000)
!       call
!     & sum34612785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77,-1.000)
!       call
!     & sum24613785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77, 1.000)
!       call
!     & sum23614785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77,-1.000)
!       call
!     & sum34621785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77, 1.000)
!       call
!     & sum24631785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77,-1.000)
!       call
!     & sum23641785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77, 1.000)
!       call
!     & sum14623785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77,-1.000)
!       call
!     & sum13624785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77, 1.000)
!       call
!     & sum14632785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77, 1.000)
!       call
!     & sum13642785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77,-1.000)
!       call
!     & sum12634785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77,-1.000)
!       call
!     & sum12643785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77, 1.000)
!       call
!     & sum34712586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77,-1.000)
!       call
!     & sum24713586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77, 1.000)
!       call
!     & sum23714586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77,-1.000)
!       call
!     & sum34721586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77, 1.000)
!       call
!     & sum24731586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77,-1.000)
!       call
!     & sum23741586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77, 1.000)
!       call
!     & sum14723586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77,-1.000)
!       call
!     & sum13724586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77, 1.000)
!       call
!     & sum14732586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77, 1.000)
!       call
!     & sum13742586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77,-1.000)
!       call
!     & sum12734586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77,-1.000)
!       call
!     & sum12743586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77, 1.000)
!       call
!     & sum23814576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77, 1.000)
!       call
!     & sum24813576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77,-1.000)
!       call
!     & sum34812576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77, 1.000)
!       call
!     & sum13824576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77,-1.000)
!       call
!     & sum14823576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77, 1.000)
!       call
!     & sum12834576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77, 1.000)
!       call
!     & sum12843576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77,-1.000)
!       call
!     & sum14832576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77,-1.000)
!       call
!     & sum13842576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77, 1.000)
!       call
!     & sum34821576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77,-1.000)
!       call
!     & sum24831576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77, 1.000)
!       call
!     & sum23841576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77,-1.000)
!       call
!     & sum34712685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77, 1.000)
!       call
!     & sum24713685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77,-1.000)
!       call
!     & sum23714685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77, 1.000)
!       call
!     & sum34721685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77,-1.000)
!       call
!     & sum24731685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77, 1.000)
!       call
!     & sum23741685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77,-1.000)
!       call
!     & sum14723685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77, 1.000)
!       call
!     & sum13724685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77,-1.000)
!       call
!     & sum14732685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77,-1.000)
!       call
!     & sum13742685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77, 1.000)
!       call
!     & sum12734685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77, 1.000)
!       call
!     & sum12743685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77,-1.000)
!       call
!     & sum23814675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77,-1.000)
!       call
!     & sum24813675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77, 1.000)
!       call
!     & sum34812675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77,-1.000)
!       call
!     & sum13824675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77, 1.000)
!       call
!     & sum14823675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77,-1.000)
!       call
!     & sum12834675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77,-1.000)
!       call
!     & sum12843675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77, 1.000)
!       call
!     & sum14832675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77, 1.000)
!       call
!     & sum13842675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77,-1.000)
!       call
!     & sum34821675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77, 1.000)
!       call
!     & sum24831675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77,-1.000)
!       call
!     & sum23841675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z77, 1.000)
c
!       do i=n0+1,n1-3
!       do j=i+1,n1-2
!       do k=j+1,n1-1
!       do l=k+1,n1
!       do a=n1+1,n3-3
!       do b=a+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)
!     & +z77(b,a,l,d,c,k,j,i)       ! 34512678(+1.000)
!     & -z77(c,a,l,d,b,k,j,i)       ! 24513678(-1.000)
!     & +z77(c,b,l,d,a,k,j,i)       ! 23514678(+1.000)
!     & -z77(b,a,l,c,d,k,j,i)       ! 34521678(-1.000)
!     & +z77(c,a,l,b,d,k,j,i)       ! 24531678(+1.000)
!     & -z77(c,b,l,a,d,k,j,i)       ! 23541678(-1.000)
!     & +z77(d,a,l,c,b,k,j,i)       ! 14523678(+1.000)
!     & -z77(d,b,l,c,a,k,j,i)       ! 13524678(-1.000)
!     & -z77(d,a,l,b,c,k,j,i)       ! 14532678(-1.000)
!     & +z77(d,b,l,a,c,k,j,i)       ! 13542678(+1.000)
!     & +z77(d,c,l,b,a,k,j,i)       ! 12534678(+1.000)
!     & -z77(d,c,l,a,b,k,j,i)       ! 12543678(-1.000)
!     & -z77(b,a,k,d,c,l,j,i)       ! 34612578(-1.000)
!     & +z77(c,a,k,d,b,l,j,i)       ! 24613578(+1.000)
!     & -z77(c,b,k,d,a,l,j,i)       ! 23614578(-1.000)
!     & +z77(b,a,k,c,d,l,j,i)       ! 34621578(+1.000)
!     & -z77(c,a,k,b,d,l,j,i)       ! 24631578(-1.000)
!     & +z77(c,b,k,a,d,l,j,i)       ! 23641578(+1.000)
!     & -z77(d,a,k,c,b,l,j,i)       ! 14623578(-1.000)
!     & +z77(d,b,k,c,a,l,j,i)       ! 13624578(+1.000)
!     & +z77(d,a,k,b,c,l,j,i)       ! 14632578(+1.000)
!     & -z77(d,b,k,a,c,l,j,i)       ! 13642578(-1.000)
!     & -z77(d,c,k,b,a,l,j,i)       ! 12634578(-1.000)
!     & +z77(d,c,k,a,b,l,j,i)       ! 12643578(+1.000)
!     & +z77(c,b,j,d,a,l,k,i)       ! 23714568(+1.000)
!     & -z77(c,a,j,d,b,l,k,i)       ! 24713568(-1.000)
!     & +z77(b,a,j,d,c,l,k,i)       ! 34712568(+1.000)
!     & -z77(d,b,j,c,a,l,k,i)       ! 13724568(-1.000)
!     & +z77(d,a,j,c,b,l,k,i)       ! 14723568(+1.000)
!     & +z77(d,c,j,b,a,l,k,i)       ! 12734568(+1.000)
!     & -z77(d,c,j,a,b,l,k,i)       ! 12743568(-1.000)
!     & -z77(d,a,j,b,c,l,k,i)       ! 14732568(-1.000)
!     & +z77(d,b,j,a,c,l,k,i)       ! 13742568(+1.000)
!     & -z77(b,a,j,c,d,l,k,i)       ! 34721568(-1.000)
!     & +z77(c,a,j,b,d,l,k,i)       ! 24731568(+1.000)
!     & -z77(c,b,j,a,d,l,k,i)       ! 23741568(-1.000)
!     & -z77(b,a,l,d,c,k,i,j)       ! 34512687(-1.000)
!     & +z77(c,a,l,d,b,k,i,j)       ! 24513687(+1.000)
!     & -z77(c,b,l,d,a,k,i,j)       ! 23514687(-1.000)
!     & +z77(b,a,l,c,d,k,i,j)       ! 34521687(+1.000)
!     & -z77(c,a,l,b,d,k,i,j)       ! 24531687(-1.000)
!     & +z77(c,b,l,a,d,k,i,j)       ! 23541687(+1.000)
!     & -z77(d,a,l,c,b,k,i,j)       ! 14523687(-1.000)
!     & +z77(d,b,l,c,a,k,i,j)       ! 13524687(+1.000)
!     & +z77(d,a,l,b,c,k,i,j)       ! 14532687(+1.000)
!     & -z77(d,b,l,a,c,k,i,j)       ! 13542687(-1.000)
!     & -z77(d,c,l,b,a,k,i,j)       ! 12534687(-1.000)
!     & +z77(d,c,l,a,b,k,i,j)       ! 12543687(+1.000)
!     & +z77(b,a,k,d,c,l,i,j)       ! 34612587(+1.000)
!     & -z77(c,a,k,d,b,l,i,j)       ! 24613587(-1.000)
!     & +z77(c,b,k,d,a,l,i,j)       ! 23614587(+1.000)
!     & -z77(b,a,k,c,d,l,i,j)       ! 34621587(-1.000)
!     & +z77(c,a,k,b,d,l,i,j)       ! 24631587(+1.000)
!     & -z77(c,b,k,a,d,l,i,j)       ! 23641587(-1.000)
!     & +z77(d,a,k,c,b,l,i,j)       ! 14623587(+1.000)
!     & -z77(d,b,k,c,a,l,i,j)       ! 13624587(-1.000)
!     & -z77(d,a,k,b,c,l,i,j)       ! 14632587(-1.000)
!     & +z77(d,b,k,a,c,l,i,j)       ! 13642587(+1.000)
!     & +z77(d,c,k,b,a,l,i,j)       ! 12634587(+1.000)
!     & -z77(d,c,k,a,b,l,i,j)       ! 12643587(-1.000)
!     & -z77(c,b,i,d,a,l,k,j)       ! 23814567(-1.000)
!     & +z77(c,a,i,d,b,l,k,j)       ! 24813567(+1.000)
!     & -z77(b,a,i,d,c,l,k,j)       ! 34812567(-1.000)
!     & +z77(d,b,i,c,a,l,k,j)       ! 13824567(+1.000)
!     & -z77(d,a,i,c,b,l,k,j)       ! 14823567(-1.000)
!     & -z77(d,c,i,b,a,l,k,j)       ! 12834567(-1.000)
!     & +z77(d,c,i,a,b,l,k,j)       ! 12843567(+1.000)
!     & +z77(d,a,i,b,c,l,k,j)       ! 14832567(+1.000)
!     & -z77(d,b,i,a,c,l,k,j)       ! 13842567(-1.000)
!     & +z77(b,a,i,c,d,l,k,j)       ! 34821567(+1.000)
!     & -z77(c,a,i,b,d,l,k,j)       ! 24831567(-1.000)
!     & +z77(c,b,i,a,d,l,k,j)       ! 23841567(+1.000)
!     & +z77(b,a,l,d,c,j,i,k)       ! 34512786(+1.000)
!     & -z77(c,a,l,d,b,j,i,k)       ! 24513786(-1.000)
!     & +z77(c,b,l,d,a,j,i,k)       ! 23514786(+1.000)
!     & -z77(b,a,l,c,d,j,i,k)       ! 34521786(-1.000)
!     & +z77(c,a,l,b,d,j,i,k)       ! 24531786(+1.000)
!     & -z77(c,b,l,a,d,j,i,k)       ! 23541786(-1.000)
!     & +z77(d,a,l,c,b,j,i,k)       ! 14523786(+1.000)
!     & -z77(d,b,l,c,a,j,i,k)       ! 13524786(-1.000)
!     & -z77(d,a,l,b,c,j,i,k)       ! 14532786(-1.000)
!     & +z77(d,b,l,a,c,j,i,k)       ! 13542786(+1.000)
!     & +z77(d,c,l,b,a,j,i,k)       ! 12534786(+1.000)
!     & -z77(d,c,l,a,b,j,i,k)       ! 12543786(-1.000)
!     & -z77(b,a,k,d,c,j,i,l)       ! 34612785(-1.000)
!     & +z77(c,a,k,d,b,j,i,l)       ! 24613785(+1.000)
!     & -z77(c,b,k,d,a,j,i,l)       ! 23614785(-1.000)
!     & +z77(b,a,k,c,d,j,i,l)       ! 34621785(+1.000)
!     & -z77(c,a,k,b,d,j,i,l)       ! 24631785(-1.000)
!     & +z77(c,b,k,a,d,j,i,l)       ! 23641785(+1.000)
!     & -z77(d,a,k,c,b,j,i,l)       ! 14623785(-1.000)
!     & +z77(d,b,k,c,a,j,i,l)       ! 13624785(+1.000)
!     & +z77(d,a,k,b,c,j,i,l)       ! 14632785(+1.000)
!     & -z77(d,b,k,a,c,j,i,l)       ! 13642785(-1.000)
!     & -z77(d,c,k,b,a,j,i,l)       ! 12634785(-1.000)
!     & +z77(d,c,k,a,b,j,i,l)       ! 12643785(+1.000)
!     & -z77(b,a,j,d,c,l,i,k)       ! 34712586(-1.000)
!     & +z77(c,a,j,d,b,l,i,k)       ! 24713586(+1.000)
!     & -z77(c,b,j,d,a,l,i,k)       ! 23714586(-1.000)
!     & +z77(b,a,j,c,d,l,i,k)       ! 34721586(+1.000)
!     & -z77(c,a,j,b,d,l,i,k)       ! 24731586(-1.000)
!     & +z77(c,b,j,a,d,l,i,k)       ! 23741586(+1.000)
!     & -z77(d,a,j,c,b,l,i,k)       ! 14723586(-1.000)
!     & +z77(d,b,j,c,a,l,i,k)       ! 13724586(+1.000)
!     & +z77(d,a,j,b,c,l,i,k)       ! 14732586(+1.000)
!     & -z77(d,b,j,a,c,l,i,k)       ! 13742586(-1.000)
!     & -z77(d,c,j,b,a,l,i,k)       ! 12734586(-1.000)
!     & +z77(d,c,j,a,b,l,i,k)       ! 12743586(+1.000)
!     & +z77(c,b,i,d,a,l,j,k)       ! 23814576(+1.000)
!     & -z77(c,a,i,d,b,l,j,k)       ! 24813576(-1.000)
!     & +z77(b,a,i,d,c,l,j,k)       ! 34812576(+1.000)
!     & -z77(d,b,i,c,a,l,j,k)       ! 13824576(-1.000)
!     & +z77(d,a,i,c,b,l,j,k)       ! 14823576(+1.000)
!     & +z77(d,c,i,b,a,l,j,k)       ! 12834576(+1.000)
!     & -z77(d,c,i,a,b,l,j,k)       ! 12843576(-1.000)
!     & -z77(d,a,i,b,c,l,j,k)       ! 14832576(-1.000)
!     & +z77(d,b,i,a,c,l,j,k)       ! 13842576(+1.000)
!     & -z77(b,a,i,c,d,l,j,k)       ! 34821576(-1.000)
!     & +z77(c,a,i,b,d,l,j,k)       ! 24831576(+1.000)
!     & -z77(c,b,i,a,d,l,j,k)       ! 23841576(-1.000)
!     & +z77(b,a,j,d,c,k,i,l)       ! 34712685(+1.000)
!     & -z77(c,a,j,d,b,k,i,l)       ! 24713685(-1.000)
!     & +z77(c,b,j,d,a,k,i,l)       ! 23714685(+1.000)
!     & -z77(b,a,j,c,d,k,i,l)       ! 34721685(-1.000)
!     & +z77(c,a,j,b,d,k,i,l)       ! 24731685(+1.000)
!     & -z77(c,b,j,a,d,k,i,l)       ! 23741685(-1.000)
!     & +z77(d,a,j,c,b,k,i,l)       ! 14723685(+1.000)
!     & -z77(d,b,j,c,a,k,i,l)       ! 13724685(-1.000)
!     & -z77(d,a,j,b,c,k,i,l)       ! 14732685(-1.000)
!     & +z77(d,b,j,a,c,k,i,l)       ! 13742685(+1.000)
!     & +z77(d,c,j,b,a,k,i,l)       ! 12734685(+1.000)
!     & -z77(d,c,j,a,b,k,i,l)       ! 12743685(-1.000)
!     & -z77(c,b,i,d,a,k,j,l)       ! 23814675(-1.000)
!     & +z77(c,a,i,d,b,k,j,l)       ! 24813675(+1.000)
!     & -z77(b,a,i,d,c,k,j,l)       ! 34812675(-1.000)
!     & +z77(d,b,i,c,a,k,j,l)       ! 13824675(+1.000)
!     & -z77(d,a,i,c,b,k,j,l)       ! 14823675(-1.000)
!     & -z77(d,c,i,b,a,k,j,l)       ! 12834675(-1.000)
!     & +z77(d,c,i,a,b,k,j,l)       ! 12843675(+1.000)
!     & +z77(d,a,i,b,c,k,j,l)       ! 14832675(+1.000)
!     & -z77(d,b,i,a,c,k,j,l)       ! 13842675(-1.000)
!     & +z77(b,a,i,c,d,k,j,l)       ! 34821675(+1.000)
!     & -z77(c,a,i,b,d,k,j,l)       ! 24831675(-1.000)
!     & +z77(c,b,i,a,d,k,j,l)       ! 23841675(+1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z77)
       deallocate(u19)
c
c
       allocate(d1(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder4213(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,s6,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s30(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
       i1=k1*k3*k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s30)
       deallocate(d1)
       deallocate(b2)
       deallocate(s6)
c
       call sum2134(n1,n3,n1,n3,n1,n3,n0,n1,x2,s30, 1.000)
       deallocate(s30)
c
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1432(n0,n3,n0,n3,n0,n3,n0,n3,
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
       call sum21(n0,n1,n0,n1,x3,q3,-1.000)
       deallocate(q3)
c
       allocate(d1(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3412(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n1,n3,n1,n3,n0,n1,intr,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s7(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       i1=k1*k3*k3
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s7)
       deallocate(d1)
       deallocate(b2)
c
       call sum4231(n0,n1,n1,n3,n1,n3,n0,n1,x6,s7, 1.000)
c
       allocate(d1(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       call reorder2431(n0,n1,n1,n3,n1,n3,n0,n1,
     & n1,n3,n0,n1,n1,n3,n0,n1,s7,d1)
       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
       allocate(u20(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1))
       i1=k1*k3*k1
       i2=k1*k1*k3
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u20)
       deallocate(d1)
       deallocate(d2)
c
!       allocate(f1(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
!       call reorder451236(n1,n3,n0,n1,n0,n1,n0,n1,n1,n3,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,u20,f1)
!       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
!       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
!       allocate(z78(n1+1:n3,n1+1:n3,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
!       i1=k1*k1*k1*k3*k3
!       i2=k1*k3*k3
!       i3=k1
!       call egemm(i1,i2,i3,f1,d2,z78)
!       deallocate(f1)
!       deallocate(d2)
c
       do i=n0+1,n1-3;do j=i+1,n1-2;do k=j+1,n1-1;do l=k+1,n1
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-3;do b=a+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
          if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1
             sum=sum
     &     - u20(c,k,j,m,d,i)*t2a(b,a,m,l)       !ckjmdibaml      (-1.000)
     &     + u20(b,k,j,m,d,i)*t2a(c,a,m,l)       !bkjmdicaml      (+1.000)
     &     - u20(a,k,j,m,d,i)*t2a(c,b,m,l)       !akjmdicbml      (-1.000)
     &     + u20(d,k,j,m,c,i)*t2a(b,a,m,l)       !dkjmcibaml      (+1.000)
     &     - u20(d,k,j,m,b,i)*t2a(c,a,m,l)       !dkjmbicaml      (-1.000)
     &     + u20(d,k,j,m,a,i)*t2a(c,b,m,l)       !dkjmaicbml      (+1.000)
     &     - u20(b,k,j,m,c,i)*t2a(d,a,m,l)       !bkjmcidaml      (-1.000)
     &     + u20(a,k,j,m,c,i)*t2a(d,b,m,l)       !akjmcidbml      (+1.000)
     &     + u20(c,k,j,m,b,i)*t2a(d,a,m,l)       !ckjmbidaml      (+1.000)
     &     - u20(c,k,j,m,a,i)*t2a(d,b,m,l)       !ckjmaidbml      (-1.000)
     &     - u20(a,k,j,m,b,i)*t2a(d,c,m,l)       !akjmbidcml      (-1.000)
     &     + u20(b,k,j,m,a,i)*t2a(d,c,m,l)       !bkjmaidcml      (+1.000)
     &     + u20(c,l,j,m,d,i)*t2a(b,a,m,k)       !cljmdibamk      (+1.000)
     &     - u20(b,l,j,m,d,i)*t2a(c,a,m,k)       !bljmdicamk      (-1.000)
     &     + u20(a,l,j,m,d,i)*t2a(c,b,m,k)       !aljmdicbmk      (+1.000)
     &     - u20(d,l,j,m,c,i)*t2a(b,a,m,k)       !dljmcibamk      (-1.000)
     &     + u20(d,l,j,m,b,i)*t2a(c,a,m,k)       !dljmbicamk      (+1.000)
     &     - u20(d,l,j,m,a,i)*t2a(c,b,m,k)       !dljmaicbmk      (-1.000)
     &     + u20(b,l,j,m,c,i)*t2a(d,a,m,k)       !bljmcidamk      (+1.000)
     &     - u20(a,l,j,m,c,i)*t2a(d,b,m,k)       !aljmcidbmk      (-1.000)
     &     - u20(c,l,j,m,b,i)*t2a(d,a,m,k)       !cljmbidamk      (-1.000)
     &     + u20(c,l,j,m,a,i)*t2a(d,b,m,k)       !cljmaidbmk      (+1.000)
     &     + u20(a,l,j,m,b,i)*t2a(d,c,m,k)       !aljmbidcmk      (+1.000)
     &     - u20(b,l,j,m,a,i)*t2a(d,c,m,k)       !bljmaidcmk      (-1.000)
     &     - u20(a,l,k,m,d,i)*t2a(c,b,m,j)       !alkmdicbmj      (-1.000)
     &     + u20(b,l,k,m,d,i)*t2a(c,a,m,j)       !blkmdicamj      (+1.000)
     &     - u20(c,l,k,m,d,i)*t2a(b,a,m,j)       !clkmdibamj      (-1.000)
     &     + u20(a,l,k,m,c,i)*t2a(d,b,m,j)       !alkmcidbmj      (+1.000)
     &     - u20(b,l,k,m,c,i)*t2a(d,a,m,j)       !blkmcidamj      (-1.000)
     &     - u20(a,l,k,m,b,i)*t2a(d,c,m,j)       !alkmbidcmj      (-1.000)
     &     + u20(b,l,k,m,a,i)*t2a(d,c,m,j)       !blkmaidcmj      (+1.000)
     &     + u20(c,l,k,m,b,i)*t2a(d,a,m,j)       !clkmbidamj      (+1.000)
     &     - u20(c,l,k,m,a,i)*t2a(d,b,m,j)       !clkmaidbmj      (-1.000)
     &     + u20(d,l,k,m,c,i)*t2a(b,a,m,j)       !dlkmcibamj      (+1.000)
     &     - u20(d,l,k,m,b,i)*t2a(c,a,m,j)       !dlkmbicamj      (-1.000)
     &     + u20(d,l,k,m,a,i)*t2a(c,b,m,j)       !dlkmaicbmj      (+1.000)
     &     + u20(c,k,i,m,d,j)*t2a(b,a,m,l)       !ckimdjbaml      (+1.000)
     &     - u20(b,k,i,m,d,j)*t2a(c,a,m,l)       !bkimdjcaml      (-1.000)
     &     + u20(a,k,i,m,d,j)*t2a(c,b,m,l)       !akimdjcbml      (+1.000)
     &     - u20(d,k,i,m,c,j)*t2a(b,a,m,l)       !dkimcjbaml      (-1.000)
     &     + u20(d,k,i,m,b,j)*t2a(c,a,m,l)       !dkimbjcaml      (+1.000)
     &     - u20(d,k,i,m,a,j)*t2a(c,b,m,l)       !dkimajcbml      (-1.000)
     &     + u20(b,k,i,m,c,j)*t2a(d,a,m,l)       !bkimcjdaml      (+1.000)
     &     - u20(a,k,i,m,c,j)*t2a(d,b,m,l)       !akimcjdbml      (-1.000)
     &     - u20(c,k,i,m,b,j)*t2a(d,a,m,l)       !ckimbjdaml      (-1.000)
     &     + u20(c,k,i,m,a,j)*t2a(d,b,m,l)       !ckimajdbml      (+1.000)
     &     + u20(a,k,i,m,b,j)*t2a(d,c,m,l)       !akimbjdcml      (+1.000)
     &     - u20(b,k,i,m,a,j)*t2a(d,c,m,l)       !bkimajdcml      (-1.000)
     &     - u20(c,l,i,m,d,j)*t2a(b,a,m,k)       !climdjbamk      (-1.000)
     &     + u20(b,l,i,m,d,j)*t2a(c,a,m,k)       !blimdjcamk      (+1.000)
     &     - u20(a,l,i,m,d,j)*t2a(c,b,m,k)       !alimdjcbmk      (-1.000)
     &     + u20(d,l,i,m,c,j)*t2a(b,a,m,k)       !dlimcjbamk      (+1.000)
     &     - u20(d,l,i,m,b,j)*t2a(c,a,m,k)       !dlimbjcamk      (-1.000)
     &     + u20(d,l,i,m,a,j)*t2a(c,b,m,k)       !dlimajcbmk      (+1.000)
     &     - u20(b,l,i,m,c,j)*t2a(d,a,m,k)       !blimcjdamk      (-1.000)
     &     + u20(a,l,i,m,c,j)*t2a(d,b,m,k)       !alimcjdbmk      (+1.000)
     &     + u20(c,l,i,m,b,j)*t2a(d,a,m,k)       !climbjdamk      (+1.000)
     &     - u20(c,l,i,m,a,j)*t2a(d,b,m,k)       !climajdbmk      (-1.000)
     &     - u20(a,l,i,m,b,j)*t2a(d,c,m,k)       !alimbjdcmk      (-1.000)
     &     + u20(b,l,i,m,a,j)*t2a(d,c,m,k)       !blimajdcmk      (+1.000)
     &     + u20(a,l,k,m,d,j)*t2a(c,b,m,i)       !alkmdjcbmi      (+1.000)
     &     - u20(b,l,k,m,d,j)*t2a(c,a,m,i)       !blkmdjcami      (-1.000)
     &     + u20(c,l,k,m,d,j)*t2a(b,a,m,i)       !clkmdjbami      (+1.000)
     &     - u20(a,l,k,m,c,j)*t2a(d,b,m,i)       !alkmcjdbmi      (-1.000)
     &     + u20(b,l,k,m,c,j)*t2a(d,a,m,i)       !blkmcjdami      (+1.000)
     &     + u20(a,l,k,m,b,j)*t2a(d,c,m,i)       !alkmbjdcmi      (+1.000)
     &     - u20(b,l,k,m,a,j)*t2a(d,c,m,i)       !blkmajdcmi      (-1.000)
     &     - u20(c,l,k,m,b,j)*t2a(d,a,m,i)       !clkmbjdami      (-1.000)
     &     + u20(c,l,k,m,a,j)*t2a(d,b,m,i)       !clkmajdbmi      (+1.000)
     &     - u20(d,l,k,m,c,j)*t2a(b,a,m,i)       !dlkmcjbami      (-1.000)
     &     + u20(d,l,k,m,b,j)*t2a(c,a,m,i)       !dlkmbjcami      (+1.000)
     &     - u20(d,l,k,m,a,j)*t2a(c,b,m,i)       !dlkmajcbmi      (-1.000)
     &     - u20(c,j,i,m,d,k)*t2a(b,a,m,l)       !cjimdkbaml      (-1.000)
     &     + u20(b,j,i,m,d,k)*t2a(c,a,m,l)       !bjimdkcaml      (+1.000)
     &     - u20(a,j,i,m,d,k)*t2a(c,b,m,l)       !ajimdkcbml      (-1.000)
     &     + u20(d,j,i,m,c,k)*t2a(b,a,m,l)       !djimckbaml      (+1.000)
     &     - u20(d,j,i,m,b,k)*t2a(c,a,m,l)       !djimbkcaml      (-1.000)
     &     + u20(d,j,i,m,a,k)*t2a(c,b,m,l)       !djimakcbml      (+1.000)
     &     - u20(b,j,i,m,c,k)*t2a(d,a,m,l)       !bjimckdaml      (-1.000)
     &     + u20(a,j,i,m,c,k)*t2a(d,b,m,l)       !ajimckdbml      (+1.000)
     &     + u20(c,j,i,m,b,k)*t2a(d,a,m,l)       !cjimbkdaml      (+1.000)
     &     - u20(c,j,i,m,a,k)*t2a(d,b,m,l)       !cjimakdbml      (-1.000)
     &     - u20(a,j,i,m,b,k)*t2a(d,c,m,l)       !ajimbkdcml      (-1.000)
     &     + u20(b,j,i,m,a,k)*t2a(d,c,m,l)       !bjimakdcml      (+1.000)
     &     + u20(c,j,i,m,d,l)*t2a(b,a,m,k)       !cjimdlbamk      (+1.000)
     &     - u20(b,j,i,m,d,l)*t2a(c,a,m,k)       !bjimdlcamk      (-1.000)
     &     + u20(a,j,i,m,d,l)*t2a(c,b,m,k)       !ajimdlcbmk      (+1.000)
     &     - u20(d,j,i,m,c,l)*t2a(b,a,m,k)       !djimclbamk      (-1.000)
     &     + u20(d,j,i,m,b,l)*t2a(c,a,m,k)       !djimblcamk      (+1.000)
     &     - u20(d,j,i,m,a,l)*t2a(c,b,m,k)       !djimalcbmk      (-1.000)
     &     + u20(b,j,i,m,c,l)*t2a(d,a,m,k)       !bjimcldamk      (+1.000)
     &     - u20(a,j,i,m,c,l)*t2a(d,b,m,k)       !ajimcldbmk      (-1.000)
     &     - u20(c,j,i,m,b,l)*t2a(d,a,m,k)       !cjimbldamk      (-1.000)
     &     + u20(c,j,i,m,a,l)*t2a(d,b,m,k)       !cjimaldbmk      (+1.000)
     &     + u20(a,j,i,m,b,l)*t2a(d,c,m,k)       !ajimbldcmk      (+1.000)
     &     - u20(b,j,i,m,a,l)*t2a(d,c,m,k)       !bjimaldcmk      (-1.000)
     &     + u20(c,l,i,m,d,k)*t2a(b,a,m,j)       !climdkbamj      (+1.000)
     &     - u20(b,l,i,m,d,k)*t2a(c,a,m,j)       !blimdkcamj      (-1.000)
     &     + u20(a,l,i,m,d,k)*t2a(c,b,m,j)       !alimdkcbmj      (+1.000)
     &     - u20(d,l,i,m,c,k)*t2a(b,a,m,j)       !dlimckbamj      (-1.000)
     &     + u20(d,l,i,m,b,k)*t2a(c,a,m,j)       !dlimbkcamj      (+1.000)
     &     - u20(d,l,i,m,a,k)*t2a(c,b,m,j)       !dlimakcbmj      (-1.000)
     &     + u20(b,l,i,m,c,k)*t2a(d,a,m,j)       !blimckdamj      (+1.000)
     &     - u20(a,l,i,m,c,k)*t2a(d,b,m,j)       !alimckdbmj      (-1.000)
     &     - u20(c,l,i,m,b,k)*t2a(d,a,m,j)       !climbkdamj      (-1.000)
     &     + u20(c,l,i,m,a,k)*t2a(d,b,m,j)       !climakdbmj      (+1.000)
     &     + u20(a,l,i,m,b,k)*t2a(d,c,m,j)       !alimbkdcmj      (+1.000)
     &     - u20(b,l,i,m,a,k)*t2a(d,c,m,j)       !blimakdcmj      (-1.000)
     &     - u20(a,l,j,m,d,k)*t2a(c,b,m,i)       !aljmdkcbmi      (-1.000)
     &     + u20(b,l,j,m,d,k)*t2a(c,a,m,i)       !bljmdkcami      (+1.000)
     &     - u20(c,l,j,m,d,k)*t2a(b,a,m,i)       !cljmdkbami      (-1.000)
     &     + u20(a,l,j,m,c,k)*t2a(d,b,m,i)       !aljmckdbmi      (+1.000)
     &     - u20(b,l,j,m,c,k)*t2a(d,a,m,i)       !bljmckdami      (-1.000)
     &     - u20(a,l,j,m,b,k)*t2a(d,c,m,i)       !aljmbkdcmi      (-1.000)
     &     + u20(b,l,j,m,a,k)*t2a(d,c,m,i)       !bljmakdcmi      (+1.000)
     &     + u20(c,l,j,m,b,k)*t2a(d,a,m,i)       !cljmbkdami      (+1.000)
     &     - u20(c,l,j,m,a,k)*t2a(d,b,m,i)       !cljmakdbmi      (-1.000)
     &     + u20(d,l,j,m,c,k)*t2a(b,a,m,i)       !dljmckbami      (+1.000)
     &     - u20(d,l,j,m,b,k)*t2a(c,a,m,i)       !dljmbkcami      (-1.000)
     &     + u20(d,l,j,m,a,k)*t2a(c,b,m,i)       !dljmakcbmi      (+1.000)
     &     - u20(c,k,i,m,d,l)*t2a(b,a,m,j)       !ckimdlbamj      (-1.000)
     &     + u20(b,k,i,m,d,l)*t2a(c,a,m,j)       !bkimdlcamj      (+1.000)
     &     - u20(a,k,i,m,d,l)*t2a(c,b,m,j)       !akimdlcbmj      (-1.000)
     &     + u20(d,k,i,m,c,l)*t2a(b,a,m,j)       !dkimclbamj      (+1.000)
     &     - u20(d,k,i,m,b,l)*t2a(c,a,m,j)       !dkimblcamj      (-1.000)
     &     + u20(d,k,i,m,a,l)*t2a(c,b,m,j)       !dkimalcbmj      (+1.000)
     &     - u20(b,k,i,m,c,l)*t2a(d,a,m,j)       !bkimcldamj      (-1.000)
     &     + u20(a,k,i,m,c,l)*t2a(d,b,m,j)       !akimcldbmj      (+1.000)
     &     + u20(c,k,i,m,b,l)*t2a(d,a,m,j)       !ckimbldamj      (+1.000)
     &     - u20(c,k,i,m,a,l)*t2a(d,b,m,j)       !ckimaldbmj      (-1.000)
     &     - u20(a,k,i,m,b,l)*t2a(d,c,m,j)       !akimbldcmj      (-1.000)
     &     + u20(b,k,i,m,a,l)*t2a(d,c,m,j)       !bkimaldcmj      (+1.000)
     &     + u20(a,k,j,m,d,l)*t2a(c,b,m,i)       !akjmdlcbmi      (+1.000)
     &     - u20(b,k,j,m,d,l)*t2a(c,a,m,i)       !bkjmdlcami      (-1.000)
     &     + u20(c,k,j,m,d,l)*t2a(b,a,m,i)       !ckjmdlbami      (+1.000)
     &     - u20(a,k,j,m,c,l)*t2a(d,b,m,i)       !akjmcldbmi      (-1.000)
     &     + u20(b,k,j,m,c,l)*t2a(d,a,m,i)       !bkjmcldami      (+1.000)
     &     + u20(a,k,j,m,b,l)*t2a(d,c,m,i)       !akjmbldcmi      (+1.000)
     &     - u20(b,k,j,m,a,l)*t2a(d,c,m,i)       !bkjmaldcmi      (-1.000)
     &     - u20(c,k,j,m,b,l)*t2a(d,a,m,i)       !ckjmbldami      (-1.000)
     &     + u20(c,k,j,m,a,l)*t2a(d,b,m,i)       !ckjmaldbmi      (+1.000)
     &     - u20(d,k,j,m,c,l)*t2a(b,a,m,i)       !dkjmclbami      (-1.000)
     &     + u20(d,k,j,m,b,l)*t2a(c,a,m,i)       !dkjmblcami      (+1.000)
     &     - u20(d,k,j,m,a,l)*t2a(c,b,m,i)       !dkjmalcbmi      (-1.000)
             enddo
             v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call
!     & sum34512678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78,-1.000)
!       call
!     & sum24513678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78, 1.000)
!       call
!     & sum23514678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78,-1.000)
!       call
!     & sum34521678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78, 1.000)
!       call
!     & sum24531678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78,-1.000)
!       call
!     & sum23541678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78, 1.000)
!       call
!     & sum14523678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78,-1.000)
!       call
!     & sum13524678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78, 1.000)
!       call
!     & sum14532678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78, 1.000)
!       call
!     & sum13542678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78,-1.000)
!       call
!     & sum12534678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78,-1.000)
!       call
!     & sum12543678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78, 1.000)
!       call
!     & sum34612578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78, 1.000)
!       call
!     & sum24613578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78,-1.000)
!       call
!     & sum23614578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78, 1.000)
!       call
!     & sum34621578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78,-1.000)
!       call
!     & sum24631578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78, 1.000)
!       call
!     & sum23641578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78,-1.000)
!       call
!     & sum14623578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78, 1.000)
!       call
!     & sum13624578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78,-1.000)
!       call
!     & sum14632578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78,-1.000)
!       call
!     & sum13642578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78, 1.000)
!       call
!     & sum12634578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78, 1.000)
!       call
!     & sum12643578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78,-1.000)
!       call
!     & sum23714568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78,-1.000)
!       call
!     & sum24713568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78, 1.000)
!       call
!     & sum34712568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78,-1.000)
!       call
!     & sum13724568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78, 1.000)
!       call
!     & sum14723568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78,-1.000)
!       call
!     & sum12734568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78,-1.000)
!       call
!     & sum12743568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78, 1.000)
!       call
!     & sum14732568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78, 1.000)
!       call
!     & sum13742568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78,-1.000)
!       call
!     & sum34721568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78, 1.000)
!       call
!     & sum24731568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78,-1.000)
!       call
!     & sum23741568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78, 1.000)
!       call
!     & sum34512687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78, 1.000)
!       call
!     & sum24513687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78,-1.000)
!       call
!     & sum23514687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78, 1.000)
!       call
!     & sum34521687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78,-1.000)
!       call
!     & sum24531687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78, 1.000)
!       call
!     & sum23541687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78,-1.000)
!       call
!     & sum14523687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78, 1.000)
!       call
!     & sum13524687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78,-1.000)
!       call
!     & sum14532687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78,-1.000)
!       call
!     & sum13542687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78, 1.000)
!       call
!     & sum12534687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78, 1.000)
!       call
!     & sum12543687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78,-1.000)
!       call
!     & sum34612587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78,-1.000)
!       call
!     & sum24613587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78, 1.000)
!       call
!     & sum23614587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78,-1.000)
!       call
!     & sum34621587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78, 1.000)
!       call
!     & sum24631587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78,-1.000)
!       call
!     & sum23641587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78, 1.000)
!       call
!     & sum14623587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78,-1.000)
!       call
!     & sum13624587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78, 1.000)
!       call
!     & sum14632587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78, 1.000)
!       call
!     & sum13642587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78,-1.000)
!       call
!     & sum12634587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78,-1.000)
!       call
!     & sum12643587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78, 1.000)
!       call
!     & sum23814567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78, 1.000)
!       call
!     & sum24813567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78,-1.000)
!       call
!     & sum34812567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78, 1.000)
!       call
!     & sum13824567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78,-1.000)
!       call
!     & sum14823567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78, 1.000)
!       call
!     & sum12834567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78, 1.000)
!       call
!     & sum12843567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78,-1.000)
!       call
!     & sum14832567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78,-1.000)
!       call
!     & sum13842567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78, 1.000)
!       call
!     & sum34821567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78,-1.000)
!       call
!     & sum24831567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78, 1.000)
!       call
!     & sum23841567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78,-1.000)
!       call
!     & sum34512786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78,-1.000)
!       call
!     & sum24513786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78, 1.000)
!       call
!     & sum23514786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78,-1.000)
!       call
!     & sum34521786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78, 1.000)
!       call
!     & sum24531786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78,-1.000)
!       call
!     & sum23541786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78, 1.000)
!       call
!     & sum14523786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78,-1.000)
!       call
!     & sum13524786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78, 1.000)
!       call
!     & sum14532786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78, 1.000)
!       call
!     & sum13542786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78,-1.000)
!       call
!     & sum12534786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78,-1.000)
!       call
!     & sum12543786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78, 1.000)
!       call
!     & sum34612785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78, 1.000)
!       call
!     & sum24613785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78,-1.000)
!       call
!     & sum23614785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78, 1.000)
!       call
!     & sum34621785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78,-1.000)
!       call
!     & sum24631785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78, 1.000)
!       call
!     & sum23641785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78,-1.000)
!       call
!     & sum14623785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78, 1.000)
!       call
!     & sum13624785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78,-1.000)
!       call
!     & sum14632785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78,-1.000)
!       call
!     & sum13642785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78, 1.000)
!       call
!     & sum12634785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78, 1.000)
!       call
!     & sum12643785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78,-1.000)
!       call
!     & sum34712586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78, 1.000)
!       call
!     & sum24713586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78,-1.000)
!       call
!     & sum23714586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78, 1.000)
!       call
!     & sum34721586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78,-1.000)
!       call
!     & sum24731586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78, 1.000)
!       call
!     & sum23741586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78,-1.000)
!       call
!     & sum14723586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78, 1.000)
!       call
!     & sum13724586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78,-1.000)
!       call
!     & sum14732586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78,-1.000)
!       call
!     & sum13742586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78, 1.000)
!       call
!     & sum12734586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78, 1.000)
!       call
!     & sum12743586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78,-1.000)
!       call
!     & sum23814576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78,-1.000)
!       call
!     & sum24813576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78, 1.000)
!       call
!     & sum34812576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78,-1.000)
!       call
!     & sum13824576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78, 1.000)
!       call
!     & sum14823576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78,-1.000)
!       call
!     & sum12834576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78,-1.000)
!       call
!     & sum12843576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78, 1.000)
!       call
!     & sum14832576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78, 1.000)
!       call
!     & sum13842576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78,-1.000)
!       call
!     & sum34821576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78, 1.000)
!       call
!     & sum24831576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78,-1.000)
!       call
!     & sum23841576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78, 1.000)
!       call
!     & sum34712685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78,-1.000)
!       call
!     & sum24713685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78, 1.000)
!       call
!     & sum23714685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78,-1.000)
!       call
!     & sum34721685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78, 1.000)
!       call
!     & sum24731685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78,-1.000)
!       call
!     & sum23741685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78, 1.000)
!       call
!     & sum14723685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78,-1.000)
!       call
!     & sum13724685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78, 1.000)
!       call
!     & sum14732685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78, 1.000)
!       call
!     & sum13742685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78,-1.000)
!       call
!     & sum12734685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78,-1.000)
!       call
!     & sum12743685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78, 1.000)
!       call
!     & sum23814675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78, 1.000)
!       call
!     & sum24813675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78,-1.000)
!       call
!     & sum34812675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78, 1.000)
!       call
!     & sum13824675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78,-1.000)
!       call
!     & sum14823675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78, 1.000)
!       call
!     & sum12834675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78, 1.000)
!       call
!     & sum12843675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78,-1.000)
!       call
!     & sum14832675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78,-1.000)
!       call
!     & sum13842675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78, 1.000)
!       call
!     & sum34821675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78,-1.000)
!       call
!     & sum24831675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78, 1.000)
!       call
!     & sum23841675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z78,-1.000)
c
!       do i=n0+1,n1-3
!       do j=i+1,n1-2
!       do k=j+1,n1-1
!       do l=k+1,n1
!       do a=n1+1,n3-3
!       do b=a+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)
!     & -z78(b,a,l,d,c,k,j,i)       ! 34512678(-1.000)
!     & +z78(c,a,l,d,b,k,j,i)       ! 24513678(+1.000)
!     & -z78(c,b,l,d,a,k,j,i)       ! 23514678(-1.000)
!     & +z78(b,a,l,c,d,k,j,i)       ! 34521678(+1.000)
!     & -z78(c,a,l,b,d,k,j,i)       ! 24531678(-1.000)
!     & +z78(c,b,l,a,d,k,j,i)       ! 23541678(+1.000)
!     & -z78(d,a,l,c,b,k,j,i)       ! 14523678(-1.000)
!     & +z78(d,b,l,c,a,k,j,i)       ! 13524678(+1.000)
!     & +z78(d,a,l,b,c,k,j,i)       ! 14532678(+1.000)
!     & -z78(d,b,l,a,c,k,j,i)       ! 13542678(-1.000)
!     & -z78(d,c,l,b,a,k,j,i)       ! 12534678(-1.000)
!     & +z78(d,c,l,a,b,k,j,i)       ! 12543678(+1.000)
!     & +z78(b,a,k,d,c,l,j,i)       ! 34612578(+1.000)
!     & -z78(c,a,k,d,b,l,j,i)       ! 24613578(-1.000)
!     & +z78(c,b,k,d,a,l,j,i)       ! 23614578(+1.000)
!     & -z78(b,a,k,c,d,l,j,i)       ! 34621578(-1.000)
!     & +z78(c,a,k,b,d,l,j,i)       ! 24631578(+1.000)
!     & -z78(c,b,k,a,d,l,j,i)       ! 23641578(-1.000)
!     & +z78(d,a,k,c,b,l,j,i)       ! 14623578(+1.000)
!     & -z78(d,b,k,c,a,l,j,i)       ! 13624578(-1.000)
!     & -z78(d,a,k,b,c,l,j,i)       ! 14632578(-1.000)
!     & +z78(d,b,k,a,c,l,j,i)       ! 13642578(+1.000)
!     & +z78(d,c,k,b,a,l,j,i)       ! 12634578(+1.000)
!     & -z78(d,c,k,a,b,l,j,i)       ! 12643578(-1.000)
!     & -z78(c,b,j,d,a,l,k,i)       ! 23714568(-1.000)
!     & +z78(c,a,j,d,b,l,k,i)       ! 24713568(+1.000)
!     & -z78(b,a,j,d,c,l,k,i)       ! 34712568(-1.000)
!     & +z78(d,b,j,c,a,l,k,i)       ! 13724568(+1.000)
!     & -z78(d,a,j,c,b,l,k,i)       ! 14723568(-1.000)
!     & -z78(d,c,j,b,a,l,k,i)       ! 12734568(-1.000)
!     & +z78(d,c,j,a,b,l,k,i)       ! 12743568(+1.000)
!     & +z78(d,a,j,b,c,l,k,i)       ! 14732568(+1.000)
!     & -z78(d,b,j,a,c,l,k,i)       ! 13742568(-1.000)
!     & +z78(b,a,j,c,d,l,k,i)       ! 34721568(+1.000)
!     & -z78(c,a,j,b,d,l,k,i)       ! 24731568(-1.000)
!     & +z78(c,b,j,a,d,l,k,i)       ! 23741568(+1.000)
!     & +z78(b,a,l,d,c,k,i,j)       ! 34512687(+1.000)
!     & -z78(c,a,l,d,b,k,i,j)       ! 24513687(-1.000)
!     & +z78(c,b,l,d,a,k,i,j)       ! 23514687(+1.000)
!     & -z78(b,a,l,c,d,k,i,j)       ! 34521687(-1.000)
!     & +z78(c,a,l,b,d,k,i,j)       ! 24531687(+1.000)
!     & -z78(c,b,l,a,d,k,i,j)       ! 23541687(-1.000)
!     & +z78(d,a,l,c,b,k,i,j)       ! 14523687(+1.000)
!     & -z78(d,b,l,c,a,k,i,j)       ! 13524687(-1.000)
!     & -z78(d,a,l,b,c,k,i,j)       ! 14532687(-1.000)
!     & +z78(d,b,l,a,c,k,i,j)       ! 13542687(+1.000)
!     & +z78(d,c,l,b,a,k,i,j)       ! 12534687(+1.000)
!     & -z78(d,c,l,a,b,k,i,j)       ! 12543687(-1.000)
!     & -z78(b,a,k,d,c,l,i,j)       ! 34612587(-1.000)
!     & +z78(c,a,k,d,b,l,i,j)       ! 24613587(+1.000)
!     & -z78(c,b,k,d,a,l,i,j)       ! 23614587(-1.000)
!     & +z78(b,a,k,c,d,l,i,j)       ! 34621587(+1.000)
!     & -z78(c,a,k,b,d,l,i,j)       ! 24631587(-1.000)
!     & +z78(c,b,k,a,d,l,i,j)       ! 23641587(+1.000)
!     & -z78(d,a,k,c,b,l,i,j)       ! 14623587(-1.000)
!     & +z78(d,b,k,c,a,l,i,j)       ! 13624587(+1.000)
!     & +z78(d,a,k,b,c,l,i,j)       ! 14632587(+1.000)
!     & -z78(d,b,k,a,c,l,i,j)       ! 13642587(-1.000)
!     & -z78(d,c,k,b,a,l,i,j)       ! 12634587(-1.000)
!     & +z78(d,c,k,a,b,l,i,j)       ! 12643587(+1.000)
!     & +z78(c,b,i,d,a,l,k,j)       ! 23814567(+1.000)
!     & -z78(c,a,i,d,b,l,k,j)       ! 24813567(-1.000)
!     & +z78(b,a,i,d,c,l,k,j)       ! 34812567(+1.000)
!     & -z78(d,b,i,c,a,l,k,j)       ! 13824567(-1.000)
!     & +z78(d,a,i,c,b,l,k,j)       ! 14823567(+1.000)
!     & +z78(d,c,i,b,a,l,k,j)       ! 12834567(+1.000)
!     & -z78(d,c,i,a,b,l,k,j)       ! 12843567(-1.000)
!     & -z78(d,a,i,b,c,l,k,j)       ! 14832567(-1.000)
!     & +z78(d,b,i,a,c,l,k,j)       ! 13842567(+1.000)
!     & -z78(b,a,i,c,d,l,k,j)       ! 34821567(-1.000)
!     & +z78(c,a,i,b,d,l,k,j)       ! 24831567(+1.000)
!     & -z78(c,b,i,a,d,l,k,j)       ! 23841567(-1.000)
!     & -z78(b,a,l,d,c,j,i,k)       ! 34512786(-1.000)
!     & +z78(c,a,l,d,b,j,i,k)       ! 24513786(+1.000)
!     & -z78(c,b,l,d,a,j,i,k)       ! 23514786(-1.000)
!     & +z78(b,a,l,c,d,j,i,k)       ! 34521786(+1.000)
!     & -z78(c,a,l,b,d,j,i,k)       ! 24531786(-1.000)
!     & +z78(c,b,l,a,d,j,i,k)       ! 23541786(+1.000)
!     & -z78(d,a,l,c,b,j,i,k)       ! 14523786(-1.000)
!     & +z78(d,b,l,c,a,j,i,k)       ! 13524786(+1.000)
!     & +z78(d,a,l,b,c,j,i,k)       ! 14532786(+1.000)
!     & -z78(d,b,l,a,c,j,i,k)       ! 13542786(-1.000)
!     & -z78(d,c,l,b,a,j,i,k)       ! 12534786(-1.000)
!     & +z78(d,c,l,a,b,j,i,k)       ! 12543786(+1.000)
!     & +z78(b,a,k,d,c,j,i,l)       ! 34612785(+1.000)
!     & -z78(c,a,k,d,b,j,i,l)       ! 24613785(-1.000)
!     & +z78(c,b,k,d,a,j,i,l)       ! 23614785(+1.000)
!     & -z78(b,a,k,c,d,j,i,l)       ! 34621785(-1.000)
!     & +z78(c,a,k,b,d,j,i,l)       ! 24631785(+1.000)
!     & -z78(c,b,k,a,d,j,i,l)       ! 23641785(-1.000)
!     & +z78(d,a,k,c,b,j,i,l)       ! 14623785(+1.000)
!     & -z78(d,b,k,c,a,j,i,l)       ! 13624785(-1.000)
!     & -z78(d,a,k,b,c,j,i,l)       ! 14632785(-1.000)
!     & +z78(d,b,k,a,c,j,i,l)       ! 13642785(+1.000)
!     & +z78(d,c,k,b,a,j,i,l)       ! 12634785(+1.000)
!     & -z78(d,c,k,a,b,j,i,l)       ! 12643785(-1.000)
!     & +z78(b,a,j,d,c,l,i,k)       ! 34712586(+1.000)
!     & -z78(c,a,j,d,b,l,i,k)       ! 24713586(-1.000)
!     & +z78(c,b,j,d,a,l,i,k)       ! 23714586(+1.000)
!     & -z78(b,a,j,c,d,l,i,k)       ! 34721586(-1.000)
!     & +z78(c,a,j,b,d,l,i,k)       ! 24731586(+1.000)
!     & -z78(c,b,j,a,d,l,i,k)       ! 23741586(-1.000)
!     & +z78(d,a,j,c,b,l,i,k)       ! 14723586(+1.000)
!     & -z78(d,b,j,c,a,l,i,k)       ! 13724586(-1.000)
!     & -z78(d,a,j,b,c,l,i,k)       ! 14732586(-1.000)
!     & +z78(d,b,j,a,c,l,i,k)       ! 13742586(+1.000)
!     & +z78(d,c,j,b,a,l,i,k)       ! 12734586(+1.000)
!     & -z78(d,c,j,a,b,l,i,k)       ! 12743586(-1.000)
!     & -z78(c,b,i,d,a,l,j,k)       ! 23814576(-1.000)
!     & +z78(c,a,i,d,b,l,j,k)       ! 24813576(+1.000)
!     & -z78(b,a,i,d,c,l,j,k)       ! 34812576(-1.000)
!     & +z78(d,b,i,c,a,l,j,k)       ! 13824576(+1.000)
!     & -z78(d,a,i,c,b,l,j,k)       ! 14823576(-1.000)
!     & -z78(d,c,i,b,a,l,j,k)       ! 12834576(-1.000)
!     & +z78(d,c,i,a,b,l,j,k)       ! 12843576(+1.000)
!     & +z78(d,a,i,b,c,l,j,k)       ! 14832576(+1.000)
!     & -z78(d,b,i,a,c,l,j,k)       ! 13842576(-1.000)
!     & +z78(b,a,i,c,d,l,j,k)       ! 34821576(+1.000)
!     & -z78(c,a,i,b,d,l,j,k)       ! 24831576(-1.000)
!     & +z78(c,b,i,a,d,l,j,k)       ! 23841576(+1.000)
!     & -z78(b,a,j,d,c,k,i,l)       ! 34712685(-1.000)
!     & +z78(c,a,j,d,b,k,i,l)       ! 24713685(+1.000)
!     & -z78(c,b,j,d,a,k,i,l)       ! 23714685(-1.000)
!     & +z78(b,a,j,c,d,k,i,l)       ! 34721685(+1.000)
!     & -z78(c,a,j,b,d,k,i,l)       ! 24731685(-1.000)
!     & +z78(c,b,j,a,d,k,i,l)       ! 23741685(+1.000)
!     & -z78(d,a,j,c,b,k,i,l)       ! 14723685(-1.000)
!     & +z78(d,b,j,c,a,k,i,l)       ! 13724685(+1.000)
!     & +z78(d,a,j,b,c,k,i,l)       ! 14732685(+1.000)
!     & -z78(d,b,j,a,c,k,i,l)       ! 13742685(-1.000)
!     & -z78(d,c,j,b,a,k,i,l)       ! 12734685(-1.000)
!     & +z78(d,c,j,a,b,k,i,l)       ! 12743685(+1.000)
!     & +z78(c,b,i,d,a,k,j,l)       ! 23814675(+1.000)
!     & -z78(c,a,i,d,b,k,j,l)       ! 24813675(-1.000)
!     & +z78(b,a,i,d,c,k,j,l)       ! 34812675(+1.000)
!     & -z78(d,b,i,c,a,k,j,l)       ! 13824675(-1.000)
!     & +z78(d,a,i,c,b,k,j,l)       ! 14823675(+1.000)
!     & +z78(d,c,i,b,a,k,j,l)       ! 12834675(+1.000)
!     & -z78(d,c,i,a,b,k,j,l)       ! 12843675(-1.000)
!     & -z78(d,a,i,b,c,k,j,l)       ! 14832675(-1.000)
!     & +z78(d,b,i,a,c,k,j,l)       ! 13842675(+1.000)
!     & -z78(b,a,i,c,d,k,j,l)       ! 34821675(-1.000)
!     & +z78(c,a,i,b,d,k,j,l)       ! 24831675(+1.000)
!     & -z78(c,b,i,a,d,k,j,l)       ! 23841675(-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z78)
       deallocate(u20)
c
       allocate(d1(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder4231(n0,n1,n1,n3,n1,n3,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,s7,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s32(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
       i1=k1*k3*k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s32)
       deallocate(d1)
       deallocate(b2)
c
       call sum3124(n1,n3,n1,n3,n1,n3,n0,n1,x10,s32, 1.000)
       deallocate(s32)
c
       allocate(d1(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       call reorder2431(n0,n1,n1,n3,n1,n3,n0,n1,
     & n1,n3,n0,n1,n1,n3,n0,n1,s7,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s31(n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1))
       i1=k1*k3*k1
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s31)
       deallocate(d1)
       deallocate(b2)
       deallocate(s7)
c
       call sum3124(n0,n1,n1,n3,n0,n1,n0,n1,x1,s31, 1.000)
       deallocate(s31)
c
       allocate(d1(n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3))
       call reorder2431(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n1,n3,n1,n3,intr,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s8(n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3))
       i1=k3*k3*k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s8)
       deallocate(d1)
       deallocate(b2)
c
!       allocate(d1(n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3))
!       call reorder2341(n1,n3,n1,n3,n1,n3,n1,n3,
!     & n1,n3,n1,n3,n1,n3,n1,n3,s8,d1)
!       allocate(h2(n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
!       call reorder12345678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,
!     & n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,t4a,h2)
!       allocate(z19(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3))
!       i1=k3*k3
!       i2=k1*k1*k1*k1*k3*k3
!       i3=k3*k3
!       call egemm(i1,i2,i3,d1,h2,z19)
!       deallocate(d1)
!       deallocate(h2)
c
       do i=n0+1,n1-3;do j=i+1,n1-2;do k=j+1,n1-1;do l=k+1,n1
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-3;do b=a+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
          if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n1+1,n3;do f=n1+1,n3
             sum=sum
     &     + (s8(c,f,e,d)*t4a(f,e,b,a,l,k,j,i)      !cfedfebalkji    (+0.500)
     &     - s8(b,f,e,d)*t4a(f,e,c,a,l,k,j,i)       !bfedfecalkji    (-0.500)
     &     + s8(a,f,e,d)*t4a(f,e,c,b,l,k,j,i)       !afedfecblkji    (+0.500)
     &     - s8(d,f,e,c)*t4a(f,e,b,a,l,k,j,i)       !dfecfebalkji    (-0.500)
     &     + s8(d,f,e,b)*t4a(f,e,c,a,l,k,j,i)       !dfebfecalkji    (+0.500)
     &     - s8(d,f,e,a)*t4a(f,e,c,b,l,k,j,i)       !dfeafecblkji    (-0.500)
     &     + s8(b,f,e,c)*t4a(f,e,d,a,l,k,j,i)       !bfecfedalkji    (+0.500)
     &     - s8(a,f,e,c)*t4a(f,e,d,b,l,k,j,i)       !afecfedblkji    (-0.500)
     &     - s8(c,f,e,b)*t4a(f,e,d,a,l,k,j,i)       !cfebfedalkji    (-0.500)
     &     + s8(c,f,e,a)*t4a(f,e,d,b,l,k,j,i)       !cfeafedblkji    (+0.500)
     &     + s8(a,f,e,b)*t4a(f,e,d,c,l,k,j,i)       !afebfedclkji    (+0.500)
     &     - s8(b,f,e,a)*t4a(f,e,d,c,l,k,j,i))/2.0d0!bfeafedclkji    (-0.500)             
             enddo;enddo
             v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call
!     & sum34567812(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z19, 0.500)
!       call
!     & sum24567813(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z19,-0.500)
!       call
!     & sum23567814(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z19, 0.500)
!       call
!     & sum34567821(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z19,-0.500)
!       call
!     & sum24567831(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z19, 0.500)
!       call
!     & sum23567841(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z19,-0.500)
!       call
!     & sum14567823(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z19, 0.500)
!       call
!     & sum13567824(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z19,-0.500)
!       call
!     & sum14567832(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z19,-0.500)
!       call
!     & sum13567842(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z19, 0.500)
!       call
!     & sum12567834(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z19, 0.500)
!       call
!     & sum12567843(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z19,-0.500)
c
!       do i=n0+1,n1-3
!       do j=i+1,n1-2
!       do k=j+1,n1-1
!       do l=k+1,n1
!       do a=n1+1,n3-3
!       do b=a+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)
!     & +(z19(b,a,l,k,j,i,d,c)      ! 34567812 (+0.500)
!     & -z19(c,a,l,k,j,i,d,b)       ! 24567813 (-0.500)
!     & +z19(c,b,l,k,j,i,d,a)       ! 23567814 (+0.500)
!     & -z19(b,a,l,k,j,i,c,d)       ! 34567821 (-0.500)
!     & +z19(c,a,l,k,j,i,b,d)       ! 24567831 (+0.500)
!     & -z19(c,b,l,k,j,i,a,d)       ! 23567841 (-0.500)
!     & +z19(d,a,l,k,j,i,c,b)       ! 14567823 (+0.500)
!     & -z19(d,b,l,k,j,i,c,a)       ! 13567824 (-0.500)
!     & -z19(d,a,l,k,j,i,b,c)       ! 14567832 (-0.500)
!     & +z19(d,b,l,k,j,i,a,c)       ! 13567842 (+0.500)
!     & +z19(d,c,l,k,j,i,b,a)       ! 12567834 (+0.500)
!     & -z19(d,c,l,k,j,i,a,b))/2.0d0! 12567843 (-0.500)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z19)
c
       allocate(d1(n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3))
       call reorder3241(n1,n3,n1,n3,n1,n3,n1,n3,
     & n1,n3,n1,n3,n1,n3,n1,n3,s8,d1)
       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
       allocate(u21(n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3))
       i1=k3*k3*k3
       i2=k1*k1*k3
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u21)
       deallocate(d1)
       deallocate(d2)
       deallocate(s8)
c
!       allocate(f1(n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!       call reorder456123(n1,n3,n0,n1,n0,n1,n1,n3,n1,n3,n1,n3,
!     & n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,u21,f1)
!       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
!     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
!       allocate(z79(n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!       i1=k1*k1*k3*k3*k3
!       i2=k1*k1*k3
!       i3=k3
!       call egemm(i1,i2,i3,f1,d2,z79)
!       deallocate(f1)
!       deallocate(d2)
c
       do i=n0+1,n1-3;do j=i+1,n1-2;do k=j+1,n1-1;do l=k+1,n1
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-3;do b=a+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
          if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n1+1,n3
             sum=sum
     &     - u21(a,l,k,f,d,c)*t2a(f,b,j,i)       !alkfdcfbji      (-1.000)
     &     - u21(a,j,i,f,d,c)*t2a(f,b,l,k)       !ajifdcfblk      (-1.000)
     &     + u21(a,l,k,f,d,b)*t2a(f,c,j,i)       !alkfdbfcji      (+1.000)
     &     - u21(b,l,k,f,d,a)*t2a(f,c,j,i)       !blkfdafcji      (-1.000)
     &     + u21(a,j,i,f,d,b)*t2a(f,c,l,k)       !ajifdbfclk      (+1.000)
     &     - u21(b,j,i,f,d,a)*t2a(f,c,l,k)       !bjifdafclk      (-1.000)
     &     + u21(a,l,k,f,c,d)*t2a(f,b,j,i)       !alkfcdfbji      (+1.000)
     &     + u21(a,j,i,f,c,d)*t2a(f,b,l,k)       !ajifcdfblk      (+1.000)
     &     - u21(a,l,k,f,b,d)*t2a(f,c,j,i)       !alkfbdfcji      (-1.000)
     &     + u21(b,l,k,f,a,d)*t2a(f,c,j,i)       !blkfadfcji      (+1.000)
     &     - u21(a,j,i,f,b,d)*t2a(f,c,l,k)       !ajifbdfclk      (-1.000)
     &     + u21(b,j,i,f,a,d)*t2a(f,c,l,k)       !bjifadfclk      (+1.000)
     &     - u21(a,l,k,f,c,b)*t2a(f,d,j,i)       !alkfcbfdji      (-1.000)
     &     + u21(b,l,k,f,c,a)*t2a(f,d,j,i)       !blkfcafdji      (+1.000)
     &     + u21(a,l,k,f,b,c)*t2a(f,d,j,i)       !alkfbcfdji      (+1.000)
     &     - u21(b,l,k,f,a,c)*t2a(f,d,j,i)       !blkfacfdji      (-1.000)
     &     - u21(c,l,k,f,b,a)*t2a(f,d,j,i)       !clkfbafdji      (-1.000)
     &     + u21(c,l,k,f,a,b)*t2a(f,d,j,i)       !clkfabfdji      (+1.000)
     &     - u21(a,j,i,f,c,b)*t2a(f,d,l,k)       !ajifcbfdlk      (-1.000)
     &     + u21(b,j,i,f,c,a)*t2a(f,d,l,k)       !bjifcafdlk      (+1.000)
     &     + u21(a,j,i,f,b,c)*t2a(f,d,l,k)       !ajifbcfdlk      (+1.000)
     &     - u21(b,j,i,f,a,c)*t2a(f,d,l,k)       !bjifacfdlk      (-1.000)
     &     - u21(c,j,i,f,b,a)*t2a(f,d,l,k)       !cjifbafdlk      (-1.000)
     &     + u21(c,j,i,f,a,b)*t2a(f,d,l,k)       !cjifabfdlk      (+1.000)
     &     + u21(a,l,j,f,d,c)*t2a(f,b,k,i)       !aljfdcfbki      (+1.000)
     &     + u21(a,k,i,f,d,c)*t2a(f,b,l,j)       !akifdcfblj      (+1.000)
     &     - u21(a,l,j,f,d,b)*t2a(f,c,k,i)       !aljfdbfcki      (-1.000)
     &     + u21(b,l,j,f,d,a)*t2a(f,c,k,i)       !bljfdafcki      (+1.000)
     &     - u21(a,k,i,f,d,b)*t2a(f,c,l,j)       !akifdbfclj      (-1.000)
     &     + u21(b,k,i,f,d,a)*t2a(f,c,l,j)       !bkifdafclj      (+1.000)
     &     - u21(a,l,j,f,c,d)*t2a(f,b,k,i)       !aljfcdfbki      (-1.000)
     &     - u21(a,k,i,f,c,d)*t2a(f,b,l,j)       !akifcdfblj      (-1.000)
     &     + u21(a,l,j,f,b,d)*t2a(f,c,k,i)       !aljfbdfcki      (+1.000)
     &     - u21(b,l,j,f,a,d)*t2a(f,c,k,i)       !bljfadfcki      (-1.000)
     &     + u21(a,k,i,f,b,d)*t2a(f,c,l,j)       !akifbdfclj      (+1.000)
     &     - u21(b,k,i,f,a,d)*t2a(f,c,l,j)       !bkifadfclj      (-1.000)
     &     + u21(a,l,j,f,c,b)*t2a(f,d,k,i)       !aljfcbfdki      (+1.000)
     &     - u21(b,l,j,f,c,a)*t2a(f,d,k,i)       !bljfcafdki      (-1.000)
     &     - u21(a,l,j,f,b,c)*t2a(f,d,k,i)       !aljfbcfdki      (-1.000)
     &     + u21(b,l,j,f,a,c)*t2a(f,d,k,i)       !bljfacfdki      (+1.000)
     &     + u21(c,l,j,f,b,a)*t2a(f,d,k,i)       !cljfbafdki      (+1.000)
     &     - u21(c,l,j,f,a,b)*t2a(f,d,k,i)       !cljfabfdki      (-1.000)
     &     + u21(a,k,i,f,c,b)*t2a(f,d,l,j)       !akifcbfdlj      (+1.000)
     &     - u21(b,k,i,f,c,a)*t2a(f,d,l,j)       !bkifcafdlj      (-1.000)
     &     - u21(a,k,i,f,b,c)*t2a(f,d,l,j)       !akifbcfdlj      (-1.000)
     &     + u21(b,k,i,f,a,c)*t2a(f,d,l,j)       !bkifacfdlj      (+1.000)
     &     + u21(c,k,i,f,b,a)*t2a(f,d,l,j)       !ckifbafdlj      (+1.000)
     &     - u21(c,k,i,f,a,b)*t2a(f,d,l,j)       !ckifabfdlj      (-1.000)
     &     - u21(a,k,j,f,d,c)*t2a(f,b,l,i)       !akjfdcfbli      (-1.000)
     &     - u21(a,l,i,f,d,c)*t2a(f,b,k,j)       !alifdcfbkj      (-1.000)
     &     + u21(a,k,j,f,d,b)*t2a(f,c,l,i)       !akjfdbfcli      (+1.000)
     &     - u21(b,k,j,f,d,a)*t2a(f,c,l,i)       !bkjfdafcli      (-1.000)
     &     + u21(a,l,i,f,d,b)*t2a(f,c,k,j)       !alifdbfckj      (+1.000)
     &     - u21(b,l,i,f,d,a)*t2a(f,c,k,j)       !blifdafckj      (-1.000)
     &     + u21(a,k,j,f,c,d)*t2a(f,b,l,i)       !akjfcdfbli      (+1.000)
     &     + u21(a,l,i,f,c,d)*t2a(f,b,k,j)       !alifcdfbkj      (+1.000)
     &     - u21(a,k,j,f,b,d)*t2a(f,c,l,i)       !akjfbdfcli      (-1.000)
     &     + u21(b,k,j,f,a,d)*t2a(f,c,l,i)       !bkjfadfcli      (+1.000)
     &     - u21(a,l,i,f,b,d)*t2a(f,c,k,j)       !alifbdfckj      (-1.000)
     &     + u21(b,l,i,f,a,d)*t2a(f,c,k,j)       !blifadfckj      (+1.000)
     &     - u21(a,k,j,f,c,b)*t2a(f,d,l,i)       !akjfcbfdli      (-1.000)
     &     + u21(b,k,j,f,c,a)*t2a(f,d,l,i)       !bkjfcafdli      (+1.000)
     &     + u21(a,k,j,f,b,c)*t2a(f,d,l,i)       !akjfbcfdli      (+1.000)
     &     - u21(b,k,j,f,a,c)*t2a(f,d,l,i)       !bkjfacfdli      (-1.000)
     &     - u21(c,k,j,f,b,a)*t2a(f,d,l,i)       !ckjfbafdli      (-1.000)
     &     + u21(c,k,j,f,a,b)*t2a(f,d,l,i)       !ckjfabfdli      (+1.000)
     &     - u21(a,l,i,f,c,b)*t2a(f,d,k,j)       !alifcbfdkj      (-1.000)
     &     + u21(b,l,i,f,c,a)*t2a(f,d,k,j)       !blifcafdkj      (+1.000)
     &     + u21(a,l,i,f,b,c)*t2a(f,d,k,j)       !alifbcfdkj      (+1.000)
     &     - u21(b,l,i,f,a,c)*t2a(f,d,k,j)       !blifacfdkj      (-1.000)
     &     - u21(c,l,i,f,b,a)*t2a(f,d,k,j)       !clifbafdkj      (-1.000)
     &     + u21(c,l,i,f,a,b)*t2a(f,d,k,j)       !clifabfdkj      (+1.000)             
             enddo
             v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call
!     & sum37812456(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z79,-1.000)
!       call
!     & sum35612478(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z79,-1.000)
!       call
!     & sum27813456(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z79, 1.000)
!       call
!     & sum27814356(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z79,-1.000)
!       call
!     & sum25613478(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z79, 1.000)
!       call
!     & sum25614378(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z79,-1.000)
!       call
!     & sum37821456(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z79, 1.000)
!       call
!     & sum35621478(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z79, 1.000)
!       call
!     & sum27831456(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z79,-1.000)
!       call
!     & sum27841356(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z79, 1.000)
!       call
!     & sum25631478(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z79,-1.000)
!       call
!     & sum25641378(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z79, 1.000)
!       call
!     & sum17823456(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z79,-1.000)
!       call
!     & sum17824356(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z79, 1.000)
!       call
!     & sum17832456(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z79, 1.000)
!       call
!     & sum17842356(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z79,-1.000)
!       call
!     & sum17834256(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z79,-1.000)
!       call
!     & sum17843256(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z79, 1.000)
!       call
!     & sum15623478(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z79,-1.000)
!       call
!     & sum15624378(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z79, 1.000)
!       call
!     & sum15632478(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z79, 1.000)
!       call
!     & sum15642378(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z79,-1.000)
!       call
!     & sum15634278(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z79,-1.000)
!       call
!     & sum15643278(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z79, 1.000)
!       call
!     & sum36812457(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z79, 1.000)
!       call
!     & sum35712468(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z79, 1.000)
!       call
!     & sum26813457(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z79,-1.000)
!       call
!     & sum26814357(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z79, 1.000)
!       call
!     & sum25713468(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z79,-1.000)
!       call
!     & sum25714368(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z79, 1.000)
!       call
!     & sum36821457(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z79,-1.000)
!       call
!     & sum35721468(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z79,-1.000)
!       call
!     & sum26831457(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z79, 1.000)
!       call
!     & sum26841357(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z79,-1.000)
!       call
!     & sum25731468(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z79, 1.000)
!       call
!     & sum25741368(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z79,-1.000)
!       call
!     & sum16823457(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z79, 1.000)
!       call
!     & sum16824357(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z79,-1.000)
!       call
!     & sum16832457(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z79,-1.000)
!       call
!     & sum16842357(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z79, 1.000)
!       call
!     & sum16834257(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z79, 1.000)
!       call
!     & sum16843257(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z79,-1.000)
!       call
!     & sum15723468(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z79, 1.000)
!       call
!     & sum15724368(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z79,-1.000)
!       call
!     & sum15732468(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z79,-1.000)
!       call
!     & sum15742368(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z79, 1.000)
!       call
!     & sum15734268(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z79, 1.000)
!       call
!     & sum15743268(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z79,-1.000)
!       call
!     & sum35812467(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z79,-1.000)
!       call
!     & sum36712458(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z79,-1.000)
!       call
!     & sum25813467(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z79, 1.000)
!       call
!     & sum25814367(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z79,-1.000)
!       call
!     & sum26713458(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z79, 1.000)
!       call
!     & sum26714358(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z79,-1.000)
!       call
!     & sum35821467(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z79, 1.000)
!       call
!     & sum36721458(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z79, 1.000)
!       call
!     & sum25831467(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z79,-1.000)
!       call
!     & sum25841367(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z79, 1.000)
!       call
!     & sum26731458(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z79,-1.000)
!       call
!     & sum26741358(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z79, 1.000)
!       call
!     & sum15823467(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z79,-1.000)
!       call
!     & sum15824367(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z79, 1.000)
!       call
!     & sum15832467(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z79, 1.000)
!       call
!     & sum15842367(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z79,-1.000)
!       call
!     & sum15834267(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z79,-1.000)
!       call
!     & sum15843267(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z79, 1.000)
!       call
!     & sum16723458(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z79,-1.000)
!       call
!     & sum16724358(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z79, 1.000)
!       call
!     & sum16732458(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z79, 1.000)
!       call
!     & sum16742358(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z79,-1.000)
!       call
!     & sum16734258(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z79,-1.000)
!       call
!     & sum16743258(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z79, 1.000)
c
!       do i=n0+1,n1-3
!       do j=i+1,n1-2
!       do k=j+1,n1-1
!       do l=k+1,n1
!       do a=n1+1,n3-3
!       do b=a+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)
!     & -z79(b,j,i,d,c,a,l,k)       ! 37812456 (-1.000)
!     & -z79(b,l,k,d,c,a,j,i)       ! 35612478 (-1.000)
!     & +z79(c,j,i,d,b,a,l,k)       ! 27813456 (+1.000)
!     & -z79(c,j,i,d,a,b,l,k)       ! 27814356 (-1.000)
!     & +z79(c,l,k,d,b,a,j,i)       ! 25613478 (+1.000)
!     & -z79(c,l,k,d,a,b,j,i)       ! 25614378 (-1.000)
!     & +z79(b,j,i,c,d,a,l,k)       ! 37821456 (+1.000)
!     & +z79(b,l,k,c,d,a,j,i)       ! 35621478 (+1.000)
!     & -z79(c,j,i,b,d,a,l,k)       ! 27831456 (-1.000)
!     & +z79(c,j,i,a,d,b,l,k)       ! 27841356 (+1.000)
!     & -z79(c,l,k,b,d,a,j,i)       ! 25631478 (-1.000)
!     & +z79(c,l,k,a,d,b,j,i)       ! 25641378 (+1.000)
!     & -z79(d,j,i,c,b,a,l,k)       ! 17823456 (-1.000)
!     & +z79(d,j,i,c,a,b,l,k)       ! 17824356 (+1.000)
!     & +z79(d,j,i,b,c,a,l,k)       ! 17832456 (+1.000)
!     & -z79(d,j,i,a,c,b,l,k)       ! 17842356 (-1.000)
!     & -z79(d,j,i,b,a,c,l,k)       ! 17834256 (-1.000)
!     & +z79(d,j,i,a,b,c,l,k)       ! 17843256 (+1.000)
!     & -z79(d,l,k,c,b,a,j,i)       ! 15623478 (-1.000)
!     & +z79(d,l,k,c,a,b,j,i)       ! 15624378 (+1.000)
!     & +z79(d,l,k,b,c,a,j,i)       ! 15632478 (+1.000)
!     & -z79(d,l,k,a,c,b,j,i)       ! 15642378 (-1.000)
!     & -z79(d,l,k,b,a,c,j,i)       ! 15634278 (-1.000)
!     & +z79(d,l,k,a,b,c,j,i)       ! 15643278 (+1.000)
!     & +z79(b,k,i,d,c,a,l,j)       ! 36812457 (+1.000)
!     & +z79(b,l,j,d,c,a,k,i)       ! 35712468 (+1.000)
!     & -z79(c,k,i,d,b,a,l,j)       ! 26813457 (-1.000)
!     & +z79(c,k,i,d,a,b,l,j)       ! 26814357 (+1.000)
!     & -z79(c,l,j,d,b,a,k,i)       ! 25713468 (-1.000)
!     & +z79(c,l,j,d,a,b,k,i)       ! 25714368 (+1.000)
!     & -z79(b,k,i,c,d,a,l,j)       ! 36821457 (-1.000)
!     & -z79(b,l,j,c,d,a,k,i)       ! 35721468 (-1.000)
!     & +z79(c,k,i,b,d,a,l,j)       ! 26831457 (+1.000)
!     & -z79(c,k,i,a,d,b,l,j)       ! 26841357 (-1.000)
!     & +z79(c,l,j,b,d,a,k,i)       ! 25731468 (+1.000)
!     & -z79(c,l,j,a,d,b,k,i)       ! 25741368 (-1.000)
!     & +z79(d,k,i,c,b,a,l,j)       ! 16823457 (+1.000)
!     & -z79(d,k,i,c,a,b,l,j)       ! 16824357 (-1.000)
!     & -z79(d,k,i,b,c,a,l,j)       ! 16832457 (-1.000)
!     & +z79(d,k,i,a,c,b,l,j)       ! 16842357 (+1.000)
!     & +z79(d,k,i,b,a,c,l,j)       ! 16834257 (+1.000)
!     & -z79(d,k,i,a,b,c,l,j)       ! 16843257 (-1.000)
!     & +z79(d,l,j,c,b,a,k,i)       ! 15723468 (+1.000)
!     & -z79(d,l,j,c,a,b,k,i)       ! 15724368 (-1.000)
!     & -z79(d,l,j,b,c,a,k,i)       ! 15732468 (-1.000)
!     & +z79(d,l,j,a,c,b,k,i)       ! 15742368 (+1.000)
!     & +z79(d,l,j,b,a,c,k,i)       ! 15734268 (+1.000)
!     & -z79(d,l,j,a,b,c,k,i)       ! 15743268 (-1.000)
!     & -z79(b,l,i,d,c,a,k,j)       ! 35812467 (-1.000)
!     & -z79(b,k,j,d,c,a,l,i)       ! 36712458 (-1.000)
!     & +z79(c,l,i,d,b,a,k,j)       ! 25813467 (+1.000)
!     & -z79(c,l,i,d,a,b,k,j)       ! 25814367 (-1.000)
!     & +z79(c,k,j,d,b,a,l,i)       ! 26713458 (+1.000)
!     & -z79(c,k,j,d,a,b,l,i)       ! 26714358 (-1.000)
!     & +z79(b,l,i,c,d,a,k,j)       ! 35821467 (+1.000)
!     & +z79(b,k,j,c,d,a,l,i)       ! 36721458 (+1.000)
!     & -z79(c,l,i,b,d,a,k,j)       ! 25831467 (-1.000)
!     & +z79(c,l,i,a,d,b,k,j)       ! 25841367 (+1.000)
!     & -z79(c,k,j,b,d,a,l,i)       ! 26731458 (-1.000)
!     & +z79(c,k,j,a,d,b,l,i)       ! 26741358 (+1.000)
!     & -z79(d,l,i,c,b,a,k,j)       ! 15823467 (-1.000)
!     & +z79(d,l,i,c,a,b,k,j)       ! 15824367 (+1.000)
!     & +z79(d,l,i,b,c,a,k,j)       ! 15832467 (+1.000)
!     & -z79(d,l,i,a,c,b,k,j)       ! 15842367 (-1.000)
!     & -z79(d,l,i,b,a,c,k,j)       ! 15834267 (-1.000)
!     & +z79(d,l,i,a,b,c,k,j)       ! 15843267 (+1.000)
!     & -z79(d,k,j,c,b,a,l,i)       ! 16723458 (-1.000)
!     & +z79(d,k,j,c,a,b,l,i)       ! 16724358 (+1.000)
!     & +z79(d,k,j,b,c,a,l,i)       ! 16732458 (+1.000)
!     & -z79(d,k,j,a,c,b,l,i)       ! 16742358 (-1.000)
!     & -z79(d,k,j,b,a,c,l,i)       ! 16734258 (-1.000)
!     & +z79(d,k,j,a,b,c,l,i)       ! 16743258 (+1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z79)
       deallocate(u21)
c
c
       allocate(d1(n1+1:n3,n0+1:n1,n1+1:n3,n1+1:n3))
       call reorder3241(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n1,n3,n1,n3,intr,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(q4(n1+1:n3,n1+1:n3))
       i1=k3*k3
       i3=k1*k3
       call egemm1(i1,i3,d1,b2,q4)
       deallocate(d1)
       deallocate(b2)
c
       x4=x4-q4
       deallocate(q4)
c
       allocate(d1(n0+1:n1,n2+1:n3,n0+1:n1,n0+1:n2))
       call reorder1432(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n2,n3,n0,n1,n0,n2,intm,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s9(n1+1:n3,n2+1:n3,n0+1:n1,n0+1:n2))
       i1=k2*k1*k4
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s9)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x8(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       x8=0.0d0
       call sum3241(n0,n2,n2,n3,n1,n3,n0,n1,x8,s9,-1.000)
       deallocate(s9)
c
       allocate(d1(n1+1:n3,n2+1:n3,n1+1:n3,n0+1:n2))
       call reorder3412(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n2,n3,n1,n3,n0,n2,intm,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s10(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
       i1=k2*k3*k4
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s10)
       deallocate(d1)
       deallocate(b2)
c
       call sum4231(n0,n2,n2,n3,n1,n3,n0,n1,x8,s10, 1.000)
       deallocate(s10)
c
       allocate(d1(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       call reorder4213(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n0,n1,n0,n1,intm,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(q5(n0+1:n1,n0+1:n1))
       i1=k1*k1
       i3=k2*k4
       call egemm1(i1,i3,d1,b2,q5)
       deallocate(d1)
       deallocate(b2)
c
       x3=x3+q5
       deallocate(q5)
c
       allocate(d1(n2+1:n3,n0+1:n2,n1+1:n3,n1+1:n3))
       call reorder4231(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n1,n3,n1,n3,intm,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(q6(n1+1:n3,n1+1:n3))
       i1=k3*k3
       i3=k2*k4
       call egemm1(i1,i3,d1,b2,q6)
       deallocate(d1)
       deallocate(b2)
c
       x4=x4+q6
       deallocate(q6)
c
       allocate(d1(n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       call reorder1243(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n0,n1,n0,n1,n0,n1,intr,d1)
       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
       allocate(u1(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1*k1
       i2=k1*k3*k3
       i3=k1
       call egemm(i1,i2,i3,d1,d2,u1)
       deallocate(d1)
       deallocate(d2)
c
!       allocate(f1(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
!       call reorder412356(n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,u1,f1)
!       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
!       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
!       allocate(z25(n1+1:n3,n1+1:n3,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
!       i1=k1*k1*k1*k3*k3
!       i2=k1*k3*k3
!       i3=k1
!       call egemm(i1,i2,i3,f1,d2,z25)
!       deallocate(f1)
!       deallocate(d2)
c
       do i=n0+1,n1-3;do j=i+1,n1-2;do k=j+1,n1-1;do l=k+1,n1
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-3;do b=a+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
          if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do n=n0+1,n1
             sum=sum
     &     + u1(b,a,l,n,j,i)*t2a(d,c,n,k)       !balnjidcnk      (+1.000)
     &     - u1(c,a,l,n,j,i)*t2a(d,b,n,k)       !calnjidbnk      (-1.000)
     &     - u1(d,a,k,n,j,i)*t2a(c,b,n,l)       !daknjicbnl      (-1.000)
     &     + u1(d,a,l,n,j,i)*t2a(c,b,n,k)       !dalnjicbnk      (+1.000)
     &     + u1(c,a,k,n,j,i)*t2a(d,b,n,l)       !caknjidbnl      (+1.000)
     &     - u1(b,a,k,n,j,i)*t2a(d,c,n,l)       !baknjidcnl      (-1.000)
     &     - u1(b,a,l,n,k,i)*t2a(d,c,n,j)       !balnkidcnj      (-1.000)
     &     + u1(c,a,l,n,k,i)*t2a(d,b,n,j)       !calnkidbnj      (+1.000)
     &     + u1(d,a,j,n,k,i)*t2a(c,b,n,l)       !dajnkicbnl      (+1.000)
     &     - u1(d,a,l,n,k,i)*t2a(c,b,n,j)       !dalnkicbnj      (-1.000)
     &     - u1(c,a,j,n,k,i)*t2a(d,b,n,l)       !cajnkidbnl      (-1.000)
     &     + u1(b,a,j,n,k,i)*t2a(d,c,n,l)       !bajnkidcnl      (+1.000)
     &     + u1(b,a,k,n,l,i)*t2a(d,c,n,j)       !baknlidcnj      (+1.000)
     &     - u1(c,a,k,n,l,i)*t2a(d,b,n,j)       !caknlidbnj      (-1.000)
     &     - u1(d,a,j,n,l,i)*t2a(c,b,n,k)       !dajnlicbnk      (-1.000)
     &     + u1(d,a,k,n,l,i)*t2a(c,b,n,j)       !daknlicbnj      (+1.000)
     &     + u1(c,a,j,n,l,i)*t2a(d,b,n,k)       !cajnlidbnk      (+1.000)
     &     - u1(b,a,j,n,l,i)*t2a(d,c,n,k)       !bajnlidcnk      (-1.000)
     &     + u1(b,a,l,n,k,j)*t2a(d,c,n,i)       !balnkjdcni      (+1.000)
     &     - u1(c,a,l,n,k,j)*t2a(d,b,n,i)       !calnkjdbni      (-1.000)
     &     - u1(d,a,i,n,k,j)*t2a(c,b,n,l)       !dainkjcbnl      (-1.000)
     &     + u1(d,a,l,n,k,j)*t2a(c,b,n,i)       !dalnkjcbni      (+1.000)
     &     + u1(c,a,i,n,k,j)*t2a(d,b,n,l)       !cainkjdbnl      (+1.000)
     &     - u1(b,a,i,n,k,j)*t2a(d,c,n,l)       !bainkjdcnl      (-1.000)
     &     - u1(b,a,k,n,l,j)*t2a(d,c,n,i)       !baknljdcni      (-1.000)
     &     + u1(c,a,k,n,l,j)*t2a(d,b,n,i)       !caknljdbni      (+1.000)
     &     + u1(d,a,i,n,l,j)*t2a(c,b,n,k)       !dainljcbnk      (+1.000)
     &     - u1(d,a,k,n,l,j)*t2a(c,b,n,i)       !daknljcbni      (-1.000)
     &     - u1(c,a,i,n,l,j)*t2a(d,b,n,k)       !cainljdbnk      (-1.000)
     &     + u1(b,a,i,n,l,j)*t2a(d,c,n,k)       !bainljdcnk      (+1.000)
     &     + u1(b,a,j,n,l,k)*t2a(d,c,n,i)       !bajnlkdcni      (+1.000)
     &     - u1(c,a,j,n,l,k)*t2a(d,b,n,i)       !cajnlkdbni      (-1.000)
     &     - u1(d,a,i,n,l,k)*t2a(c,b,n,j)       !dainlkcbnj      (-1.000)
     &     + u1(d,a,j,n,l,k)*t2a(c,b,n,i)       !dajnlkcbni      (+1.000)
     &     + u1(c,a,i,n,l,k)*t2a(d,b,n,j)       !cainlkdbnj      (+1.000)
     &     - u1(b,a,i,n,l,k)*t2a(d,c,n,j)       !bainlkdcnj      (-1.000)
             enddo
             v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call
!     & sum12634578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z25, 1.000)
!       call
!     & sum13624578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z25,-1.000)
!       call
!     & sum23514678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z25,-1.000)
!       call
!     & sum23614578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z25, 1.000)
!       call
!     & sum13524678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z25, 1.000)
!       call
!     & sum12534678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z25,-1.000)
!       call
!     & sum12734568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z25,-1.000)
!       call
!     & sum13724568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z25, 1.000)
!       call
!     & sum23514768(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z25, 1.000)
!       call
!     & sum23714568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z25,-1.000)
!       call
!     & sum13524768(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z25,-1.000)
!       call
!     & sum12534768(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z25, 1.000)
!       call
!     & sum12734658(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z25, 1.000)
!       call
!     & sum13724658(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z25,-1.000)
!       call
!     & sum23614758(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z25,-1.000)
!       call
!     & sum23714658(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z25, 1.000)
!       call
!     & sum13624758(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z25, 1.000)
!       call
!     & sum12634758(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z25,-1.000)
!       call
!     & sum12834567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z25, 1.000)
!       call
!     & sum13824567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z25,-1.000)
!       call
!     & sum23514867(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z25,-1.000)
!       call
!     & sum23814567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z25, 1.000)
!       call
!     & sum13524867(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z25, 1.000)
!       call
!     & sum12534867(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z25,-1.000)
!       call
!     & sum12834657(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z25,-1.000)
!       call
!     & sum13824657(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z25, 1.000)
!       call
!     & sum23614857(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z25, 1.000)
!       call
!     & sum23814657(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z25,-1.000)
!       call
!     & sum13624857(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z25,-1.000)
!       call
!     & sum12634857(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z25, 1.000)
!       call
!     & sum12834756(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z25, 1.000)
!       call
!     & sum13824756(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z25,-1.000)
!       call
!     & sum23714856(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z25,-1.000)
!       call
!     & sum23814756(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z25, 1.000)
!       call
!     & sum13724856(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z25, 1.000)
!       call
!     & sum12734856(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z25,-1.000)
!c
!       do i=n0+1,n1-3
!       do j=i+1,n1-2
!       do k=j+1,n1-1
!       do l=k+1,n1
!       do a=n1+1,n3-3
!       do b=a+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)
!     & +z25(d,c,k,b,a,l,j,i)       ! 12634578 (+1.000)
!     & -z25(d,b,k,c,a,l,j,i)       ! 13624578 (-1.000)
!     & -z25(c,b,l,d,a,k,j,i)       ! 23514678 (-1.000)
!     & +z25(c,b,k,d,a,l,j,i)       ! 23614578 (+1.000)
!     & +z25(d,b,l,c,a,k,j,i)       ! 13524678 (+1.000)
!     & -z25(d,c,l,b,a,k,j,i)       ! 12534678 (-1.000)
!     & -z25(d,c,j,b,a,l,k,i)       ! 12734568 (-1.000)
!     & +z25(d,b,j,c,a,l,k,i)       ! 13724568 (+1.000)
!     & +z25(c,b,l,d,a,j,k,i)       ! 23514768 (+1.000)
!     & -z25(c,b,j,d,a,l,k,i)       ! 23714568 (-1.000)
!     & -z25(d,b,l,c,a,j,k,i)       ! 13524768 (-1.000)
!     & +z25(d,c,l,b,a,j,k,i)       ! 12534768 (+1.000)
!     & +z25(d,c,j,b,a,k,l,i)       ! 12734658 (+1.000)
!     & -z25(d,b,j,c,a,k,l,i)       ! 13724658 (-1.000)
!     & -z25(c,b,k,d,a,j,l,i)       ! 23614758 (-1.000)
!     & +z25(c,b,j,d,a,k,l,i)       ! 23714658 (+1.000)
!     & +z25(d,b,k,c,a,j,l,i)       ! 13624758 (+1.000)
!     & -z25(d,c,k,b,a,j,l,i)       ! 12634758 (-1.000)
!     & +z25(d,c,i,b,a,l,k,j)       ! 12834567 (+1.000)
!     & -z25(d,b,i,c,a,l,k,j)       ! 13824567 (-1.000)
!     & -z25(c,b,l,d,a,i,k,j)       ! 23514867 (-1.000)
!     & +z25(c,b,i,d,a,l,k,j)       ! 23814567 (+1.000)
!     & +z25(d,b,l,c,a,i,k,j)       ! 13524867 (+1.000)
!     & -z25(d,c,l,b,a,i,k,j)       ! 12534867 (-1.000)
!     & -z25(d,c,i,b,a,k,l,j)       ! 12834657 (-1.000)
!     & +z25(d,b,i,c,a,k,l,j)       ! 13824657 (+1.000)
!     & +z25(c,b,k,d,a,i,l,j)       ! 23614857 (+1.000)
!     & -z25(c,b,i,d,a,k,l,j)       ! 23814657 (-1.000)
!     & -z25(d,b,k,c,a,i,l,j)       ! 13624857 (-1.000)
!     & +z25(d,c,k,b,a,i,l,j)       ! 12634857 (+1.000)
!     & +z25(d,c,i,b,a,j,l,k)       ! 12834756 (+1.000)
!     & -z25(d,b,i,c,a,j,l,k)       ! 13824756 (-1.000)
!     & -z25(c,b,j,d,a,i,l,k)       ! 23714856 (-1.000)
!     & +z25(c,b,i,d,a,j,l,k)       ! 23814756 (+1.000)
!     & +z25(d,b,j,c,a,i,l,k)       ! 13724856 (+1.000)
!     & -z25(d,c,j,b,a,i,l,k)       ! 12734856 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z25)
       deallocate(u1)
c
       allocate(d1(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       call reorder4213(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n1,n3,n0,n1,intr,d1)
       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
       allocate(u2(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1))
       i1=k1*k3*k1
       i2=k1*k1*k3
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u2)
       deallocate(d1)
       deallocate(d2)
c
!       allocate(f1(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
!       call reorder451236(n1,n3,n0,n1,n0,n1,n0,n1,n1,n3,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,u2,f1)
!       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
!       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
!       allocate(z26(n1+1:n3,n1+1:n3,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
!       i1=k1*k1*k1*k3*k3
!       i2=k1*k3*k3
!       i3=k1
!       call egemm(i1,i2,i3,f1,d2,z26)
!       deallocate(f1)
!       deallocate(d2)
c
       do i=n0+1,n1-3;do j=i+1,n1-2;do k=j+1,n1-1;do l=k+1,n1
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-3;do b=a+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
          if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1
             sum=sum
     &     - u2(c,k,j,m,d,i)*t2a(b,a,m,l)       !ckjmdibaml      (-1.000)
     &     + u2(b,k,j,m,d,i)*t2a(c,a,m,l)       !bkjmdicaml      (+1.000)
     &     - u2(a,k,j,m,d,i)*t2a(c,b,m,l)       !akjmdicbml      (-1.000)
     &     + u2(d,k,j,m,c,i)*t2a(b,a,m,l)       !dkjmcibaml      (+1.000)
     &     - u2(d,k,j,m,b,i)*t2a(c,a,m,l)       !dkjmbicaml      (-1.000)
     &     + u2(d,k,j,m,a,i)*t2a(c,b,m,l)       !dkjmaicbml      (+1.000)
     &     - u2(b,k,j,m,c,i)*t2a(d,a,m,l)       !bkjmcidaml      (-1.000)
     &     + u2(a,k,j,m,c,i)*t2a(d,b,m,l)       !akjmcidbml      (+1.000)
     &     + u2(c,k,j,m,b,i)*t2a(d,a,m,l)       !ckjmbidaml      (+1.000)
     &     - u2(c,k,j,m,a,i)*t2a(d,b,m,l)       !ckjmaidbml      (-1.000)
     &     - u2(a,k,j,m,b,i)*t2a(d,c,m,l)       !akjmbidcml      (-1.000)
     &     + u2(b,k,j,m,a,i)*t2a(d,c,m,l)       !bkjmaidcml      (+1.000)
     &     + u2(c,l,j,m,d,i)*t2a(b,a,m,k)       !cljmdibamk      (+1.000)
     &     - u2(b,l,j,m,d,i)*t2a(c,a,m,k)       !bljmdicamk      (-1.000)
     &     + u2(a,l,j,m,d,i)*t2a(c,b,m,k)       !aljmdicbmk      (+1.000)
     &     - u2(d,l,j,m,c,i)*t2a(b,a,m,k)       !dljmcibamk      (-1.000)
     &     + u2(d,l,j,m,b,i)*t2a(c,a,m,k)       !dljmbicamk      (+1.000)
     &     - u2(d,l,j,m,a,i)*t2a(c,b,m,k)       !dljmaicbmk      (-1.000)
     &     + u2(b,l,j,m,c,i)*t2a(d,a,m,k)       !bljmcidamk      (+1.000)
     &     - u2(a,l,j,m,c,i)*t2a(d,b,m,k)       !aljmcidbmk      (-1.000)
     &     - u2(c,l,j,m,b,i)*t2a(d,a,m,k)       !cljmbidamk      (-1.000)
     &     + u2(c,l,j,m,a,i)*t2a(d,b,m,k)       !cljmaidbmk      (+1.000)
     &     + u2(a,l,j,m,b,i)*t2a(d,c,m,k)       !aljmbidcmk      (+1.000)
     &     - u2(b,l,j,m,a,i)*t2a(d,c,m,k)       !bljmaidcmk      (-1.000)
     &     - u2(a,l,k,m,d,i)*t2a(c,b,m,j)       !alkmdicbmj      (-1.000)
     &     + u2(b,l,k,m,d,i)*t2a(c,a,m,j)       !blkmdicamj      (+1.000)
     &     - u2(c,l,k,m,d,i)*t2a(b,a,m,j)       !clkmdibamj      (-1.000)
     &     + u2(a,l,k,m,c,i)*t2a(d,b,m,j)       !alkmcidbmj      (+1.000)
     &     - u2(b,l,k,m,c,i)*t2a(d,a,m,j)       !blkmcidamj      (-1.000)
     &     - u2(a,l,k,m,b,i)*t2a(d,c,m,j)       !alkmbidcmj      (-1.000)
     &     + u2(b,l,k,m,a,i)*t2a(d,c,m,j)       !blkmaidcmj      (+1.000)
     &     + u2(c,l,k,m,b,i)*t2a(d,a,m,j)       !clkmbidamj      (+1.000)
     &     - u2(c,l,k,m,a,i)*t2a(d,b,m,j)       !clkmaidbmj      (-1.000)
     &     + u2(d,l,k,m,c,i)*t2a(b,a,m,j)       !dlkmcibamj      (+1.000)
     &     - u2(d,l,k,m,b,i)*t2a(c,a,m,j)       !dlkmbicamj      (-1.000)
     &     + u2(d,l,k,m,a,i)*t2a(c,b,m,j)       !dlkmaicbmj      (+1.000)
     &     + u2(c,k,i,m,d,j)*t2a(b,a,m,l)       !ckimdjbaml      (+1.000)
     &     - u2(b,k,i,m,d,j)*t2a(c,a,m,l)       !bkimdjcaml      (-1.000)
     &     + u2(a,k,i,m,d,j)*t2a(c,b,m,l)       !akimdjcbml      (+1.000)
     &     - u2(d,k,i,m,c,j)*t2a(b,a,m,l)       !dkimcjbaml      (-1.000)
     &     + u2(d,k,i,m,b,j)*t2a(c,a,m,l)       !dkimbjcaml      (+1.000)
     &     - u2(d,k,i,m,a,j)*t2a(c,b,m,l)       !dkimajcbml      (-1.000)
     &     + u2(b,k,i,m,c,j)*t2a(d,a,m,l)       !bkimcjdaml      (+1.000)
     &     - u2(a,k,i,m,c,j)*t2a(d,b,m,l)       !akimcjdbml      (-1.000)
     &     - u2(c,k,i,m,b,j)*t2a(d,a,m,l)       !ckimbjdaml      (-1.000)
     &     + u2(c,k,i,m,a,j)*t2a(d,b,m,l)       !ckimajdbml      (+1.000)
     &     + u2(a,k,i,m,b,j)*t2a(d,c,m,l)       !akimbjdcml      (+1.000)
     &     - u2(b,k,i,m,a,j)*t2a(d,c,m,l)       !bkimajdcml      (-1.000)
     &     - u2(c,l,i,m,d,j)*t2a(b,a,m,k)       !climdjbamk      (-1.000)
     &     + u2(b,l,i,m,d,j)*t2a(c,a,m,k)       !blimdjcamk      (+1.000)
     &     - u2(a,l,i,m,d,j)*t2a(c,b,m,k)       !alimdjcbmk      (-1.000)
     &     + u2(d,l,i,m,c,j)*t2a(b,a,m,k)       !dlimcjbamk      (+1.000)
     &     - u2(d,l,i,m,b,j)*t2a(c,a,m,k)       !dlimbjcamk      (-1.000)
     &     + u2(d,l,i,m,a,j)*t2a(c,b,m,k)       !dlimajcbmk      (+1.000)
     &     - u2(b,l,i,m,c,j)*t2a(d,a,m,k)       !blimcjdamk      (-1.000)
     &     + u2(a,l,i,m,c,j)*t2a(d,b,m,k)       !alimcjdbmk      (+1.000)
     &     + u2(c,l,i,m,b,j)*t2a(d,a,m,k)       !climbjdamk      (+1.000)
     &     - u2(c,l,i,m,a,j)*t2a(d,b,m,k)       !climajdbmk      (-1.000)
     &     - u2(a,l,i,m,b,j)*t2a(d,c,m,k)       !alimbjdcmk      (-1.000)
     &     + u2(b,l,i,m,a,j)*t2a(d,c,m,k)       !blimajdcmk      (+1.000)
     &     + u2(a,l,k,m,d,j)*t2a(c,b,m,i)       !alkmdjcbmi      (+1.000)
     &     - u2(b,l,k,m,d,j)*t2a(c,a,m,i)       !blkmdjcami      (-1.000)
     &     + u2(c,l,k,m,d,j)*t2a(b,a,m,i)       !clkmdjbami      (+1.000)
     &     - u2(a,l,k,m,c,j)*t2a(d,b,m,i)       !alkmcjdbmi      (-1.000)
     &     + u2(b,l,k,m,c,j)*t2a(d,a,m,i)       !blkmcjdami      (+1.000)
     &     + u2(a,l,k,m,b,j)*t2a(d,c,m,i)       !alkmbjdcmi      (+1.000)
     &     - u2(b,l,k,m,a,j)*t2a(d,c,m,i)       !blkmajdcmi      (-1.000)
     &     - u2(c,l,k,m,b,j)*t2a(d,a,m,i)       !clkmbjdami      (-1.000)
     &     + u2(c,l,k,m,a,j)*t2a(d,b,m,i)       !clkmajdbmi      (+1.000)
     &     - u2(d,l,k,m,c,j)*t2a(b,a,m,i)       !dlkmcjbami      (-1.000)
     &     + u2(d,l,k,m,b,j)*t2a(c,a,m,i)       !dlkmbjcami      (+1.000)
     &     - u2(d,l,k,m,a,j)*t2a(c,b,m,i)       !dlkmajcbmi      (-1.000)
     &     - u2(c,j,i,m,d,k)*t2a(b,a,m,l)       !cjimdkbaml      (-1.000)
     &     + u2(b,j,i,m,d,k)*t2a(c,a,m,l)       !bjimdkcaml      (+1.000)
     &     - u2(a,j,i,m,d,k)*t2a(c,b,m,l)       !ajimdkcbml      (-1.000)
     &     + u2(d,j,i,m,c,k)*t2a(b,a,m,l)       !djimckbaml      (+1.000)
     &     - u2(d,j,i,m,b,k)*t2a(c,a,m,l)       !djimbkcaml      (-1.000)
     &     + u2(d,j,i,m,a,k)*t2a(c,b,m,l)       !djimakcbml      (+1.000)
     &     - u2(b,j,i,m,c,k)*t2a(d,a,m,l)       !bjimckdaml      (-1.000)
     &     + u2(a,j,i,m,c,k)*t2a(d,b,m,l)       !ajimckdbml      (+1.000)
     &     + u2(c,j,i,m,b,k)*t2a(d,a,m,l)       !cjimbkdaml      (+1.000)
     &     - u2(c,j,i,m,a,k)*t2a(d,b,m,l)       !cjimakdbml      (-1.000)
     &     - u2(a,j,i,m,b,k)*t2a(d,c,m,l)       !ajimbkdcml      (-1.000)
     &     + u2(b,j,i,m,a,k)*t2a(d,c,m,l)       !bjimakdcml      (+1.000)
     &     + u2(c,j,i,m,d,l)*t2a(b,a,m,k)       !cjimdlbamk      (+1.000)
     &     - u2(b,j,i,m,d,l)*t2a(c,a,m,k)       !bjimdlcamk      (-1.000)
     &     + u2(a,j,i,m,d,l)*t2a(c,b,m,k)       !ajimdlcbmk      (+1.000)
     &     - u2(d,j,i,m,c,l)*t2a(b,a,m,k)       !djimclbamk      (-1.000)
     &     + u2(d,j,i,m,b,l)*t2a(c,a,m,k)       !djimblcamk      (+1.000)
     &     - u2(d,j,i,m,a,l)*t2a(c,b,m,k)       !djimalcbmk      (-1.000)
     &     + u2(b,j,i,m,c,l)*t2a(d,a,m,k)       !bjimcldamk      (+1.000)
     &     - u2(a,j,i,m,c,l)*t2a(d,b,m,k)       !ajimcldbmk      (-1.000)
     &     - u2(c,j,i,m,b,l)*t2a(d,a,m,k)       !cjimbldamk      (-1.000)
     &     + u2(c,j,i,m,a,l)*t2a(d,b,m,k)       !cjimaldbmk      (+1.000)
     &     + u2(a,j,i,m,b,l)*t2a(d,c,m,k)       !ajimbldcmk      (+1.000)
     &     - u2(b,j,i,m,a,l)*t2a(d,c,m,k)       !bjimaldcmk      (-1.000)
     &     + u2(c,l,i,m,d,k)*t2a(b,a,m,j)       !climdkbamj      (+1.000)
     &     - u2(b,l,i,m,d,k)*t2a(c,a,m,j)       !blimdkcamj      (-1.000)
     &     + u2(a,l,i,m,d,k)*t2a(c,b,m,j)       !alimdkcbmj      (+1.000)
     &     - u2(d,l,i,m,c,k)*t2a(b,a,m,j)       !dlimckbamj      (-1.000)
     &     + u2(d,l,i,m,b,k)*t2a(c,a,m,j)       !dlimbkcamj      (+1.000)
     &     - u2(d,l,i,m,a,k)*t2a(c,b,m,j)       !dlimakcbmj      (-1.000)
     &     + u2(b,l,i,m,c,k)*t2a(d,a,m,j)       !blimckdamj      (+1.000)
     &     - u2(a,l,i,m,c,k)*t2a(d,b,m,j)       !alimckdbmj      (-1.000)
     &     - u2(c,l,i,m,b,k)*t2a(d,a,m,j)       !climbkdamj      (-1.000)
     &     + u2(c,l,i,m,a,k)*t2a(d,b,m,j)       !climakdbmj      (+1.000)
     &     + u2(a,l,i,m,b,k)*t2a(d,c,m,j)       !alimbkdcmj      (+1.000)
     &     - u2(b,l,i,m,a,k)*t2a(d,c,m,j)       !blimakdcmj      (-1.000)
     &     - u2(a,l,j,m,d,k)*t2a(c,b,m,i)       !aljmdkcbmi      (-1.000)
     &     + u2(b,l,j,m,d,k)*t2a(c,a,m,i)       !bljmdkcami      (+1.000)
     &     - u2(c,l,j,m,d,k)*t2a(b,a,m,i)       !cljmdkbami      (-1.000)
     &     + u2(a,l,j,m,c,k)*t2a(d,b,m,i)       !aljmckdbmi      (+1.000)
     &     - u2(b,l,j,m,c,k)*t2a(d,a,m,i)       !bljmckdami      (-1.000)
     &     - u2(a,l,j,m,b,k)*t2a(d,c,m,i)       !aljmbkdcmi      (-1.000)
     &     + u2(b,l,j,m,a,k)*t2a(d,c,m,i)       !bljmakdcmi      (+1.000)
     &     + u2(c,l,j,m,b,k)*t2a(d,a,m,i)       !cljmbkdami      (+1.000)
     &     - u2(c,l,j,m,a,k)*t2a(d,b,m,i)       !cljmakdbmi      (-1.000)
     &     + u2(d,l,j,m,c,k)*t2a(b,a,m,i)       !dljmckbami      (+1.000)
     &     - u2(d,l,j,m,b,k)*t2a(c,a,m,i)       !dljmbkcami      (-1.000)
     &     + u2(d,l,j,m,a,k)*t2a(c,b,m,i)       !dljmakcbmi      (+1.000)
     &     - u2(c,k,i,m,d,l)*t2a(b,a,m,j)       !ckimdlbamj      (-1.000)
     &     + u2(b,k,i,m,d,l)*t2a(c,a,m,j)       !bkimdlcamj      (+1.000)
     &     - u2(a,k,i,m,d,l)*t2a(c,b,m,j)       !akimdlcbmj      (-1.000)
     &     + u2(d,k,i,m,c,l)*t2a(b,a,m,j)       !dkimclbamj      (+1.000)
     &     - u2(d,k,i,m,b,l)*t2a(c,a,m,j)       !dkimblcamj      (-1.000)
     &     + u2(d,k,i,m,a,l)*t2a(c,b,m,j)       !dkimalcbmj      (+1.000)
     &     - u2(b,k,i,m,c,l)*t2a(d,a,m,j)       !bkimcldamj      (-1.000)
     &     + u2(a,k,i,m,c,l)*t2a(d,b,m,j)       !akimcldbmj      (+1.000)
     &     + u2(c,k,i,m,b,l)*t2a(d,a,m,j)       !ckimbldamj      (+1.000)
     &     - u2(c,k,i,m,a,l)*t2a(d,b,m,j)       !ckimaldbmj      (-1.000)
     &     - u2(a,k,i,m,b,l)*t2a(d,c,m,j)       !akimbldcmj      (-1.000)
     &     + u2(b,k,i,m,a,l)*t2a(d,c,m,j)       !bkimaldcmj      (+1.000)
     &     + u2(a,k,j,m,d,l)*t2a(c,b,m,i)       !akjmdlcbmi      (+1.000)
     &     - u2(b,k,j,m,d,l)*t2a(c,a,m,i)       !bkjmdlcami      (-1.000)
     &     + u2(c,k,j,m,d,l)*t2a(b,a,m,i)       !ckjmdlbami      (+1.000)
     &     - u2(a,k,j,m,c,l)*t2a(d,b,m,i)       !akjmcldbmi      (-1.000)
     &     + u2(b,k,j,m,c,l)*t2a(d,a,m,i)       !bkjmcldami      (+1.000)
     &     + u2(a,k,j,m,b,l)*t2a(d,c,m,i)       !akjmbldcmi      (+1.000)
     &     - u2(b,k,j,m,a,l)*t2a(d,c,m,i)       !bkjmaldcmi      (-1.000)
     &     - u2(c,k,j,m,b,l)*t2a(d,a,m,i)       !ckjmbldami      (-1.000)
     &     + u2(c,k,j,m,a,l)*t2a(d,b,m,i)       !ckjmaldbmi      (+1.000)
     &     - u2(d,k,j,m,c,l)*t2a(b,a,m,i)       !dkjmclbami      (-1.000)
     &     + u2(d,k,j,m,b,l)*t2a(c,a,m,i)       !dkjmblcami      (+1.000)
     &     - u2(d,k,j,m,a,l)*t2a(c,b,m,i)       !dkjmalcbmi      (-1.000)
             enddo
             v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call
!     & sum34512678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26,-1.000)
!       call
!     & sum24513678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26, 1.000)
!       call
!     & sum23514678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26,-1.000)
!       call
!     & sum34521678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26, 1.000)
!       call
!     & sum24531678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26,-1.000)
!       call
!     & sum23541678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26, 1.000)
!       call
!     & sum14523678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26,-1.000)
!       call
!     & sum13524678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26, 1.000)
!       call
!     & sum14532678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26, 1.000)
!       call
!     & sum13542678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26,-1.000)
!       call
!     & sum12534678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26,-1.000)
!       call
!     & sum12543678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26, 1.000)
!       call
!     & sum34612578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26, 1.000)
!       call
!     & sum24613578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26,-1.000)
!       call
!     & sum23614578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26, 1.000)
!       call
!     & sum34621578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26,-1.000)
!       call
!     & sum24631578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26, 1.000)
!       call
!     & sum23641578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26,-1.000)
!       call
!     & sum14623578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26, 1.000)
!       call
!     & sum13624578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26,-1.000)
!       call
!     & sum14632578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26,-1.000)
!       call
!     & sum13642578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26, 1.000)
!       call
!     & sum12634578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26, 1.000)
!       call
!     & sum12643578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26,-1.000)
!       call
!     & sum23714568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26,-1.000)
!       call
!     & sum24713568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26, 1.000)
!       call
!     & sum34712568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26,-1.000)
!       call
!     & sum13724568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26, 1.000)
!       call
!     & sum14723568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26,-1.000)
!       call
!     & sum12734568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26,-1.000)
!       call
!     & sum12743568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26, 1.000)
!       call
!     & sum14732568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26, 1.000)
!       call
!     & sum13742568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26,-1.000)
!       call
!     & sum34721568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26, 1.000)
!       call
!     & sum24731568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26,-1.000)
!       call
!     & sum23741568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26, 1.000)
!       call
!     & sum34512687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26, 1.000)
!       call
!     & sum24513687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26,-1.000)
!       call
!     & sum23514687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26, 1.000)
!       call
!     & sum34521687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26,-1.000)
!       call
!     & sum24531687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26, 1.000)
!       call
!     & sum23541687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26,-1.000)
!       call
!     & sum14523687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26, 1.000)
!       call
!     & sum13524687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26,-1.000)
!       call
!     & sum14532687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26,-1.000)
!       call
!     & sum13542687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26, 1.000)
!       call
!     & sum12534687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26, 1.000)
!       call
!     & sum12543687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26,-1.000)
!       call
!     & sum34612587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26,-1.000)
!       call
!     & sum24613587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26, 1.000)
!       call
!     & sum23614587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26,-1.000)
!       call
!     & sum34621587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26, 1.000)
!       call
!     & sum24631587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26,-1.000)
!       call
!     & sum23641587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26, 1.000)
!       call
!     & sum14623587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26,-1.000)
!       call
!     & sum13624587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26, 1.000)
!       call
!     & sum14632587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26, 1.000)
!       call
!     & sum13642587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26,-1.000)
!       call
!     & sum12634587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26,-1.000)
!       call
!     & sum12643587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26, 1.000)
!       call
!     & sum23814567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26, 1.000)
!       call
!     & sum24813567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26,-1.000)
!       call
!     & sum34812567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26, 1.000)
!       call
!     & sum13824567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26,-1.000)
!       call
!     & sum14823567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26, 1.000)
!       call
!     & sum12834567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26, 1.000)
!       call
!     & sum12843567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26,-1.000)
!       call
!     & sum14832567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26,-1.000)
!       call
!     & sum13842567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26, 1.000)
!       call
!     & sum34821567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26,-1.000)
!       call
!     & sum24831567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26, 1.000)
!       call
!     & sum23841567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26,-1.000)
!       call
!     & sum34512786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26,-1.000)
!       call
!     & sum24513786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26, 1.000)
!       call
!     & sum23514786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26,-1.000)
!       call
!     & sum34521786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26, 1.000)
!       call
!     & sum24531786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26,-1.000)
!       call
!     & sum23541786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26, 1.000)
!       call
!     & sum14523786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26,-1.000)
!       call
!     & sum13524786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26, 1.000)
!       call
!     & sum14532786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26, 1.000)
!       call
!     & sum13542786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26,-1.000)
!       call
!     & sum12534786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26,-1.000)
!       call
!     & sum12543786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26, 1.000)
!       call
!     & sum34612785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26, 1.000)
!       call
!     & sum24613785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26,-1.000)
!       call
!     & sum23614785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26, 1.000)
!       call
!     & sum34621785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26,-1.000)
!       call
!     & sum24631785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26, 1.000)
!       call
!     & sum23641785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26,-1.000)
!       call
!     & sum14623785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26, 1.000)
!       call
!     & sum13624785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26,-1.000)
!       call
!     & sum14632785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26,-1.000)
!       call
!     & sum13642785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26, 1.000)
!       call
!     & sum12634785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26, 1.000)
!       call
!     & sum12643785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26,-1.000)
!       call
!     & sum34712586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26, 1.000)
!       call
!     & sum24713586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26,-1.000)
!       call
!     & sum23714586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26, 1.000)
!       call
!     & sum34721586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26,-1.000)
!       call
!     & sum24731586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26, 1.000)
!       call
!     & sum23741586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26,-1.000)
!       call
!     & sum14723586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26, 1.000)
!       call
!     & sum13724586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26,-1.000)
!       call
!     & sum14732586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26,-1.000)
!       call
!     & sum13742586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26, 1.000)
!       call
!     & sum12734586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26, 1.000)
!       call
!     & sum12743586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26,-1.000)
!       call
!     & sum23814576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26,-1.000)
!       call
!     & sum24813576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26, 1.000)
!       call
!     & sum34812576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26,-1.000)
!       call
!     & sum13824576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26, 1.000)
!       call
!     & sum14823576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26,-1.000)
!       call
!     & sum12834576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26,-1.000)
!       call
!     & sum12843576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26, 1.000)
!       call
!     & sum14832576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26, 1.000)
!       call
!     & sum13842576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26,-1.000)
!       call
!     & sum34821576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26, 1.000)
!       call
!     & sum24831576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26,-1.000)
!       call
!     & sum23841576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26, 1.000)
!       call
!     & sum34712685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26,-1.000)
!       call
!     & sum24713685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26, 1.000)
!       call
!     & sum23714685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26,-1.000)
!       call
!     & sum34721685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26, 1.000)
!       call
!     & sum24731685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26,-1.000)
!       call
!     & sum23741685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26, 1.000)
!       call
!     & sum14723685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26,-1.000)
!       call
!     & sum13724685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26, 1.000)
!       call
!     & sum14732685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26, 1.000)
!       call
!     & sum13742685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26,-1.000)
!       call
!     & sum12734685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26,-1.000)
!       call
!     & sum12743685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26, 1.000)
!       call
!     & sum23814675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26, 1.000)
!       call
!     & sum24813675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26,-1.000)
!       call
!     & sum34812675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26, 1.000)
!       call
!     & sum13824675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26,-1.000)
!       call
!     & sum14823675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26, 1.000)
!       call
!     & sum12834675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26, 1.000)
!       call
!     & sum12843675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26,-1.000)
!       call
!     & sum14832675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26,-1.000)
!       call
!     & sum13842675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26, 1.000)
!       call
!     & sum34821675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26,-1.000)
!       call
!     & sum24831675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26, 1.000)
!       call
!     & sum23841675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z26,-1.000)
c
!       do i=n0+1,n1-3
!       do j=i+1,n1-2
!       do k=j+1,n1-1
!       do l=k+1,n1
!       do a=n1+1,n3-3
!       do b=a+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)
!     & -z26(b,a,l,d,c,k,j,i)       ! 34512678 (-1.000)
!     & +z26(c,a,l,d,b,k,j,i)       ! 24513678 (+1.000)
!     & -z26(c,b,l,d,a,k,j,i)       ! 23514678 (-1.000)
!     & +z26(b,a,l,c,d,k,j,i)       ! 34521678 (+1.000)
!     & -z26(c,a,l,b,d,k,j,i)       ! 24531678 (-1.000)
!     & +z26(c,b,l,a,d,k,j,i)       ! 23541678 (+1.000)
!     & -z26(d,a,l,c,b,k,j,i)       ! 14523678 (-1.000)
!     & +z26(d,b,l,c,a,k,j,i)       ! 13524678 (+1.000)
!     & +z26(d,a,l,b,c,k,j,i)       ! 14532678 (+1.000)
!     & -z26(d,b,l,a,c,k,j,i)       ! 13542678 (-1.000)
!     & -z26(d,c,l,b,a,k,j,i)       ! 12534678 (-1.000)
!     & +z26(d,c,l,a,b,k,j,i)       ! 12543678 (+1.000)
!     & +z26(b,a,k,d,c,l,j,i)       ! 34612578 (+1.000)
!     & -z26(c,a,k,d,b,l,j,i)       ! 24613578 (-1.000)
!     & +z26(c,b,k,d,a,l,j,i)       ! 23614578 (+1.000)
!     & -z26(b,a,k,c,d,l,j,i)       ! 34621578 (-1.000)
!     & +z26(c,a,k,b,d,l,j,i)       ! 24631578 (+1.000)
!     & -z26(c,b,k,a,d,l,j,i)       ! 23641578 (-1.000)
!     & +z26(d,a,k,c,b,l,j,i)       ! 14623578 (+1.000)
!     & -z26(d,b,k,c,a,l,j,i)       ! 13624578 (-1.000)
!     & -z26(d,a,k,b,c,l,j,i)       ! 14632578 (-1.000)
!     & +z26(d,b,k,a,c,l,j,i)       ! 13642578 (+1.000)
!     & +z26(d,c,k,b,a,l,j,i)       ! 12634578 (+1.000)
!     & -z26(d,c,k,a,b,l,j,i)       ! 12643578 (-1.000)
!     & -z26(c,b,j,d,a,l,k,i)       ! 23714568 (-1.000)
!     & +z26(c,a,j,d,b,l,k,i)       ! 24713568 (+1.000)
!     & -z26(b,a,j,d,c,l,k,i)       ! 34712568 (-1.000)
!     & +z26(d,b,j,c,a,l,k,i)       ! 13724568 (+1.000)
!     & -z26(d,a,j,c,b,l,k,i)       ! 14723568 (-1.000)
!     & -z26(d,c,j,b,a,l,k,i)       ! 12734568 (-1.000)
!     & +z26(d,c,j,a,b,l,k,i)       ! 12743568 (+1.000)
!     & +z26(d,a,j,b,c,l,k,i)       ! 14732568 (+1.000)
!     & -z26(d,b,j,a,c,l,k,i)       ! 13742568 (-1.000)
!     & +z26(b,a,j,c,d,l,k,i)       ! 34721568 (+1.000)
!     & -z26(c,a,j,b,d,l,k,i)       ! 24731568 (-1.000)
!     & +z26(c,b,j,a,d,l,k,i)       ! 23741568 (+1.000)
!     & +z26(b,a,l,d,c,k,i,j)       ! 34512687 (+1.000)
!     & -z26(c,a,l,d,b,k,i,j)       ! 24513687 (-1.000)
!     & +z26(c,b,l,d,a,k,i,j)       ! 23514687 (+1.000)
!     & -z26(b,a,l,c,d,k,i,j)       ! 34521687 (-1.000)
!     & +z26(c,a,l,b,d,k,i,j)       ! 24531687 (+1.000)
!     & -z26(c,b,l,a,d,k,i,j)       ! 23541687 (-1.000)
!     & +z26(d,a,l,c,b,k,i,j)       ! 14523687 (+1.000)
!     & -z26(d,b,l,c,a,k,i,j)       ! 13524687 (-1.000)
!     & -z26(d,a,l,b,c,k,i,j)       ! 14532687 (-1.000)
!     & +z26(d,b,l,a,c,k,i,j)       ! 13542687 (+1.000)
!     & +z26(d,c,l,b,a,k,i,j)       ! 12534687 (+1.000)
!     & -z26(d,c,l,a,b,k,i,j)       ! 12543687 (-1.000)
!     & -z26(b,a,k,d,c,l,i,j)       ! 34612587 (-1.000)
!     & +z26(c,a,k,d,b,l,i,j)       ! 24613587 (+1.000)
!     & -z26(c,b,k,d,a,l,i,j)       ! 23614587 (-1.000)
!     & +z26(b,a,k,c,d,l,i,j)       ! 34621587 (+1.000)
!     & -z26(c,a,k,b,d,l,i,j)       ! 24631587 (-1.000)
!     & +z26(c,b,k,a,d,l,i,j)       ! 23641587 (+1.000)
!     & -z26(d,a,k,c,b,l,i,j)       ! 14623587 (-1.000)
!     & +z26(d,b,k,c,a,l,i,j)       ! 13624587 (+1.000)
!     & +z26(d,a,k,b,c,l,i,j)       ! 14632587 (+1.000)
!     & -z26(d,b,k,a,c,l,i,j)       ! 13642587 (-1.000)
!     & -z26(d,c,k,b,a,l,i,j)       ! 12634587 (-1.000)
!     & +z26(d,c,k,a,b,l,i,j)       ! 12643587 (+1.000)
!     & +z26(c,b,i,d,a,l,k,j)       ! 23814567 (+1.000)
!     & -z26(c,a,i,d,b,l,k,j)       ! 24813567 (-1.000)
!     & +z26(b,a,i,d,c,l,k,j)       ! 34812567 (+1.000)
!     & -z26(d,b,i,c,a,l,k,j)       ! 13824567 (-1.000)
!     & +z26(d,a,i,c,b,l,k,j)       ! 14823567 (+1.000)
!     & +z26(d,c,i,b,a,l,k,j)       ! 12834567 (+1.000)
!     & -z26(d,c,i,a,b,l,k,j)       ! 12843567 (-1.000)
!     & -z26(d,a,i,b,c,l,k,j)       ! 14832567 (-1.000)
!     & +z26(d,b,i,a,c,l,k,j)       ! 13842567 (+1.000)
!     & -z26(b,a,i,c,d,l,k,j)       ! 34821567 (-1.000)
!     & +z26(c,a,i,b,d,l,k,j)       ! 24831567 (+1.000)
!     & -z26(c,b,i,a,d,l,k,j)       ! 23841567 (-1.000)
!     & -z26(b,a,l,d,c,j,i,k)       ! 34512786 (-1.000)
!     & +z26(c,a,l,d,b,j,i,k)       ! 24513786 (+1.000)
!     & -z26(c,b,l,d,a,j,i,k)       ! 23514786 (-1.000)
!     & +z26(b,a,l,c,d,j,i,k)       ! 34521786 (+1.000)
!     & -z26(c,a,l,b,d,j,i,k)       ! 24531786 (-1.000)
!     & +z26(c,b,l,a,d,j,i,k)       ! 23541786 (+1.000)
!     & -z26(d,a,l,c,b,j,i,k)       ! 14523786 (-1.000)
!     & +z26(d,b,l,c,a,j,i,k)       ! 13524786 (+1.000)
!     & +z26(d,a,l,b,c,j,i,k)       ! 14532786 (+1.000)
!     & -z26(d,b,l,a,c,j,i,k)       ! 13542786 (-1.000)
!     & -z26(d,c,l,b,a,j,i,k)       ! 12534786 (-1.000)
!     & +z26(d,c,l,a,b,j,i,k)       ! 12543786 (+1.000)
!     & +z26(b,a,k,d,c,j,i,l)       ! 34612785 (+1.000)
!     & -z26(c,a,k,d,b,j,i,l)       ! 24613785 (-1.000)
!     & +z26(c,b,k,d,a,j,i,l)       ! 23614785 (+1.000)
!     & -z26(b,a,k,c,d,j,i,l)       ! 34621785 (-1.000)
!     & +z26(c,a,k,b,d,j,i,l)       ! 24631785 (+1.000)
!     & -z26(c,b,k,a,d,j,i,l)       ! 23641785 (-1.000)
!     & +z26(d,a,k,c,b,j,i,l)       ! 14623785 (+1.000)
!     & -z26(d,b,k,c,a,j,i,l)       ! 13624785 (-1.000)
!     & -z26(d,a,k,b,c,j,i,l)       ! 14632785 (-1.000)
!     & +z26(d,b,k,a,c,j,i,l)       ! 13642785 (+1.000)
!     & +z26(d,c,k,b,a,j,i,l)       ! 12634785 (+1.000)
!     & -z26(d,c,k,a,b,j,i,l)       ! 12643785 (-1.000)
!     & +z26(b,a,j,d,c,l,i,k)       ! 34712586 (+1.000)
!     & -z26(c,a,j,d,b,l,i,k)       ! 24713586 (-1.000)
!     & +z26(c,b,j,d,a,l,i,k)       ! 23714586 (+1.000)
!     & -z26(b,a,j,c,d,l,i,k)       ! 34721586 (-1.000)
!     & +z26(c,a,j,b,d,l,i,k)       ! 24731586 (+1.000)
!     & -z26(c,b,j,a,d,l,i,k)       ! 23741586 (-1.000)
!     & +z26(d,a,j,c,b,l,i,k)       ! 14723586 (+1.000)
!     & -z26(d,b,j,c,a,l,i,k)       ! 13724586 (-1.000)
!     & -z26(d,a,j,b,c,l,i,k)       ! 14732586 (-1.000)
!     & +z26(d,b,j,a,c,l,i,k)       ! 13742586 (+1.000)
!     & +z26(d,c,j,b,a,l,i,k)       ! 12734586 (+1.000)
!     & -z26(d,c,j,a,b,l,i,k)       ! 12743586 (-1.000)
!     & -z26(c,b,i,d,a,l,j,k)       ! 23814576 (-1.000)
!     & +z26(c,a,i,d,b,l,j,k)       ! 24813576 (+1.000)
!     & -z26(b,a,i,d,c,l,j,k)       ! 34812576 (-1.000)
!     & +z26(d,b,i,c,a,l,j,k)       ! 13824576 (+1.000)
!     & -z26(d,a,i,c,b,l,j,k)       ! 14823576 (-1.000)
!     & -z26(d,c,i,b,a,l,j,k)       ! 12834576 (-1.000)
!     & +z26(d,c,i,a,b,l,j,k)       ! 12843576 (+1.000)
!     & +z26(d,a,i,b,c,l,j,k)       ! 14832576 (+1.000)
!     & -z26(d,b,i,a,c,l,j,k)       ! 13842576 (-1.000)
!     & +z26(b,a,i,c,d,l,j,k)       ! 34821576 (+1.000)
!     & -z26(c,a,i,b,d,l,j,k)       ! 24831576 (-1.000)
!     & +z26(c,b,i,a,d,l,j,k)       ! 23841576 (+1.000)
!     & -z26(b,a,j,d,c,k,i,l)       ! 34712685 (-1.000)
!     & +z26(c,a,j,d,b,k,i,l)       ! 24713685 (+1.000)
!     & -z26(c,b,j,d,a,k,i,l)       ! 23714685 (-1.000)
!     & +z26(b,a,j,c,d,k,i,l)       ! 34721685 (+1.000)
!     & -z26(c,a,j,b,d,k,i,l)       ! 24731685 (-1.000)
!     & +z26(c,b,j,a,d,k,i,l)       ! 23741685 (+1.000)
!     & -z26(d,a,j,c,b,k,i,l)       ! 14723685 (-1.000)
!     & +z26(d,b,j,c,a,k,i,l)       ! 13724685 (+1.000)
!     & +z26(d,a,j,b,c,k,i,l)       ! 14732685 (+1.000)
!     & -z26(d,b,j,a,c,k,i,l)       ! 13742685 (-1.000)
!     & -z26(d,c,j,b,a,k,i,l)       ! 12734685 (-1.000)
!     & +z26(d,c,j,a,b,k,i,l)       ! 12743685 (+1.000)
!     & +z26(c,b,i,d,a,k,j,l)       ! 23814675 (+1.000)
!     & -z26(c,a,i,d,b,k,j,l)       ! 24813675 (-1.000)
!     & +z26(b,a,i,d,c,k,j,l)       ! 34812675 (+1.000)
!     & -z26(d,b,i,c,a,k,j,l)       ! 13824675 (-1.000)
!     & +z26(d,a,i,c,b,k,j,l)       ! 14823675 (+1.000)
!     & +z26(d,c,i,b,a,k,j,l)       ! 12834675 (+1.000)
!     & -z26(d,c,i,a,b,k,j,l)       ! 12843675 (-1.000)
!     & -z26(d,a,i,b,c,k,j,l)       ! 14832675 (-1.000)
!     & +z26(d,b,i,a,c,k,j,l)       ! 13842675 (+1.000)
!     & -z26(b,a,i,c,d,k,j,l)       ! 34821675 (-1.000)
!     & +z26(c,a,i,b,d,k,j,l)       ! 24831675 (+1.000)
!     & -z26(c,b,i,a,d,k,j,l)       ! 23841675 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z26)
       deallocate(u2)
c
       allocate(d1(n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3))
       call reorder3421(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n1,n3,n1,n3,n1,n3,intr,d1)
       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
       allocate(u3(n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3))
       i1=k3*k3*k3
       i2=k1*k1*k3
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u3)
       deallocate(d1)
       deallocate(d2)
c
!       allocate(f1(n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!       call reorder456123(n1,n3,n0,n1,n0,n1,n1,n3,n1,n3,n1,n3,
!     & n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,u3,f1)
!       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
!     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
!       allocate(z27(n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!       i1=k1*k1*k3*k3*k3
!       i2=k1*k1*k3
!       i3=k3
!       call egemm(i1,i2,i3,f1,d2,z27)
!       deallocate(f1)
!       deallocate(d2)
c
       do i=n0+1,n1-3;do j=i+1,n1-2;do k=j+1,n1-1;do l=k+1,n1
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-3;do b=a+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
          if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n1+1,n3
             sum=sum
     &     - u3(a,l,k,f,d,c)*t2a(f,b,j,i)       !alkfdcfbji      (-1.000)
     &     - u3(a,j,i,f,d,c)*t2a(f,b,l,k)       !ajifdcfblk      (-1.000)
     &     + u3(a,l,k,f,d,b)*t2a(f,c,j,i)       !alkfdbfcji      (+1.000)
     &     - u3(b,l,k,f,d,a)*t2a(f,c,j,i)       !blkfdafcji      (-1.000)
     &     + u3(a,j,i,f,d,b)*t2a(f,c,l,k)       !ajifdbfclk      (+1.000)
     &     - u3(b,j,i,f,d,a)*t2a(f,c,l,k)       !bjifdafclk      (-1.000)
     &     - u3(a,l,k,f,c,b)*t2a(f,d,j,i)       !alkfcbfdji      (-1.000)
     &     + u3(b,l,k,f,c,a)*t2a(f,d,j,i)       !blkfcafdji      (+1.000)
     &     - u3(c,l,k,f,b,a)*t2a(f,d,j,i)       !clkfbafdji      (-1.000)
     &     - u3(a,j,i,f,c,b)*t2a(f,d,l,k)       !ajifcbfdlk      (-1.000)
     &     + u3(b,j,i,f,c,a)*t2a(f,d,l,k)       !bjifcafdlk      (+1.000)
     &     - u3(c,j,i,f,b,a)*t2a(f,d,l,k)       !cjifbafdlk      (-1.000)
     &     + u3(a,l,j,f,d,c)*t2a(f,b,k,i)       !aljfdcfbki      (+1.000)
     &     + u3(a,k,i,f,d,c)*t2a(f,b,l,j)       !akifdcfblj      (+1.000)
     &     - u3(a,l,j,f,d,b)*t2a(f,c,k,i)       !aljfdbfcki      (-1.000)
     &     + u3(b,l,j,f,d,a)*t2a(f,c,k,i)       !bljfdafcki      (+1.000)
     &     - u3(a,k,i,f,d,b)*t2a(f,c,l,j)       !akifdbfclj      (-1.000)
     &     + u3(b,k,i,f,d,a)*t2a(f,c,l,j)       !bkifdafclj      (+1.000)
     &     + u3(a,l,j,f,c,b)*t2a(f,d,k,i)       !aljfcbfdki      (+1.000)
     &     - u3(b,l,j,f,c,a)*t2a(f,d,k,i)       !bljfcafdki      (-1.000)
     &     + u3(c,l,j,f,b,a)*t2a(f,d,k,i)       !cljfbafdki      (+1.000)
     &     + u3(a,k,i,f,c,b)*t2a(f,d,l,j)       !akifcbfdlj      (+1.000)
     &     - u3(b,k,i,f,c,a)*t2a(f,d,l,j)       !bkifcafdlj      (-1.000)
     &     + u3(c,k,i,f,b,a)*t2a(f,d,l,j)       !ckifbafdlj      (+1.000)
     &     - u3(a,k,j,f,d,c)*t2a(f,b,l,i)       !akjfdcfbli      (-1.000)
     &     - u3(a,l,i,f,d,c)*t2a(f,b,k,j)       !alifdcfbkj      (-1.000)
     &     + u3(a,k,j,f,d,b)*t2a(f,c,l,i)       !akjfdbfcli      (+1.000)
     &     - u3(b,k,j,f,d,a)*t2a(f,c,l,i)       !bkjfdafcli      (-1.000)
     &     + u3(a,l,i,f,d,b)*t2a(f,c,k,j)       !alifdbfckj      (+1.000)
     &     - u3(b,l,i,f,d,a)*t2a(f,c,k,j)       !blifdafckj      (-1.000)
     &     - u3(a,k,j,f,c,b)*t2a(f,d,l,i)       !akjfcbfdli      (-1.000)
     &     + u3(b,k,j,f,c,a)*t2a(f,d,l,i)       !bkjfcafdli      (+1.000)
     &     - u3(c,k,j,f,b,a)*t2a(f,d,l,i)       !ckjfbafdli      (-1.000)
     &     - u3(a,l,i,f,c,b)*t2a(f,d,k,j)       !alifcbfdkj      (-1.000)
     &     + u3(b,l,i,f,c,a)*t2a(f,d,k,j)       !blifcafdkj      (+1.000)
     &     - u3(c,l,i,f,b,a)*t2a(f,d,k,j)       !clifbafdkj      (-1.000)
             enddo
             v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call
!     & sum37812456(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z27,-1.000)
!       call
!     & sum35612478(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z27,-1.000)
!       call
!     & sum27813456(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z27, 1.000)
!       call
!     & sum27814356(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z27,-1.000)
!       call
!     & sum25613478(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z27, 1.000)
!       call
!     & sum25614378(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z27,-1.000)
!       call
!     & sum17823456(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z27,-1.000)
!       call
!     & sum17824356(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z27, 1.000)
!       call
!     & sum17834256(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z27,-1.000)
!       call
!     & sum15623478(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z27,-1.000)
!       call
!     & sum15624378(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z27, 1.000)
!       call
!     & sum15634278(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z27,-1.000)
!       call
!     & sum36812457(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z27, 1.000)
!       call
!     & sum35712468(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z27, 1.000)
!       call
!     & sum26813457(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z27,-1.000)
!       call
!     & sum26814357(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z27, 1.000)
!       call
!     & sum25713468(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z27,-1.000)
!       call
!     & sum25714368(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z27, 1.000)
!       call
!     & sum16823457(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z27, 1.000)
!       call
!     & sum16824357(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z27,-1.000)
!       call
!     & sum16834257(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z27, 1.000)
!       call
!     & sum15723468(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z27, 1.000)
!       call
!     & sum15724368(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z27,-1.000)
!       call
!     & sum15734268(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z27, 1.000)
!       call
!     & sum35812467(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z27,-1.000)
!       call
!     & sum36712458(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z27,-1.000)
!       call
!     & sum25813467(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z27, 1.000)
!       call
!     & sum25814367(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z27,-1.000)
!       call
!     & sum26713458(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z27, 1.000)
!       call
!     & sum26714358(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z27,-1.000)
!       call
!     & sum15823467(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z27,-1.000)
!       call
!     & sum15824367(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z27, 1.000)
!       call
!     & sum15834267(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z27,-1.000)
!       call
!     & sum16723458(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z27,-1.000)
!       call
!     & sum16724358(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z27, 1.000)
!       call
!     & sum16734258(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z27,-1.000)
c
!       do i=n0+1,n1-3
!       do j=i+1,n1-2
!       do k=j+1,n1-1
!       do l=k+1,n1
!       do a=n1+1,n3-3
!       do b=a+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)
!     & -z27(b,j,i,d,c,a,l,k)       ! 37812456 (-1.000)
!     & -z27(b,l,k,d,c,a,j,i)       ! 35612478 (-1.000)
!     & +z27(c,j,i,d,b,a,l,k)       ! 27813456 (+1.000)
!     & -z27(c,j,i,d,a,b,l,k)       ! 27814356 (-1.000)
!     & +z27(c,l,k,d,b,a,j,i)       ! 25613478 (+1.000)
!     & -z27(c,l,k,d,a,b,j,i)       ! 25614378 (-1.000)
!     & -z27(d,j,i,c,b,a,l,k)       ! 17823456 (-1.000)
!     & +z27(d,j,i,c,a,b,l,k)       ! 17824356 (+1.000)
!     & -z27(d,j,i,b,a,c,l,k)       ! 17834256 (-1.000)
!     & -z27(d,l,k,c,b,a,j,i)       ! 15623478 (-1.000)
!     & +z27(d,l,k,c,a,b,j,i)       ! 15624378 (+1.000)
!     & -z27(d,l,k,b,a,c,j,i)       ! 15634278 (-1.000)
!     & +z27(b,k,i,d,c,a,l,j)       ! 36812457 (+1.000)
!     & +z27(b,l,j,d,c,a,k,i)       ! 35712468 (+1.000)
!     & -z27(c,k,i,d,b,a,l,j)       ! 26813457 (-1.000)
!     & +z27(c,k,i,d,a,b,l,j)       ! 26814357 (+1.000)
!     & -z27(c,l,j,d,b,a,k,i)       ! 25713468 (-1.000)
!     & +z27(c,l,j,d,a,b,k,i)       ! 25714368 (+1.000)
!     & +z27(d,k,i,c,b,a,l,j)       ! 16823457 (+1.000)
!     & -z27(d,k,i,c,a,b,l,j)       ! 16824357 (-1.000)
!     & +z27(d,k,i,b,a,c,l,j)       ! 16834257 (+1.000)
!     & +z27(d,l,j,c,b,a,k,i)       ! 15723468 (+1.000)
!     & -z27(d,l,j,c,a,b,k,i)       ! 15724368 (-1.000)
!     & +z27(d,l,j,b,a,c,k,i)       ! 15734268 (+1.000)
!     & -z27(b,l,i,d,c,a,k,j)       ! 35812467 (-1.000)
!     & -z27(b,k,j,d,c,a,l,i)       ! 36712458 (-1.000)
!     & +z27(c,l,i,d,b,a,k,j)       ! 25813467 (+1.000)
!     & -z27(c,l,i,d,a,b,k,j)       ! 25814367 (-1.000)
!     & +z27(c,k,j,d,b,a,l,i)       ! 26713458 (+1.000)
!     & -z27(c,k,j,d,a,b,l,i)       ! 26714358 (-1.000)
!     & -z27(d,l,i,c,b,a,k,j)       ! 15823467 (-1.000)
!     & +z27(d,l,i,c,a,b,k,j)       ! 15824367 (+1.000)
!     & -z27(d,l,i,b,a,c,k,j)       ! 15834267 (-1.000)
!     & -z27(d,k,j,c,b,a,l,i)       ! 16723458 (-1.000)
!     & +z27(d,k,j,c,a,b,l,i)       ! 16724358 (+1.000)
!     & -z27(d,k,j,b,a,c,l,i)       ! 16734258 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z27)
       deallocate(u3)
c
       allocate(b1(n1+1:n3,n0+1:n1))
       call reorder12(0,n3,0,n3,
     & n1,n3,n0,n1,fockr,b1)
       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
       allocate(s11(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1
       i2=k1*k1*k3
       i3=k3
       call egemm(i1,i2,i3,b1,d2,s11)
       deallocate(b1)
       deallocate(d2)
c
       call sum2341(n0,n1,n1,n3,n0,n1,n0,n1,x1,s11, 1.000)
       deallocate(s11)
c
       allocate(b1(n0+1:n1,n1+1:n3))
       call reorder21(0,n3,0,n3,
     & n0,n1,n1,n3,fockr,b1)
       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
       allocate(s12(n1+1:n3,n1+1:n3,n0+1:n1,n1+1:n3))
       i1=k3
       i2=k1*k3*k3
       i3=k1
       call egemm(i1,i2,i3,b1,d2,s12)
       deallocate(b1)
       deallocate(d2)
c
       call sum2341(n1,n3,n1,n3,n1,n3,n0,n1,x2,s12,-1.000)
       deallocate(s12)
c
       allocate(d1(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       call reorder4132(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n0,n1,n0,n1,intr,d1)
       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
       allocate(u4(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1*k1
       i2=k1*k1*k3
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u4)
       deallocate(d1)
       deallocate(d2)
c
!       allocate(f1(n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
!       call reorder641235(n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,n0,n1,
!     & n0,n1,n0,n1,n1,n3,n0,n1,n0,n1,n0,n1,u4,f1)
!       allocate(f2(n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
!       call reorder451236(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,
!     & n0,n1,n0,n1,n1,n3,n1,n3,n1,n3,n0,n1,t3a,f2)
!       allocate(z30(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
!       i1=k1*k1*k1*k3
!       i2=k1*k3*k3*k3
!       i3=k1*k1
!       call egemm(i1,i2,i3,f1,f2,z30)
!       deallocate(f1)
!       deallocate(f2)
c
       do i=n0+1,n1-3;do j=i+1,n1-2;do k=j+1,n1-1;do l=k+1,n1
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-3;do b=a+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
          if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1;do n=n0+1,n1
             sum=sum
     &     + (u4(d,k,j,m,i,n)*t3a(c,b,a,n,m,l)      !dkjmincbanml    (+0.500)
     &     - u4(c,k,j,m,i,n)*t3a(d,b,a,n,m,l)       !ckjmindbanml    (-0.500)
     &     + u4(b,k,j,m,i,n)*t3a(d,c,a,n,m,l)       !bkjmindcanml    (+0.500)
     &     - u4(a,k,j,m,i,n)*t3a(d,c,b,n,m,l)       !akjmindcbnml    (-0.500)
     &     - u4(d,l,j,m,i,n)*t3a(c,b,a,n,m,k)       !dljmincbanmk    (-0.500)
     &     + u4(c,l,j,m,i,n)*t3a(d,b,a,n,m,k)       !cljmindbanmk    (+0.500)
     &     - u4(b,l,j,m,i,n)*t3a(d,c,a,n,m,k)       !bljmindcanmk    (-0.500)
     &     + u4(a,l,j,m,i,n)*t3a(d,c,b,n,m,k)       !aljmindcbnmk    (+0.500)
     &     + u4(d,l,k,m,i,n)*t3a(c,b,a,n,m,j)       !dlkmincbanmj    (+0.500)
     &     - u4(c,l,k,m,i,n)*t3a(d,b,a,n,m,j)       !clkmindbanmj    (-0.500)
     &     + u4(b,l,k,m,i,n)*t3a(d,c,a,n,m,j)       !blkmindcanmj    (+0.500)
     &     - u4(a,l,k,m,i,n)*t3a(d,c,b,n,m,j)       !alkmindcbnmj    (-0.500)
     &     - u4(d,k,i,m,j,n)*t3a(c,b,a,n,m,l)       !dkimjncbanml    (-0.500)
     &     + u4(c,k,i,m,j,n)*t3a(d,b,a,n,m,l)       !ckimjndbanml    (+0.500)
     &     - u4(b,k,i,m,j,n)*t3a(d,c,a,n,m,l)       !bkimjndcanml    (-0.500)
     &     + u4(a,k,i,m,j,n)*t3a(d,c,b,n,m,l)       !akimjndcbnml    (+0.500)
     &     + u4(d,l,i,m,j,n)*t3a(c,b,a,n,m,k)       !dlimjncbanmk    (+0.500)
     &     - u4(c,l,i,m,j,n)*t3a(d,b,a,n,m,k)       !climjndbanmk    (-0.500)
     &     + u4(b,l,i,m,j,n)*t3a(d,c,a,n,m,k)       !blimjndcanmk    (+0.500)
     &     - u4(a,l,i,m,j,n)*t3a(d,c,b,n,m,k)       !alimjndcbnmk    (-0.500)
     &     + u4(d,j,i,m,k,n)*t3a(c,b,a,n,m,l)       !djimkncbanml    (+0.500)
     &     - u4(c,j,i,m,k,n)*t3a(d,b,a,n,m,l)       !cjimkndbanml    (-0.500)
     &     + u4(b,j,i,m,k,n)*t3a(d,c,a,n,m,l)       !bjimkndcanml    (+0.500)
     &     - u4(a,j,i,m,k,n)*t3a(d,c,b,n,m,l)       !ajimkndcbnml    (-0.500)
     &     - u4(d,j,i,m,l,n)*t3a(c,b,a,n,m,k)       !djimlncbanmk    (-0.500)
     &     + u4(c,j,i,m,l,n)*t3a(d,b,a,n,m,k)       !cjimlndbanmk    (+0.500)
     &     - u4(b,j,i,m,l,n)*t3a(d,c,a,n,m,k)       !bjimlndcanmk    (-0.500)
     &     + u4(a,j,i,m,l,n)*t3a(d,c,b,n,m,k)       !ajimlndcbnmk    (+0.500)
     &     - u4(d,l,i,m,k,n)*t3a(c,b,a,n,m,j)       !dlimkncbanmj    (-0.500)
     &     + u4(c,l,i,m,k,n)*t3a(d,b,a,n,m,j)       !climkndbanmj    (+0.500)
     &     - u4(b,l,i,m,k,n)*t3a(d,c,a,n,m,j)       !blimkndcanmj    (-0.500)
     &     + u4(a,l,i,m,k,n)*t3a(d,c,b,n,m,j)       !alimkndcbnmj    (+0.500)
     &     + u4(d,k,i,m,l,n)*t3a(c,b,a,n,m,j)       !dkimlncbanmj    (+0.500)
     &     - u4(c,k,i,m,l,n)*t3a(d,b,a,n,m,j)       !ckimlndbanmj    (-0.500)
     &     + u4(b,k,i,m,l,n)*t3a(d,c,a,n,m,j)       !bkimlndcanmj    (+0.500)
     &     - u4(a,k,i,m,l,n)*t3a(d,c,b,n,m,j)       !akimlndcbnmj    (-0.500)
     &     - u4(d,l,k,m,j,n)*t3a(c,b,a,n,m,i)       !dlkmjncbanmi    (-0.500)
     &     + u4(c,l,k,m,j,n)*t3a(d,b,a,n,m,i)       !clkmjndbanmi    (+0.500)
     &     - u4(b,l,k,m,j,n)*t3a(d,c,a,n,m,i)       !blkmjndcanmi    (-0.500)
     &     + u4(a,l,k,m,j,n)*t3a(d,c,b,n,m,i)       !alkmjndcbnmi    (+0.500)
     &     + u4(d,l,j,m,k,n)*t3a(c,b,a,n,m,i)       !dljmkncbanmi    (+0.500)
     &     - u4(c,l,j,m,k,n)*t3a(d,b,a,n,m,i)       !cljmkndbanmi    (-0.500)
     &     + u4(b,l,j,m,k,n)*t3a(d,c,a,n,m,i)       !bljmkndcanmi    (+0.500)
     &     - u4(a,l,j,m,k,n)*t3a(d,c,b,n,m,i)       !aljmkndcbnmi    (-0.500)
     &     - u4(d,k,j,m,l,n)*t3a(c,b,a,n,m,i)       !dkjmlncbanmi    (-0.500)
     &     + u4(c,k,j,m,l,n)*t3a(d,b,a,n,m,i)       !ckjmlndbanmi    (+0.500)
     &     - u4(b,k,j,m,l,n)*t3a(d,c,a,n,m,i)       !bkjmlndcanmi    (-0.500)
     &     + u4(a,k,j,m,l,n)*t3a(d,c,b,n,m,i))/2.0d0!akjmlndcbnmi    (+0.500)
             enddo;enddo
             v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call
!     & sum23451678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z30, 0.500)
!       call
!     & sum13452678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z30,-0.500)
!       call
!     & sum12453678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z30, 0.500)
!       call
!     & sum12354678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z30,-0.500)
!       call
!     & sum23461578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z30,-0.500)
!       call
!     & sum13462578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z30, 0.500)
!       call
!     & sum12463578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z30,-0.500)
!       call
!     & sum12364578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z30, 0.500)
!       call
!     & sum23471568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z30, 0.500)
!       call
!     & sum13472568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z30,-0.500)
!       call
!     & sum12473568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z30, 0.500)
!       call
!     & sum12374568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z30,-0.500)
!       call
!     & sum23451687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z30,-0.500)
!       call
!     & sum13452687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z30, 0.500)
!       call
!     & sum12453687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z30,-0.500)
!       call
!     & sum12354687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z30, 0.500)
!       call
!     & sum23461587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z30, 0.500)
!       call
!     & sum13462587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z30,-0.500)
!       call
!     & sum12463587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z30, 0.500)
!       call
!     & sum12364587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z30,-0.500)
!       call
!     & sum23451786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z30, 0.500)
!       call
!     & sum13452786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z30,-0.500)
!       call
!     & sum12453786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z30, 0.500)
!       call
!     & sum12354786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z30,-0.500)
!       call
!     & sum23461785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z30,-0.500)
!       call
!     & sum13462785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z30, 0.500)
!       call
!     & sum12463785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z30,-0.500)
!       call
!     & sum12364785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z30, 0.500)
!       call
!     & sum23471586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z30,-0.500)
!       call
!     & sum13472586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z30, 0.500)
!       call
!     & sum12473586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z30,-0.500)
!       call
!     & sum12374586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z30, 0.500)
!       call
!     & sum23471685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z30, 0.500)
!       call
!     & sum13472685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z30,-0.500)
!       call
!     & sum12473685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z30, 0.500)
!       call
!     & sum12374685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z30,-0.500)
!       call
!     & sum23481567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z30,-0.500)
!       call
!     & sum13482567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z30, 0.500)
!       call
!     & sum12483567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z30,-0.500)
!       call
!     & sum12384567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z30, 0.500)
!       call
!     & sum23481576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z30, 0.500)
!       call
!     & sum13482576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z30,-0.500)
!       call
!     & sum12483576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z30, 0.500)
!       call
!     & sum12384576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z30,-0.500)
!       call
!     & sum23481675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z30,-0.500)
!       call
!     & sum13482675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z30, 0.500)
!       call
!     & sum12483675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z30,-0.500)
!       call
!     & sum12384675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z30, 0.500)
!c
!       do i=n0+1,n1-3
!       do j=i+1,n1-2
!       do k=j+1,n1-1
!       do l=k+1,n1
!       do a=n1+1,n3-3
!       do b=a+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)
!     & +(z30(c,b,a,l,d,k,j,i)      ! 23451678 (+0.500)
!     & -z30(d,b,a,l,c,k,j,i)       ! 13452678 (-0.500)
!     & +z30(d,c,a,l,b,k,j,i)       ! 12453678 (+0.500)
!     & -z30(d,c,b,l,a,k,j,i)       ! 12354678 (-0.500)
!     & -z30(c,b,a,k,d,l,j,i)       ! 23461578 (-0.500)
!     & +z30(d,b,a,k,c,l,j,i)       ! 13462578 (+0.500)
!     & -z30(d,c,a,k,b,l,j,i)       ! 12463578 (-0.500)
!     & +z30(d,c,b,k,a,l,j,i)       ! 12364578 (+0.500)
!     & +z30(c,b,a,j,d,l,k,i)       ! 23471568 (+0.500)
!     & -z30(d,b,a,j,c,l,k,i)       ! 13472568 (-0.500)
!     & +z30(d,c,a,j,b,l,k,i)       ! 12473568 (+0.500)
!     & -z30(d,c,b,j,a,l,k,i)       ! 12374568 (-0.500)
!     & -z30(c,b,a,l,d,k,i,j)       ! 23451687 (-0.500)
!     & +z30(d,b,a,l,c,k,i,j)       ! 13452687 (+0.500)
!     & -z30(d,c,a,l,b,k,i,j)       ! 12453687 (-0.500)
!     & +z30(d,c,b,l,a,k,i,j)       ! 12354687 (+0.500)
!     & +z30(c,b,a,k,d,l,i,j)       ! 23461587 (+0.500)
!     & -z30(d,b,a,k,c,l,i,j)       ! 13462587 (-0.500)
!     & +z30(d,c,a,k,b,l,i,j)       ! 12463587 (+0.500)
!     & -z30(d,c,b,k,a,l,i,j)       ! 12364587 (-0.500)
!     & +z30(c,b,a,l,d,j,i,k)       ! 23451786 (+0.500)
!     & -z30(d,b,a,l,c,j,i,k)       ! 13452786 (-0.500)
!     & +z30(d,c,a,l,b,j,i,k)       ! 12453786 (+0.500)
!     & -z30(d,c,b,l,a,j,i,k)       ! 12354786 (-0.500)
!     & -z30(c,b,a,k,d,j,i,l)       ! 23461785 (-0.500)
!     & +z30(d,b,a,k,c,j,i,l)       ! 13462785 (+0.500)
!     & -z30(d,c,a,k,b,j,i,l)       ! 12463785 (-0.500)
!     & +z30(d,c,b,k,a,j,i,l)       ! 12364785 (+0.500)
!     & -z30(c,b,a,j,d,l,i,k)       ! 23471586 (-0.500)
!     & +z30(d,b,a,j,c,l,i,k)       ! 13472586 (+0.500)
!     & -z30(d,c,a,j,b,l,i,k)       ! 12473586 (-0.500)
!     & +z30(d,c,b,j,a,l,i,k)       ! 12374586 (+0.500)
!     & +z30(c,b,a,j,d,k,i,l)       ! 23471685 (+0.500)
!     & -z30(d,b,a,j,c,k,i,l)       ! 13472685 (-0.500)
!     & +z30(d,c,a,j,b,k,i,l)       ! 12473685 (+0.500)
!     & -z30(d,c,b,j,a,k,i,l)       ! 12374685 (-0.500)
!     & -z30(c,b,a,i,d,l,k,j)       ! 23481567 (-0.500)
!     & +z30(d,b,a,i,c,l,k,j)       ! 13482567 (+0.500)
!     & -z30(d,c,a,i,b,l,k,j)       ! 12483567 (-0.500)
!     & +z30(d,c,b,i,a,l,k,j)       ! 12384567 (+0.500)
!     & +z30(c,b,a,i,d,l,j,k)       ! 23481576 (+0.500)
!     & -z30(d,b,a,i,c,l,j,k)       ! 13482576 (-0.500)
!     & +z30(d,c,a,i,b,l,j,k)       ! 12483576 (+0.500)
!     & -z30(d,c,b,i,a,l,j,k)       ! 12384576 (-0.500)
!     & -z30(c,b,a,i,d,k,j,l)       ! 23481675 (-0.500)
!     & +z30(d,b,a,i,c,k,j,l)       ! 13482675 (+0.500)
!     & -z30(d,c,a,i,b,k,j,l)       ! 12483675 (-0.500)
!     & +z30(d,c,b,i,a,k,j,l))/2.0d0! 12384675 (+0.500)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z30)
       deallocate(u4)
c
       allocate(d1(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       call reorder4213(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n0,n1,n0,n1,intr,d1)
       allocate(f2(n1+1:n3,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder142356(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,
     & n1,n3,n0,n1,n1,n3,n1,n3,n0,n1,n0,n1,t3a,f2)
       allocate(u5(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1
       i2=k1*k1*k3*k3
       i3=k1*k3
       call egemm(i1,i2,i3,d1,f2,u5)
       deallocate(d1)
       deallocate(f2)
c
!       allocate(f1(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
!       call reorder512346(n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,u5,f1)
!       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
!       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
!       allocate(z31(n1+1:n3,n1+1:n3,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
!       i1=k1*k1*k1*k3*k3
!       i2=k1*k3*k3
!       i3=k1
!       call egemm(i1,i2,i3,f1,d2,z31)
!       deallocate(f1)
!       deallocate(d2)
c
       do i=n0+1,n1-3;do j=i+1,n1-2;do k=j+1,n1-1;do l=k+1,n1
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-3;do b=a+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
          if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1
             sum=sum
     &     + u5(b,a,l,k,m,i)*t2a(d,c,m,j)       !balkmidcmj      (+1.000)
     &     - u5(c,a,l,k,m,i)*t2a(d,b,m,j)       !calkmidbmj      (-1.000)
     &     + u5(c,b,l,k,m,i)*t2a(d,a,m,j)       !cblkmidamj      (+1.000)
     &     + u5(d,a,l,k,m,i)*t2a(c,b,m,j)       !dalkmicbmj      (+1.000)
     &     - u5(d,b,l,k,m,i)*t2a(c,a,m,j)       !dblkmicamj      (-1.000)
     &     + u5(d,c,l,k,m,i)*t2a(b,a,m,j)       !dclkmibamj      (+1.000)
     &     - u5(b,a,l,j,m,i)*t2a(d,c,m,k)       !baljmidcmk      (-1.000)
     &     + u5(c,a,l,j,m,i)*t2a(d,b,m,k)       !caljmidbmk      (+1.000)
     &     - u5(c,b,l,j,m,i)*t2a(d,a,m,k)       !cbljmidamk      (-1.000)
     &     - u5(d,a,l,j,m,i)*t2a(c,b,m,k)       !daljmicbmk      (-1.000)
     &     + u5(d,b,l,j,m,i)*t2a(c,a,m,k)       !dbljmicamk      (+1.000)
     &     - u5(d,c,l,j,m,i)*t2a(b,a,m,k)       !dcljmibamk      (-1.000)
     &     + u5(b,a,k,j,m,i)*t2a(d,c,m,l)       !bakjmidcml      (+1.000)
     &     - u5(c,a,k,j,m,i)*t2a(d,b,m,l)       !cakjmidbml      (-1.000)
     &     + u5(c,b,k,j,m,i)*t2a(d,a,m,l)       !cbkjmidaml      (+1.000)
     &     + u5(d,a,k,j,m,i)*t2a(c,b,m,l)       !dakjmicbml      (+1.000)
     &     - u5(d,b,k,j,m,i)*t2a(c,a,m,l)       !dbkjmicaml      (-1.000)
     &     + u5(d,c,k,j,m,i)*t2a(b,a,m,l)       !dckjmibaml      (+1.000)
     &     - u5(b,a,l,k,m,j)*t2a(d,c,m,i)       !balkmjdcmi      (-1.000)
     &     + u5(c,a,l,k,m,j)*t2a(d,b,m,i)       !calkmjdbmi      (+1.000)
     &     - u5(c,b,l,k,m,j)*t2a(d,a,m,i)       !cblkmjdami      (-1.000)
     &     - u5(d,a,l,k,m,j)*t2a(c,b,m,i)       !dalkmjcbmi      (-1.000)
     &     + u5(d,b,l,k,m,j)*t2a(c,a,m,i)       !dblkmjcami      (+1.000)
     &     - u5(d,c,l,k,m,j)*t2a(b,a,m,i)       !dclkmjbami      (-1.000)
     &     + u5(b,a,l,j,m,k)*t2a(d,c,m,i)       !baljmkdcmi      (+1.000)
     &     - u5(c,a,l,j,m,k)*t2a(d,b,m,i)       !caljmkdbmi      (-1.000)
     &     + u5(c,b,l,j,m,k)*t2a(d,a,m,i)       !cbljmkdami      (+1.000)
     &     + u5(d,a,l,j,m,k)*t2a(c,b,m,i)       !daljmkcbmi      (+1.000)
     &     - u5(d,b,l,j,m,k)*t2a(c,a,m,i)       !dbljmkcami      (-1.000)
     &     + u5(d,c,l,j,m,k)*t2a(b,a,m,i)       !dcljmkbami      (+1.000)
     &     - u5(b,a,k,j,m,l)*t2a(d,c,m,i)       !bakjmldcmi      (-1.000)
     &     + u5(c,a,k,j,m,l)*t2a(d,b,m,i)       !cakjmldbmi      (+1.000)
     &     - u5(c,b,k,j,m,l)*t2a(d,a,m,i)       !cbkjmldami      (-1.000)
     &     - u5(d,a,k,j,m,l)*t2a(c,b,m,i)       !dakjmlcbmi      (-1.000)
     &     + u5(d,b,k,j,m,l)*t2a(c,a,m,i)       !dbkjmlcami      (+1.000)
     &     - u5(d,c,k,j,m,l)*t2a(b,a,m,i)       !dckjmlbami      (-1.000)
     &     + u5(b,a,l,i,m,j)*t2a(d,c,m,k)       !balimjdcmk      (+1.000)
     &     - u5(c,a,l,i,m,j)*t2a(d,b,m,k)       !calimjdbmk      (-1.000)
     &     + u5(c,b,l,i,m,j)*t2a(d,a,m,k)       !cblimjdamk      (+1.000)
     &     + u5(d,a,l,i,m,j)*t2a(c,b,m,k)       !dalimjcbmk      (+1.000)
     &     - u5(d,b,l,i,m,j)*t2a(c,a,m,k)       !dblimjcamk      (-1.000)
     &     + u5(d,c,l,i,m,j)*t2a(b,a,m,k)       !dclimjbamk      (+1.000)
     &     - u5(b,a,k,i,m,j)*t2a(d,c,m,l)       !bakimjdcml      (-1.000)
     &     + u5(c,a,k,i,m,j)*t2a(d,b,m,l)       !cakimjdbml      (+1.000)
     &     - u5(c,b,k,i,m,j)*t2a(d,a,m,l)       !cbkimjdaml      (-1.000)
     &     - u5(d,a,k,i,m,j)*t2a(c,b,m,l)       !dakimjcbml      (-1.000)
     &     + u5(d,b,k,i,m,j)*t2a(c,a,m,l)       !dbkimjcaml      (+1.000)
     &     - u5(d,c,k,i,m,j)*t2a(b,a,m,l)       !dckimjbaml      (-1.000)
     &     - u5(b,a,l,i,m,k)*t2a(d,c,m,j)       !balimkdcmj      (-1.000)
     &     + u5(c,a,l,i,m,k)*t2a(d,b,m,j)       !calimkdbmj      (+1.000)
     &     - u5(c,b,l,i,m,k)*t2a(d,a,m,j)       !cblimkdamj      (-1.000)
     &     - u5(d,a,l,i,m,k)*t2a(c,b,m,j)       !dalimkcbmj      (-1.000)
     &     + u5(d,b,l,i,m,k)*t2a(c,a,m,j)       !dblimkcamj      (+1.000)
     &     - u5(d,c,l,i,m,k)*t2a(b,a,m,j)       !dclimkbamj      (-1.000)
     &     + u5(b,a,k,i,m,l)*t2a(d,c,m,j)       !bakimldcmj      (+1.000)
     &     - u5(c,a,k,i,m,l)*t2a(d,b,m,j)       !cakimldbmj      (-1.000)
     &     + u5(c,b,k,i,m,l)*t2a(d,a,m,j)       !cbkimldamj      (+1.000)
     &     + u5(d,a,k,i,m,l)*t2a(c,b,m,j)       !dakimlcbmj      (+1.000)
     &     - u5(d,b,k,i,m,l)*t2a(c,a,m,j)       !dbkimlcamj      (-1.000)
     &     + u5(d,c,k,i,m,l)*t2a(b,a,m,j)       !dckimlbamj      (+1.000)
     &     + u5(b,a,j,i,m,k)*t2a(d,c,m,l)       !bajimkdcml      (+1.000)
     &     - u5(c,a,j,i,m,k)*t2a(d,b,m,l)       !cajimkdbml      (-1.000)
     &     + u5(c,b,j,i,m,k)*t2a(d,a,m,l)       !cbjimkdaml      (+1.000)
     &     + u5(d,a,j,i,m,k)*t2a(c,b,m,l)       !dajimkcbml      (+1.000)
     &     - u5(d,b,j,i,m,k)*t2a(c,a,m,l)       !dbjimkcaml      (-1.000)
     &     + u5(d,c,j,i,m,k)*t2a(b,a,m,l)       !dcjimkbaml      (+1.000)
     &     - u5(b,a,j,i,m,l)*t2a(d,c,m,k)       !bajimldcmk      (-1.000)
     &     + u5(c,a,j,i,m,l)*t2a(d,b,m,k)       !cajimldbmk      (+1.000)
     &     - u5(c,b,j,i,m,l)*t2a(d,a,m,k)       !cbjimldamk      (-1.000)
     &     - u5(d,a,j,i,m,l)*t2a(c,b,m,k)       !dajimlcbmk      (-1.000)
     &     + u5(d,b,j,i,m,l)*t2a(c,a,m,k)       !dbjimlcamk      (+1.000)
     &     - u5(d,c,j,i,m,l)*t2a(b,a,m,k)       !dcjimlbamk      (-1.000)
             enddo
             v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call
!     & sum12734568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z31, 1.000)
!       call
!     & sum13724568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z31,-1.000)
!       call
!     & sum14723568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z31, 1.000)
!       call
!     & sum23714568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z31, 1.000)
!       call
!     & sum24713568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z31,-1.000)
!       call
!     & sum34712568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z31, 1.000)
!       call
!     & sum12634578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z31,-1.000)
!       call
!     & sum13624578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z31, 1.000)
!       call
!     & sum14623578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z31,-1.000)
!       call
!     & sum23614578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z31,-1.000)
!       call
!     & sum24613578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z31, 1.000)
!       call
!     & sum34612578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z31,-1.000)
!       call
!     & sum12534678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z31, 1.000)
!       call
!     & sum13524678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z31,-1.000)
!       call
!     & sum14523678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z31, 1.000)
!       call
!     & sum23514678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z31, 1.000)
!       call
!     & sum24513678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z31,-1.000)
!       call
!     & sum34512678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z31, 1.000)
!       call
!     & sum12834567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z31,-1.000)
!       call
!     & sum13824567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z31, 1.000)
!       call
!     & sum14823567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z31,-1.000)
!       call
!     & sum23814567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z31,-1.000)
!       call
!     & sum24813567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z31, 1.000)
!       call
!     & sum34812567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z31,-1.000)
!       call
!     & sum12834576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z31, 1.000)
!       call
!     & sum13824576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z31,-1.000)
!       call
!     & sum14823576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z31, 1.000)
!       call
!     & sum23814576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z31, 1.000)
!       call
!     & sum24813576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z31,-1.000)
!       call
!     & sum34812576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z31, 1.000)
!       call
!     & sum12834675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z31,-1.000)
!       call
!     & sum13824675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z31, 1.000)
!       call
!     & sum14823675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z31,-1.000)
!       call
!     & sum23814675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z31,-1.000)
!       call
!     & sum24813675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z31, 1.000)
!       call
!     & sum34812675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z31,-1.000)
!       call
!     & sum12634587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z31, 1.000)
!       call
!     & sum13624587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z31,-1.000)
!       call
!     & sum14623587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z31, 1.000)
!       call
!     & sum23614587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z31, 1.000)
!       call
!     & sum24613587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z31,-1.000)
!       call
!     & sum34612587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z31, 1.000)
!       call
!     & sum12534687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z31,-1.000)
!       call
!     & sum13524687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z31, 1.000)
!       call
!     & sum14523687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z31,-1.000)
!       call
!     & sum23514687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z31,-1.000)
!       call
!     & sum24513687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z31, 1.000)
!       call
!     & sum34512687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z31,-1.000)
!       call
!     & sum12734586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z31,-1.000)
!       call
!     & sum13724586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z31, 1.000)
!       call
!     & sum14723586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z31,-1.000)
!       call
!     & sum23714586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z31,-1.000)
!       call
!     & sum24713586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z31, 1.000)
!       call
!     & sum34712586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z31,-1.000)
!       call
!     & sum12734685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z31, 1.000)
!       call
!     & sum13724685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z31,-1.000)
!       call
!     & sum14723685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z31, 1.000)
!       call
!     & sum23714685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z31, 1.000)
!       call
!     & sum24713685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z31,-1.000)
!       call
!     & sum34712685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z31, 1.000)
!       call
!     & sum12534786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z31, 1.000)
!       call
!     & sum13524786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z31,-1.000)
!       call
!     & sum14523786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z31, 1.000)
!       call
!     & sum23514786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z31, 1.000)
!       call
!     & sum24513786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z31,-1.000)
!       call
!     & sum34512786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z31, 1.000)
!       call
!     & sum12634785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z31,-1.000)
!       call
!     & sum13624785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z31, 1.000)
!       call
!     & sum14623785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z31,-1.000)
!       call
!     & sum23614785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z31,-1.000)
!       call
!     & sum24613785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z31, 1.000)
!       call
!     & sum34612785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z31,-1.000)
!c
!       do i=n0+1,n1-3
!       do j=i+1,n1-2
!       do k=j+1,n1-1
!       do l=k+1,n1
!       do a=n1+1,n3-3
!       do b=a+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)
!     & +z31(d,c,j,b,a,l,k,i)       ! 12734568 (+1.000)
!     & -z31(d,b,j,c,a,l,k,i)       ! 13724568 (-1.000)
!     & +z31(d,a,j,c,b,l,k,i)       ! 14723568 (+1.000)
!     & +z31(c,b,j,d,a,l,k,i)       ! 23714568 (+1.000)
!     & -z31(c,a,j,d,b,l,k,i)       ! 24713568 (-1.000)
!     & +z31(b,a,j,d,c,l,k,i)       ! 34712568 (+1.000)
!     & -z31(d,c,k,b,a,l,j,i)       ! 12634578 (-1.000)
!     & +z31(d,b,k,c,a,l,j,i)       ! 13624578 (+1.000)
!     & -z31(d,a,k,c,b,l,j,i)       ! 14623578 (-1.000)
!     & -z31(c,b,k,d,a,l,j,i)       ! 23614578 (-1.000)
!     & +z31(c,a,k,d,b,l,j,i)       ! 24613578 (+1.000)
!     & -z31(b,a,k,d,c,l,j,i)       ! 34612578 (-1.000)
!     & +z31(d,c,l,b,a,k,j,i)       ! 12534678 (+1.000)
!     & -z31(d,b,l,c,a,k,j,i)       ! 13524678 (-1.000)
!     & +z31(d,a,l,c,b,k,j,i)       ! 14523678 (+1.000)
!     & +z31(c,b,l,d,a,k,j,i)       ! 23514678 (+1.000)
!     & -z31(c,a,l,d,b,k,j,i)       ! 24513678 (-1.000)
!     & +z31(b,a,l,d,c,k,j,i)       ! 34512678 (+1.000)
!     & -z31(d,c,i,b,a,l,k,j)       ! 12834567 (-1.000)
!     & +z31(d,b,i,c,a,l,k,j)       ! 13824567 (+1.000)
!     & -z31(d,a,i,c,b,l,k,j)       ! 14823567 (-1.000)
!     & -z31(c,b,i,d,a,l,k,j)       ! 23814567 (-1.000)
!     & +z31(c,a,i,d,b,l,k,j)       ! 24813567 (+1.000)
!     & -z31(b,a,i,d,c,l,k,j)       ! 34812567 (-1.000)
!     & +z31(d,c,i,b,a,l,j,k)       ! 12834576 (+1.000)
!     & -z31(d,b,i,c,a,l,j,k)       ! 13824576 (-1.000)
!     & +z31(d,a,i,c,b,l,j,k)       ! 14823576 (+1.000)
!     & +z31(c,b,i,d,a,l,j,k)       ! 23814576 (+1.000)
!     & -z31(c,a,i,d,b,l,j,k)       ! 24813576 (-1.000)
!     & +z31(b,a,i,d,c,l,j,k)       ! 34812576 (+1.000)
!     & -z31(d,c,i,b,a,k,j,l)       ! 12834675 (-1.000)
!     & +z31(d,b,i,c,a,k,j,l)       ! 13824675 (+1.000)
!     & -z31(d,a,i,c,b,k,j,l)       ! 14823675 (-1.000)
!     & -z31(c,b,i,d,a,k,j,l)       ! 23814675 (-1.000)
!     & +z31(c,a,i,d,b,k,j,l)       ! 24813675 (+1.000)
!     & -z31(b,a,i,d,c,k,j,l)       ! 34812675 (-1.000)
!     & +z31(d,c,k,b,a,l,i,j)       ! 12634587 (+1.000)
!     & -z31(d,b,k,c,a,l,i,j)       ! 13624587 (-1.000)
!     & +z31(d,a,k,c,b,l,i,j)       ! 14623587 (+1.000)
!     & +z31(c,b,k,d,a,l,i,j)       ! 23614587 (+1.000)
!     & -z31(c,a,k,d,b,l,i,j)       ! 24613587 (-1.000)
!     & +z31(b,a,k,d,c,l,i,j)       ! 34612587 (+1.000)
!     & -z31(d,c,l,b,a,k,i,j)       ! 12534687 (-1.000)
!     & +z31(d,b,l,c,a,k,i,j)       ! 13524687 (+1.000)
!     & -z31(d,a,l,c,b,k,i,j)       ! 14523687 (-1.000)
!     & -z31(c,b,l,d,a,k,i,j)       ! 23514687 (-1.000)
!     & +z31(c,a,l,d,b,k,i,j)       ! 24513687 (+1.000)
!     & -z31(b,a,l,d,c,k,i,j)       ! 34512687 (-1.000)
!     & -z31(d,c,j,b,a,l,i,k)       ! 12734586 (-1.000)
!     & +z31(d,b,j,c,a,l,i,k)       ! 13724586 (+1.000)
!     & -z31(d,a,j,c,b,l,i,k)       ! 14723586 (-1.000)
!     & -z31(c,b,j,d,a,l,i,k)       ! 23714586 (-1.000)
!     & +z31(c,a,j,d,b,l,i,k)       ! 24713586 (+1.000)
!     & -z31(b,a,j,d,c,l,i,k)       ! 34712586 (-1.000)
!     & +z31(d,c,j,b,a,k,i,l)       ! 12734685 (+1.000)
!     & -z31(d,b,j,c,a,k,i,l)       ! 13724685 (-1.000)
!     & +z31(d,a,j,c,b,k,i,l)       ! 14723685 (+1.000)
!     & +z31(c,b,j,d,a,k,i,l)       ! 23714685 (+1.000)
!     & -z31(c,a,j,d,b,k,i,l)       ! 24713685 (-1.000)
!     & +z31(b,a,j,d,c,k,i,l)       ! 34712685 (+1.000)
!     & +z31(d,c,l,b,a,j,i,k)       ! 12534786 (+1.000)
!     & -z31(d,b,l,c,a,j,i,k)       ! 13524786 (-1.000)
!     & +z31(d,a,l,c,b,j,i,k)       ! 14523786 (+1.000)
!     & +z31(c,b,l,d,a,j,i,k)       ! 23514786 (+1.000)
!     & -z31(c,a,l,d,b,j,i,k)       ! 24513786 (-1.000)
!     & +z31(b,a,l,d,c,j,i,k)       ! 34512786 (+1.000)
!     & -z31(d,c,k,b,a,j,i,l)       ! 12634785 (-1.000)
!     & +z31(d,b,k,c,a,j,i,l)       ! 13624785 (+1.000)
!     & -z31(d,a,k,c,b,j,i,l)       ! 14623785 (-1.000)
!     & -z31(c,b,k,d,a,j,i,l)       ! 23614785 (-1.000)
!     & +z31(c,a,k,d,b,j,i,l)       ! 24613785 (+1.000)
!     & -z31(b,a,k,d,c,j,i,l)       ! 34612785 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z31)
       deallocate(u5)
c
c
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1432(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n0,n1,n0,n1,intr,d1)
       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
       allocate(s13(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1
       i2=k1*k3
       i3=k3*k1
       call egemm(i1,i2,i3,d1,d2,s13)
       deallocate(d1)
       deallocate(d2)
c
       call sum2341(n0,n1,n1,n3,n0,n1,n0,n1,x9,s13, 1.000)
       deallocate(s13)
c
       allocate(d1(n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1))
       call reorder1243(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n0,n1,n1,n3,n0,n1,intr,d1)
       allocate(d2(n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3))
       call reorder4312(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n0,n1,n1,n3,n1,n3,t2a,d2)
       allocate(s14(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
       i1=k1*k3
       i2=k3*k3
       i3=k1*k1
       call egemm(i1,i2,i3,d1,d2,s14)
       deallocate(d1)
       deallocate(d2)
c
       call sum2314(n1,n3,n1,n3,n1,n3,n0,n1,x2,s14, 0.500)
       deallocate(s14)
c
       allocate(d1(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3412(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n1,n3,n1,n3,n0,n1,intr,d1)
       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
       allocate(u6(n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       i1=k1*k3*k3
       i2=k1*k1*k3
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u6)
       deallocate(d1)
       deallocate(d2)
c
!       allocate(f1(n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!       call reorder645123(n1,n3,n0,n1,n0,n1,n1,n3,n1,n3,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,u6,f1)
!       allocate(f2(n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!       call reorder412356(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,t3a,f2)
!       allocate(z34(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!       i1=k1*k1*k3*k3
!       i2=k1*k1*k3*k3
!       i3=k3*k1
!       call egemm(i1,i2,i3,f1,f2,z34)
!       deallocate(f1)
!       deallocate(f2)
c
       do i=n0+1,n1-3;do j=i+1,n1-2;do k=j+1,n1-1;do l=k+1,n1
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-3;do b=a+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
          if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n1+1,n3;do m=n0+1,n1
             sum=sum
     &     + u6(c,j,i,f,d,m)*t3a(f,b,a,m,l,k)     !cjifdmfbamlk    (+1.000)
     &     - u6(b,j,i,f,d,m)*t3a(f,c,a,m,l,k)     !bjifdmfcamlk    (-1.000)
     &     + u6(a,j,i,f,d,m)*t3a(f,c,b,m,l,k)     !ajifdmfcbmlk    (+1.000)
     &     - u6(d,j,i,f,c,m)*t3a(f,b,a,m,l,k)     !djifcmfbamlk    (-1.000)
     &     + u6(d,j,i,f,b,m)*t3a(f,c,a,m,l,k)     !djifbmfcamlk    (+1.000)
     &     - u6(d,j,i,f,a,m)*t3a(f,c,b,m,l,k)     !djifamfcbmlk    (-1.000)
     &     + u6(b,j,i,f,c,m)*t3a(f,d,a,m,l,k)     !bjifcmfdamlk    (+1.000)
     &     - u6(a,j,i,f,c,m)*t3a(f,d,b,m,l,k)     !ajifcmfdbmlk    (-1.000)
     &     - u6(c,j,i,f,b,m)*t3a(f,d,a,m,l,k)     !cjifbmfdamlk    (-1.000)
     &     + u6(c,j,i,f,a,m)*t3a(f,d,b,m,l,k)     !cjifamfdbmlk    (+1.000)
     &     + u6(a,j,i,f,b,m)*t3a(f,d,c,m,l,k)     !ajifbmfdcmlk    (+1.000)
     &     - u6(b,j,i,f,a,m)*t3a(f,d,c,m,l,k)     !bjifamfdcmlk    (-1.000)
     &     - u6(c,k,i,f,d,m)*t3a(f,b,a,m,l,j)     !ckifdmfbamlj    (-1.000)
     &     + u6(b,k,i,f,d,m)*t3a(f,c,a,m,l,j)     !bkifdmfcamlj    (+1.000)
     &     - u6(a,k,i,f,d,m)*t3a(f,c,b,m,l,j)     !akifdmfcbmlj    (-1.000)
     &     + u6(d,k,i,f,c,m)*t3a(f,b,a,m,l,j)     !dkifcmfbamlj    (+1.000)
     &     - u6(d,k,i,f,b,m)*t3a(f,c,a,m,l,j)     !dkifbmfcamlj    (-1.000)
     &     + u6(d,k,i,f,a,m)*t3a(f,c,b,m,l,j)     !dkifamfcbmlj    (+1.000)
     &     - u6(b,k,i,f,c,m)*t3a(f,d,a,m,l,j)     !bkifcmfdamlj    (-1.000)
     &     + u6(a,k,i,f,c,m)*t3a(f,d,b,m,l,j)     !akifcmfdbmlj    (+1.000)
     &     + u6(c,k,i,f,b,m)*t3a(f,d,a,m,l,j)     !ckifbmfdamlj    (+1.000)
     &     - u6(c,k,i,f,a,m)*t3a(f,d,b,m,l,j)     !ckifamfdbmlj    (-1.000)
     &     - u6(a,k,i,f,b,m)*t3a(f,d,c,m,l,j)     !akifbmfdcmlj    (-1.000)
     &     + u6(b,k,i,f,a,m)*t3a(f,d,c,m,l,j)     !bkifamfdcmlj    (+1.000)
     &     + u6(c,l,i,f,d,m)*t3a(f,b,a,m,k,j)     !clifdmfbamkj    (+1.000)
     &     - u6(b,l,i,f,d,m)*t3a(f,c,a,m,k,j)     !blifdmfcamkj    (-1.000)
     &     + u6(a,l,i,f,d,m)*t3a(f,c,b,m,k,j)     !alifdmfcbmkj    (+1.000)
     &     - u6(d,l,i,f,c,m)*t3a(f,b,a,m,k,j)     !dlifcmfbamkj    (-1.000)
     &     + u6(d,l,i,f,b,m)*t3a(f,c,a,m,k,j)     !dlifbmfcamkj    (+1.000)
     &     - u6(d,l,i,f,a,m)*t3a(f,c,b,m,k,j)     !dlifamfcbmkj    (-1.000)
     &     + u6(b,l,i,f,c,m)*t3a(f,d,a,m,k,j)     !blifcmfdamkj    (+1.000)
     &     - u6(a,l,i,f,c,m)*t3a(f,d,b,m,k,j)     !alifcmfdbmkj    (-1.000)
     &     - u6(c,l,i,f,b,m)*t3a(f,d,a,m,k,j)     !clifbmfdamkj    (-1.000)
     &     + u6(c,l,i,f,a,m)*t3a(f,d,b,m,k,j)     !clifamfdbmkj    (+1.000)
     &     + u6(a,l,i,f,b,m)*t3a(f,d,c,m,k,j)     !alifbmfdcmkj    (+1.000)
     &     - u6(b,l,i,f,a,m)*t3a(f,d,c,m,k,j)     !blifamfdcmkj    (-1.000)
     &     + u6(c,k,j,f,d,m)*t3a(f,b,a,m,l,i)     !ckjfdmfbamli    (+1.000)
     &     - u6(b,k,j,f,d,m)*t3a(f,c,a,m,l,i)     !bkjfdmfcamli    (-1.000)
     &     + u6(a,k,j,f,d,m)*t3a(f,c,b,m,l,i)     !akjfdmfcbmli    (+1.000)
     &     - u6(d,k,j,f,c,m)*t3a(f,b,a,m,l,i)     !dkjfcmfbamli    (-1.000)
     &     + u6(d,k,j,f,b,m)*t3a(f,c,a,m,l,i)     !dkjfbmfcamli    (+1.000)
     &     - u6(d,k,j,f,a,m)*t3a(f,c,b,m,l,i)     !dkjfamfcbmli    (-1.000)
     &     + u6(b,k,j,f,c,m)*t3a(f,d,a,m,l,i)     !bkjfcmfdamli    (+1.000)
     &     - u6(a,k,j,f,c,m)*t3a(f,d,b,m,l,i)     !akjfcmfdbmli    (-1.000)
     &     - u6(c,k,j,f,b,m)*t3a(f,d,a,m,l,i)     !ckjfbmfdamli    (-1.000)
     &     + u6(c,k,j,f,a,m)*t3a(f,d,b,m,l,i)     !ckjfamfdbmli    (+1.000)
     &     + u6(a,k,j,f,b,m)*t3a(f,d,c,m,l,i)     !akjfbmfdcmli    (+1.000)
     &     - u6(b,k,j,f,a,m)*t3a(f,d,c,m,l,i)     !bkjfamfdcmli    (-1.000)
     &     - u6(c,l,j,f,d,m)*t3a(f,b,a,m,k,i)     !cljfdmfbamki    (-1.000)
     &     + u6(b,l,j,f,d,m)*t3a(f,c,a,m,k,i)     !bljfdmfcamki    (+1.000)
     &     - u6(a,l,j,f,d,m)*t3a(f,c,b,m,k,i)     !aljfdmfcbmki    (-1.000)
     &     + u6(d,l,j,f,c,m)*t3a(f,b,a,m,k,i)     !dljfcmfbamki    (+1.000)
     &     - u6(d,l,j,f,b,m)*t3a(f,c,a,m,k,i)     !dljfbmfcamki    (-1.000)
     &     + u6(d,l,j,f,a,m)*t3a(f,c,b,m,k,i)     !dljfamfcbmki    (+1.000)
     &     - u6(b,l,j,f,c,m)*t3a(f,d,a,m,k,i)     !bljfcmfdamki    (-1.000)
     &     + u6(a,l,j,f,c,m)*t3a(f,d,b,m,k,i)     !aljfcmfdbmki    (+1.000)
     &     + u6(c,l,j,f,b,m)*t3a(f,d,a,m,k,i)     !cljfbmfdamki    (+1.000)
     &     - u6(c,l,j,f,a,m)*t3a(f,d,b,m,k,i)     !cljfamfdbmki    (-1.000)
     &     - u6(a,l,j,f,b,m)*t3a(f,d,c,m,k,i)     !aljfbmfdcmki    (-1.000)
     &     + u6(b,l,j,f,a,m)*t3a(f,d,c,m,k,i)     !bljfamfdcmki    (+1.000)
     &     + u6(c,l,k,f,d,m)*t3a(f,b,a,m,j,i)     !clkfdmfbamji    (+1.000)
     &     - u6(b,l,k,f,d,m)*t3a(f,c,a,m,j,i)     !blkfdmfcamji    (-1.000)
     &     + u6(a,l,k,f,d,m)*t3a(f,c,b,m,j,i)     !alkfdmfcbmji    (+1.000)
     &     - u6(d,l,k,f,c,m)*t3a(f,b,a,m,j,i)     !dlkfcmfbamji    (-1.000)
     &     + u6(d,l,k,f,b,m)*t3a(f,c,a,m,j,i)     !dlkfbmfcamji    (+1.000)
     &     - u6(d,l,k,f,a,m)*t3a(f,c,b,m,j,i)     !dlkfamfcbmji    (-1.000)
     &     + u6(b,l,k,f,c,m)*t3a(f,d,a,m,j,i)     !blkfcmfdamji    (+1.000)
     &     - u6(a,l,k,f,c,m)*t3a(f,d,b,m,j,i)     !alkfcmfdbmji    (-1.000)
     &     - u6(c,l,k,f,b,m)*t3a(f,d,a,m,j,i)     !clkfbmfdamji    (-1.000)
     &     + u6(c,l,k,f,a,m)*t3a(f,d,b,m,j,i)     !clkfamfdbmji    (+1.000)
     &     + u6(a,l,k,f,b,m)*t3a(f,d,c,m,j,i)     !alkfbmfdcmji    (+1.000)
     &     - u6(b,l,k,f,a,m)*t3a(f,d,c,m,j,i)     !blkfamfdcmji    (-1.000)       
             enddo;enddo
             v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call
!     & sum34561278(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z34, 1.000)
!       call
!     & sum24561378(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z34,-1.000)
!       call
!     & sum23561478(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z34, 1.000)
!       call
!     & sum34562178(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z34,-1.000)
!       call
!     & sum24563178(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z34, 1.000)
!       call
!     & sum23564178(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z34,-1.000)
!       call
!     & sum14562378(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z34, 1.000)
!       call
!     & sum13562478(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z34,-1.000)
!       call
!     & sum14563278(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z34,-1.000)
!       call
!     & sum13564278(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z34, 1.000)
!       call
!     & sum12563478(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z34, 1.000)
!       call
!     & sum12564378(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z34,-1.000)
!       call
!     & sum34571268(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z34,-1.000)
!       call
!     & sum24571368(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z34, 1.000)
!       call
!     & sum23571468(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z34,-1.000)
!       call
!     & sum34572168(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z34, 1.000)
!       call
!     & sum24573168(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z34,-1.000)
!       call
!     & sum23574168(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z34, 1.000)
!       call
!     & sum14572368(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z34,-1.000)
!       call
!     & sum13572468(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z34, 1.000)
!       call
!     & sum14573268(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z34, 1.000)
!       call
!     & sum13574268(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z34,-1.000)
!       call
!     & sum12573468(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z34,-1.000)
!       call
!     & sum12574368(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z34, 1.000)
!       call
!     & sum34671258(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z34, 1.000)
!       call
!     & sum24671358(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z34,-1.000)
!       call
!     & sum23671458(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z34, 1.000)
!       call
!     & sum34672158(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z34,-1.000)
!       call
!     & sum24673158(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z34, 1.000)
!       call
!     & sum23674158(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z34,-1.000)
!       call
!     & sum14672358(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z34, 1.000)
!       call
!     & sum13672458(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z34,-1.000)
!       call
!     & sum14673258(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z34,-1.000)
!       call
!     & sum13674258(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z34, 1.000)
!       call
!     & sum12673458(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z34, 1.000)
!       call
!     & sum12674358(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z34,-1.000)
!       call
!     & sum34581267(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z34, 1.000)
!       call
!     & sum24581367(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z34,-1.000)
!       call
!     & sum23581467(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z34, 1.000)
!       call
!     & sum34582167(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z34,-1.000)
!       call
!     & sum24583167(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z34, 1.000)
!       call
!     & sum23584167(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z34,-1.000)
!       call
!     & sum14582367(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z34, 1.000)
!       call
!     & sum13582467(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z34,-1.000)
!       call
!     & sum14583267(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z34,-1.000)
!       call
!     & sum13584267(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z34, 1.000)
!       call
!     & sum12583467(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z34, 1.000)
!       call
!     & sum12584367(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z34,-1.000)
!       call
!     & sum34681257(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z34,-1.000)
!       call
!     & sum24681357(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z34, 1.000)
!       call
!     & sum23681457(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z34,-1.000)
!       call
!     & sum34682157(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z34, 1.000)
!       call
!     & sum24683157(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z34,-1.000)
!       call
!     & sum23684157(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z34, 1.000)
!       call
!     & sum14682357(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z34,-1.000)
!       call
!     & sum13682457(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z34, 1.000)
!       call
!     & sum14683257(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z34, 1.000)
!       call
!     & sum13684257(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z34,-1.000)
!       call
!     & sum12683457(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z34,-1.000)
!       call
!     & sum12684357(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z34, 1.000)
!       call
!     & sum34781256(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z34, 1.000)
!       call
!     & sum24781356(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z34,-1.000)
!       call
!     & sum23781456(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z34, 1.000)
!       call
!     & sum34782156(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z34,-1.000)
!       call
!     & sum24783156(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z34, 1.000)
!       call
!     & sum23784156(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z34,-1.000)
!       call
!     & sum14782356(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z34, 1.000)
!       call
!     & sum13782456(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z34,-1.000)
!       call
!     & sum14783256(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z34,-1.000)
!       call
!     & sum13784256(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z34, 1.000)
!       call
!     & sum12783456(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z34, 1.000)
!       call
!     & sum12784356(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z34,-1.000)
!c
!       do i=n0+1,n1-3
!       do j=i+1,n1-2
!       do k=j+1,n1-1
!       do l=k+1,n1
!       do a=n1+1,n3-3
!       do b=a+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)
!     & +z34(b,a,l,k,d,c,j,i)       ! 34561278 (+1.000)
!     & -z34(c,a,l,k,d,b,j,i)       ! 24561378 (-1.000)
!     & +z34(c,b,l,k,d,a,j,i)       ! 23561478 (+1.000)
!     & -z34(b,a,l,k,c,d,j,i)       ! 34562178 (-1.000)
!     & +z34(c,a,l,k,b,d,j,i)       ! 24563178 (+1.000)
!     & -z34(c,b,l,k,a,d,j,i)       ! 23564178 (-1.000)
!     & +z34(d,a,l,k,c,b,j,i)       ! 14562378 (+1.000)
!     & -z34(d,b,l,k,c,a,j,i)       ! 13562478 (-1.000)
!     & -z34(d,a,l,k,b,c,j,i)       ! 14563278 (-1.000)
!     & +z34(d,b,l,k,a,c,j,i)       ! 13564278 (+1.000)
!     & +z34(d,c,l,k,b,a,j,i)       ! 12563478 (+1.000)
!     & -z34(d,c,l,k,a,b,j,i)       ! 12564378 (-1.000)
!     & -z34(b,a,l,j,d,c,k,i)       ! 34571268 (-1.000)
!     & +z34(c,a,l,j,d,b,k,i)       ! 24571368 (+1.000)
!     & -z34(c,b,l,j,d,a,k,i)       ! 23571468 (-1.000)
!     & +z34(b,a,l,j,c,d,k,i)       ! 34572168 (+1.000)
!     & -z34(c,a,l,j,b,d,k,i)       ! 24573168 (-1.000)
!     & +z34(c,b,l,j,a,d,k,i)       ! 23574168 (+1.000)
!     & -z34(d,a,l,j,c,b,k,i)       ! 14572368 (-1.000)
!     & +z34(d,b,l,j,c,a,k,i)       ! 13572468 (+1.000)
!     & +z34(d,a,l,j,b,c,k,i)       ! 14573268 (+1.000)
!     & -z34(d,b,l,j,a,c,k,i)       ! 13574268 (-1.000)
!     & -z34(d,c,l,j,b,a,k,i)       ! 12573468 (-1.000)
!     & +z34(d,c,l,j,a,b,k,i)       ! 12574368 (+1.000)
!     & +z34(b,a,k,j,d,c,l,i)       ! 34671258 (+1.000)
!     & -z34(c,a,k,j,d,b,l,i)       ! 24671358 (-1.000)
!     & +z34(c,b,k,j,d,a,l,i)       ! 23671458 (+1.000)
!     & -z34(b,a,k,j,c,d,l,i)       ! 34672158 (-1.000)
!     & +z34(c,a,k,j,b,d,l,i)       ! 24673158 (+1.000)
!     & -z34(c,b,k,j,a,d,l,i)       ! 23674158 (-1.000)
!     & +z34(d,a,k,j,c,b,l,i)       ! 14672358 (+1.000)
!     & -z34(d,b,k,j,c,a,l,i)       ! 13672458 (-1.000)
!     & -z34(d,a,k,j,b,c,l,i)       ! 14673258 (-1.000)
!     & +z34(d,b,k,j,a,c,l,i)       ! 13674258 (+1.000)
!     & +z34(d,c,k,j,b,a,l,i)       ! 12673458 (+1.000)
!     & -z34(d,c,k,j,a,b,l,i)       ! 12674358 (-1.000)
!     & +z34(b,a,l,i,d,c,k,j)       ! 34581267 (+1.000)
!     & -z34(c,a,l,i,d,b,k,j)       ! 24581367 (-1.000)
!     & +z34(c,b,l,i,d,a,k,j)       ! 23581467 (+1.000)
!     & -z34(b,a,l,i,c,d,k,j)       ! 34582167 (-1.000)
!     & +z34(c,a,l,i,b,d,k,j)       ! 24583167 (+1.000)
!     & -z34(c,b,l,i,a,d,k,j)       ! 23584167 (-1.000)
!     & +z34(d,a,l,i,c,b,k,j)       ! 14582367 (+1.000)
!     & -z34(d,b,l,i,c,a,k,j)       ! 13582467 (-1.000)
!     & -z34(d,a,l,i,b,c,k,j)       ! 14583267 (-1.000)
!     & +z34(d,b,l,i,a,c,k,j)       ! 13584267 (+1.000)
!     & +z34(d,c,l,i,b,a,k,j)       ! 12583467 (+1.000)
!     & -z34(d,c,l,i,a,b,k,j)       ! 12584367 (-1.000)
!     & -z34(b,a,k,i,d,c,l,j)       ! 34681257 (-1.000)
!     & +z34(c,a,k,i,d,b,l,j)       ! 24681357 (+1.000)
!     & -z34(c,b,k,i,d,a,l,j)       ! 23681457 (-1.000)
!     & +z34(b,a,k,i,c,d,l,j)       ! 34682157 (+1.000)
!     & -z34(c,a,k,i,b,d,l,j)       ! 24683157 (-1.000)
!     & +z34(c,b,k,i,a,d,l,j)       ! 23684157 (+1.000)
!     & -z34(d,a,k,i,c,b,l,j)       ! 14682357 (-1.000)
!     & +z34(d,b,k,i,c,a,l,j)       ! 13682457 (+1.000)
!     & +z34(d,a,k,i,b,c,l,j)       ! 14683257 (+1.000)
!     & -z34(d,b,k,i,a,c,l,j)       ! 13684257 (-1.000)
!     & -z34(d,c,k,i,b,a,l,j)       ! 12683457 (-1.000)
!     & +z34(d,c,k,i,a,b,l,j)       ! 12684357 (+1.000)
!     & +z34(b,a,j,i,d,c,l,k)       ! 34781256 (+1.000)
!     & -z34(c,a,j,i,d,b,l,k)       ! 24781356 (-1.000)
!     & +z34(c,b,j,i,d,a,l,k)       ! 23781456 (+1.000)
!     & -z34(b,a,j,i,c,d,l,k)       ! 34782156 (-1.000)
!     & +z34(c,a,j,i,b,d,l,k)       ! 24783156 (+1.000)
!     & -z34(c,b,j,i,a,d,l,k)       ! 23784156 (-1.000)
!     & +z34(d,a,j,i,c,b,l,k)       ! 14782356 (+1.000)
!     & -z34(d,b,j,i,c,a,l,k)       ! 13782456 (-1.000)
!     & -z34(d,a,j,i,b,c,l,k)       ! 14783256 (-1.000)
!     & +z34(d,b,j,i,a,c,l,k)       ! 13784256 (+1.000)
!     & +z34(d,c,j,i,b,a,l,k)       ! 12783456 (+1.000)
!     & -z34(d,c,j,i,a,b,l,k)       ! 12784356 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z34)
       deallocate(u6)
c
       allocate(d1(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder4312(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n1,n3,n1,n3,n0,n1,intr,d1)
       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
       allocate(s15(n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1))
       i1=k1*k3
       i2=k1*k1
       i3=k3*k3
       call egemm(i1,i2,i3,d1,d2,s15)
       deallocate(d1)
       deallocate(d2)
c
       call sum3421(n0,n1,n1,n3,n0,n1,n0,n1,x1,s15, 0.500)
       deallocate(s15)
c
       allocate(d1(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder4312(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n1,n3,n1,n3,n0,n1,intr,d1)
       allocate(f2(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       call reorder123456(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,
     & n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,t3a,f2)
       allocate(u7(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1))
       i1=k1*k3
       i2=k1*k1*k1*k3
       i3=k3*k3
       call egemm(i1,i2,i3,d1,f2,u7)
       deallocate(d1)
       deallocate(f2)
c
!       allocate(f1(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
!       call reorder651234(n1,n3,n0,n1,n0,n1,n0,n1,n1,n3,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,u7,f1)
!       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
!       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
!       allocate(z36(n1+1:n3,n1+1:n3,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
!       i1=k1*k1*k1*k3*k3
!       i2=k1*k3*k3
!       i3=k1
!       call egemm(i1,i2,i3,f1,d2,z36)
!       deallocate(f1)
!       deallocate(d2)
!c
       do i=n0+1,n1-3;do j=i+1,n1-2;do k=j+1,n1-1;do l=k+1,n1
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-3;do b=a+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
          if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1
             sum=sum                            !top 2 switched
     &     + (u7(b,l,k,j,d,m)*t2a(c,a,m,i)      !blkjdmcami      (+0.500)
     &     - u7(a,l,k,j,d,m)*t2a(c,b,m,i)       !alkjdmcbmi      (-0.500)
     &     - u7(c,l,k,j,d,m)*t2a(b,a,m,i)       !clkjdmbami      (-0.500)
     &     + u7(a,l,k,j,c,m)*t2a(d,b,m,i)       !alkjcmdbmi      (+0.500)
     &     - u7(b,l,k,j,c,m)*t2a(d,a,m,i)       !blkjcmdami      (-0.500)
     &     - u7(a,l,k,j,b,m)*t2a(d,c,m,i)       !alkjbmdcmi      (-0.500)
     &     + u7(b,l,k,j,a,m)*t2a(d,c,m,i)       !blkjamdcmi      (+0.500)
     &     + u7(c,l,k,j,b,m)*t2a(d,a,m,i)       !clkjbmdami      (+0.500)
     &     - u7(c,l,k,j,a,m)*t2a(d,b,m,i)       !clkjamdbmi      (-0.500)
     &     + u7(d,l,k,j,c,m)*t2a(b,a,m,i)       !dlkjcmbami      (+0.500)
     &     - u7(d,l,k,j,b,m)*t2a(c,a,m,i)       !dlkjbmcami      (-0.500)
     &     + u7(d,l,k,j,a,m)*t2a(c,b,m,i)       !dlkjamcbmi      (+0.500)
     &     + u7(a,l,k,i,d,m)*t2a(c,b,m,j)       !alkidmcbmj      (+0.500)
     &     - u7(b,l,k,i,d,m)*t2a(c,a,m,j)       !blkidmcamj      (-0.500)
     &     + u7(c,l,k,i,d,m)*t2a(b,a,m,j)       !clkidmbamj      (+0.500)
     &     - u7(a,l,k,i,c,m)*t2a(d,b,m,j)       !alkicmdbmj      (-0.500)
     &     + u7(b,l,k,i,c,m)*t2a(d,a,m,j)       !blkicmdamj      (+0.500)
     &     + u7(a,l,k,i,b,m)*t2a(d,c,m,j)       !alkibmdcmj      (+0.500)
     &     - u7(b,l,k,i,a,m)*t2a(d,c,m,j)       !blkiamdcmj      (-0.500)
     &     - u7(c,l,k,i,b,m)*t2a(d,a,m,j)       !clkibmdamj      (-0.500)
     &     + u7(c,l,k,i,a,m)*t2a(d,b,m,j)       !clkiamdbmj      (+0.500)
     &     - u7(d,l,k,i,c,m)*t2a(b,a,m,j)       !dlkicmbamj      (-0.500)
     &     + u7(d,l,k,i,b,m)*t2a(c,a,m,j)       !dlkibmcamj      (+0.500)
     &     - u7(d,l,k,i,a,m)*t2a(c,b,m,j)       !dlkiamcbmj      (-0.500)
     &     - u7(a,l,j,i,d,m)*t2a(c,b,m,k)       !aljidmcbmk      (-0.500)
     &     + u7(b,l,j,i,d,m)*t2a(c,a,m,k)       !bljidmcamk      (+0.500)
     &     - u7(c,l,j,i,d,m)*t2a(b,a,m,k)       !cljidmbamk      (-0.500)
     &     + u7(a,l,j,i,c,m)*t2a(d,b,m,k)       !aljicmdbmk      (+0.500)
     &     - u7(b,l,j,i,c,m)*t2a(d,a,m,k)       !bljicmdamk      (-0.500)
     &     - u7(a,l,j,i,b,m)*t2a(d,c,m,k)       !aljibmdcmk      (-0.500)
     &     + u7(b,l,j,i,a,m)*t2a(d,c,m,k)       !bljiamdcmk      (+0.500)
     &     + u7(c,l,j,i,b,m)*t2a(d,a,m,k)       !cljibmdamk      (+0.500)
     &     - u7(c,l,j,i,a,m)*t2a(d,b,m,k)       !cljiamdbmk      (-0.500)
     &     + u7(d,l,j,i,c,m)*t2a(b,a,m,k)       !dljicmbamk      (+0.500)
     &     - u7(d,l,j,i,b,m)*t2a(c,a,m,k)       !dljibmcamk      (-0.500)
     &     + u7(d,l,j,i,a,m)*t2a(c,b,m,k)       !dljiamcbmk      (+0.500)
     &     + u7(a,k,j,i,d,m)*t2a(c,b,m,l)       !akjidmcbml      (+0.500)
     &     - u7(b,k,j,i,d,m)*t2a(c,a,m,l)       !bkjidmcaml      (-0.500)
     &     + u7(c,k,j,i,d,m)*t2a(b,a,m,l)       !ckjidmbaml      (+0.500)
     &     - u7(a,k,j,i,c,m)*t2a(d,b,m,l)       !akjicmdbml      (-0.500)
     &     + u7(b,k,j,i,c,m)*t2a(d,a,m,l)       !bkjicmdaml      (+0.500)
     &     + u7(a,k,j,i,b,m)*t2a(d,c,m,l)       !akjibmdcml      (+0.500)
     &     - u7(b,k,j,i,a,m)*t2a(d,c,m,l)       !bkjiamdcml      (-0.500)
     &     - u7(c,k,j,i,b,m)*t2a(d,a,m,l)       !ckjibmdaml      (-0.500)
     &     + u7(c,k,j,i,a,m)*t2a(d,b,m,l)       !ckjiamdbml      (+0.500)
     &     - u7(d,k,j,i,c,m)*t2a(b,a,m,l)       !dkjicmbaml      (-0.500)
     &     + u7(d,k,j,i,b,m)*t2a(c,a,m,l)       !dkjibmcaml      (+0.500)
     &     - u7(d,k,j,i,a,m)*t2a(c,b,m,l))/2.0d0!dkjiamcbml      (-0.500)             
             enddo
             v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call
!     & sum23814567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z36,-0.500)
!       call
!     & sum24813567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z36, 0.500)
!       call
!     & sum34812567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z36,-0.500)
!       call
!     & sum13824567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z36, 0.500)
!       call
!     & sum14823567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z36,-0.500)
!       call
!     & sum12834567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z36,-0.500)
!       call
!     & sum12843567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z36, 0.500)
!       call
!     & sum14832567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z36, 0.500)
!       call
!     & sum13842567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z36,-0.500)
!       call
!     & sum34821567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z36, 0.500)
!       call
!     & sum24831567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z36,-0.500)
!       call
!     & sum23841567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z36, 0.500)
!       call
!     & sum23714568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z36, 0.500)
!       call
!     & sum24713568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z36,-0.500)
!       call
!     & sum34712568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z36, 0.500)
!       call
!     & sum13724568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z36,-0.500)
!       call
!     & sum14723568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z36, 0.500)
!       call
!     & sum12734568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z36, 0.500)
!       call
!     & sum12743568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z36,-0.500)
!       call
!     & sum14732568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z36,-0.500)
!       call
!     & sum13742568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z36, 0.500)
!       call
!     & sum34721568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z36,-0.500)
!       call
!     & sum24731568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z36, 0.500)
!       call
!     & sum23741568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z36,-0.500)
!       call
!     & sum23614578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z36,-0.500)
!       call
!     & sum24613578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z36, 0.500)
!       call
!     & sum34612578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z36,-0.500)
!       call
!     & sum13624578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z36, 0.500)
!       call
!     & sum14623578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z36,-0.500)
!       call
!     & sum12634578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z36,-0.500)
!       call
!     & sum12643578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z36, 0.500)
!       call
!     & sum14632578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z36, 0.500)
!       call
!     & sum13642578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z36,-0.500)
!       call
!     & sum34621578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z36, 0.500)
!       call
!     & sum24631578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z36,-0.500)
!       call
!     & sum23641578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z36, 0.500)
!       call
!     & sum23514678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z36, 0.500)
!       call
!     & sum24513678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z36,-0.500)
!       call
!     & sum34512678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z36, 0.500)
!       call
!     & sum13524678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z36,-0.500)
!       call
!     & sum14523678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z36, 0.500)
!       call
!     & sum12534678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z36, 0.500)
!       call
!     & sum12543678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z36,-0.500)
!       call
!     & sum14532678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z36,-0.500)
!       call
!     & sum13542678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z36, 0.500)
!       call
!     & sum34521678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z36,-0.500)
!       call
!     & sum24531678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z36, 0.500)
!       call
!     & sum23541678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z36,-0.500)
!c
!       do i=n0+1,n1-3
!       do j=i+1,n1-2
!       do k=j+1,n1-1
!       do l=k+1,n1
!       do a=n1+1,n3-3
!       do b=a+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)
!     & +(z36(c,a,i,d,b,l,k,j)      ! 24813567 (+0.500) top 2 switched
!     & -z36(c,b,i,d,a,l,k,j)       ! 23814567 (-0.500)
!     & -z36(b,a,i,d,c,l,k,j)       ! 34812567 (-0.500)
!     & +z36(d,b,i,c,a,l,k,j)       ! 13824567 (+0.500)
!     & -z36(d,a,i,c,b,l,k,j)       ! 14823567 (-0.500)
!     & -z36(d,c,i,b,a,l,k,j)       ! 12834567 (-0.500)
!     & +z36(d,c,i,a,b,l,k,j)       ! 12843567 (+0.500)
!     & +z36(d,a,i,b,c,l,k,j)       ! 14832567 (+0.500)
!     & -z36(d,b,i,a,c,l,k,j)       ! 13842567 (-0.500)
!     & +z36(b,a,i,c,d,l,k,j)       ! 34821567 (+0.500)
!     & -z36(c,a,i,b,d,l,k,j)       ! 24831567 (-0.500)
!     & +z36(c,b,i,a,d,l,k,j)       ! 23841567 (+0.500)
!     & +z36(c,b,j,d,a,l,k,i)       ! 23714568 (+0.500)
!     & -z36(c,a,j,d,b,l,k,i)       ! 24713568 (-0.500)
!     & +z36(b,a,j,d,c,l,k,i)       ! 34712568 (+0.500)
!     & -z36(d,b,j,c,a,l,k,i)       ! 13724568 (-0.500)
!     & +z36(d,a,j,c,b,l,k,i)       ! 14723568 (+0.500)
!     & +z36(d,c,j,b,a,l,k,i)       ! 12734568 (+0.500)
!     & -z36(d,c,j,a,b,l,k,i)       ! 12743568 (-0.500)
!     & -z36(d,a,j,b,c,l,k,i)       ! 14732568 (-0.500)
!     & +z36(d,b,j,a,c,l,k,i)       ! 13742568 (+0.500)
!     & -z36(b,a,j,c,d,l,k,i)       ! 34721568 (-0.500)
!     & +z36(c,a,j,b,d,l,k,i)       ! 24731568 (+0.500)
!     & -z36(c,b,j,a,d,l,k,i)       ! 23741568 (-0.500)
!     & -z36(c,b,k,d,a,l,j,i)       ! 23614578 (-0.500)
!     & +z36(c,a,k,d,b,l,j,i)       ! 24613578 (+0.500)
!     & -z36(b,a,k,d,c,l,j,i)       ! 34612578 (-0.500)
!     & +z36(d,b,k,c,a,l,j,i)       ! 13624578 (+0.500)
!     & -z36(d,a,k,c,b,l,j,i)       ! 14623578 (-0.500)
!     & -z36(d,c,k,b,a,l,j,i)       ! 12634578 (-0.500)
!     & +z36(d,c,k,a,b,l,j,i)       ! 12643578 (+0.500)
!     & +z36(d,a,k,b,c,l,j,i)       ! 14632578 (+0.500)
!     & -z36(d,b,k,a,c,l,j,i)       ! 13642578 (-0.500)
!     & +z36(b,a,k,c,d,l,j,i)       ! 34621578 (+0.500)
!     & -z36(c,a,k,b,d,l,j,i)       ! 24631578 (-0.500)
!     & +z36(c,b,k,a,d,l,j,i)       ! 23641578 (+0.500)
!     & +z36(c,b,l,d,a,k,j,i)       ! 23514678 (+0.500)
!     & -z36(c,a,l,d,b,k,j,i)       ! 24513678 (-0.500)
!     & +z36(b,a,l,d,c,k,j,i)       ! 34512678 (+0.500)
!     & -z36(d,b,l,c,a,k,j,i)       ! 13524678 (-0.500)
!     & +z36(d,a,l,c,b,k,j,i)       ! 14523678 (+0.500)
!     & +z36(d,c,l,b,a,k,j,i)       ! 12534678 (+0.500)
!     & -z36(d,c,l,a,b,k,j,i)       ! 12543678 (-0.500)
!     & -z36(d,a,l,b,c,k,j,i)       ! 14532678 (-0.500)
!     & +z36(d,b,l,a,c,k,j,i)       ! 13542678 (+0.500)
!     & -z36(b,a,l,c,d,k,j,i)       ! 34521678 (-0.500)
!     & +z36(c,a,l,b,d,k,j,i)       ! 24531678 (+0.500)
!     & -z36(c,b,l,a,d,k,j,i))/2.0d0! 23541678 (-0.500)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z36)
       deallocate(u7)
c
       allocate(d1(n1+1:n3,n0+1:n1,n1+1:n3,n1+1:n3))
       call reorder3241(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n1,n3,n1,n3,intr,d1)
       allocate(d2(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       call reorder1324(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n0,n1,n1,n3,n0,n1,t2a,d2)
       allocate(s16(n1+1:n3,n0+1:n1,n1+1:n3,n1+1:n3))
       i1=k3*k3
       i2=k1*k3
       i3=k1*k3
       call egemm(i1,i2,i3,d1,d2,s16)
       deallocate(d1)
       deallocate(d2)
c
       call sum3412(n1,n3,n1,n3,n1,n3,n0,n1,x10,s16,-1.000)
       deallocate(s16)
c
       allocate(d1(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       call reorder4213(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n0,n1,n0,n1,intm,d1)
       allocate(f2(n2+1:n3,n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder142356(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n2,n3,n0,n2,n1,n3,n1,n3,n0,n1,n0,n1,t3b,f2)
       allocate(u8(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1
       i2=k1*k1*k3*k3
       i3=k2*k4
       call egemm(i1,i2,i3,d1,f2,u8)
       deallocate(d1)
       deallocate(f2)
c
!       allocate(f1(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
!       call reorder512346(n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,u8,f1)
!       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
!       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
!       allocate(z38(n1+1:n3,n1+1:n3,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
!       i1=k1*k1*k1*k3*k3
!       i2=k1*k3*k3
!       i3=k1
!       call egemm(i1,i2,i3,f1,d2,z38)
!       deallocate(f1)
!       deallocate(d2)
c
       do i=n0+1,n1-3;do j=i+1,n1-2;do k=j+1,n1-1;do l=k+1,n1
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-3;do b=a+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
          if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1
             sum=sum
     &     + u8(b,a,l,k,m,i)*t2a(d,c,m,j)       !balkmidcmj      (+1.000)
     &     - u8(c,a,l,k,m,i)*t2a(d,b,m,j)       !calkmidbmj      (-1.000)
     &     + u8(c,b,l,k,m,i)*t2a(d,a,m,j)       !cblkmidamj      (+1.000)
     &     + u8(d,a,l,k,m,i)*t2a(c,b,m,j)       !dalkmicbmj      (+1.000)
     &     - u8(d,b,l,k,m,i)*t2a(c,a,m,j)       !dblkmicamj      (-1.000)
     &     + u8(d,c,l,k,m,i)*t2a(b,a,m,j)       !dclkmibamj      (+1.000)
     &     - u8(b,a,l,j,m,i)*t2a(d,c,m,k)       !baljmidcmk      (-1.000)
     &     + u8(c,a,l,j,m,i)*t2a(d,b,m,k)       !caljmidbmk      (+1.000)
     &     - u8(c,b,l,j,m,i)*t2a(d,a,m,k)       !cbljmidamk      (-1.000)
     &     - u8(d,a,l,j,m,i)*t2a(c,b,m,k)       !daljmicbmk      (-1.000)
     &     + u8(d,b,l,j,m,i)*t2a(c,a,m,k)       !dbljmicamk      (+1.000)
     &     - u8(d,c,l,j,m,i)*t2a(b,a,m,k)       !dcljmibamk      (-1.000)
     &     + u8(b,a,k,j,m,i)*t2a(d,c,m,l)       !bakjmidcml      (+1.000)
     &     - u8(c,a,k,j,m,i)*t2a(d,b,m,l)       !cakjmidbml      (-1.000)
     &     + u8(c,b,k,j,m,i)*t2a(d,a,m,l)       !cbkjmidaml      (+1.000)
     &     + u8(d,a,k,j,m,i)*t2a(c,b,m,l)       !dakjmicbml      (+1.000)
     &     - u8(d,b,k,j,m,i)*t2a(c,a,m,l)       !dbkjmicaml      (-1.000)
     &     + u8(d,c,k,j,m,i)*t2a(b,a,m,l)       !dckjmibaml      (+1.000)
     &     - u8(b,a,l,k,m,j)*t2a(d,c,m,i)       !balkmjdcmi      (-1.000)
     &     + u8(c,a,l,k,m,j)*t2a(d,b,m,i)       !calkmjdbmi      (+1.000)
     &     - u8(c,b,l,k,m,j)*t2a(d,a,m,i)       !cblkmjdami      (-1.000)
     &     - u8(d,a,l,k,m,j)*t2a(c,b,m,i)       !dalkmjcbmi      (-1.000)
     &     + u8(d,b,l,k,m,j)*t2a(c,a,m,i)       !dblkmjcami      (+1.000)
     &     - u8(d,c,l,k,m,j)*t2a(b,a,m,i)       !dclkmjbami      (-1.000)
     &     + u8(b,a,l,j,m,k)*t2a(d,c,m,i)       !baljmkdcmi      (+1.000)
     &     - u8(c,a,l,j,m,k)*t2a(d,b,m,i)       !caljmkdbmi      (-1.000)
     &     + u8(c,b,l,j,m,k)*t2a(d,a,m,i)       !cbljmkdami      (+1.000)
     &     + u8(d,a,l,j,m,k)*t2a(c,b,m,i)       !daljmkcbmi      (+1.000)
     &     - u8(d,b,l,j,m,k)*t2a(c,a,m,i)       !dbljmkcami      (-1.000)
     &     + u8(d,c,l,j,m,k)*t2a(b,a,m,i)       !dcljmkbami      (+1.000)
     &     - u8(b,a,k,j,m,l)*t2a(d,c,m,i)       !bakjmldcmi      (-1.000)
     &     + u8(c,a,k,j,m,l)*t2a(d,b,m,i)       !cakjmldbmi      (+1.000)
     &     - u8(c,b,k,j,m,l)*t2a(d,a,m,i)       !cbkjmldami      (-1.000)
     &     - u8(d,a,k,j,m,l)*t2a(c,b,m,i)       !dakjmlcbmi      (-1.000)
     &     + u8(d,b,k,j,m,l)*t2a(c,a,m,i)       !dbkjmlcami      (+1.000)
     &     - u8(d,c,k,j,m,l)*t2a(b,a,m,i)       !dckjmlbami      (-1.000)
     &     + u8(b,a,l,i,m,j)*t2a(d,c,m,k)       !balimjdcmk      (+1.000)
     &     - u8(c,a,l,i,m,j)*t2a(d,b,m,k)       !calimjdbmk      (-1.000)
     &     + u8(c,b,l,i,m,j)*t2a(d,a,m,k)       !cblimjdamk      (+1.000)
     &     + u8(d,a,l,i,m,j)*t2a(c,b,m,k)       !dalimjcbmk      (+1.000)
     &     - u8(d,b,l,i,m,j)*t2a(c,a,m,k)       !dblimjcamk      (-1.000)
     &     + u8(d,c,l,i,m,j)*t2a(b,a,m,k)       !dclimjbamk      (+1.000)
     &     - u8(b,a,k,i,m,j)*t2a(d,c,m,l)       !bakimjdcml      (-1.000)
     &     + u8(c,a,k,i,m,j)*t2a(d,b,m,l)       !cakimjdbml      (+1.000)
     &     - u8(c,b,k,i,m,j)*t2a(d,a,m,l)       !cbkimjdaml      (-1.000)
     &     - u8(d,a,k,i,m,j)*t2a(c,b,m,l)       !dakimjcbml      (-1.000)
     &     + u8(d,b,k,i,m,j)*t2a(c,a,m,l)       !dbkimjcaml      (+1.000)
     &     - u8(d,c,k,i,m,j)*t2a(b,a,m,l)       !dckimjbaml      (-1.000)
     &     - u8(b,a,l,i,m,k)*t2a(d,c,m,j)       !balimkdcmj      (-1.000)
     &     + u8(c,a,l,i,m,k)*t2a(d,b,m,j)       !calimkdbmj      (+1.000)
     &     - u8(c,b,l,i,m,k)*t2a(d,a,m,j)       !cblimkdamj      (-1.000)
     &     - u8(d,a,l,i,m,k)*t2a(c,b,m,j)       !dalimkcbmj      (-1.000)
     &     + u8(d,b,l,i,m,k)*t2a(c,a,m,j)       !dblimkcamj      (+1.000)
     &     - u8(d,c,l,i,m,k)*t2a(b,a,m,j)       !dclimkbamj      (-1.000)
     &     + u8(b,a,k,i,m,l)*t2a(d,c,m,j)       !bakimldcmj      (+1.000)
     &     - u8(c,a,k,i,m,l)*t2a(d,b,m,j)       !cakimldbmj      (-1.000)
     &     + u8(c,b,k,i,m,l)*t2a(d,a,m,j)       !cbkimldamj      (+1.000)
     &     + u8(d,a,k,i,m,l)*t2a(c,b,m,j)       !dakimlcbmj      (+1.000)
     &     - u8(d,b,k,i,m,l)*t2a(c,a,m,j)       !dbkimlcamj      (-1.000)
     &     + u8(d,c,k,i,m,l)*t2a(b,a,m,j)       !dckimlbamj      (+1.000)
     &     + u8(b,a,j,i,m,k)*t2a(d,c,m,l)       !bajimkdcml      (+1.000)
     &     - u8(c,a,j,i,m,k)*t2a(d,b,m,l)       !cajimkdbml      (-1.000)
     &     + u8(c,b,j,i,m,k)*t2a(d,a,m,l)       !cbjimkdaml      (+1.000)
     &     + u8(d,a,j,i,m,k)*t2a(c,b,m,l)       !dajimkcbml      (+1.000)
     &     - u8(d,b,j,i,m,k)*t2a(c,a,m,l)       !dbjimkcaml      (-1.000)
     &     + u8(d,c,j,i,m,k)*t2a(b,a,m,l)       !dcjimkbaml      (+1.000)
     &     - u8(b,a,j,i,m,l)*t2a(d,c,m,k)       !bajimldcmk      (-1.000)
     &     + u8(c,a,j,i,m,l)*t2a(d,b,m,k)       !cajimldbmk      (+1.000)
     &     - u8(c,b,j,i,m,l)*t2a(d,a,m,k)       !cbjimldamk      (-1.000)
     &     - u8(d,a,j,i,m,l)*t2a(c,b,m,k)       !dajimlcbmk      (-1.000)
     &     + u8(d,b,j,i,m,l)*t2a(c,a,m,k)       !dbjimlcamk      (+1.000)
     &     - u8(d,c,j,i,m,l)*t2a(b,a,m,k)       !dcjimlbamk      (-1.000)        
             enddo
             v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call
!     & sum12734568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z38, 1.000)
!       call
!     & sum13724568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z38,-1.000)
!       call
!     & sum14723568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z38, 1.000)
!       call
!     & sum23714568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z38, 1.000)
!       call
!     & sum24713568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z38,-1.000)
!       call
!     & sum34712568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z38, 1.000)
!       call
!     & sum12634578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z38,-1.000)
!       call
!     & sum13624578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z38, 1.000)
!       call
!     & sum14623578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z38,-1.000)
!       call
!     & sum23614578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z38,-1.000)
!       call
!     & sum24613578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z38, 1.000)
!       call
!     & sum34612578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z38,-1.000)
!       call
!     & sum12534678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z38, 1.000)
!       call
!     & sum13524678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z38,-1.000)
!       call
!     & sum14523678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z38, 1.000)
!       call
!     & sum23514678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z38, 1.000)
!       call
!     & sum24513678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z38,-1.000)
!       call
!     & sum34512678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z38, 1.000)
!       call
!     & sum12834567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z38,-1.000)
!       call
!     & sum13824567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z38, 1.000)
!       call
!     & sum14823567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z38,-1.000)
!       call
!     & sum23814567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z38,-1.000)
!       call
!     & sum24813567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z38, 1.000)
!       call
!     & sum34812567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z38,-1.000)
!       call
!     & sum12834576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z38, 1.000)
!       call
!     & sum13824576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z38,-1.000)
!       call
!     & sum14823576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z38, 1.000)
!       call
!     & sum23814576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z38, 1.000)
!       call
!     & sum24813576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z38,-1.000)
!       call
!     & sum34812576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z38, 1.000)
!       call
!     & sum12834675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z38,-1.000)
!       call
!     & sum13824675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z38, 1.000)
!       call
!     & sum14823675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z38,-1.000)
!       call
!     & sum23814675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z38,-1.000)
!       call
!     & sum24813675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z38, 1.000)
!       call
!     & sum34812675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z38,-1.000)
!       call
!     & sum12634587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z38, 1.000)
!       call
!     & sum13624587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z38,-1.000)
!       call
!     & sum14623587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z38, 1.000)
!       call
!     & sum23614587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z38, 1.000)
!       call
!     & sum24613587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z38,-1.000)
!       call
!     & sum34612587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z38, 1.000)
!       call
!     & sum12534687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z38,-1.000)
!       call
!     & sum13524687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z38, 1.000)
!       call
!     & sum14523687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z38,-1.000)
!       call
!     & sum23514687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z38,-1.000)
!       call
!     & sum24513687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z38, 1.000)
!       call
!     & sum34512687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z38,-1.000)
!       call
!     & sum12734586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z38,-1.000)
!       call
!     & sum13724586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z38, 1.000)
!       call
!     & sum14723586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z38,-1.000)
!       call
!     & sum23714586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z38,-1.000)
!       call
!     & sum24713586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z38, 1.000)
!       call
!     & sum34712586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z38,-1.000)
!       call
!     & sum12734685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z38, 1.000)
!       call
!     & sum13724685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z38,-1.000)
!       call
!     & sum14723685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z38, 1.000)
!       call
!     & sum23714685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z38, 1.000)
!       call
!     & sum24713685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z38,-1.000)
!       call
!     & sum34712685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z38, 1.000)
!       call
!     & sum12534786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z38, 1.000)
!       call
!     & sum13524786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z38,-1.000)
!       call
!     & sum14523786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z38, 1.000)
!       call
!     & sum23514786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z38, 1.000)
!       call
!     & sum24513786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z38,-1.000)
!       call
!     & sum34512786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z38, 1.000)
!       call
!     & sum12634785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z38,-1.000)
!       call
!     & sum13624785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z38, 1.000)
!       call
!     & sum14623785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z38,-1.000)
!       call
!     & sum23614785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z38,-1.000)
!       call
!     & sum24613785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z38, 1.000)
!       call
!     & sum34612785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z38,-1.000)
!c
!       do i=n0+1,n1-3
!       do j=i+1,n1-2
!       do k=j+1,n1-1
!       do l=k+1,n1
!       do a=n1+1,n3-3
!       do b=a+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)
!     & +z38(d,c,j,b,a,l,k,i)       ! 12734568 (+1.000)
!     & -z38(d,b,j,c,a,l,k,i)       ! 13724568 (-1.000)
!     & +z38(d,a,j,c,b,l,k,i)       ! 14723568 (+1.000)
!     & +z38(c,b,j,d,a,l,k,i)       ! 23714568 (+1.000)
!     & -z38(c,a,j,d,b,l,k,i)       ! 24713568 (-1.000)
!     & +z38(b,a,j,d,c,l,k,i)       ! 34712568 (+1.000)
!     & -z38(d,c,k,b,a,l,j,i)       ! 12634578 (-1.000)
!     & +z38(d,b,k,c,a,l,j,i)       ! 13624578 (+1.000)
!     & -z38(d,a,k,c,b,l,j,i)       ! 14623578 (-1.000)
!     & -z38(c,b,k,d,a,l,j,i)       ! 23614578 (-1.000)
!     & +z38(c,a,k,d,b,l,j,i)       ! 24613578 (+1.000)
!     & -z38(b,a,k,d,c,l,j,i)       ! 34612578 (-1.000)
!     & +z38(d,c,l,b,a,k,j,i)       ! 12534678 (+1.000)
!     & -z38(d,b,l,c,a,k,j,i)       ! 13524678 (-1.000)
!     & +z38(d,a,l,c,b,k,j,i)       ! 14523678 (+1.000)
!     & +z38(c,b,l,d,a,k,j,i)       ! 23514678 (+1.000)
!     & -z38(c,a,l,d,b,k,j,i)       ! 24513678 (-1.000)
!     & +z38(b,a,l,d,c,k,j,i)       ! 34512678 (+1.000)
!     & -z38(d,c,i,b,a,l,k,j)       ! 12834567 (-1.000)
!     & +z38(d,b,i,c,a,l,k,j)       ! 13824567 (+1.000)
!     & -z38(d,a,i,c,b,l,k,j)       ! 14823567 (-1.000)
!     & -z38(c,b,i,d,a,l,k,j)       ! 23814567 (-1.000)
!     & +z38(c,a,i,d,b,l,k,j)       ! 24813567 (+1.000)
!     & -z38(b,a,i,d,c,l,k,j)       ! 34812567 (-1.000)
!     & +z38(d,c,i,b,a,l,j,k)       ! 12834576 (+1.000)
!     & -z38(d,b,i,c,a,l,j,k)       ! 13824576 (-1.000)
!     & +z38(d,a,i,c,b,l,j,k)       ! 14823576 (+1.000)
!     & +z38(c,b,i,d,a,l,j,k)       ! 23814576 (+1.000)
!     & -z38(c,a,i,d,b,l,j,k)       ! 24813576 (-1.000)
!     & +z38(b,a,i,d,c,l,j,k)       ! 34812576 (+1.000)
!     & -z38(d,c,i,b,a,k,j,l)       ! 12834675 (-1.000)
!     & +z38(d,b,i,c,a,k,j,l)       ! 13824675 (+1.000)
!     & -z38(d,a,i,c,b,k,j,l)       ! 14823675 (-1.000)
!     & -z38(c,b,i,d,a,k,j,l)       ! 23814675 (-1.000)
!     & +z38(c,a,i,d,b,k,j,l)       ! 24813675 (+1.000)
!     & -z38(b,a,i,d,c,k,j,l)       ! 34812675 (-1.000)
!     & +z38(d,c,k,b,a,l,i,j)       ! 12634587 (+1.000)
!     & -z38(d,b,k,c,a,l,i,j)       ! 13624587 (-1.000)
!     & +z38(d,a,k,c,b,l,i,j)       ! 14623587 (+1.000)
!     & +z38(c,b,k,d,a,l,i,j)       ! 23614587 (+1.000)
!     & -z38(c,a,k,d,b,l,i,j)       ! 24613587 (-1.000)
!     & +z38(b,a,k,d,c,l,i,j)       ! 34612587 (+1.000)
!     & -z38(d,c,l,b,a,k,i,j)       ! 12534687 (-1.000)
!     & +z38(d,b,l,c,a,k,i,j)       ! 13524687 (+1.000)
!     & -z38(d,a,l,c,b,k,i,j)       ! 14523687 (-1.000)
!     & -z38(c,b,l,d,a,k,i,j)       ! 23514687 (-1.000)
!     & +z38(c,a,l,d,b,k,i,j)       ! 24513687 (+1.000)
!     & -z38(b,a,l,d,c,k,i,j)       ! 34512687 (-1.000)
!     & -z38(d,c,j,b,a,l,i,k)       ! 12734586 (-1.000)
!     & +z38(d,b,j,c,a,l,i,k)       ! 13724586 (+1.000)
!     & -z38(d,a,j,c,b,l,i,k)       ! 14723586 (-1.000)
!     & -z38(c,b,j,d,a,l,i,k)       ! 23714586 (-1.000)
!     & +z38(c,a,j,d,b,l,i,k)       ! 24713586 (+1.000)
!     & -z38(b,a,j,d,c,l,i,k)       ! 34712586 (-1.000)
!     & +z38(d,c,j,b,a,k,i,l)       ! 12734685 (+1.000)
!     & -z38(d,b,j,c,a,k,i,l)       ! 13724685 (-1.000)
!     & +z38(d,a,j,c,b,k,i,l)       ! 14723685 (+1.000)
!     & +z38(c,b,j,d,a,k,i,l)       ! 23714685 (+1.000)
!     & -z38(c,a,j,d,b,k,i,l)       ! 24713685 (-1.000)
!     & +z38(b,a,j,d,c,k,i,l)       ! 34712685 (+1.000)
!     & +z38(d,c,l,b,a,j,i,k)       ! 12534786 (+1.000)
!     & -z38(d,b,l,c,a,j,i,k)       ! 13524786 (-1.000)
!     & +z38(d,a,l,c,b,j,i,k)       ! 14523786 (+1.000)
!     & +z38(c,b,l,d,a,j,i,k)       ! 23514786 (+1.000)
!     & -z38(c,a,l,d,b,j,i,k)       ! 24513786 (-1.000)
!     & +z38(b,a,l,d,c,j,i,k)       ! 34512786 (+1.000)
!     & -z38(d,c,k,b,a,j,i,l)       ! 12634785 (-1.000)
!     & +z38(d,b,k,c,a,j,i,l)       ! 13624785 (+1.000)
!     & -z38(d,a,k,c,b,j,i,l)       ! 14623785 (-1.000)
!     & -z38(c,b,k,d,a,j,i,l)       ! 23614785 (-1.000)
!     & +z38(c,a,k,d,b,j,i,l)       ! 24613785 (+1.000)
!     & -z38(b,a,k,d,c,j,i,l)       ! 34612785 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z38)
       deallocate(u8)
c
       allocate(d1(n1+1:n3,n2+1:n3,n1+1:n3,n0+1:n2))
       call reorder3412(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n2,n3,n1,n3,n0,n2,intm,d1)
       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
       allocate(u9(n1+1:n3,n0+1:n1,n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
       i1=k2*k3*k4
       i2=k1*k1*k3
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u9)
       deallocate(d1)
       deallocate(d2)
c
!       allocate(f1(n0+1:n2,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!       call reorder645123(n1,n3,n0,n1,n0,n1,n2,n3,n1,n3,n0,n2,
!     & n0,n2,n2,n3,n1,n3,n1,n3,n0,n1,n0,n1,u9,f1)
!       allocate(f2(n0+1:n2,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!       call reorder412356(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
!     & n0,n2,n2,n3,n1,n3,n1,n3,n0,n1,n0,n1,t3b,f2)
!       allocate(z39(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!       i1=k1*k1*k3*k3
!       i2=k1*k1*k3*k3
!       i3=k4*k2
!       call egemm(i1,i2,i3,f1,f2,z39)
!       deallocate(f1)
!       deallocate(f2)
!c
       do i=n0+1,n1-3;do j=i+1,n1-2;do k=j+1,n1-1;do l=k+1,n1
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-3;do b=a+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
          if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n2+1,n3;do m=n0+1,n2
             sum=sum
     &     + u9(c,j,i,f,d,m)*t3b(f,b,a,m,l,k)     !cjifdmfbamlk    (+1.000)
     &     - u9(b,j,i,f,d,m)*t3b(f,c,a,m,l,k)     !bjifdmfcamlk    (-1.000)
     &     + u9(a,j,i,f,d,m)*t3b(f,c,b,m,l,k)     !ajifdmfcbmlk    (+1.000)
     &     - u9(d,j,i,f,c,m)*t3b(f,b,a,m,l,k)     !djifcmfbamlk    (-1.000)
     &     + u9(d,j,i,f,b,m)*t3b(f,c,a,m,l,k)     !djifbmfcamlk    (+1.000)
     &     - u9(d,j,i,f,a,m)*t3b(f,c,b,m,l,k)     !djifamfcbmlk    (-1.000)
     &     + u9(b,j,i,f,c,m)*t3b(f,d,a,m,l,k)     !bjifcmfdamlk    (+1.000)
     &     - u9(a,j,i,f,c,m)*t3b(f,d,b,m,l,k)     !ajifcmfdbmlk    (-1.000)
     &     - u9(c,j,i,f,b,m)*t3b(f,d,a,m,l,k)     !cjifbmfdamlk    (-1.000)
     &     + u9(c,j,i,f,a,m)*t3b(f,d,b,m,l,k)     !cjifamfdbmlk    (+1.000)
     &     + u9(a,j,i,f,b,m)*t3b(f,d,c,m,l,k)     !ajifbmfdcmlk    (+1.000)
     &     - u9(b,j,i,f,a,m)*t3b(f,d,c,m,l,k)     !bjifamfdcmlk    (-1.000)
     &     - u9(c,k,i,f,d,m)*t3b(f,b,a,m,l,j)     !ckifdmfbamlj    (-1.000)
     &     + u9(b,k,i,f,d,m)*t3b(f,c,a,m,l,j)     !bkifdmfcamlj    (+1.000)
     &     - u9(a,k,i,f,d,m)*t3b(f,c,b,m,l,j)     !akifdmfcbmlj    (-1.000)
     &     + u9(d,k,i,f,c,m)*t3b(f,b,a,m,l,j)     !dkifcmfbamlj    (+1.000)
     &     - u9(d,k,i,f,b,m)*t3b(f,c,a,m,l,j)     !dkifbmfcamlj    (-1.000)
     &     + u9(d,k,i,f,a,m)*t3b(f,c,b,m,l,j)     !dkifamfcbmlj    (+1.000)
     &     - u9(b,k,i,f,c,m)*t3b(f,d,a,m,l,j)     !bkifcmfdamlj    (-1.000)
     &     + u9(a,k,i,f,c,m)*t3b(f,d,b,m,l,j)     !akifcmfdbmlj    (+1.000)
     &     + u9(c,k,i,f,b,m)*t3b(f,d,a,m,l,j)     !ckifbmfdamlj    (+1.000)
     &     - u9(c,k,i,f,a,m)*t3b(f,d,b,m,l,j)     !ckifamfdbmlj    (-1.000)
     &     - u9(a,k,i,f,b,m)*t3b(f,d,c,m,l,j)     !akifbmfdcmlj    (-1.000)
     &     + u9(b,k,i,f,a,m)*t3b(f,d,c,m,l,j)     !bkifamfdcmlj    (+1.000)
     &     + u9(c,l,i,f,d,m)*t3b(f,b,a,m,k,j)     !clifdmfbamkj    (+1.000)
     &     - u9(b,l,i,f,d,m)*t3b(f,c,a,m,k,j)     !blifdmfcamkj    (-1.000)
     &     + u9(a,l,i,f,d,m)*t3b(f,c,b,m,k,j)     !alifdmfcbmkj    (+1.000)
     &     - u9(d,l,i,f,c,m)*t3b(f,b,a,m,k,j)     !dlifcmfbamkj    (-1.000)
     &     + u9(d,l,i,f,b,m)*t3b(f,c,a,m,k,j)     !dlifbmfcamkj    (+1.000)
     &     - u9(d,l,i,f,a,m)*t3b(f,c,b,m,k,j)     !dlifamfcbmkj    (-1.000)
     &     + u9(b,l,i,f,c,m)*t3b(f,d,a,m,k,j)     !blifcmfdamkj    (+1.000)
     &     - u9(a,l,i,f,c,m)*t3b(f,d,b,m,k,j)     !alifcmfdbmkj    (-1.000)
     &     - u9(c,l,i,f,b,m)*t3b(f,d,a,m,k,j)     !clifbmfdamkj    (-1.000)
     &     + u9(c,l,i,f,a,m)*t3b(f,d,b,m,k,j)     !clifamfdbmkj    (+1.000)
     &     + u9(a,l,i,f,b,m)*t3b(f,d,c,m,k,j)     !alifbmfdcmkj    (+1.000)
     &     - u9(b,l,i,f,a,m)*t3b(f,d,c,m,k,j)     !blifamfdcmkj    (-1.000)
     &     + u9(c,k,j,f,d,m)*t3b(f,b,a,m,l,i)     !ckjfdmfbamli    (+1.000)
     &     - u9(b,k,j,f,d,m)*t3b(f,c,a,m,l,i)     !bkjfdmfcamli    (-1.000)
     &     + u9(a,k,j,f,d,m)*t3b(f,c,b,m,l,i)     !akjfdmfcbmli    (+1.000)
     &     - u9(d,k,j,f,c,m)*t3b(f,b,a,m,l,i)     !dkjfcmfbamli    (-1.000)
     &     + u9(d,k,j,f,b,m)*t3b(f,c,a,m,l,i)     !dkjfbmfcamli    (+1.000)
     &     - u9(d,k,j,f,a,m)*t3b(f,c,b,m,l,i)     !dkjfamfcbmli    (-1.000)
     &     + u9(b,k,j,f,c,m)*t3b(f,d,a,m,l,i)     !bkjfcmfdamli    (+1.000)
     &     - u9(a,k,j,f,c,m)*t3b(f,d,b,m,l,i)     !akjfcmfdbmli    (-1.000)
     &     - u9(c,k,j,f,b,m)*t3b(f,d,a,m,l,i)     !ckjfbmfdamli    (-1.000)
     &     + u9(c,k,j,f,a,m)*t3b(f,d,b,m,l,i)     !ckjfamfdbmli    (+1.000)
     &     + u9(a,k,j,f,b,m)*t3b(f,d,c,m,l,i)     !akjfbmfdcmli    (+1.000)
     &     - u9(b,k,j,f,a,m)*t3b(f,d,c,m,l,i)     !bkjfamfdcmli    (-1.000)
     &     - u9(c,l,j,f,d,m)*t3b(f,b,a,m,k,i)     !cljfdmfbamki    (-1.000)
     &     + u9(b,l,j,f,d,m)*t3b(f,c,a,m,k,i)     !bljfdmfcamki    (+1.000)
     &     - u9(a,l,j,f,d,m)*t3b(f,c,b,m,k,i)     !aljfdmfcbmki    (-1.000)
     &     + u9(d,l,j,f,c,m)*t3b(f,b,a,m,k,i)     !dljfcmfbamki    (+1.000)
     &     - u9(d,l,j,f,b,m)*t3b(f,c,a,m,k,i)     !dljfbmfcamki    (-1.000)
     &     + u9(d,l,j,f,a,m)*t3b(f,c,b,m,k,i)     !dljfamfcbmki    (+1.000)
     &     - u9(b,l,j,f,c,m)*t3b(f,d,a,m,k,i)     !bljfcmfdamki    (-1.000)
     &     + u9(a,l,j,f,c,m)*t3b(f,d,b,m,k,i)     !aljfcmfdbmki    (+1.000)
     &     + u9(c,l,j,f,b,m)*t3b(f,d,a,m,k,i)     !cljfbmfdamki    (+1.000)
     &     - u9(c,l,j,f,a,m)*t3b(f,d,b,m,k,i)     !cljfamfdbmki    (-1.000)
     &     - u9(a,l,j,f,b,m)*t3b(f,d,c,m,k,i)     !aljfbmfdcmki    (-1.000)
     &     + u9(b,l,j,f,a,m)*t3b(f,d,c,m,k,i)     !bljfamfdcmki    (+1.000)
     &     + u9(c,l,k,f,d,m)*t3b(f,b,a,m,j,i)     !clkfdmfbamji    (+1.000)
     &     - u9(b,l,k,f,d,m)*t3b(f,c,a,m,j,i)     !blkfdmfcamji    (-1.000)
     &     + u9(a,l,k,f,d,m)*t3b(f,c,b,m,j,i)     !alkfdmfcbmji    (+1.000)
     &     - u9(d,l,k,f,c,m)*t3b(f,b,a,m,j,i)     !dlkfcmfbamji    (-1.000)
     &     + u9(d,l,k,f,b,m)*t3b(f,c,a,m,j,i)     !dlkfbmfcamji    (+1.000)
     &     - u9(d,l,k,f,a,m)*t3b(f,c,b,m,j,i)     !dlkfamfcbmji    (-1.000)
     &     + u9(b,l,k,f,c,m)*t3b(f,d,a,m,j,i)     !blkfcmfdamji    (+1.000)
     &     - u9(a,l,k,f,c,m)*t3b(f,d,b,m,j,i)     !alkfcmfdbmji    (-1.000)
     &     - u9(c,l,k,f,b,m)*t3b(f,d,a,m,j,i)     !clkfbmfdamji    (-1.000)
     &     + u9(c,l,k,f,a,m)*t3b(f,d,b,m,j,i)     !clkfamfdbmji    (+1.000)
     &     + u9(a,l,k,f,b,m)*t3b(f,d,c,m,j,i)     !alkfbmfdcmji    (+1.000)
     &     - u9(b,l,k,f,a,m)*t3b(f,d,c,m,j,i)     !blkfamfdcmji    (-1.000)
             enddo;enddo
             v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call
!     & sum34561278(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z39, 1.000)
!       call
!     & sum24561378(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z39,-1.000)
!       call
!     & sum23561478(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z39, 1.000)
!       call
!     & sum34562178(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z39,-1.000)
!       call
!     & sum24563178(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z39, 1.000)
!       call
!     & sum23564178(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z39,-1.000)
!       call
!     & sum14562378(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z39, 1.000)
!       call
!     & sum13562478(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z39,-1.000)
!       call
!     & sum14563278(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z39,-1.000)
!       call
!     & sum13564278(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z39, 1.000)
!       call
!     & sum12563478(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z39, 1.000)
!       call
!     & sum12564378(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z39,-1.000)
!       call
!     & sum34571268(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z39,-1.000)
!       call
!     & sum24571368(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z39, 1.000)
!       call
!     & sum23571468(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z39,-1.000)
!       call
!     & sum34572168(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z39, 1.000)
!       call
!     & sum24573168(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z39,-1.000)
!       call
!     & sum23574168(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z39, 1.000)
!       call
!     & sum14572368(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z39,-1.000)
!       call
!     & sum13572468(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z39, 1.000)
!       call
!     & sum14573268(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z39, 1.000)
!       call
!     & sum13574268(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z39,-1.000)
!       call
!     & sum12573468(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z39,-1.000)
!       call
!     & sum12574368(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z39, 1.000)
!       call
!     & sum34671258(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z39, 1.000)
!       call
!     & sum24671358(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z39,-1.000)
!       call
!     & sum23671458(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z39, 1.000)
!       call
!     & sum34672158(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z39,-1.000)
!       call
!     & sum24673158(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z39, 1.000)
!       call
!     & sum23674158(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z39,-1.000)
!       call
!     & sum14672358(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z39, 1.000)
!       call
!     & sum13672458(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z39,-1.000)
!       call
!     & sum14673258(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z39,-1.000)
!       call
!     & sum13674258(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z39, 1.000)
!       call
!     & sum12673458(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z39, 1.000)
!       call
!     & sum12674358(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z39,-1.000)
!       call
!     & sum34581267(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z39, 1.000)
!       call
!     & sum24581367(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z39,-1.000)
!       call
!     & sum23581467(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z39, 1.000)
!       call
!     & sum34582167(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z39,-1.000)
!       call
!     & sum24583167(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z39, 1.000)
!       call
!     & sum23584167(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z39,-1.000)
!       call
!     & sum14582367(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z39, 1.000)
!       call
!     & sum13582467(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z39,-1.000)
!       call
!     & sum14583267(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z39,-1.000)
!       call
!     & sum13584267(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z39, 1.000)
!       call
!     & sum12583467(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z39, 1.000)
!       call
!     & sum12584367(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z39,-1.000)
!       call
!     & sum34681257(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z39,-1.000)
!       call
!     & sum24681357(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z39, 1.000)
!       call
!     & sum23681457(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z39,-1.000)
!       call
!     & sum34682157(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z39, 1.000)
!       call
!     & sum24683157(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z39,-1.000)
!       call
!     & sum23684157(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z39, 1.000)
!       call
!     & sum14682357(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z39,-1.000)
!       call
!     & sum13682457(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z39, 1.000)
!       call
!     & sum14683257(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z39, 1.000)
!       call
!     & sum13684257(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z39,-1.000)
!       call
!     & sum12683457(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z39,-1.000)
!       call
!     & sum12684357(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z39, 1.000)
!       call
!     & sum34781256(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z39, 1.000)
!       call
!     & sum24781356(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z39,-1.000)
!       call
!     & sum23781456(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z39, 1.000)
!       call
!     & sum34782156(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z39,-1.000)
!       call
!     & sum24783156(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z39, 1.000)
!       call
!     & sum23784156(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z39,-1.000)
!       call
!     & sum14782356(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z39, 1.000)
!       call
!     & sum13782456(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z39,-1.000)
!       call
!     & sum14783256(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z39,-1.000)
!       call
!     & sum13784256(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z39, 1.000)
!       call
!     & sum12783456(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z39, 1.000)
!       call
!     & sum12784356(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z39,-1.000)
!c
!       do i=n0+1,n1-3
!       do j=i+1,n1-2
!       do k=j+1,n1-1
!       do l=k+1,n1
!       do a=n1+1,n3-3
!       do b=a+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)
!     & +z39(b,a,l,k,d,c,j,i)       ! 34561278 (+1.000)
!     & -z39(c,a,l,k,d,b,j,i)       ! 24561378 (-1.000)
!     & +z39(c,b,l,k,d,a,j,i)       ! 23561478 (+1.000)
!     & -z39(b,a,l,k,c,d,j,i)       ! 34562178 (-1.000)
!     & +z39(c,a,l,k,b,d,j,i)       ! 24563178 (+1.000)
!     & -z39(c,b,l,k,a,d,j,i)       ! 23564178 (-1.000)
!     & +z39(d,a,l,k,c,b,j,i)       ! 14562378 (+1.000)
!     & -z39(d,b,l,k,c,a,j,i)       ! 13562478 (-1.000)
!     & -z39(d,a,l,k,b,c,j,i)       ! 14563278 (-1.000)
!     & +z39(d,b,l,k,a,c,j,i)       ! 13564278 (+1.000)
!     & +z39(d,c,l,k,b,a,j,i)       ! 12563478 (+1.000)
!     & -z39(d,c,l,k,a,b,j,i)       ! 12564378 (-1.000)
!     & -z39(b,a,l,j,d,c,k,i)       ! 34571268 (-1.000)
!     & +z39(c,a,l,j,d,b,k,i)       ! 24571368 (+1.000)
!     & -z39(c,b,l,j,d,a,k,i)       ! 23571468 (-1.000)
!     & +z39(b,a,l,j,c,d,k,i)       ! 34572168 (+1.000)
!     & -z39(c,a,l,j,b,d,k,i)       ! 24573168 (-1.000)
!     & +z39(c,b,l,j,a,d,k,i)       ! 23574168 (+1.000)
!     & -z39(d,a,l,j,c,b,k,i)       ! 14572368 (-1.000)
!     & +z39(d,b,l,j,c,a,k,i)       ! 13572468 (+1.000)
!     & +z39(d,a,l,j,b,c,k,i)       ! 14573268 (+1.000)
!     & -z39(d,b,l,j,a,c,k,i)       ! 13574268 (-1.000)
!     & -z39(d,c,l,j,b,a,k,i)       ! 12573468 (-1.000)
!     & +z39(d,c,l,j,a,b,k,i)       ! 12574368 (+1.000)
!     & +z39(b,a,k,j,d,c,l,i)       ! 34671258 (+1.000)
!     & -z39(c,a,k,j,d,b,l,i)       ! 24671358 (-1.000)
!     & +z39(c,b,k,j,d,a,l,i)       ! 23671458 (+1.000)
!     & -z39(b,a,k,j,c,d,l,i)       ! 34672158 (-1.000)
!     & +z39(c,a,k,j,b,d,l,i)       ! 24673158 (+1.000)
!     & -z39(c,b,k,j,a,d,l,i)       ! 23674158 (-1.000)
!     & +z39(d,a,k,j,c,b,l,i)       ! 14672358 (+1.000)
!     & -z39(d,b,k,j,c,a,l,i)       ! 13672458 (-1.000)
!     & -z39(d,a,k,j,b,c,l,i)       ! 14673258 (-1.000)
!     & +z39(d,b,k,j,a,c,l,i)       ! 13674258 (+1.000)
!     & +z39(d,c,k,j,b,a,l,i)       ! 12673458 (+1.000)
!     & -z39(d,c,k,j,a,b,l,i)       ! 12674358 (-1.000)
!     & +z39(b,a,l,i,d,c,k,j)       ! 34581267 (+1.000)
!     & -z39(c,a,l,i,d,b,k,j)       ! 24581367 (-1.000)
!     & +z39(c,b,l,i,d,a,k,j)       ! 23581467 (+1.000)
!     & -z39(b,a,l,i,c,d,k,j)       ! 34582167 (-1.000)
!     & +z39(c,a,l,i,b,d,k,j)       ! 24583167 (+1.000)
!     & -z39(c,b,l,i,a,d,k,j)       ! 23584167 (-1.000)
!     & +z39(d,a,l,i,c,b,k,j)       ! 14582367 (+1.000)
!     & -z39(d,b,l,i,c,a,k,j)       ! 13582467 (-1.000)
!     & -z39(d,a,l,i,b,c,k,j)       ! 14583267 (-1.000)
!     & +z39(d,b,l,i,a,c,k,j)       ! 13584267 (+1.000)
!     & +z39(d,c,l,i,b,a,k,j)       ! 12583467 (+1.000)
!     & -z39(d,c,l,i,a,b,k,j)       ! 12584367 (-1.000)
!     & -z39(b,a,k,i,d,c,l,j)       ! 34681257 (-1.000)
!     & +z39(c,a,k,i,d,b,l,j)       ! 24681357 (+1.000)
!     & -z39(c,b,k,i,d,a,l,j)       ! 23681457 (-1.000)
!     & +z39(b,a,k,i,c,d,l,j)       ! 34682157 (+1.000)
!     & -z39(c,a,k,i,b,d,l,j)       ! 24683157 (-1.000)
!     & +z39(c,b,k,i,a,d,l,j)       ! 23684157 (+1.000)
!     & -z39(d,a,k,i,c,b,l,j)       ! 14682357 (-1.000)
!     & +z39(d,b,k,i,c,a,l,j)       ! 13682457 (+1.000)
!     & +z39(d,a,k,i,b,c,l,j)       ! 14683257 (+1.000)
!     & -z39(d,b,k,i,a,c,l,j)       ! 13684257 (-1.000)
!     & -z39(d,c,k,i,b,a,l,j)       ! 12683457 (-1.000)
!     & +z39(d,c,k,i,a,b,l,j)       ! 12684357 (+1.000)
!     & +z39(b,a,j,i,d,c,l,k)       ! 34781256 (+1.000)
!     & -z39(c,a,j,i,d,b,l,k)       ! 24781356 (-1.000)
!     & +z39(c,b,j,i,d,a,l,k)       ! 23781456 (+1.000)
!     & -z39(b,a,j,i,c,d,l,k)       ! 34782156 (-1.000)
!     & +z39(c,a,j,i,b,d,l,k)       ! 24783156 (+1.000)
!     & -z39(c,b,j,i,a,d,l,k)       ! 23784156 (-1.000)
!     & +z39(d,a,j,i,c,b,l,k)       ! 14782356 (+1.000)
!     & -z39(d,b,j,i,c,a,l,k)       ! 13782456 (-1.000)
!     & -z39(d,a,j,i,b,c,l,k)       ! 14783256 (-1.000)
!     & +z39(d,b,j,i,a,c,l,k)       ! 13784256 (+1.000)
!     & +z39(d,c,j,i,b,a,l,k)       ! 12783456 (+1.000)
!     & -z39(d,c,j,i,a,b,l,k)       ! 12784356 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z39)
       deallocate(u9)
c
       allocate(d1(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder3412(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n1,n3,n0,n1,n0,n1,intr,d1)
       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
       allocate(u10(n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       i1=k1*k1*k3
       i2=k1*k1*k3
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u10)
       deallocate(d1)
       deallocate(d2)
c
!       allocate(f1(n0+1:n1,n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
!       call reorder546123(n1,n3,n0,n1,n0,n1,n1,n3,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n0,n1,n1,n3,n0,n1,n0,n1,u10,f1)
!       allocate(h2(n0+1:n1,n1+1:n3,n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!       call reorder61523478(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n0,n1,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,t4a,h2)
!       allocate(z40(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
!       i1=k1*k1*k3
!       i2=k1*k1*k3*k3*k3
!       i3=k1*k3*k1
!       call egemm(i1,i2,i3,f1,h2,z40)
!       deallocate(f1)
!       deallocate(h2)
c
       do i=n0+1,n1-3;do j=i+1,n1-2;do k=j+1,n1-1;do l=k+1,n1
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-3;do b=a+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
          if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n1+1,n3;do m=n0+1,n1;do n=n0+1,n1
             sum=sum
     &     + (u10(d,j,i,f,m,n)*t4a(f,c,b,a,n,m,l,k)      !djifmnfcbanmlk  (+0.500)
     &     - u10(c,j,i,f,m,n)*t4a(f,d,b,a,n,m,l,k)       !cjifmnfdbanmlk  (-0.500)
     &     + u10(b,j,i,f,m,n)*t4a(f,d,c,a,n,m,l,k)       !bjifmnfdcanmlk  (+0.500)
     &     - u10(a,j,i,f,m,n)*t4a(f,d,c,b,n,m,l,k)       !ajifmnfdcbnmlk  (-0.500)
     &     - u10(d,k,i,f,m,n)*t4a(f,c,b,a,n,m,l,j)       !dkifmnfcbanmlj  (-0.500)
     &     + u10(c,k,i,f,m,n)*t4a(f,d,b,a,n,m,l,j)       !ckifmnfdbanmlj  (+0.500)
     &     - u10(b,k,i,f,m,n)*t4a(f,d,c,a,n,m,l,j)       !bkifmnfdcanmlj  (-0.500)
     &     + u10(a,k,i,f,m,n)*t4a(f,d,c,b,n,m,l,j)       !akifmnfdcbnmlj  (+0.500)
     &     + u10(d,l,i,f,m,n)*t4a(f,c,b,a,n,m,k,j)       !dlifmnfcbanmkj  (+0.500)
     &     - u10(c,l,i,f,m,n)*t4a(f,d,b,a,n,m,k,j)       !clifmnfdbanmkj  (-0.500)
     &     + u10(b,l,i,f,m,n)*t4a(f,d,c,a,n,m,k,j)       !blifmnfdcanmkj  (+0.500)
     &     - u10(a,l,i,f,m,n)*t4a(f,d,c,b,n,m,k,j)       !alifmnfdcbnmkj  (-0.500)
     &     + u10(d,k,j,f,m,n)*t4a(f,c,b,a,n,m,l,i)       !dkjfmnfcbanmli  (+0.500)
     &     - u10(c,k,j,f,m,n)*t4a(f,d,b,a,n,m,l,i)       !ckjfmnfdbanmli  (-0.500)
     &     + u10(b,k,j,f,m,n)*t4a(f,d,c,a,n,m,l,i)       !bkjfmnfdcanmli  (+0.500)
     &     - u10(a,k,j,f,m,n)*t4a(f,d,c,b,n,m,l,i)       !akjfmnfdcbnmli  (-0.500)
     &     - u10(d,l,j,f,m,n)*t4a(f,c,b,a,n,m,k,i)       !dljfmnfcbanmki  (-0.500)
     &     + u10(c,l,j,f,m,n)*t4a(f,d,b,a,n,m,k,i)       !cljfmnfdbanmki  (+0.500)
     &     - u10(b,l,j,f,m,n)*t4a(f,d,c,a,n,m,k,i)       !bljfmnfdcanmki  (-0.500)
     &     + u10(a,l,j,f,m,n)*t4a(f,d,c,b,n,m,k,i)       !aljfmnfdcbnmki  (+0.500)
     &     + u10(d,l,k,f,m,n)*t4a(f,c,b,a,n,m,j,i)       !dlkfmnfcbanmji  (+0.500)
     &     - u10(c,l,k,f,m,n)*t4a(f,d,b,a,n,m,j,i)       !clkfmnfdbanmji  (-0.500)
     &     + u10(b,l,k,f,m,n)*t4a(f,d,c,a,n,m,j,i)       !blkfmnfdcanmji  (+0.500)
     &     - u10(a,l,k,f,m,n)*t4a(f,d,c,b,n,m,j,i))/2.0d0!alkfmnfdcbnmji  (-0.500)             
             enddo;enddo;enddo
             v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call
!     & sum23456178(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z40, 0.500)
!       call
!     & sum13456278(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z40,-0.500)
!       call
!     & sum12456378(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z40, 0.500)
!       call
!     & sum12356478(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z40,-0.500)
!       call
!     & sum23457168(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z40,-0.500)
!       call
!     & sum13457268(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z40, 0.500)
!       call
!     & sum12457368(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z40,-0.500)
!       call
!     & sum12357468(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z40, 0.500)
!       call
!     & sum23467158(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z40, 0.500)
!       call
!     & sum13467258(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z40,-0.500)
!       call
!     & sum12467358(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z40, 0.500)
!       call
!     & sum12367458(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z40,-0.500)
!       call
!     & sum23458167(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z40, 0.500)
!       call
!     & sum13458267(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z40,-0.500)
!       call
!     & sum12458367(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z40, 0.500)
!       call
!     & sum12358467(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z40,-0.500)
!       call
!     & sum23468157(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z40,-0.500)
!       call
!     & sum13468257(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z40, 0.500)
!       call
!     & sum12468357(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z40,-0.500)
!       call
!     & sum12368457(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z40, 0.500)
!       call
!     & sum23478156(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z40, 0.500)
!       call
!     & sum13478256(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z40,-0.500)
!       call
!     & sum12478356(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z40, 0.500)
!       call
!     & sum12378456(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z40,-0.500)
c
!       do i=n0+1,n1-3
!       do j=i+1,n1-2
!       do k=j+1,n1-1
!       do l=k+1,n1
!       do a=n1+1,n3-3
!       do b=a+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)
!     & +(z40(c,b,a,l,k,d,j,i)      ! 23456178 (+0.500)
!     & -z40(d,b,a,l,k,c,j,i)       ! 13456278 (-0.500)
!     & +z40(d,c,a,l,k,b,j,i)       ! 12456378 (+0.500)
!     & -z40(d,c,b,l,k,a,j,i)       ! 12356478 (-0.500)
!     & -z40(c,b,a,l,j,d,k,i)       ! 23457168 (-0.500)
!     & +z40(d,b,a,l,j,c,k,i)       ! 13457268 (+0.500)
!     & -z40(d,c,a,l,j,b,k,i)       ! 12457368 (-0.500)
!     & +z40(d,c,b,l,j,a,k,i)       ! 12357468 (+0.500)
!     & +z40(c,b,a,k,j,d,l,i)       ! 23467158 (+0.500)
!     & -z40(d,b,a,k,j,c,l,i)       ! 13467258 (-0.500)
!     & +z40(d,c,a,k,j,b,l,i)       ! 12467358 (+0.500)
!     & -z40(d,c,b,k,j,a,l,i)       ! 12367458 (-0.500)
!     & +z40(c,b,a,l,i,d,k,j)       ! 23458167 (+0.500)
!     & -z40(d,b,a,l,i,c,k,j)       ! 13458267 (-0.500)
!     & +z40(d,c,a,l,i,b,k,j)       ! 12458367 (+0.500)
!     & -z40(d,c,b,l,i,a,k,j)       ! 12358467 (-0.500)
!     & -z40(c,b,a,k,i,d,l,j)       ! 23468157 (-0.500)
!     & +z40(d,b,a,k,i,c,l,j)       ! 13468257 (+0.500)
!     & -z40(d,c,a,k,i,b,l,j)       ! 12468357 (-0.500)
!     & +z40(d,c,b,k,i,a,l,j)       ! 12368457 (+0.500)
!     & +z40(c,b,a,j,i,d,l,k)       ! 23478156 (+0.500)
!     & -z40(d,b,a,j,i,c,l,k)       ! 13478256 (-0.500)
!     & +z40(d,c,a,j,i,b,l,k)       ! 12478356 (+0.500)
!     & -z40(d,c,b,j,i,a,l,k))/2.0d0! 12378456 (-0.500)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z40)
c
c
       allocate(f1(n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder465123(n1,n3,n0,n1,n0,n1,n1,n3,n0,n1,n0,n1,
     & n1,n3,n0,n1,n0,n1,n1,n3,n0,n1,n0,n1,u10,f1)
       allocate(d2(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       call reorder1324(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n0,n1,n1,n3,n0,n1,t2a,d2)
       allocate(u30(n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       i1=k1*k1*k3*k1
       i2=k1*k3
       i3=k1*k3
       call egemm(i1,i2,i3,f1,d2,u30)
       deallocate(f1)
       deallocate(d2)
c
!       allocate(f1(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
!       call reorder341256(n1,n3,n0,n1,n0,n1,n1,n3,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,u30,f1)
!       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
!       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
!       allocate(z98(n1+1:n3,n1+1:n3,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
!       i1=k1*k1*k1*k3*k3
!       i2=k1*k3*k3
!       i3=k1
!       call egemm(i1,i2,i3,f1,d2,z98)
!       deallocate(f1)
!       deallocate(d2)
c
       do i=n0+1,n1-3;do j=i+1,n1-2;do k=j+1,n1-1;do l=k+1,n1
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-3;do b=a+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
          if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1
             sum=sum
     &     + u30(b,l,m,d,j,i)*t2a(c,a,m,k)       !blmdjicamk      (+1.000)
     &     + u30(c,k,m,d,j,i)*t2a(b,a,m,l)       !ckmdjibaml      (+1.000)
     &     - u30(c,l,m,d,j,i)*t2a(b,a,m,k)       !clmdjibamk      (-1.000)
     &     - u30(b,k,m,d,j,i)*t2a(c,a,m,l)       !bkmdjicaml      (-1.000)
     &     - u30(b,l,m,c,j,i)*t2a(d,a,m,k)       !blmcjidamk      (-1.000)
     &     - u30(d,k,m,c,j,i)*t2a(b,a,m,l)       !dkmcjibaml      (-1.000)
     &     + u30(c,l,m,b,j,i)*t2a(d,a,m,k)       !clmbjidamk      (+1.000)
     &     + u30(d,k,m,b,j,i)*t2a(c,a,m,l)       !dkmbjicaml      (+1.000)
     &     - u30(c,l,m,a,j,i)*t2a(d,b,m,k)       !clmajidbmk      (-1.000)
     &     - u30(d,k,m,a,j,i)*t2a(c,b,m,l)       !dkmajicbml      (-1.000)
     &     + u30(d,l,m,c,j,i)*t2a(b,a,m,k)       !dlmcjibamk      (+1.000)
     &     + u30(b,k,m,c,j,i)*t2a(d,a,m,l)       !bkmcjidaml      (+1.000)
     &     - u30(d,l,m,b,j,i)*t2a(c,a,m,k)       !dlmbjicamk      (-1.000)
     &     - u30(c,k,m,b,j,i)*t2a(d,a,m,l)       !ckmbjidaml      (-1.000)
     &     + u30(d,l,m,a,j,i)*t2a(c,b,m,k)       !dlmajicbmk      (+1.000)
     &     + u30(c,k,m,a,j,i)*t2a(d,b,m,l)       !ckmajidbml      (+1.000)
     &     - u30(b,l,m,d,k,i)*t2a(c,a,m,j)       !blmdkicamj      (-1.000)
     &     - u30(c,j,m,d,k,i)*t2a(b,a,m,l)       !cjmdkibaml      (-1.000)
     &     + u30(c,l,m,d,k,i)*t2a(b,a,m,j)       !clmdkibamj      (+1.000)
     &     + u30(b,j,m,d,k,i)*t2a(c,a,m,l)       !bjmdkicaml      (+1.000)
     &     + u30(b,l,m,c,k,i)*t2a(d,a,m,j)       !blmckidamj      (+1.000)
     &     + u30(d,j,m,c,k,i)*t2a(b,a,m,l)       !djmckibaml      (+1.000)
     &     - u30(c,l,m,b,k,i)*t2a(d,a,m,j)       !clmbkidamj      (-1.000)
     &     - u30(d,j,m,b,k,i)*t2a(c,a,m,l)       !djmbkicaml      (-1.000)
     &     + u30(c,l,m,a,k,i)*t2a(d,b,m,j)       !clmakidbmj      (+1.000)
     &     + u30(d,j,m,a,k,i)*t2a(c,b,m,l)       !djmakicbml      (+1.000)
     &     - u30(d,l,m,c,k,i)*t2a(b,a,m,j)       !dlmckibamj      (-1.000)
     &     - u30(b,j,m,c,k,i)*t2a(d,a,m,l)       !bjmckidaml      (-1.000)
     &     + u30(d,l,m,b,k,i)*t2a(c,a,m,j)       !dlmbkicamj      (+1.000)
     &     + u30(c,j,m,b,k,i)*t2a(d,a,m,l)       !cjmbkidaml      (+1.000)
     &     - u30(d,l,m,a,k,i)*t2a(c,b,m,j)       !dlmakicbmj      (-1.000)
     &     - u30(c,j,m,a,k,i)*t2a(d,b,m,l)       !cjmakidbml      (-1.000)
     &     - u30(b,l,m,c,k,j)*t2a(d,a,m,i)       !blmckjdami      (-1.000)
     &     - u30(d,i,m,c,k,j)*t2a(b,a,m,l)       !dimckjbaml      (-1.000)
     &     - u30(c,l,m,a,k,j)*t2a(d,b,m,i)       !clmakjdbmi      (-1.000)
     &     + u30(c,l,m,b,k,j)*t2a(d,a,m,i)       !clmbkjdami      (+1.000)
     &     + u30(d,i,m,b,k,j)*t2a(c,a,m,l)       !dimbkjcaml      (+1.000)
     &     - u30(d,i,m,a,k,j)*t2a(c,b,m,l)       !dimakjcbml      (-1.000)
     &     + u30(b,l,m,d,k,j)*t2a(c,a,m,i)       !blmdkjcami      (+1.000)
     &     + u30(c,i,m,d,k,j)*t2a(b,a,m,l)       !cimdkjbaml      (+1.000)
     &     - u30(c,l,m,d,k,j)*t2a(b,a,m,i)       !clmdkjbami      (-1.000)
     &     - u30(b,i,m,d,k,j)*t2a(c,a,m,l)       !bimdkjcaml      (-1.000)
     &     + u30(d,l,m,a,k,j)*t2a(c,b,m,i)       !dlmakjcbmi      (+1.000)
     &     - u30(d,l,m,b,k,j)*t2a(c,a,m,i)       !dlmbkjcami      (-1.000)
     &     - u30(c,i,m,b,k,j)*t2a(d,a,m,l)       !cimbkjdaml      (-1.000)
     &     + u30(c,i,m,a,k,j)*t2a(d,b,m,l)       !cimakjdbml      (+1.000)
     &     + u30(d,l,m,c,k,j)*t2a(b,a,m,i)       !dlmckjbami      (+1.000)
     &     + u30(b,i,m,c,k,j)*t2a(d,a,m,l)       !bimckjdaml      (+1.000)
     &     + u30(b,k,m,d,l,i)*t2a(c,a,m,j)       !bkmdlicamj      (+1.000)
     &     + u30(c,j,m,d,l,i)*t2a(b,a,m,k)       !cjmdlibamk      (+1.000)
     &     - u30(c,k,m,d,l,i)*t2a(b,a,m,j)       !ckmdlibamj      (-1.000)
     &     - u30(b,j,m,d,l,i)*t2a(c,a,m,k)       !bjmdlicamk      (-1.000)
     &     - u30(b,k,m,c,l,i)*t2a(d,a,m,j)       !bkmclidamj      (-1.000)
     &     - u30(d,j,m,c,l,i)*t2a(b,a,m,k)       !djmclibamk      (-1.000)
     &     + u30(c,k,m,b,l,i)*t2a(d,a,m,j)       !ckmblidamj      (+1.000)
     &     + u30(d,j,m,b,l,i)*t2a(c,a,m,k)       !djmblicamk      (+1.000)
     &     - u30(c,k,m,a,l,i)*t2a(d,b,m,j)       !ckmalidbmj      (-1.000)
     &     - u30(d,j,m,a,l,i)*t2a(c,b,m,k)       !djmalicbmk      (-1.000)
     &     + u30(d,k,m,c,l,i)*t2a(b,a,m,j)       !dkmclibamj      (+1.000)
     &     + u30(b,j,m,c,l,i)*t2a(d,a,m,k)       !bjmclidamk      (+1.000)
     &     - u30(d,k,m,b,l,i)*t2a(c,a,m,j)       !dkmblicamj      (-1.000)
     &     - u30(c,j,m,b,l,i)*t2a(d,a,m,k)       !cjmblidamk      (-1.000)
     &     + u30(d,k,m,a,l,i)*t2a(c,b,m,j)       !dkmalicbmj      (+1.000)
     &     + u30(c,j,m,a,l,i)*t2a(d,b,m,k)       !cjmalidbmk      (+1.000)
     &     + u30(b,k,m,c,l,j)*t2a(d,a,m,i)       !bkmcljdami      (+1.000)
     &     + u30(d,i,m,c,l,j)*t2a(b,a,m,k)       !dimcljbamk      (+1.000)
     &     + u30(c,k,m,a,l,j)*t2a(d,b,m,i)       !ckmaljdbmi      (+1.000)
     &     - u30(c,k,m,b,l,j)*t2a(d,a,m,i)       !ckmbljdami      (-1.000)
     &     - u30(d,i,m,b,l,j)*t2a(c,a,m,k)       !dimbljcamk      (-1.000)
     &     + u30(d,i,m,a,l,j)*t2a(c,b,m,k)       !dimaljcbmk      (+1.000)
     &     - u30(b,k,m,d,l,j)*t2a(c,a,m,i)       !bkmdljcami      (-1.000)
     &     - u30(c,i,m,d,l,j)*t2a(b,a,m,k)       !cimdljbamk      (-1.000)
     &     + u30(c,k,m,d,l,j)*t2a(b,a,m,i)       !ckmdljbami      (+1.000)
     &     + u30(b,i,m,d,l,j)*t2a(c,a,m,k)       !bimdljcamk      (+1.000)
     &     - u30(d,k,m,a,l,j)*t2a(c,b,m,i)       !dkmaljcbmi      (-1.000)
     &     + u30(d,k,m,b,l,j)*t2a(c,a,m,i)       !dkmbljcami      (+1.000)
     &     + u30(c,i,m,b,l,j)*t2a(d,a,m,k)       !cimbljdamk      (+1.000)
     &     - u30(c,i,m,a,l,j)*t2a(d,b,m,k)       !cimaljdbmk      (-1.000)
     &     - u30(d,k,m,c,l,j)*t2a(b,a,m,i)       !dkmcljbami      (-1.000)
     &     - u30(b,i,m,c,l,j)*t2a(d,a,m,k)       !bimcljdamk      (-1.000)
     &     - u30(c,j,m,a,l,k)*t2a(d,b,m,i)       !cjmalkdbmi      (-1.000)
     &     - u30(d,i,m,a,l,k)*t2a(c,b,m,j)       !dimalkcbmj      (-1.000)
     &     + u30(c,j,m,b,l,k)*t2a(d,a,m,i)       !cjmblkdami      (+1.000)
     &     + u30(d,i,m,b,l,k)*t2a(c,a,m,j)       !dimblkcamj      (+1.000)
     &     - u30(b,j,m,c,l,k)*t2a(d,a,m,i)       !bjmclkdami      (-1.000)
     &     - u30(d,i,m,c,l,k)*t2a(b,a,m,j)       !dimclkbamj      (-1.000)
     &     + u30(d,j,m,a,l,k)*t2a(c,b,m,i)       !djmalkcbmi      (+1.000)
     &     + u30(c,i,m,a,l,k)*t2a(d,b,m,j)       !cimalkdbmj      (+1.000)
     &     - u30(d,j,m,b,l,k)*t2a(c,a,m,i)       !djmblkcami      (-1.000)
     &     - u30(c,i,m,b,l,k)*t2a(d,a,m,j)       !cimblkdamj      (-1.000)
     &     + u30(d,j,m,c,l,k)*t2a(b,a,m,i)       !djmclkbami      (+1.000)
     &     + u30(b,i,m,c,l,k)*t2a(d,a,m,j)       !bimclkdamj      (+1.000)
     &     + u30(b,j,m,d,l,k)*t2a(c,a,m,i)       !bjmdlkcami      (+1.000)
     &     + u30(c,i,m,d,l,k)*t2a(b,a,m,j)       !cimdlkbamj      (+1.000)
     &     - u30(c,j,m,d,l,k)*t2a(b,a,m,i)       !cjmdlkbami      (-1.000)
     &     - u30(b,i,m,d,l,k)*t2a(c,a,m,j)       !bimdlkcamj      (-1.000)             
             enddo
             v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call
!     & sum24613578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98, 1.000)
!       call
!     & sum34512678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98, 1.000)
!       call
!     & sum34612578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98,-1.000)
!       call
!     & sum24513678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98,-1.000)
!       call
!     & sum14623578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98,-1.000)
!       call
!     & sum34521678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98,-1.000)
!       call
!     & sum14632578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98, 1.000)
!       call
!     & sum24531678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98, 1.000)
!       call
!     & sum13642578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98,-1.000)
!       call
!     & sum23541678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98,-1.000)
!       call
!     & sum34621578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98, 1.000)
!       call
!     & sum14523678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98, 1.000)
!       call
!     & sum24631578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98,-1.000)
!       call
!     & sum14532678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98,-1.000)
!       call
!     & sum23641578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98, 1.000)
!       call
!     & sum13542678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98, 1.000)
!       call
!     & sum24713568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98,-1.000)
!       call
!     & sum34512768(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98,-1.000)
!       call
!     & sum34712568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98, 1.000)
!       call
!     & sum24513768(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98, 1.000)
!       call
!     & sum14723568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98, 1.000)
!       call
!     & sum34521768(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98, 1.000)
!       call
!     & sum14732568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98,-1.000)
!       call
!     & sum24531768(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98,-1.000)
!       call
!     & sum13742568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98, 1.000)
!       call
!     & sum23541768(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98, 1.000)
!       call
!     & sum34721568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98,-1.000)
!       call
!     & sum14523768(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98,-1.000)
!       call
!     & sum24731568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98, 1.000)
!       call
!     & sum14532768(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98, 1.000)
!       call
!     & sum23741568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98,-1.000)
!       call
!     & sum13542768(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98,-1.000)
!       call
!     & sum14823567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98,-1.000)
!       call
!     & sum34521867(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98,-1.000)
!       call
!     & sum13842567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98,-1.000)
!       call
!     & sum14832567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98, 1.000)
!       call
!     & sum24531867(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98, 1.000)
!       call
!     & sum23541867(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98,-1.000)
!       call
!     & sum24813567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98, 1.000)
!       call
!     & sum34512867(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98, 1.000)
!       call
!     & sum34812567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98,-1.000)
!       call
!     & sum24513867(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98,-1.000)
!       call
!     & sum23841567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98, 1.000)
!       call
!     & sum24831567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98,-1.000)
!       call
!     & sum14532867(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98,-1.000)
!       call
!     & sum13542867(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98, 1.000)
!       call
!     & sum34821567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98, 1.000)
!       call
!     & sum14523867(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98, 1.000)
!       call
!     & sum24713658(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98, 1.000)
!       call
!     & sum34612758(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98, 1.000)
!       call
!     & sum34712658(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98,-1.000)
!       call
!     & sum24613758(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98,-1.000)
!       call
!     & sum14723658(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98,-1.000)
!       call
!     & sum34621758(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98,-1.000)
!       call
!     & sum14732658(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98, 1.000)
!       call
!     & sum24631758(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98, 1.000)
!       call
!     & sum13742658(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98,-1.000)
!       call
!     & sum23641758(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98,-1.000)
!       call
!     & sum34721658(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98, 1.000)
!       call
!     & sum14623758(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98, 1.000)
!       call
!     & sum24731658(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98,-1.000)
!       call
!     & sum14632758(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98,-1.000)
!       call
!     & sum23741658(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98, 1.000)
!       call
!     & sum13642758(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98, 1.000)
!       call
!     & sum14823657(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98, 1.000)
!       call
!     & sum34621857(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98, 1.000)
!       call
!     & sum13842657(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98, 1.000)
!       call
!     & sum14832657(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98,-1.000)
!       call
!     & sum24631857(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98,-1.000)
!       call
!     & sum23641857(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98, 1.000)
!       call
!     & sum24813657(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98,-1.000)
!       call
!     & sum34612857(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98,-1.000)
!       call
!     & sum34812657(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98, 1.000)
!       call
!     & sum24613857(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98, 1.000)
!       call
!     & sum23841657(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98,-1.000)
!       call
!     & sum24831657(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98, 1.000)
!       call
!     & sum14632857(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98, 1.000)
!       call
!     & sum13642857(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98,-1.000)
!       call
!     & sum34821657(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98,-1.000)
!       call
!     & sum14623857(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98,-1.000)
!       call
!     & sum13842756(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98,-1.000)
!       call
!     & sum23741856(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98,-1.000)
!       call
!     & sum14832756(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98, 1.000)
!       call
!     & sum24731856(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98, 1.000)
!       call
!     & sum14823756(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98,-1.000)
!       call
!     & sum34721856(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98,-1.000)
!       call
!     & sum23841756(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98, 1.000)
!       call
!     & sum13742856(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98, 1.000)
!       call
!     & sum24831756(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98,-1.000)
!       call
!     & sum14732856(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98,-1.000)
!       call
!     & sum34821756(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98, 1.000)
!       call
!     & sum14723856(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98, 1.000)
!       call
!     & sum24813756(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98, 1.000)
!       call
!     & sum34712856(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98, 1.000)
!       call
!     & sum34812756(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98,-1.000)
!       call
!     & sum24713856(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z98,-1.000)
!c
!       do i=n0+1,n1-3
!       do j=i+1,n1-2
!       do k=j+1,n1-1
!       do l=k+1,n1
!       do a=n1+1,n3-3
!       do b=a+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)
!     & +z98(c,a,k,d,b,l,j,i)       ! 24613578 (+1.000)
!     & +z98(b,a,l,d,c,k,j,i)       ! 34512678 (+1.000)
!     & -z98(b,a,k,d,c,l,j,i)       ! 34612578 (-1.000)
!     & -z98(c,a,l,d,b,k,j,i)       ! 24513678 (-1.000)
!     & -z98(d,a,k,c,b,l,j,i)       ! 14623578 (-1.000)
!     & -z98(b,a,l,c,d,k,j,i)       ! 34521678 (-1.000)
!     & +z98(d,a,k,b,c,l,j,i)       ! 14632578 (+1.000)
!     & +z98(c,a,l,b,d,k,j,i)       ! 24531678 (+1.000)
!     & -z98(d,b,k,a,c,l,j,i)       ! 13642578 (-1.000)
!     & -z98(c,b,l,a,d,k,j,i)       ! 23541678 (-1.000)
!     & +z98(b,a,k,c,d,l,j,i)       ! 34621578 (+1.000)
!     & +z98(d,a,l,c,b,k,j,i)       ! 14523678 (+1.000)
!     & -z98(c,a,k,b,d,l,j,i)       ! 24631578 (-1.000)
!     & -z98(d,a,l,b,c,k,j,i)       ! 14532678 (-1.000)
!     & +z98(c,b,k,a,d,l,j,i)       ! 23641578 (+1.000)
!     & +z98(d,b,l,a,c,k,j,i)       ! 13542678 (+1.000)
!     & -z98(c,a,j,d,b,l,k,i)       ! 24713568 (-1.000)
!     & -z98(b,a,l,d,c,j,k,i)       ! 34512768 (-1.000)
!     & +z98(b,a,j,d,c,l,k,i)       ! 34712568 (+1.000)
!     & +z98(c,a,l,d,b,j,k,i)       ! 24513768 (+1.000)
!     & +z98(d,a,j,c,b,l,k,i)       ! 14723568 (+1.000)
!     & +z98(b,a,l,c,d,j,k,i)       ! 34521768 (+1.000)
!     & -z98(d,a,j,b,c,l,k,i)       ! 14732568 (-1.000)
!     & -z98(c,a,l,b,d,j,k,i)       ! 24531768 (-1.000)
!     & +z98(d,b,j,a,c,l,k,i)       ! 13742568 (+1.000)
!     & +z98(c,b,l,a,d,j,k,i)       ! 23541768 (+1.000)
!     & -z98(b,a,j,c,d,l,k,i)       ! 34721568 (-1.000)
!     & -z98(d,a,l,c,b,j,k,i)       ! 14523768 (-1.000)
!     & +z98(c,a,j,b,d,l,k,i)       ! 24731568 (+1.000)
!     & +z98(d,a,l,b,c,j,k,i)       ! 14532768 (+1.000)
!     & -z98(c,b,j,a,d,l,k,i)       ! 23741568 (-1.000)
!     & -z98(d,b,l,a,c,j,k,i)       ! 13542768 (-1.000)
!     & -z98(d,a,i,c,b,l,k,j)       ! 14823567 (-1.000)
!     & -z98(b,a,l,c,d,i,k,j)       ! 34521867 (-1.000)
!     & -z98(d,b,i,a,c,l,k,j)       ! 13842567 (-1.000)
!     & +z98(d,a,i,b,c,l,k,j)       ! 14832567 (+1.000)
!     & +z98(c,a,l,b,d,i,k,j)       ! 24531867 (+1.000)
!     & -z98(c,b,l,a,d,i,k,j)       ! 23541867 (-1.000)
!     & +z98(c,a,i,d,b,l,k,j)       ! 24813567 (+1.000)
!     & +z98(b,a,l,d,c,i,k,j)       ! 34512867 (+1.000)
!     & -z98(b,a,i,d,c,l,k,j)       ! 34812567 (-1.000)
!     & -z98(c,a,l,d,b,i,k,j)       ! 24513867 (-1.000)
!     & +z98(c,b,i,a,d,l,k,j)       ! 23841567 (+1.000)
!     & -z98(c,a,i,b,d,l,k,j)       ! 24831567 (-1.000)
!     & -z98(d,a,l,b,c,i,k,j)       ! 14532867 (-1.000)
!     & +z98(d,b,l,a,c,i,k,j)       ! 13542867 (+1.000)
!     & +z98(b,a,i,c,d,l,k,j)       ! 34821567 (+1.000)
!     & +z98(d,a,l,c,b,i,k,j)       ! 14523867 (+1.000)
!     & +z98(c,a,j,d,b,k,l,i)       ! 24713658 (+1.000)
!     & +z98(b,a,k,d,c,j,l,i)       ! 34612758 (+1.000)
!     & -z98(b,a,j,d,c,k,l,i)       ! 34712658 (-1.000)
!     & -z98(c,a,k,d,b,j,l,i)       ! 24613758 (-1.000)
!     & -z98(d,a,j,c,b,k,l,i)       ! 14723658 (-1.000)
!     & -z98(b,a,k,c,d,j,l,i)       ! 34621758 (-1.000)
!     & +z98(d,a,j,b,c,k,l,i)       ! 14732658 (+1.000)
!     & +z98(c,a,k,b,d,j,l,i)       ! 24631758 (+1.000)
!     & -z98(d,b,j,a,c,k,l,i)       ! 13742658 (-1.000)
!     & -z98(c,b,k,a,d,j,l,i)       ! 23641758 (-1.000)
!     & +z98(b,a,j,c,d,k,l,i)       ! 34721658 (+1.000)
!     & +z98(d,a,k,c,b,j,l,i)       ! 14623758 (+1.000)
!     & -z98(c,a,j,b,d,k,l,i)       ! 24731658 (-1.000)
!     & -z98(d,a,k,b,c,j,l,i)       ! 14632758 (-1.000)
!     & +z98(c,b,j,a,d,k,l,i)       ! 23741658 (+1.000)
!     & +z98(d,b,k,a,c,j,l,i)       ! 13642758 (+1.000)
!     & +z98(d,a,i,c,b,k,l,j)       ! 14823657 (+1.000)
!     & +z98(b,a,k,c,d,i,l,j)       ! 34621857 (+1.000)
!     & +z98(d,b,i,a,c,k,l,j)       ! 13842657 (+1.000)
!     & -z98(d,a,i,b,c,k,l,j)       ! 14832657 (-1.000)
!     & -z98(c,a,k,b,d,i,l,j)       ! 24631857 (-1.000)
!     & +z98(c,b,k,a,d,i,l,j)       ! 23641857 (+1.000)
!     & -z98(c,a,i,d,b,k,l,j)       ! 24813657 (-1.000)
!     & -z98(b,a,k,d,c,i,l,j)       ! 34612857 (-1.000)
!     & +z98(b,a,i,d,c,k,l,j)       ! 34812657 (+1.000)
!     & +z98(c,a,k,d,b,i,l,j)       ! 24613857 (+1.000)
!     & -z98(c,b,i,a,d,k,l,j)       ! 23841657 (-1.000)
!     & +z98(c,a,i,b,d,k,l,j)       ! 24831657 (+1.000)
!     & +z98(d,a,k,b,c,i,l,j)       ! 14632857 (+1.000)
!     & -z98(d,b,k,a,c,i,l,j)       ! 13642857 (-1.000)
!     & -z98(b,a,i,c,d,k,l,j)       ! 34821657 (-1.000)
!     & -z98(d,a,k,c,b,i,l,j)       ! 14623857 (-1.000)
!     & -z98(d,b,i,a,c,j,l,k)       ! 13842756 (-1.000)
!     & -z98(c,b,j,a,d,i,l,k)       ! 23741856 (-1.000)
!     & +z98(d,a,i,b,c,j,l,k)       ! 14832756 (+1.000)
!     & +z98(c,a,j,b,d,i,l,k)       ! 24731856 (+1.000)
!     & -z98(d,a,i,c,b,j,l,k)       ! 14823756 (-1.000)
!     & -z98(b,a,j,c,d,i,l,k)       ! 34721856 (-1.000)
!     & +z98(c,b,i,a,d,j,l,k)       ! 23841756 (+1.000)
!     & +z98(d,b,j,a,c,i,l,k)       ! 13742856 (+1.000)
!     & -z98(c,a,i,b,d,j,l,k)       ! 24831756 (-1.000)
!     & -z98(d,a,j,b,c,i,l,k)       ! 14732856 (-1.000)
!     & +z98(b,a,i,c,d,j,l,k)       ! 34821756 (+1.000)
!     & +z98(d,a,j,c,b,i,l,k)       ! 14723856 (+1.000)
!     & +z98(c,a,i,d,b,j,l,k)       ! 24813756 (+1.000)
!     & +z98(b,a,j,d,c,i,l,k)       ! 34712856 (+1.000)
!     & -z98(b,a,i,d,c,j,l,k)       ! 34812756 (-1.000)
!     & -z98(c,a,j,d,b,i,l,k)       ! 24713856 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z98)
       deallocate(u30)
c
       allocate(f1(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       call reorder541236(n1,n3,n0,n1,n0,n1,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,u10,f1)
       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
       allocate(u29(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1*k1*k3
       i2=k1*k3
       i3=k3*k1
       call egemm(i1,i2,i3,f1,d2,u29)
       deallocate(f1)
       deallocate(d2)
c
!       allocate(f1(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
!       call reorder631245(n1,n3,n0,n1,n1,n3,n0,n1,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,u29,f1)
!       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
!       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
!       allocate(z97(n1+1:n3,n1+1:n3,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
!       i1=k1*k1*k1*k3*k3
!       i2=k1*k3*k3
!       i3=k1
!       call egemm(i1,i2,i3,f1,d2,z97)
!       deallocate(f1)
!       deallocate(d2)
c
       do i=n0+1,n1-3;do j=i+1,n1-2;do k=j+1,n1-1;do l=k+1,n1
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-3;do b=a+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
          if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do n=n0+1,n1
             sum=sum
     &     + u29(a,l,d,j,i,n)*t2a(c,b,n,k)       !aldjincbnk      (+1.000)
     &     - u29(a,k,d,j,i,n)*t2a(c,b,n,l)       !akdjincbnl      (-1.000)
     &     - u29(a,l,c,j,i,n)*t2a(d,b,n,k)       !alcjindbnk      (-1.000)
     &     + u29(a,l,b,j,i,n)*t2a(d,c,n,k)       !albjindcnk      (+1.000)
     &     - u29(b,l,a,j,i,n)*t2a(d,c,n,k)       !blajindcnk      (-1.000)
     &     + u29(a,k,c,j,i,n)*t2a(d,b,n,l)       !akcjindbnl      (+1.000)
     &     - u29(a,k,b,j,i,n)*t2a(d,c,n,l)       !akbjindcnl      (-1.000)
     &     + u29(b,k,a,j,i,n)*t2a(d,c,n,l)       !bkajindcnl      (+1.000)
     &     - u29(a,l,d,k,i,n)*t2a(c,b,n,j)       !aldkincbnj      (-1.000)
     &     + u29(a,j,d,k,i,n)*t2a(c,b,n,l)       !ajdkincbnl      (+1.000)
     &     + u29(a,l,c,k,i,n)*t2a(d,b,n,j)       !alckindbnj      (+1.000)
     &     - u29(a,l,b,k,i,n)*t2a(d,c,n,j)       !albkindcnj      (-1.000)
     &     + u29(b,l,a,k,i,n)*t2a(d,c,n,j)       !blakindcnj      (+1.000)
     &     - u29(a,j,c,k,i,n)*t2a(d,b,n,l)       !ajckindbnl      (-1.000)
     &     + u29(a,j,b,k,i,n)*t2a(d,c,n,l)       !ajbkindcnl      (+1.000)
     &     - u29(b,j,a,k,i,n)*t2a(d,c,n,l)       !bjakindcnl      (-1.000)
     &     + u29(a,l,b,k,j,n)*t2a(d,c,n,i)       !albkjndcni      (+1.000)
     &     - u29(b,l,a,k,j,n)*t2a(d,c,n,i)       !blakjndcni      (-1.000)
     &     - u29(a,l,c,k,j,n)*t2a(d,b,n,i)       !alckjndbni      (-1.000)
     &     + u29(a,l,d,k,j,n)*t2a(c,b,n,i)       !aldkjncbni      (+1.000)
     &     - u29(a,i,d,k,j,n)*t2a(c,b,n,l)       !aidkjncbnl      (-1.000)
     &     + u29(a,i,c,k,j,n)*t2a(d,b,n,l)       !aickjndbnl      (+1.000)
     &     + u29(b,i,a,k,j,n)*t2a(d,c,n,l)       !biakjndcnl      (+1.000)
     &     - u29(a,i,b,k,j,n)*t2a(d,c,n,l)       !aibkjndcnl      (-1.000)
     &     + u29(a,k,d,l,i,n)*t2a(c,b,n,j)       !akdlincbnj      (+1.000)
     &     - u29(a,j,d,l,i,n)*t2a(c,b,n,k)       !ajdlincbnk      (-1.000)
     &     - u29(a,k,c,l,i,n)*t2a(d,b,n,j)       !akclindbnj      (-1.000)
     &     + u29(a,k,b,l,i,n)*t2a(d,c,n,j)       !akblindcnj      (+1.000)
     &     - u29(b,k,a,l,i,n)*t2a(d,c,n,j)       !bkalindcnj      (-1.000)
     &     + u29(a,j,c,l,i,n)*t2a(d,b,n,k)       !ajclindbnk      (+1.000)
     &     - u29(a,j,b,l,i,n)*t2a(d,c,n,k)       !ajblindcnk      (-1.000)
     &     + u29(b,j,a,l,i,n)*t2a(d,c,n,k)       !bjalindcnk      (+1.000)
     &     - u29(a,k,b,l,j,n)*t2a(d,c,n,i)       !akbljndcni      (-1.000)
     &     + u29(b,k,a,l,j,n)*t2a(d,c,n,i)       !bkaljndcni      (+1.000)
     &     + u29(a,k,c,l,j,n)*t2a(d,b,n,i)       !akcljndbni      (+1.000)
     &     - u29(a,k,d,l,j,n)*t2a(c,b,n,i)       !akdljncbni      (-1.000)
     &     + u29(a,i,d,l,j,n)*t2a(c,b,n,k)       !aidljncbnk      (+1.000)
     &     - u29(a,i,c,l,j,n)*t2a(d,b,n,k)       !aicljndbnk      (-1.000)
     &     - u29(b,i,a,l,j,n)*t2a(d,c,n,k)       !bialjndcnk      (-1.000)
     &     + u29(a,i,b,l,j,n)*t2a(d,c,n,k)       !aibljndcnk      (+1.000)
     &     - u29(b,j,a,l,k,n)*t2a(d,c,n,i)       !bjalkndcni      (-1.000)
     &     + u29(a,j,b,l,k,n)*t2a(d,c,n,i)       !ajblkndcni      (+1.000)
     &     - u29(a,j,c,l,k,n)*t2a(d,b,n,i)       !ajclkndbni      (-1.000)
     &     + u29(b,i,a,l,k,n)*t2a(d,c,n,j)       !bialkndcnj      (+1.000)
     &     - u29(a,i,b,l,k,n)*t2a(d,c,n,j)       !aiblkndcnj      (-1.000)
     &     + u29(a,i,c,l,k,n)*t2a(d,b,n,j)       !aiclkndbnj      (+1.000)
     &     + u29(a,j,d,l,k,n)*t2a(c,b,n,i)       !ajdlkncbni      (+1.000)
     &     - u29(a,i,d,l,k,n)*t2a(c,b,n,j)       !aidlkncbnj      (-1.000)             
             enddo
             v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call
!     & sum23614578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z97, 1.000)
!       call
!     & sum23514678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z97,-1.000)
!       call
!     & sum13624578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z97,-1.000)
!       call
!     & sum12634578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z97, 1.000)
!       call
!     & sum12643578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z97,-1.000)
!       call
!     & sum13524678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z97, 1.000)
!       call
!     & sum12534678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z97,-1.000)
!       call
!     & sum12543678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z97, 1.000)
!       call
!     & sum23714568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z97,-1.000)
!       call
!     & sum23514768(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z97, 1.000)
!       call
!     & sum13724568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z97, 1.000)
!       call
!     & sum12734568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z97,-1.000)
!       call
!     & sum12743568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z97, 1.000)
!       call
!     & sum13524768(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z97,-1.000)
!       call
!     & sum12534768(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z97, 1.000)
!       call
!     & sum12543768(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z97,-1.000)
!       call
!     & sum12834567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z97, 1.000)
!       call
!     & sum12843567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z97,-1.000)
!       call
!     & sum13824567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z97,-1.000)
!       call
!     & sum23814567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z97, 1.000)
!       call
!     & sum23514867(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z97,-1.000)
!       call
!     & sum13524867(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z97, 1.000)
!       call
!     & sum12543867(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z97, 1.000)
!       call
!     & sum12534867(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z97,-1.000)
!       call
!     & sum23714658(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z97, 1.000)
!       call
!     & sum23614758(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z97,-1.000)
!       call
!     & sum13724658(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z97,-1.000)
!       call
!     & sum12734658(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z97, 1.000)
!       call
!     & sum12743658(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z97,-1.000)
!       call
!     & sum13624758(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z97, 1.000)
!       call
!     & sum12634758(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z97,-1.000)
!       call
!     & sum12643758(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z97, 1.000)
!       call
!     & sum12834657(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z97,-1.000)
!       call
!     & sum12843657(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z97, 1.000)
!       call
!     & sum13824657(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z97, 1.000)
!       call
!     & sum23814657(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z97,-1.000)
!       call
!     & sum23614857(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z97, 1.000)
!       call
!     & sum13624857(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z97,-1.000)
!       call
!     & sum12643857(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z97,-1.000)
!       call
!     & sum12634857(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z97, 1.000)
!       call
!     & sum12843756(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z97,-1.000)
!       call
!     & sum12834756(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z97, 1.000)
!       call
!     & sum13824756(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z97,-1.000)
!       call
!     & sum12743856(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z97, 1.000)
!       call
!     & sum12734856(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z97,-1.000)
!       call
!     & sum13724856(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z97, 1.000)
!       call
!     & sum23814756(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z97, 1.000)
!       call
!     & sum23714856(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z97,-1.000)
!c
!       do i=n0+1,n1-3
!       do j=i+1,n1-2
!       do k=j+1,n1-1
!       do l=k+1,n1
!       do a=n1+1,n3-3
!       do b=a+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)
!     & +z97(c,b,k,d,a,l,j,i)       ! 23614578 (+1.000)
!     & -z97(c,b,l,d,a,k,j,i)       ! 23514678 (-1.000)
!     & -z97(d,b,k,c,a,l,j,i)       ! 13624578 (-1.000)
!     & +z97(d,c,k,b,a,l,j,i)       ! 12634578 (+1.000)
!     & -z97(d,c,k,a,b,l,j,i)       ! 12643578 (-1.000)
!     & +z97(d,b,l,c,a,k,j,i)       ! 13524678 (+1.000)
!     & -z97(d,c,l,b,a,k,j,i)       ! 12534678 (-1.000)
!     & +z97(d,c,l,a,b,k,j,i)       ! 12543678 (+1.000)
!     & -z97(c,b,j,d,a,l,k,i)       ! 23714568 (-1.000)
!     & +z97(c,b,l,d,a,j,k,i)       ! 23514768 (+1.000)
!     & +z97(d,b,j,c,a,l,k,i)       ! 13724568 (+1.000)
!     & -z97(d,c,j,b,a,l,k,i)       ! 12734568 (-1.000)
!     & +z97(d,c,j,a,b,l,k,i)       ! 12743568 (+1.000)
!     & -z97(d,b,l,c,a,j,k,i)       ! 13524768 (-1.000)
!     & +z97(d,c,l,b,a,j,k,i)       ! 12534768 (+1.000)
!     & -z97(d,c,l,a,b,j,k,i)       ! 12543768 (-1.000)
!     & +z97(d,c,i,b,a,l,k,j)       ! 12834567 (+1.000)
!     & -z97(d,c,i,a,b,l,k,j)       ! 12843567 (-1.000)
!     & -z97(d,b,i,c,a,l,k,j)       ! 13824567 (-1.000)
!     & +z97(c,b,i,d,a,l,k,j)       ! 23814567 (+1.000)
!     & -z97(c,b,l,d,a,i,k,j)       ! 23514867 (-1.000)
!     & +z97(d,b,l,c,a,i,k,j)       ! 13524867 (+1.000)
!     & +z97(d,c,l,a,b,i,k,j)       ! 12543867 (+1.000)
!     & -z97(d,c,l,b,a,i,k,j)       ! 12534867 (-1.000)
!     & +z97(c,b,j,d,a,k,l,i)       ! 23714658 (+1.000)
!     & -z97(c,b,k,d,a,j,l,i)       ! 23614758 (-1.000)
!     & -z97(d,b,j,c,a,k,l,i)       ! 13724658 (-1.000)
!     & +z97(d,c,j,b,a,k,l,i)       ! 12734658 (+1.000)
!     & -z97(d,c,j,a,b,k,l,i)       ! 12743658 (-1.000)
!     & +z97(d,b,k,c,a,j,l,i)       ! 13624758 (+1.000)
!     & -z97(d,c,k,b,a,j,l,i)       ! 12634758 (-1.000)
!     & +z97(d,c,k,a,b,j,l,i)       ! 12643758 (+1.000)
!     & -z97(d,c,i,b,a,k,l,j)       ! 12834657 (-1.000)
!     & +z97(d,c,i,a,b,k,l,j)       ! 12843657 (+1.000)
!     & +z97(d,b,i,c,a,k,l,j)       ! 13824657 (+1.000)
!     & -z97(c,b,i,d,a,k,l,j)       ! 23814657 (-1.000)
!     & +z97(c,b,k,d,a,i,l,j)       ! 23614857 (+1.000)
!     & -z97(d,b,k,c,a,i,l,j)       ! 13624857 (-1.000)
!     & -z97(d,c,k,a,b,i,l,j)       ! 12643857 (-1.000)
!     & +z97(d,c,k,b,a,i,l,j)       ! 12634857 (+1.000)
!     & -z97(d,c,i,a,b,j,l,k)       ! 12843756 (-1.000)
!     & +z97(d,c,i,b,a,j,l,k)       ! 12834756 (+1.000)
!     & -z97(d,b,i,c,a,j,l,k)       ! 13824756 (-1.000)
!     & +z97(d,c,j,a,b,i,l,k)       ! 12743856 (+1.000)
!     & -z97(d,c,j,b,a,i,l,k)       ! 12734856 (-1.000)
!     & +z97(d,b,j,c,a,i,l,k)       ! 13724856 (+1.000)
!     & +z97(c,b,i,d,a,j,l,k)       ! 23814756 (+1.000)
!     & -z97(c,b,j,d,a,i,l,k)       ! 23714856 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z97)
       deallocate(u29)
c
       allocate(f1(n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder564123(n1,n3,n0,n1,n0,n1,n1,n3,n0,n1,n0,n1,
     & n0,n1,n0,n1,n1,n3,n1,n3,n0,n1,n0,n1,u10,f1)
       allocate(d2(n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3))
       call reorder4312(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n0,n1,n1,n3,n1,n3,t2a,d2)
       allocate(u28(n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       i1=k1*k1*k3*k3
       i2=k3*k3
       i3=k1*k1
       call egemm(i1,i2,i3,f1,d2,u28)
       deallocate(f1)
       deallocate(d2)
c
!       allocate(f1(n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!       call reorder341256(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,
!     & n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,u28,f1)
!       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
!     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
!       allocate(z96(n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!       i1=k1*k1*k3*k3*k3
!       i2=k1*k1*k3
!       i3=k3
!       call egemm(i1,i2,i3,f1,d2,z96)
!       deallocate(f1)
!       deallocate(d2)
c
       do i=n0+1,n1-3;do j=i+1,n1-2;do k=j+1,n1-1;do l=k+1,n1
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-3;do b=a+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
          if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n1+1,n3
             sum=sum                             !top 2 switched
     &     + (u28(c,a,f,b,l,k)*t2a(f,d,j,i)      !cafblkfdji      (+0.500)
     &     - u28(b,a,f,c,l,k)*t2a(f,d,j,i)       !bafclkfdji      (-0.500)
     &     - u28(c,b,f,a,l,k)*t2a(f,d,j,i)       !cbfalkfdji      (-0.500)
     &     - u28(b,a,f,c,j,i)*t2a(f,d,l,k)       !bafcjifdlk      (-0.500)
     &     + u28(c,a,f,b,j,i)*t2a(f,d,l,k)       !cafbjifdlk      (+0.500)
     &     - u28(c,b,f,a,j,i)*t2a(f,d,l,k)       !cbfajifdlk      (-0.500)
     &     - u28(d,a,f,b,l,k)*t2a(f,c,j,i)       !dafblkfcji      (-0.500)
     &     + u28(d,b,f,a,l,k)*t2a(f,c,j,i)       !dbfalkfcji      (+0.500)
     &     - u28(d,a,f,b,j,i)*t2a(f,c,l,k)       !dafbjifclk      (-0.500)
     &     + u28(d,b,f,a,j,i)*t2a(f,c,l,k)       !dbfajifclk      (+0.500)
     &     - u28(d,c,f,a,l,k)*t2a(f,b,j,i)       !dcfalkfbji      (-0.500)
     &     - u28(d,c,f,a,j,i)*t2a(f,b,l,k)       !dcfajifblk      (-0.500)
     &     + u28(b,a,f,c,l,j)*t2a(f,d,k,i)       !bafcljfdki      (+0.500)
     &     - u28(c,a,f,b,l,j)*t2a(f,d,k,i)       !cafbljfdki      (-0.500)
     &     + u28(c,b,f,a,l,j)*t2a(f,d,k,i)       !cbfaljfdki      (+0.500)
     &     + u28(b,a,f,c,k,i)*t2a(f,d,l,j)       !bafckifdlj      (+0.500)
     &     - u28(c,a,f,b,k,i)*t2a(f,d,l,j)       !cafbkifdlj      (-0.500)
     &     + u28(c,b,f,a,k,i)*t2a(f,d,l,j)       !cbfakifdlj      (+0.500)
     &     + u28(d,a,f,b,l,j)*t2a(f,c,k,i)       !dafbljfcki      (+0.500)
     &     - u28(d,b,f,a,l,j)*t2a(f,c,k,i)       !dbfaljfcki      (-0.500)
     &     + u28(d,a,f,b,k,i)*t2a(f,c,l,j)       !dafbkifclj      (+0.500)
     &     - u28(d,b,f,a,k,i)*t2a(f,c,l,j)       !dbfakifclj      (-0.500)
     &     + u28(d,c,f,a,l,j)*t2a(f,b,k,i)       !dcfaljfbki      (+0.500)
     &     + u28(d,c,f,a,k,i)*t2a(f,b,l,j)       !dcfakifblj      (+0.500)
     &     - u28(b,a,f,c,k,j)*t2a(f,d,l,i)       !bafckjfdli      (-0.500)
     &     + u28(c,a,f,b,k,j)*t2a(f,d,l,i)       !cafbkjfdli      (+0.500)
     &     - u28(c,b,f,a,k,j)*t2a(f,d,l,i)       !cbfakjfdli      (-0.500)
     &     - u28(b,a,f,c,l,i)*t2a(f,d,k,j)       !bafclifdkj      (-0.500)
     &     + u28(c,a,f,b,l,i)*t2a(f,d,k,j)       !cafblifdkj      (+0.500)
     &     - u28(c,b,f,a,l,i)*t2a(f,d,k,j)       !cbfalifdkj      (-0.500)
     &     - u28(d,a,f,b,k,j)*t2a(f,c,l,i)       !dafbkjfcli      (-0.500)
     &     + u28(d,b,f,a,k,j)*t2a(f,c,l,i)       !dbfakjfcli      (+0.500)
     &     - u28(d,a,f,b,l,i)*t2a(f,c,k,j)       !dafblifckj      (-0.500)
     &     + u28(d,b,f,a,l,i)*t2a(f,c,k,j)       !dbfalifckj      (+0.500)
     &     - u28(d,c,f,a,k,j)*t2a(f,b,l,i)       !dcfakjfbli      (-0.500)
     &     - u28(d,c,f,a,l,i)*t2a(f,b,k,j))/2.0d0!dcfalifbkj      (-0.500)
             enddo
             v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call
!     & sum17823456(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z96,-0.500)
!       call
!     & sum17832456(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z96, 0.500)
!       call
!     & sum17842356(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z96,-0.500)
!       call
!     & sum15623478(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z96,-0.500)
!       call
!     & sum15632478(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z96, 0.500)
!       call
!     & sum15642378(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z96,-0.500)
!       call
!     & sum27831456(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z96,-0.500)
!       call
!     & sum27841356(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z96, 0.500)
!       call
!     & sum25631478(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z96,-0.500)
!       call
!     & sum25641378(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z96, 0.500)
!       call
!     & sum37841256(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z96,-0.500)
!       call
!     & sum35641278(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z96,-0.500)
!       call
!     & sum16823457(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z96, 0.500)
!       call
!     & sum16832457(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z96,-0.500)
!       call
!     & sum16842357(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z96, 0.500)
!       call
!     & sum15723468(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z96, 0.500)
!       call
!     & sum15732468(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z96,-0.500)
!       call
!     & sum15742368(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z96, 0.500)
!       call
!     & sum26831457(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z96, 0.500)
!       call
!     & sum26841357(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z96,-0.500)
!       call
!     & sum25731468(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z96, 0.500)
!       call
!     & sum25741368(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z96,-0.500)
!       call
!     & sum36841257(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z96, 0.500)
!       call
!     & sum35741268(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z96, 0.500)
!       call
!     & sum15823467(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z96,-0.500)
!       call
!     & sum15832467(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z96, 0.500)
!       call
!     & sum15842367(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z96,-0.500)
!       call
!     & sum16723458(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z96,-0.500)
!       call
!     & sum16732458(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z96, 0.500)
!       call
!     & sum16742358(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z96,-0.500)
!       call
!     & sum25831467(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z96,-0.500)
!       call
!     & sum25841367(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z96, 0.500)
!       call
!     & sum26731458(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z96,-0.500)
!       call
!     & sum26741358(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z96, 0.500)
!       call
!     & sum35841267(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z96,-0.500)
!       call
!     & sum36741258(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z96,-0.500)
!c
!       do i=n0+1,n1-3
!       do j=i+1,n1-2
!       do k=j+1,n1-1
!       do l=k+1,n1
!       do a=n1+1,n3-3
!       do b=a+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)
!     & +(z96(d,j,i,b,c,a,l,k)      ! 17832456 (+0.500) top two swithched
!     & -z96(d,j,i,c,b,a,l,k)       ! 17823456 (-0.500)
!     & -z96(d,j,i,a,c,b,l,k)       ! 17842356 (-0.500)
!     & -z96(d,l,k,c,b,a,j,i)       ! 15623478 (-0.500)
!     & +z96(d,l,k,b,c,a,j,i)       ! 15632478 (+0.500)
!     & -z96(d,l,k,a,c,b,j,i)       ! 15642378 (-0.500)
!     & -z96(c,j,i,b,d,a,l,k)       ! 27831456 (-0.500)
!     & +z96(c,j,i,a,d,b,l,k)       ! 27841356 (+0.500)
!     & -z96(c,l,k,b,d,a,j,i)       ! 25631478 (-0.500)
!     & +z96(c,l,k,a,d,b,j,i)       ! 25641378 (+0.500)
!     & -z96(b,j,i,a,d,c,l,k)       ! 37841256 (-0.500)
!     & -z96(b,l,k,a,d,c,j,i)       ! 35641278 (-0.500)
!     & +z96(d,k,i,c,b,a,l,j)       ! 16823457 (+0.500)
!     & -z96(d,k,i,b,c,a,l,j)       ! 16832457 (-0.500)
!     & +z96(d,k,i,a,c,b,l,j)       ! 16842357 (+0.500)
!     & +z96(d,l,j,c,b,a,k,i)       ! 15723468 (+0.500)
!     & -z96(d,l,j,b,c,a,k,i)       ! 15732468 (-0.500)
!     & +z96(d,l,j,a,c,b,k,i)       ! 15742368 (+0.500)
!     & +z96(c,k,i,b,d,a,l,j)       ! 26831457 (+0.500)
!     & -z96(c,k,i,a,d,b,l,j)       ! 26841357 (-0.500)
!     & +z96(c,l,j,b,d,a,k,i)       ! 25731468 (+0.500)
!     & -z96(c,l,j,a,d,b,k,i)       ! 25741368 (-0.500)
!     & +z96(b,k,i,a,d,c,l,j)       ! 36841257 (+0.500)
!     & +z96(b,l,j,a,d,c,k,i)       ! 35741268 (+0.500)
!     & -z96(d,l,i,c,b,a,k,j)       ! 15823467 (-0.500)
!     & +z96(d,l,i,b,c,a,k,j)       ! 15832467 (+0.500)
!     & -z96(d,l,i,a,c,b,k,j)       ! 15842367 (-0.500)
!     & -z96(d,k,j,c,b,a,l,i)       ! 16723458 (-0.500)
!     & +z96(d,k,j,b,c,a,l,i)       ! 16732458 (+0.500)
!     & -z96(d,k,j,a,c,b,l,i)       ! 16742358 (-0.500)
!     & -z96(c,l,i,b,d,a,k,j)       ! 25831467 (-0.500)
!     & +z96(c,l,i,a,d,b,k,j)       ! 25841367 (+0.500)
!     & -z96(c,k,j,b,d,a,l,i)       ! 26731458 (-0.500)
!     & +z96(c,k,j,a,d,b,l,i)       ! 26741358 (+0.500)
!     & -z96(b,l,i,a,d,c,k,j)       ! 35841267 (-0.500)
!     & -z96(b,k,j,a,d,c,l,i))/2.0d0! 36741258 (-0.500)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z96)
       deallocate(u28)
c
       allocate(f1(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       call reorder541236(n1,n3,n0,n1,n0,n1,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,u10,f1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s43(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1*k1*k3
       i3=k3*k1
       call egemm1(i1,i3,f1,b2,s43)
       deallocate(f1)
       deallocate(b2)
c
       call sum2341(n0,n1,n1,n3,n0,n1,n0,n1,x1,s43,-1.000)
       deallocate(s43)
c
       allocate(f1(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       call reorder541236(n1,n3,n0,n1,n0,n1,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,u10,f1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(u24(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1*k1*k3*k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,f1,b2,u24)
       deallocate(f1)
       deallocate(b2)
c
!       allocate(f1(n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!       call reorder621345(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,u24,f1)
!       allocate(f2(n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!       call reorder412356(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,t3a,f2)
!       allocate(z84(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!       i1=k1*k1*k3*k3
!       i2=k1*k1*k3*k3
!       i3=k3*k1
!       call egemm(i1,i2,i3,f1,f2,z84)
!       deallocate(f1)
!       deallocate(f2)
c
       do i=n0+1,n1-3;do j=i+1,n1-2;do k=j+1,n1-1;do l=k+1,n1
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-3;do b=a+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
          if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n1+1,n3;do n=n0+1,n1
             sum=sum
     &     - u24(d,f,c,j,i,n)*t3a(f,b,a,n,l,k)     !dfcjinfbanlk    (-1.000)
     &     + u24(d,f,b,j,i,n)*t3a(f,c,a,n,l,k)     !dfbjinfcanlk    (+1.000)
     &     - u24(d,f,a,j,i,n)*t3a(f,c,b,n,l,k)     !dfajinfcbnlk    (-1.000)
     &     + u24(c,f,d,j,i,n)*t3a(f,b,a,n,l,k)     !cfdjinfbanlk    (+1.000)
     &     - u24(b,f,d,j,i,n)*t3a(f,c,a,n,l,k)     !bfdjinfcanlk    (-1.000)
     &     + u24(a,f,d,j,i,n)*t3a(f,c,b,n,l,k)     !afdjinfcbnlk    (+1.000)
     &     - u24(c,f,b,j,i,n)*t3a(f,d,a,n,l,k)     !cfbjinfdanlk    (-1.000)
     &     + u24(c,f,a,j,i,n)*t3a(f,d,b,n,l,k)     !cfajinfdbnlk    (+1.000)
     &     + u24(b,f,c,j,i,n)*t3a(f,d,a,n,l,k)     !bfcjinfdanlk    (+1.000)
     &     - u24(a,f,c,j,i,n)*t3a(f,d,b,n,l,k)     !afcjinfdbnlk    (-1.000)
     &     - u24(b,f,a,j,i,n)*t3a(f,d,c,n,l,k)     !bfajinfdcnlk    (-1.000)
     &     + u24(a,f,b,j,i,n)*t3a(f,d,c,n,l,k)     !afbjinfdcnlk    (+1.000)
     &     + u24(d,f,c,k,i,n)*t3a(f,b,a,n,l,j)     !dfckinfbanlj    (+1.000)
     &     - u24(d,f,b,k,i,n)*t3a(f,c,a,n,l,j)     !dfbkinfcanlj    (-1.000)
     &     + u24(d,f,a,k,i,n)*t3a(f,c,b,n,l,j)     !dfakinfcbnlj    (+1.000)
     &     - u24(c,f,d,k,i,n)*t3a(f,b,a,n,l,j)     !cfdkinfbanlj    (-1.000)
     &     + u24(b,f,d,k,i,n)*t3a(f,c,a,n,l,j)     !bfdkinfcanlj    (+1.000)
     &     - u24(a,f,d,k,i,n)*t3a(f,c,b,n,l,j)     !afdkinfcbnlj    (-1.000)
     &     + u24(c,f,b,k,i,n)*t3a(f,d,a,n,l,j)     !cfbkinfdanlj    (+1.000)
     &     - u24(c,f,a,k,i,n)*t3a(f,d,b,n,l,j)     !cfakinfdbnlj    (-1.000)
     &     - u24(b,f,c,k,i,n)*t3a(f,d,a,n,l,j)     !bfckinfdanlj    (-1.000)
     &     + u24(a,f,c,k,i,n)*t3a(f,d,b,n,l,j)     !afckinfdbnlj    (+1.000)
     &     + u24(b,f,a,k,i,n)*t3a(f,d,c,n,l,j)     !bfakinfdcnlj    (+1.000)
     &     - u24(a,f,b,k,i,n)*t3a(f,d,c,n,l,j)     !afbkinfdcnlj    (-1.000)
     &     - u24(d,f,c,l,i,n)*t3a(f,b,a,n,k,j)     !dfclinfbankj    (-1.000)
     &     + u24(d,f,b,l,i,n)*t3a(f,c,a,n,k,j)     !dfblinfcankj    (+1.000)
     &     - u24(d,f,a,l,i,n)*t3a(f,c,b,n,k,j)     !dfalinfcbnkj    (-1.000)
     &     + u24(c,f,d,l,i,n)*t3a(f,b,a,n,k,j)     !cfdlinfbankj    (+1.000)
     &     - u24(b,f,d,l,i,n)*t3a(f,c,a,n,k,j)     !bfdlinfcankj    (-1.000)
     &     + u24(a,f,d,l,i,n)*t3a(f,c,b,n,k,j)     !afdlinfcbnkj    (+1.000)
     &     - u24(c,f,b,l,i,n)*t3a(f,d,a,n,k,j)     !cfblinfdankj    (-1.000)
     &     + u24(c,f,a,l,i,n)*t3a(f,d,b,n,k,j)     !cfalinfdbnkj    (+1.000)
     &     + u24(b,f,c,l,i,n)*t3a(f,d,a,n,k,j)     !bfclinfdankj    (+1.000)
     &     - u24(a,f,c,l,i,n)*t3a(f,d,b,n,k,j)     !afclinfdbnkj    (-1.000)
     &     - u24(b,f,a,l,i,n)*t3a(f,d,c,n,k,j)     !bfalinfdcnkj    (-1.000)
     &     + u24(a,f,b,l,i,n)*t3a(f,d,c,n,k,j)     !afblinfdcnkj    (+1.000)
     &     - u24(d,f,c,k,j,n)*t3a(f,b,a,n,l,i)     !dfckjnfbanli    (-1.000)
     &     + u24(d,f,b,k,j,n)*t3a(f,c,a,n,l,i)     !dfbkjnfcanli    (+1.000)
     &     - u24(d,f,a,k,j,n)*t3a(f,c,b,n,l,i)     !dfakjnfcbnli    (-1.000)
     &     + u24(c,f,d,k,j,n)*t3a(f,b,a,n,l,i)     !cfdkjnfbanli    (+1.000)
     &     - u24(b,f,d,k,j,n)*t3a(f,c,a,n,l,i)     !bfdkjnfcanli    (-1.000)
     &     + u24(a,f,d,k,j,n)*t3a(f,c,b,n,l,i)     !afdkjnfcbnli    (+1.000)
     &     - u24(c,f,b,k,j,n)*t3a(f,d,a,n,l,i)     !cfbkjnfdanli    (-1.000)
     &     + u24(c,f,a,k,j,n)*t3a(f,d,b,n,l,i)     !cfakjnfdbnli    (+1.000)
     &     + u24(b,f,c,k,j,n)*t3a(f,d,a,n,l,i)     !bfckjnfdanli    (+1.000)
     &     - u24(a,f,c,k,j,n)*t3a(f,d,b,n,l,i)     !afckjnfdbnli    (-1.000)
     &     - u24(b,f,a,k,j,n)*t3a(f,d,c,n,l,i)     !bfakjnfdcnli    (-1.000)
     &     + u24(a,f,b,k,j,n)*t3a(f,d,c,n,l,i)     !afbkjnfdcnli    (+1.000)
     &     + u24(d,f,c,l,j,n)*t3a(f,b,a,n,k,i)     !dfcljnfbanki    (+1.000)
     &     - u24(d,f,b,l,j,n)*t3a(f,c,a,n,k,i)     !dfbljnfcanki    (-1.000)
     &     + u24(d,f,a,l,j,n)*t3a(f,c,b,n,k,i)     !dfaljnfcbnki    (+1.000)
     &     - u24(c,f,d,l,j,n)*t3a(f,b,a,n,k,i)     !cfdljnfbanki    (-1.000)
     &     + u24(b,f,d,l,j,n)*t3a(f,c,a,n,k,i)     !bfdljnfcanki    (+1.000)
     &     - u24(a,f,d,l,j,n)*t3a(f,c,b,n,k,i)     !afdljnfcbnki    (-1.000)
     &     + u24(c,f,b,l,j,n)*t3a(f,d,a,n,k,i)     !cfbljnfdanki    (+1.000)
     &     - u24(c,f,a,l,j,n)*t3a(f,d,b,n,k,i)     !cfaljnfdbnki    (-1.000)
     &     - u24(b,f,c,l,j,n)*t3a(f,d,a,n,k,i)     !bfcljnfdanki    (-1.000)
     &     + u24(a,f,c,l,j,n)*t3a(f,d,b,n,k,i)     !afcljnfdbnki    (+1.000)
     &     + u24(b,f,a,l,j,n)*t3a(f,d,c,n,k,i)     !bfaljnfdcnki    (+1.000)
     &     - u24(a,f,b,l,j,n)*t3a(f,d,c,n,k,i)     !afbljnfdcnki    (-1.000)
     &     - u24(d,f,c,l,k,n)*t3a(f,b,a,n,j,i)     !dfclknfbanji    (-1.000)
     &     + u24(d,f,b,l,k,n)*t3a(f,c,a,n,j,i)     !dfblknfcanji    (+1.000)
     &     - u24(d,f,a,l,k,n)*t3a(f,c,b,n,j,i)     !dfalknfcbnji    (-1.000)
     &     + u24(c,f,d,l,k,n)*t3a(f,b,a,n,j,i)     !cfdlknfbanji    (+1.000)
     &     - u24(b,f,d,l,k,n)*t3a(f,c,a,n,j,i)     !bfdlknfcanji    (-1.000)
     &     + u24(a,f,d,l,k,n)*t3a(f,c,b,n,j,i)     !afdlknfcbnji    (+1.000)
     &     - u24(c,f,b,l,k,n)*t3a(f,d,a,n,j,i)     !cfblknfdanji    (-1.000)
     &     + u24(c,f,a,l,k,n)*t3a(f,d,b,n,j,i)     !cfalknfdbnji    (+1.000)
     &     + u24(b,f,c,l,k,n)*t3a(f,d,a,n,j,i)     !bfclknfdanji    (+1.000)
     &     - u24(a,f,c,l,k,n)*t3a(f,d,b,n,j,i)     !afclknfdbnji    (-1.000)
     &     - u24(b,f,a,l,k,n)*t3a(f,d,c,n,j,i)     !bfalknfdcnji    (-1.000)
     &     + u24(a,f,b,l,k,n)*t3a(f,d,c,n,j,i)     !afblknfdcnji    (+1.000)             
             enddo;enddo
             v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call
!     & sum34561278(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z84,-1.000)
!       call
!     & sum24561378(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z84, 1.000)
!       call
!     & sum23561478(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z84,-1.000)
!       call
!     & sum34562178(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z84, 1.000)
!       call
!     & sum24563178(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z84,-1.000)
!       call
!     & sum23564178(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z84, 1.000)
!       call
!     & sum14562378(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z84,-1.000)
!       call
!     & sum13562478(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z84, 1.000)
!       call
!     & sum14563278(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z84, 1.000)
!       call
!     & sum13564278(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z84,-1.000)
!       call
!     & sum12563478(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z84,-1.000)
!       call
!     & sum12564378(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z84, 1.000)
!       call
!     & sum34571268(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z84, 1.000)
!       call
!     & sum24571368(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z84,-1.000)
!       call
!     & sum23571468(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z84, 1.000)
!       call
!     & sum34572168(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z84,-1.000)
!       call
!     & sum24573168(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z84, 1.000)
!       call
!     & sum23574168(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z84,-1.000)
!       call
!     & sum14572368(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z84, 1.000)
!       call
!     & sum13572468(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z84,-1.000)
!       call
!     & sum14573268(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z84,-1.000)
!       call
!     & sum13574268(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z84, 1.000)
!       call
!     & sum12573468(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z84, 1.000)
!       call
!     & sum12574368(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z84,-1.000)
!       call
!     & sum34671258(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z84,-1.000)
!       call
!     & sum24671358(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z84, 1.000)
!       call
!     & sum23671458(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z84,-1.000)
!       call
!     & sum34672158(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z84, 1.000)
!       call
!     & sum24673158(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z84,-1.000)
!       call
!     & sum23674158(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z84, 1.000)
!       call
!     & sum14672358(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z84,-1.000)
!       call
!     & sum13672458(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z84, 1.000)
!       call
!     & sum14673258(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z84, 1.000)
!       call
!     & sum13674258(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z84,-1.000)
!       call
!     & sum12673458(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z84,-1.000)
!       call
!     & sum12674358(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z84, 1.000)
!       call
!     & sum34581267(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z84,-1.000)
!       call
!     & sum24581367(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z84, 1.000)
!       call
!     & sum23581467(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z84,-1.000)
!       call
!     & sum34582167(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z84, 1.000)
!       call
!     & sum24583167(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z84,-1.000)
!       call
!     & sum23584167(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z84, 1.000)
!       call
!     & sum14582367(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z84,-1.000)
!       call
!     & sum13582467(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z84, 1.000)
!       call
!     & sum14583267(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z84, 1.000)
!       call
!     & sum13584267(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z84,-1.000)
!       call
!     & sum12583467(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z84,-1.000)
!       call
!     & sum12584367(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z84, 1.000)
!       call
!     & sum34681257(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z84, 1.000)
!       call
!     & sum24681357(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z84,-1.000)
!       call
!     & sum23681457(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z84, 1.000)
!       call
!     & sum34682157(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z84,-1.000)
!       call
!     & sum24683157(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z84, 1.000)
!       call
!     & sum23684157(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z84,-1.000)
!       call
!     & sum14682357(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z84, 1.000)
!       call
!     & sum13682457(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z84,-1.000)
!       call
!     & sum14683257(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z84,-1.000)
!       call
!     & sum13684257(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z84, 1.000)
!       call
!     & sum12683457(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z84, 1.000)
!       call
!     & sum12684357(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z84,-1.000)
!       call
!     & sum34781256(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z84,-1.000)
!       call
!     & sum24781356(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z84, 1.000)
!       call
!     & sum23781456(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z84,-1.000)
!       call
!     & sum34782156(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z84, 1.000)
!       call
!     & sum24783156(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z84,-1.000)
!       call
!     & sum23784156(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z84, 1.000)
!       call
!     & sum14782356(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z84,-1.000)
!       call
!     & sum13782456(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z84, 1.000)
!       call
!     & sum14783256(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z84, 1.000)
!       call
!     & sum13784256(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z84,-1.000)
!       call
!     & sum12783456(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z84,-1.000)
!       call
!     & sum12784356(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z84, 1.000)
!c
!       do i=n0+1,n1-3
!       do j=i+1,n1-2
!       do k=j+1,n1-1
!       do l=k+1,n1
!       do a=n1+1,n3-3
!       do b=a+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)
!     & -z84(b,a,l,k,d,c,j,i)       ! 34561278 (-1.000)
!     & +z84(c,a,l,k,d,b,j,i)       ! 24561378 (+1.000)
!     & -z84(c,b,l,k,d,a,j,i)       ! 23561478 (-1.000)
!     & +z84(b,a,l,k,c,d,j,i)       ! 34562178 (+1.000)
!     & -z84(c,a,l,k,b,d,j,i)       ! 24563178 (-1.000)
!     & +z84(c,b,l,k,a,d,j,i)       ! 23564178 (+1.000)
!     & -z84(d,a,l,k,c,b,j,i)       ! 14562378 (-1.000)
!     & +z84(d,b,l,k,c,a,j,i)       ! 13562478 (+1.000)
!     & +z84(d,a,l,k,b,c,j,i)       ! 14563278 (+1.000)
!     & -z84(d,b,l,k,a,c,j,i)       ! 13564278 (-1.000)
!     & -z84(d,c,l,k,b,a,j,i)       ! 12563478 (-1.000)
!     & +z84(d,c,l,k,a,b,j,i)       ! 12564378 (+1.000)
!     & +z84(b,a,l,j,d,c,k,i)       ! 34571268 (+1.000)
!     & -z84(c,a,l,j,d,b,k,i)       ! 24571368 (-1.000)
!     & +z84(c,b,l,j,d,a,k,i)       ! 23571468 (+1.000)
!     & -z84(b,a,l,j,c,d,k,i)       ! 34572168 (-1.000)
!     & +z84(c,a,l,j,b,d,k,i)       ! 24573168 (+1.000)
!     & -z84(c,b,l,j,a,d,k,i)       ! 23574168 (-1.000)
!     & +z84(d,a,l,j,c,b,k,i)       ! 14572368 (+1.000)
!     & -z84(d,b,l,j,c,a,k,i)       ! 13572468 (-1.000)
!     & -z84(d,a,l,j,b,c,k,i)       ! 14573268 (-1.000)
!     & +z84(d,b,l,j,a,c,k,i)       ! 13574268 (+1.000)
!     & +z84(d,c,l,j,b,a,k,i)       ! 12573468 (+1.000)
!     & -z84(d,c,l,j,a,b,k,i)       ! 12574368 (-1.000)
!     & -z84(b,a,k,j,d,c,l,i)       ! 34671258 (-1.000)
!     & +z84(c,a,k,j,d,b,l,i)       ! 24671358 (+1.000)
!     & -z84(c,b,k,j,d,a,l,i)       ! 23671458 (-1.000)
!     & +z84(b,a,k,j,c,d,l,i)       ! 34672158 (+1.000)
!     & -z84(c,a,k,j,b,d,l,i)       ! 24673158 (-1.000)
!     & +z84(c,b,k,j,a,d,l,i)       ! 23674158 (+1.000)
!     & -z84(d,a,k,j,c,b,l,i)       ! 14672358 (-1.000)
!     & +z84(d,b,k,j,c,a,l,i)       ! 13672458 (+1.000)
!     & +z84(d,a,k,j,b,c,l,i)       ! 14673258 (+1.000)
!     & -z84(d,b,k,j,a,c,l,i)       ! 13674258 (-1.000)
!     & -z84(d,c,k,j,b,a,l,i)       ! 12673458 (-1.000)
!     & +z84(d,c,k,j,a,b,l,i)       ! 12674358 (+1.000)
!     & -z84(b,a,l,i,d,c,k,j)       ! 34581267 (-1.000)
!     & +z84(c,a,l,i,d,b,k,j)       ! 24581367 (+1.000)
!     & -z84(c,b,l,i,d,a,k,j)       ! 23581467 (-1.000)
!     & +z84(b,a,l,i,c,d,k,j)       ! 34582167 (+1.000)
!     & -z84(c,a,l,i,b,d,k,j)       ! 24583167 (-1.000)
!     & +z84(c,b,l,i,a,d,k,j)       ! 23584167 (+1.000)
!     & -z84(d,a,l,i,c,b,k,j)       ! 14582367 (-1.000)
!     & +z84(d,b,l,i,c,a,k,j)       ! 13582467 (+1.000)
!     & +z84(d,a,l,i,b,c,k,j)       ! 14583267 (+1.000)
!     & -z84(d,b,l,i,a,c,k,j)       ! 13584267 (-1.000)
!     & -z84(d,c,l,i,b,a,k,j)       ! 12583467 (-1.000)
!     & +z84(d,c,l,i,a,b,k,j)       ! 12584367 (+1.000)
!     & +z84(b,a,k,i,d,c,l,j)       ! 34681257 (+1.000)
!     & -z84(c,a,k,i,d,b,l,j)       ! 24681357 (-1.000)
!     & +z84(c,b,k,i,d,a,l,j)       ! 23681457 (+1.000)
!     & -z84(b,a,k,i,c,d,l,j)       ! 34682157 (-1.000)
!     & +z84(c,a,k,i,b,d,l,j)       ! 24683157 (+1.000)
!     & -z84(c,b,k,i,a,d,l,j)       ! 23684157 (-1.000)
!     & +z84(d,a,k,i,c,b,l,j)       ! 14682357 (+1.000)
!     & -z84(d,b,k,i,c,a,l,j)       ! 13682457 (-1.000)
!     & -z84(d,a,k,i,b,c,l,j)       ! 14683257 (-1.000)
!     & +z84(d,b,k,i,a,c,l,j)       ! 13684257 (+1.000)
!     & +z84(d,c,k,i,b,a,l,j)       ! 12683457 (+1.000)
!     & -z84(d,c,k,i,a,b,l,j)       ! 12684357 (-1.000)
!     & -z84(b,a,j,i,d,c,l,k)       ! 34781256 (-1.000)
!     & +z84(c,a,j,i,d,b,l,k)       ! 24781356 (+1.000)
!     & -z84(c,b,j,i,d,a,l,k)       ! 23781456 (-1.000)
!     & +z84(b,a,j,i,c,d,l,k)       ! 34782156 (+1.000)
!     & -z84(c,a,j,i,b,d,l,k)       ! 24783156 (-1.000)
!     & +z84(c,b,j,i,a,d,l,k)       ! 23784156 (+1.000)
!     & -z84(d,a,j,i,c,b,l,k)       ! 14782356 (-1.000)
!     & +z84(d,b,j,i,c,a,l,k)       ! 13782456 (+1.000)
!     & +z84(d,a,j,i,b,c,l,k)       ! 14783256 (+1.000)
!     & -z84(d,b,j,i,a,c,l,k)       ! 13784256 (-1.000)
!     & -z84(d,c,j,i,b,a,l,k)       ! 12783456 (-1.000)
!     & +z84(d,c,j,i,a,b,l,k)       ! 12784356 (+1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z84)
c
       allocate(f1(n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder621345(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,u24,f1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(u35(n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       i1=k1*k1*k3*k3*k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,f1,b2,u35)
       deallocate(f1)
       deallocate(b2)
       deallocate(u24)
c
!       allocate(f1(n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!       call reorder213456(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,
!     & n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,u35,f1)
!       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
!     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
!       allocate(z105(n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!       i1=k1*k1*k3*k3*k3
!       i2=k1*k1*k3
!       i3=k3
!       call egemm(i1,i2,i3,f1,d2,z105)
!       deallocate(f1)
!       deallocate(d2)
c
       do i=n0+1,n1-3;do j=i+1,n1-2;do k=j+1,n1-1;do l=k+1,n1
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-3;do b=a+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
          if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n1+1,n3
             sum=sum
     &     - u35(d,f,c,a,l,k)*t2a(f,b,j,i)       !dfcalkfbji      (-1.000)
     &     - u35(d,f,c,a,j,i)*t2a(f,b,l,k)       !dfcajifblk      (-1.000)
     &     + u35(d,f,b,a,l,k)*t2a(f,c,j,i)       !dfbalkfcji      (+1.000)
     &     - u35(d,f,a,b,l,k)*t2a(f,c,j,i)       !dfablkfcji      (-1.000)
     &     + u35(d,f,b,a,j,i)*t2a(f,c,l,k)       !dfbajifclk      (+1.000)
     &     - u35(d,f,a,b,j,i)*t2a(f,c,l,k)       !dfabjifclk      (-1.000)
     &     - u35(c,f,b,a,l,k)*t2a(f,d,j,i)       !cfbalkfdji      (-1.000)
     &     + u35(c,f,a,b,l,k)*t2a(f,d,j,i)       !cfablkfdji      (+1.000)
     &     - u35(b,f,a,c,l,k)*t2a(f,d,j,i)       !bfaclkfdji      (-1.000)
     &     - u35(c,f,b,a,j,i)*t2a(f,d,l,k)       !cfbajifdlk      (-1.000)
     &     + u35(c,f,a,b,j,i)*t2a(f,d,l,k)       !cfabjifdlk      (+1.000)
     &     - u35(b,f,a,c,j,i)*t2a(f,d,l,k)       !bfacjifdlk      (-1.000)
     &     + u35(d,f,c,a,l,j)*t2a(f,b,k,i)       !dfcaljfbki      (+1.000)
     &     + u35(d,f,c,a,k,i)*t2a(f,b,l,j)       !dfcakifblj      (+1.000)
     &     - u35(d,f,b,a,l,j)*t2a(f,c,k,i)       !dfbaljfcki      (-1.000)
     &     + u35(d,f,a,b,l,j)*t2a(f,c,k,i)       !dfabljfcki      (+1.000)
     &     - u35(d,f,b,a,k,i)*t2a(f,c,l,j)       !dfbakifclj      (-1.000)
     &     + u35(d,f,a,b,k,i)*t2a(f,c,l,j)       !dfabkifclj      (+1.000)
     &     + u35(c,f,b,a,l,j)*t2a(f,d,k,i)       !cfbaljfdki      (+1.000)
     &     - u35(c,f,a,b,l,j)*t2a(f,d,k,i)       !cfabljfdki      (-1.000)
     &     + u35(b,f,a,c,l,j)*t2a(f,d,k,i)       !bfacljfdki      (+1.000)
     &     + u35(c,f,b,a,k,i)*t2a(f,d,l,j)       !cfbakifdlj      (+1.000)
     &     - u35(c,f,a,b,k,i)*t2a(f,d,l,j)       !cfabkifdlj      (-1.000)
     &     + u35(b,f,a,c,k,i)*t2a(f,d,l,j)       !bfackifdlj      (+1.000)
     &     - u35(d,f,c,a,k,j)*t2a(f,b,l,i)       !dfcakjfbli      (-1.000)
     &     - u35(d,f,c,a,l,i)*t2a(f,b,k,j)       !dfcalifbkj      (-1.000)
     &     + u35(d,f,b,a,k,j)*t2a(f,c,l,i)       !dfbakjfcli      (+1.000)
     &     - u35(d,f,a,b,k,j)*t2a(f,c,l,i)       !dfabkjfcli      (-1.000)
     &     + u35(d,f,b,a,l,i)*t2a(f,c,k,j)       !dfbalifckj      (+1.000)
     &     - u35(d,f,a,b,l,i)*t2a(f,c,k,j)       !dfablifckj      (-1.000)
     &     - u35(c,f,b,a,k,j)*t2a(f,d,l,i)       !cfbakjfdli      (-1.000)
     &     + u35(c,f,a,b,k,j)*t2a(f,d,l,i)       !cfabkjfdli      (+1.000)
     &     - u35(b,f,a,c,k,j)*t2a(f,d,l,i)       !bfackjfdli      (-1.000)
     &     - u35(c,f,b,a,l,i)*t2a(f,d,k,j)       !cfbalifdkj      (-1.000)
     &     + u35(c,f,a,b,l,i)*t2a(f,d,k,j)       !cfablifdkj      (+1.000)
     &     - u35(b,f,a,c,l,i)*t2a(f,d,k,j)       !bfaclifdkj      (-1.000)
             enddo
             v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call
!     & sum37812456(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z105,-1.000)
!       call
!     & sum35612478(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z105,-1.000)
!       call
!     & sum27813456(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z105, 1.000)
!       call
!     & sum27814356(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z105,-1.000)
!       call
!     & sum25613478(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z105, 1.000)
!       call
!     & sum25614378(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z105,-1.000)
!       call
!     & sum17823456(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z105,-1.000)
!       call
!     & sum17824356(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z105, 1.000)
!       call
!     & sum17834256(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z105,-1.000)
!       call
!     & sum15623478(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z105,-1.000)
!       call
!     & sum15624378(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z105, 1.000)
!       call
!     & sum15634278(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z105,-1.000)
!       call
!     & sum36812457(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z105, 1.000)
!       call
!     & sum35712468(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z105, 1.000)
!       call
!     & sum26813457(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z105,-1.000)
!       call
!     & sum26814357(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z105, 1.000)
!       call
!     & sum25713468(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z105,-1.000)
!       call
!     & sum25714368(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z105, 1.000)
!       call
!     & sum16823457(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z105, 1.000)
!       call
!     & sum16824357(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z105,-1.000)
!       call
!     & sum16834257(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z105, 1.000)
!       call
!     & sum15723468(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z105, 1.000)
!       call
!     & sum15724368(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z105,-1.000)
!       call
!     & sum15734268(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z105, 1.000)
!       call
!     & sum35812467(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z105,-1.000)
!       call
!     & sum36712458(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z105,-1.000)
!       call
!     & sum25813467(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z105, 1.000)
!       call
!     & sum25814367(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z105,-1.000)
!       call
!     & sum26713458(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z105, 1.000)
!       call
!     & sum26714358(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z105,-1.000)
!       call
!     & sum15823467(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z105,-1.000)
!       call
!     & sum15824367(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z105, 1.000)
!       call
!     & sum15834267(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z105,-1.000)
!       call
!     & sum16723458(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z105,-1.000)
!       call
!     & sum16724358(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z105, 1.000)
!       call
!     & sum16734258(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z105,-1.000)
c
!       do i=n0+1,n1-3
!       do j=i+1,n1-2
!       do k=j+1,n1-1
!       do l=k+1,n1
!       do a=n1+1,n3-3
!       do b=a+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)
!     & -z105(b,j,i,d,c,a,l,k)       ! 37812456 (-1.000)
!     & -z105(b,l,k,d,c,a,j,i)       ! 35612478 (-1.000)
!     & +z105(c,j,i,d,b,a,l,k)       ! 27813456 (+1.000)
!     & -z105(c,j,i,d,a,b,l,k)       ! 27814356 (-1.000)
!     & +z105(c,l,k,d,b,a,j,i)       ! 25613478 (+1.000)
!     & -z105(c,l,k,d,a,b,j,i)       ! 25614378 (-1.000)
!     & -z105(d,j,i,c,b,a,l,k)       ! 17823456 (-1.000)
!     & +z105(d,j,i,c,a,b,l,k)       ! 17824356 (+1.000)
!     & -z105(d,j,i,b,a,c,l,k)       ! 17834256 (-1.000)
!     & -z105(d,l,k,c,b,a,j,i)       ! 15623478 (-1.000)
!     & +z105(d,l,k,c,a,b,j,i)       ! 15624378 (+1.000)
!     & -z105(d,l,k,b,a,c,j,i)       ! 15634278 (-1.000)
!     & +z105(b,k,i,d,c,a,l,j)       ! 36812457 (+1.000)
!     & +z105(b,l,j,d,c,a,k,i)       ! 35712468 (+1.000)
!     & -z105(c,k,i,d,b,a,l,j)       ! 26813457 (-1.000)
!     & +z105(c,k,i,d,a,b,l,j)       ! 26814357 (+1.000)
!     & -z105(c,l,j,d,b,a,k,i)       ! 25713468 (-1.000)
!     & +z105(c,l,j,d,a,b,k,i)       ! 25714368 (+1.000)
!     & +z105(d,k,i,c,b,a,l,j)       ! 16823457 (+1.000)
!     & -z105(d,k,i,c,a,b,l,j)       ! 16824357 (-1.000)
!     & +z105(d,k,i,b,a,c,l,j)       ! 16834257 (+1.000)
!     & +z105(d,l,j,c,b,a,k,i)       ! 15723468 (+1.000)
!     & -z105(d,l,j,c,a,b,k,i)       ! 15724368 (-1.000)
!     & +z105(d,l,j,b,a,c,k,i)       ! 15734268 (+1.000)
!     & -z105(b,l,i,d,c,a,k,j)       ! 35812467 (-1.000)
!     & -z105(b,k,j,d,c,a,l,i)       ! 36712458 (-1.000)
!     & +z105(c,l,i,d,b,a,k,j)       ! 25813467 (+1.000)
!     & -z105(c,l,i,d,a,b,k,j)       ! 25814367 (-1.000)
!     & +z105(c,k,j,d,b,a,l,i)       ! 26713458 (+1.000)
!     & -z105(c,k,j,d,a,b,l,i)       ! 26714358 (-1.000)
!     & -z105(d,l,i,c,b,a,k,j)       ! 15823467 (-1.000)
!     & +z105(d,l,i,c,a,b,k,j)       ! 15824367 (+1.000)
!     & -z105(d,l,i,b,a,c,k,j)       ! 15834267 (-1.000)
!     & -z105(d,k,j,c,b,a,l,i)       ! 16723458 (-1.000)
!     & +z105(d,k,j,c,a,b,l,i)       ! 16724358 (+1.000)
!     & -z105(d,k,j,b,a,c,l,i)       ! 16734258 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z105)
       deallocate(u35)
c
       allocate(f1(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       call reorder451236(n1,n3,n0,n1,n0,n1,n1,n3,n0,n1,n0,n1,
     & n1,n3,n0,n1,n1,n3,n0,n1,n0,n1,n0,n1,u10,f1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(u22(n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1*k1*k3*k1
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,f1,b2,u22)
       deallocate(f1)
       deallocate(b2)
       deallocate(u10)
c
!       allocate(f1(n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
!       call reorder623451(n0,n1,n0,n1,n1,n3,n0,n1,n0,n1,n0,n1,
!     & n0,n1,n0,n1,n1,n3,n0,n1,n0,n1,n0,n1,u22,f1)
!       allocate(f2(n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
!       call reorder451236(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,
!     & n0,n1,n0,n1,n1,n3,n1,n3,n1,n3,n0,n1,t3a,f2)
!       allocate(z80(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
!       i1=k1*k1*k1*k3
!       i2=k1*k3*k3*k3
!       i3=k1*k1
!       call egemm(i1,i2,i3,f1,f2,z80)
!       deallocate(f1)
!       deallocate(f2)
c
       do i=n0+1,n1-3;do j=i+1,n1-2;do k=j+1,n1-1;do l=k+1,n1
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-3;do b=a+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
          if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1;do n=n0+1,n1
             sum=sum                                 !top 2 switched
     &     + (u22(i,m,c,k,j,n)*t3a(d,b,a,n,m,l)      !imckjndbanml    (+0.500)
     &     - u22(i,m,d,k,j,n)*t3a(c,b,a,n,m,l)       !imdkjncbanml    (-0.500)
     &     - u22(i,m,b,k,j,n)*t3a(d,c,a,n,m,l)       !imbkjndcanml    (-0.500)
     &     + u22(i,m,a,k,j,n)*t3a(d,c,b,n,m,l)       !imakjndcbnml    (+0.500)
     &     + u22(i,m,d,l,j,n)*t3a(c,b,a,n,m,k)       !imdljncbanmk    (+0.500)
     &     - u22(i,m,c,l,j,n)*t3a(d,b,a,n,m,k)       !imcljndbanmk    (-0.500)
     &     + u22(i,m,b,l,j,n)*t3a(d,c,a,n,m,k)       !imbljndcanmk    (+0.500)
     &     - u22(i,m,a,l,j,n)*t3a(d,c,b,n,m,k)       !imaljndcbnmk    (-0.500)
     &     - u22(i,m,d,l,k,n)*t3a(c,b,a,n,m,j)       !imdlkncbanmj    (-0.500)
     &     + u22(i,m,c,l,k,n)*t3a(d,b,a,n,m,j)       !imclkndbanmj    (+0.500)
     &     - u22(i,m,b,l,k,n)*t3a(d,c,a,n,m,j)       !imblkndcanmj    (-0.500)
     &     + u22(i,m,a,l,k,n)*t3a(d,c,b,n,m,j)       !imalkndcbnmj    (+0.500)
     &     + u22(j,m,d,k,i,n)*t3a(c,b,a,n,m,l)       !jmdkincbanml    (+0.500)
     &     - u22(j,m,c,k,i,n)*t3a(d,b,a,n,m,l)       !jmckindbanml    (-0.500)
     &     + u22(j,m,b,k,i,n)*t3a(d,c,a,n,m,l)       !jmbkindcanml    (+0.500)
     &     - u22(j,m,a,k,i,n)*t3a(d,c,b,n,m,l)       !jmakindcbnml    (-0.500)
     &     - u22(j,m,d,l,i,n)*t3a(c,b,a,n,m,k)       !jmdlincbanmk    (-0.500)
     &     + u22(j,m,c,l,i,n)*t3a(d,b,a,n,m,k)       !jmclindbanmk    (+0.500)
     &     - u22(j,m,b,l,i,n)*t3a(d,c,a,n,m,k)       !jmblindcanmk    (-0.500)
     &     + u22(j,m,a,l,i,n)*t3a(d,c,b,n,m,k)       !jmalindcbnmk    (+0.500)
     &     - u22(k,m,d,j,i,n)*t3a(c,b,a,n,m,l)       !kmdjincbanml    (-0.500)
     &     + u22(k,m,c,j,i,n)*t3a(d,b,a,n,m,l)       !kmcjindbanml    (+0.500)
     &     - u22(k,m,b,j,i,n)*t3a(d,c,a,n,m,l)       !kmbjindcanml    (-0.500)
     &     + u22(k,m,a,j,i,n)*t3a(d,c,b,n,m,l)       !kmajindcbnml    (+0.500)
     &     + u22(l,m,d,j,i,n)*t3a(c,b,a,n,m,k)       !lmdjincbanmk    (+0.500)
     &     - u22(l,m,c,j,i,n)*t3a(d,b,a,n,m,k)       !lmcjindbanmk    (-0.500)
     &     + u22(l,m,b,j,i,n)*t3a(d,c,a,n,m,k)       !lmbjindcanmk    (+0.500)
     &     - u22(l,m,a,j,i,n)*t3a(d,c,b,n,m,k)       !lmajindcbnmk    (-0.500)
     &     + u22(k,m,d,l,i,n)*t3a(c,b,a,n,m,j)       !kmdlincbanmj    (+0.500)
     &     - u22(k,m,c,l,i,n)*t3a(d,b,a,n,m,j)       !kmclindbanmj    (-0.500)
     &     + u22(k,m,b,l,i,n)*t3a(d,c,a,n,m,j)       !kmblindcanmj    (+0.500)
     &     - u22(k,m,a,l,i,n)*t3a(d,c,b,n,m,j)       !kmalindcbnmj    (-0.500)
     &     - u22(l,m,d,k,i,n)*t3a(c,b,a,n,m,j)       !lmdkincbanmj    (-0.500)
     &     + u22(l,m,c,k,i,n)*t3a(d,b,a,n,m,j)       !lmckindbanmj    (+0.500)
     &     - u22(l,m,b,k,i,n)*t3a(d,c,a,n,m,j)       !lmbkindcanmj    (-0.500)
     &     + u22(l,m,a,k,i,n)*t3a(d,c,b,n,m,j)       !lmakindcbnmj    (+0.500)
     &     + u22(j,m,d,l,k,n)*t3a(c,b,a,n,m,i)       !jmdlkncbanmi    (+0.500)
     &     - u22(j,m,c,l,k,n)*t3a(d,b,a,n,m,i)       !jmclkndbanmi    (-0.500)
     &     + u22(j,m,b,l,k,n)*t3a(d,c,a,n,m,i)       !jmblkndcanmi    (+0.500)
     &     - u22(j,m,a,l,k,n)*t3a(d,c,b,n,m,i)       !jmalkndcbnmi    (-0.500)
     &     - u22(k,m,d,l,j,n)*t3a(c,b,a,n,m,i)       !kmdljncbanmi    (-0.500)
     &     + u22(k,m,c,l,j,n)*t3a(d,b,a,n,m,i)       !kmcljndbanmi    (+0.500)
     &     - u22(k,m,b,l,j,n)*t3a(d,c,a,n,m,i)       !kmbljndcanmi    (-0.500)
     &     + u22(k,m,a,l,j,n)*t3a(d,c,b,n,m,i)       !kmaljndcbnmi    (+0.500)
     &     + u22(l,m,d,k,j,n)*t3a(c,b,a,n,m,i)       !lmdkjncbanmi    (+0.500)
     &     - u22(l,m,c,k,j,n)*t3a(d,b,a,n,m,i)       !lmckjndbanmi    (-0.500)
     &     + u22(l,m,b,k,j,n)*t3a(d,c,a,n,m,i)       !lmbkjndcanmi    (+0.500)
     &     - u22(l,m,a,k,j,n)*t3a(d,c,b,n,m,i))/2.0d0!lmakjndcbnmi    (-0.500)           
             enddo;enddo
             v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call
!     & sum23451678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z80,-0.500)
!       call
!     & sum13452678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z80, 0.500)
!       call
!     & sum12453678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z80,-0.500)
!       call
!     & sum12354678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z80, 0.500)
!       call
!     & sum23461578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z80, 0.500)
!       call
!     & sum13462578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z80,-0.500)
!       call
!     & sum12463578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z80, 0.500)
!       call
!     & sum12364578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z80,-0.500)
!       call
!     & sum23471568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z80,-0.500)
!       call
!     & sum13472568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z80, 0.500)
!       call
!     & sum12473568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z80,-0.500)
!       call
!     & sum12374568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z80, 0.500)
!       call
!     & sum23451687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z80, 0.500)
!       call
!     & sum13452687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z80,-0.500)
!       call
!     & sum12453687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z80, 0.500)
!       call
!     & sum12354687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z80,-0.500)
!       call
!     & sum23461587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z80,-0.500)
!       call
!     & sum13462587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z80, 0.500)
!       call
!     & sum12463587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z80,-0.500)
!       call
!     & sum12364587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z80, 0.500)
!       call
!     & sum23451786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z80,-0.500)
!       call
!     & sum13452786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z80, 0.500)
!       call
!     & sum12453786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z80,-0.500)
!       call
!     & sum12354786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z80, 0.500)
!       call
!     & sum23461785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z80, 0.500)
!       call
!     & sum13462785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z80,-0.500)
!       call
!     & sum12463785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z80, 0.500)
!       call
!     & sum12364785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z80,-0.500)
!       call
!     & sum23471586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z80, 0.500)
!       call
!     & sum13472586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z80,-0.500)
!       call
!     & sum12473586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z80, 0.500)
!       call
!     & sum12374586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z80,-0.500)
!       call
!     & sum23471685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z80,-0.500)
!       call
!     & sum13472685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z80, 0.500)
!       call
!     & sum12473685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z80,-0.500)
!       call
!     & sum12374685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z80, 0.500)
!       call
!     & sum23481567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z80, 0.500)
!       call
!     & sum13482567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z80,-0.500)
!       call
!     & sum12483567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z80, 0.500)
!       call
!     & sum12384567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z80,-0.500)
!       call
!     & sum23481576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z80,-0.500)
!       call
!     & sum13482576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z80, 0.500)
!       call
!     & sum12483576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z80,-0.500)
!       call
!     & sum12384576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z80, 0.500)
!       call
!     & sum23481675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z80, 0.500)
!       call
!     & sum13482675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z80,-0.500)
!       call
!     & sum12483675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z80, 0.500)
!       call
!     & sum12384675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z80,-0.500)
!c
!       do i=n0+1,n1-3
!       do j=i+1,n1-2
!       do k=j+1,n1-1
!       do l=k+1,n1
!       do a=n1+1,n3-3
!       do b=a+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)
!     & +(z80(d,b,a,l,c,k,j,i)      ! 13452678 (+0.500) top two switched
!     & -z80(c,b,a,l,d,k,j,i)       ! 23451678 (-0.500)
!     & -z80(d,c,a,l,b,k,j,i)       ! 12453678 (-0.500)
!     & +z80(d,c,b,l,a,k,j,i)       ! 12354678 (+0.500)
!     & +z80(c,b,a,k,d,l,j,i)       ! 23461578 (+0.500)
!     & -z80(d,b,a,k,c,l,j,i)       ! 13462578 (-0.500)
!     & +z80(d,c,a,k,b,l,j,i)       ! 12463578 (+0.500)
!     & -z80(d,c,b,k,a,l,j,i)       ! 12364578 (-0.500)
!     & -z80(c,b,a,j,d,l,k,i)       ! 23471568 (-0.500)
!     & +z80(d,b,a,j,c,l,k,i)       ! 13472568 (+0.500)
!     & -z80(d,c,a,j,b,l,k,i)       ! 12473568 (-0.500)
!     & +z80(d,c,b,j,a,l,k,i)       ! 12374568 (+0.500)
!     & +z80(c,b,a,l,d,k,i,j)       ! 23451687 (+0.500)
!     & -z80(d,b,a,l,c,k,i,j)       ! 13452687 (-0.500)
!     & +z80(d,c,a,l,b,k,i,j)       ! 12453687 (+0.500)
!     & -z80(d,c,b,l,a,k,i,j)       ! 12354687 (-0.500)
!     & -z80(c,b,a,k,d,l,i,j)       ! 23461587 (-0.500)
!     & +z80(d,b,a,k,c,l,i,j)       ! 13462587 (+0.500)
!     & -z80(d,c,a,k,b,l,i,j)       ! 12463587 (-0.500)
!     & +z80(d,c,b,k,a,l,i,j)       ! 12364587 (+0.500)
!     & -z80(c,b,a,l,d,j,i,k)       ! 23451786 (-0.500)
!     & +z80(d,b,a,l,c,j,i,k)       ! 13452786 (+0.500)
!     & -z80(d,c,a,l,b,j,i,k)       ! 12453786 (-0.500)
!     & +z80(d,c,b,l,a,j,i,k)       ! 12354786 (+0.500)
!     & +z80(c,b,a,k,d,j,i,l)       ! 23461785 (+0.500)
!     & -z80(d,b,a,k,c,j,i,l)       ! 13462785 (-0.500)
!     & +z80(d,c,a,k,b,j,i,l)       ! 12463785 (+0.500)
!     & -z80(d,c,b,k,a,j,i,l)       ! 12364785 (-0.500)
!     & +z80(c,b,a,j,d,l,i,k)       ! 23471586 (+0.500)
!     & -z80(d,b,a,j,c,l,i,k)       ! 13472586 (-0.500)
!     & +z80(d,c,a,j,b,l,i,k)       ! 12473586 (+0.500)
!     & -z80(d,c,b,j,a,l,i,k)       ! 12374586 (-0.500)
!     & -z80(c,b,a,j,d,k,i,l)       ! 23471685 (-0.500)
!     & +z80(d,b,a,j,c,k,i,l)       ! 13472685 (+0.500)
!     & -z80(d,c,a,j,b,k,i,l)       ! 12473685 (-0.500)
!     & +z80(d,c,b,j,a,k,i,l)       ! 12374685 (+0.500)
!     & +z80(c,b,a,i,d,l,k,j)       ! 23481567 (+0.500)
!     & -z80(d,b,a,i,c,l,k,j)       ! 13482567 (-0.500)
!     & +z80(d,c,a,i,b,l,k,j)       ! 12483567 (+0.500)
!     & -z80(d,c,b,i,a,l,k,j)       ! 12384567 (-0.500)
!     & -z80(c,b,a,i,d,l,j,k)       ! 23481576 (-0.500)
!     & +z80(d,b,a,i,c,l,j,k)       ! 13482576 (+0.500)
!     & -z80(d,c,a,i,b,l,j,k)       ! 12483576 (-0.500)
!     & +z80(d,c,b,i,a,l,j,k)       ! 12384576 (+0.500)
!     & +z80(c,b,a,i,d,k,j,l)       ! 23481675 (+0.500)
!     & -z80(d,b,a,i,c,k,j,l)       ! 13482675 (-0.500)
!     & +z80(d,c,a,i,b,k,j,l)       ! 12483675 (+0.500)
!     & -z80(d,c,b,i,a,k,j,l))/2.0d0! 12384675 (-0.500)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z80)
c
c
       allocate(f1(n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       call reorder263451(n0,n1,n0,n1,n1,n3,n0,n1,n0,n1,n0,n1,
     & n0,n1,n0,n1,n1,n3,n0,n1,n0,n1,n0,n1,u22,f1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(u34(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1*k1*k3*k1
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,f1,b2,u34)
       deallocate(f1)
       deallocate(b2)
       deallocate(u22)
c
!       allocate(f1(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
!       call reorder213456(n1,n3,n0,n1,n1,n3,n0,n1,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,u34,f1)
!       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
!       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
!       allocate(z104(n1+1:n3,n1+1:n3,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
!       i1=k1*k1*k1*k3*k3
!       i2=k1*k3*k3
!       i3=k1
!       call egemm(i1,i2,i3,f1,d2,z104)
!       deallocate(f1)
!       deallocate(d2)
c
       do i=n0+1,n1-3;do j=i+1,n1-2;do k=j+1,n1-1;do l=k+1,n1
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-3;do b=a+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
          if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do n=n0+1,n1
             sum=sum
     &     - u34(d,n,c,k,j,i)*t2a(b,a,n,l)          !dnckjibanl      (-1.000)
     &     + u34(d,n,b,k,j,i)*t2a(c,a,n,l)          !dnbkjicanl      (+1.000)
     &     - u34(d,n,a,k,j,i)*t2a(c,b,n,l)          !dnakjicbnl      (-1.000)
     &     + u34(c,n,d,k,j,i)*t2a(b,a,n,l)          !cndkjibanl      (+1.000)
     &     - u34(b,n,d,k,j,i)*t2a(c,a,n,l)          !bndkjicanl      (-1.000)
     &     + u34(a,n,d,k,j,i)*t2a(c,b,n,l)          !andkjicbnl      (+1.000)
     &     - u34(c,n,b,k,j,i)*t2a(d,a,n,l)          !cnbkjidanl      (-1.000)
     &     + u34(c,n,a,k,j,i)*t2a(d,b,n,l)          !cnakjidbnl      (+1.000)
     &     + u34(b,n,c,k,j,i)*t2a(d,a,n,l)          !bnckjidanl      (+1.000)
     &     - u34(a,n,c,k,j,i)*t2a(d,b,n,l)          !anckjidbnl      (-1.000)
     &     - u34(b,n,a,k,j,i)*t2a(d,c,n,l)          !bnakjidcnl      (-1.000)
     &     + u34(a,n,b,k,j,i)*t2a(d,c,n,l)          !anbkjidcnl      (+1.000)
     &     + u34(d,n,c,l,j,i)*t2a(b,a,n,k)          !dncljibank      (+1.000)
     &     - u34(d,n,b,l,j,i)*t2a(c,a,n,k)          !dnbljicank      (-1.000)
     &     + u34(d,n,a,l,j,i)*t2a(c,b,n,k)          !dnaljicbnk      (+1.000)
     &     - u34(c,n,d,l,j,i)*t2a(b,a,n,k)          !cndljibank      (-1.000)
     &     + u34(b,n,d,l,j,i)*t2a(c,a,n,k)          !bndljicank      (+1.000)
     &     - u34(a,n,d,l,j,i)*t2a(c,b,n,k)          !andljicbnk      (-1.000)
     &     + u34(c,n,b,l,j,i)*t2a(d,a,n,k)          !cnbljidank      (+1.000)
     &     - u34(c,n,a,l,j,i)*t2a(d,b,n,k)          !cnaljidbnk      (-1.000)
     &     - u34(b,n,c,l,j,i)*t2a(d,a,n,k)          !bncljidank      (-1.000)
     &     + u34(a,n,c,l,j,i)*t2a(d,b,n,k)          !ancljidbnk      (+1.000)
     &     + u34(b,n,a,l,j,i)*t2a(d,c,n,k)          !bnaljidcnk      (+1.000)
     &     - u34(a,n,b,l,j,i)*t2a(d,c,n,k)          !anbljidcnk      (-1.000)
     &     - u34(d,n,a,l,k,i)*t2a(c,b,n,j)          !dnalkicbnj      (-1.000)
     &     + u34(d,n,b,l,k,i)*t2a(c,a,n,j)          !dnblkicanj      (+1.000)
     &     - u34(d,n,c,l,k,i)*t2a(b,a,n,j)          !dnclkibanj      (-1.000)
     &     + u34(c,n,a,l,k,i)*t2a(d,b,n,j)          !cnalkidbnj      (+1.000)
     &     - u34(c,n,b,l,k,i)*t2a(d,a,n,j)          !cnblkidanj      (-1.000)
     &     - u34(b,n,a,l,k,i)*t2a(d,c,n,j)          !bnalkidcnj      (-1.000)
     &     + u34(a,n,b,l,k,i)*t2a(d,c,n,j)          !anblkidcnj      (+1.000)
     &     + u34(b,n,c,l,k,i)*t2a(d,a,n,j)          !bnclkidanj      (+1.000)
     &     - u34(a,n,c,l,k,i)*t2a(d,b,n,j)          !anclkidbnj      (-1.000)
     &     + u34(c,n,d,l,k,i)*t2a(b,a,n,j)          !cndlkibanj      (+1.000)
     &     - u34(b,n,d,l,k,i)*t2a(c,a,n,j)          !bndlkicanj      (-1.000)
     &     + u34(a,n,d,l,k,i)*t2a(c,b,n,j)          !andlkicbnj      (+1.000)
     &     + u34(d,n,c,k,i,j)*t2a(b,a,n,l)          !dnckijbanl      (+1.000)
     &     - u34(d,n,b,k,i,j)*t2a(c,a,n,l)          !dnbkijcanl      (-1.000)
     &     + u34(d,n,a,k,i,j)*t2a(c,b,n,l)          !dnakijcbnl      (+1.000)
     &     - u34(c,n,d,k,i,j)*t2a(b,a,n,l)          !cndkijbanl      (-1.000)
     &     + u34(b,n,d,k,i,j)*t2a(c,a,n,l)          !bndkijcanl      (+1.000)
     &     - u34(a,n,d,k,i,j)*t2a(c,b,n,l)          !andkijcbnl      (-1.000)
     &     + u34(c,n,b,k,i,j)*t2a(d,a,n,l)          !cnbkijdanl      (+1.000)
     &     - u34(c,n,a,k,i,j)*t2a(d,b,n,l)          !cnakijdbnl      (-1.000)
     &     - u34(b,n,c,k,i,j)*t2a(d,a,n,l)          !bnckijdanl      (-1.000)
     &     + u34(a,n,c,k,i,j)*t2a(d,b,n,l)          !anckijdbnl      (+1.000)
     &     + u34(b,n,a,k,i,j)*t2a(d,c,n,l)          !bnakijdcnl      (+1.000)
     &     - u34(a,n,b,k,i,j)*t2a(d,c,n,l)          !anbkijdcnl      (-1.000)
     &     - u34(d,n,c,l,i,j)*t2a(b,a,n,k)          !dnclijbank      (-1.000)
     &     + u34(d,n,b,l,i,j)*t2a(c,a,n,k)          !dnblijcank      (+1.000)
     &     - u34(d,n,a,l,i,j)*t2a(c,b,n,k)          !dnalijcbnk      (-1.000)
     &     + u34(c,n,d,l,i,j)*t2a(b,a,n,k)          !cndlijbank      (+1.000)
     &     - u34(b,n,d,l,i,j)*t2a(c,a,n,k)          !bndlijcank      (-1.000)
     &     + u34(a,n,d,l,i,j)*t2a(c,b,n,k)          !andlijcbnk      (+1.000)
     &     - u34(c,n,b,l,i,j)*t2a(d,a,n,k)          !cnblijdank      (-1.000)
     &     + u34(c,n,a,l,i,j)*t2a(d,b,n,k)          !cnalijdbnk      (+1.000)
     &     + u34(b,n,c,l,i,j)*t2a(d,a,n,k)          !bnclijdank      (+1.000)
     &     - u34(a,n,c,l,i,j)*t2a(d,b,n,k)          !anclijdbnk      (-1.000)
     &     - u34(b,n,a,l,i,j)*t2a(d,c,n,k)          !bnalijdcnk      (-1.000)
     &     + u34(a,n,b,l,i,j)*t2a(d,c,n,k)          !anblijdcnk      (+1.000)
     &     + u34(d,n,a,l,k,j)*t2a(c,b,n,i)          !dnalkjcbni      (+1.000)
     &     - u34(d,n,b,l,k,j)*t2a(c,a,n,i)          !dnblkjcani      (-1.000)
     &     + u34(d,n,c,l,k,j)*t2a(b,a,n,i)          !dnclkjbani      (+1.000)
     &     - u34(c,n,a,l,k,j)*t2a(d,b,n,i)          !cnalkjdbni      (-1.000)
     &     + u34(c,n,b,l,k,j)*t2a(d,a,n,i)          !cnblkjdani      (+1.000)
     &     + u34(b,n,a,l,k,j)*t2a(d,c,n,i)          !bnalkjdcni      (+1.000)
     &     - u34(a,n,b,l,k,j)*t2a(d,c,n,i)          !anblkjdcni      (-1.000)
     &     - u34(b,n,c,l,k,j)*t2a(d,a,n,i)          !bnclkjdani      (-1.000)
     &     + u34(a,n,c,l,k,j)*t2a(d,b,n,i)          !anclkjdbni      (+1.000)
     &     - u34(c,n,d,l,k,j)*t2a(b,a,n,i)          !cndlkjbani      (-1.000)
     &     + u34(b,n,d,l,k,j)*t2a(c,a,n,i)          !bndlkjcani      (+1.000)
     &     - u34(a,n,d,l,k,j)*t2a(c,b,n,i)          !andlkjcbni      (-1.000)
     &     - u34(d,n,c,j,i,k)*t2a(b,a,n,l)          !dncjikbanl      (-1.000)
     &     + u34(d,n,b,j,i,k)*t2a(c,a,n,l)          !dnbjikcanl      (+1.000)
     &     - u34(d,n,a,j,i,k)*t2a(c,b,n,l)          !dnajikcbnl      (-1.000)
     &     + u34(c,n,d,j,i,k)*t2a(b,a,n,l)          !cndjikbanl      (+1.000)
     &     - u34(b,n,d,j,i,k)*t2a(c,a,n,l)          !bndjikcanl      (-1.000)
     &     + u34(a,n,d,j,i,k)*t2a(c,b,n,l)          !andjikcbnl      (+1.000)
     &     - u34(c,n,b,j,i,k)*t2a(d,a,n,l)          !cnbjikdanl      (-1.000)
     &     + u34(c,n,a,j,i,k)*t2a(d,b,n,l)          !cnajikdbnl      (+1.000)
     &     + u34(b,n,c,j,i,k)*t2a(d,a,n,l)          !bncjikdanl      (+1.000)
     &     - u34(a,n,c,j,i,k)*t2a(d,b,n,l)          !ancjikdbnl      (-1.000)
     &     - u34(b,n,a,j,i,k)*t2a(d,c,n,l)          !bnajikdcnl      (-1.000)
     &     + u34(a,n,b,j,i,k)*t2a(d,c,n,l)          !anbjikdcnl      (+1.000)
     &     + u34(d,n,c,j,i,l)*t2a(b,a,n,k)          !dncjilbank      (+1.000)
     &     - u34(d,n,b,j,i,l)*t2a(c,a,n,k)          !dnbjilcank      (-1.000)
     &     + u34(d,n,a,j,i,l)*t2a(c,b,n,k)          !dnajilcbnk      (+1.000)
     &     - u34(c,n,d,j,i,l)*t2a(b,a,n,k)          !cndjilbank      (-1.000)
     &     + u34(b,n,d,j,i,l)*t2a(c,a,n,k)          !bndjilcank      (+1.000)
     &     - u34(a,n,d,j,i,l)*t2a(c,b,n,k)          !andjilcbnk      (-1.000)
     &     + u34(c,n,b,j,i,l)*t2a(d,a,n,k)          !cnbjildank      (+1.000)
     &     - u34(c,n,a,j,i,l)*t2a(d,b,n,k)          !cnajildbnk      (-1.000)
     &     - u34(b,n,c,j,i,l)*t2a(d,a,n,k)          !bncjildank      (-1.000)
     &     + u34(a,n,c,j,i,l)*t2a(d,b,n,k)          !ancjildbnk      (+1.000)
     &     + u34(b,n,a,j,i,l)*t2a(d,c,n,k)          !bnajildcnk      (+1.000)
     &     - u34(a,n,b,j,i,l)*t2a(d,c,n,k)          !anbjildcnk      (-1.000)
     &     + u34(d,n,c,l,i,k)*t2a(b,a,n,j)          !dnclikbanj      (+1.000)
     &     - u34(d,n,b,l,i,k)*t2a(c,a,n,j)          !dnblikcanj      (-1.000)
     &     + u34(d,n,a,l,i,k)*t2a(c,b,n,j)          !dnalikcbnj      (+1.000)
     &     - u34(c,n,d,l,i,k)*t2a(b,a,n,j)          !cndlikbanj      (-1.000)
     &     + u34(b,n,d,l,i,k)*t2a(c,a,n,j)          !bndlikcanj      (+1.000)
     &     - u34(a,n,d,l,i,k)*t2a(c,b,n,j)          !andlikcbnj      (-1.000)
     &     + u34(c,n,b,l,i,k)*t2a(d,a,n,j)          !cnblikdanj      (+1.000)
     &     - u34(c,n,a,l,i,k)*t2a(d,b,n,j)          !cnalikdbnj      (-1.000)
     &     - u34(b,n,c,l,i,k)*t2a(d,a,n,j)          !bnclikdanj      (-1.000)
     &     + u34(a,n,c,l,i,k)*t2a(d,b,n,j)          !anclikdbnj      (+1.000)
     &     + u34(b,n,a,l,i,k)*t2a(d,c,n,j)          !bnalikdcnj      (+1.000)
     &     - u34(a,n,b,l,i,k)*t2a(d,c,n,j)          !anblikdcnj      (-1.000)
     &     - u34(d,n,a,l,j,k)*t2a(c,b,n,i)          !dnaljkcbni      (-1.000)
     &     + u34(d,n,b,l,j,k)*t2a(c,a,n,i)          !dnbljkcani      (+1.000)
     &     - u34(d,n,c,l,j,k)*t2a(b,a,n,i)          !dncljkbani      (-1.000)
     &     + u34(c,n,a,l,j,k)*t2a(d,b,n,i)          !cnaljkdbni      (+1.000)
     &     - u34(c,n,b,l,j,k)*t2a(d,a,n,i)          !cnbljkdani      (-1.000)
     &     - u34(b,n,a,l,j,k)*t2a(d,c,n,i)          !bnaljkdcni      (-1.000)
     &     + u34(a,n,b,l,j,k)*t2a(d,c,n,i)          !anbljkdcni      (+1.000)
     &     + u34(b,n,c,l,j,k)*t2a(d,a,n,i)          !bncljkdani      (+1.000)
     &     - u34(a,n,c,l,j,k)*t2a(d,b,n,i)          !ancljkdbni      (-1.000)
     &     + u34(c,n,d,l,j,k)*t2a(b,a,n,i)          !cndljkbani      (+1.000)
     &     - u34(b,n,d,l,j,k)*t2a(c,a,n,i)          !bndljkcani      (-1.000)
     &     + u34(a,n,d,l,j,k)*t2a(c,b,n,i)          !andljkcbni      (+1.000)
     &     - u34(d,n,c,k,i,l)*t2a(b,a,n,j)          !dnckilbanj      (-1.000)
     &     + u34(d,n,b,k,i,l)*t2a(c,a,n,j)          !dnbkilcanj      (+1.000)
     &     - u34(d,n,a,k,i,l)*t2a(c,b,n,j)          !dnakilcbnj      (-1.000)
     &     + u34(c,n,d,k,i,l)*t2a(b,a,n,j)          !cndkilbanj      (+1.000)
     &     - u34(b,n,d,k,i,l)*t2a(c,a,n,j)          !bndkilcanj      (-1.000)
     &     + u34(a,n,d,k,i,l)*t2a(c,b,n,j)          !andkilcbnj      (+1.000)
     &     - u34(c,n,b,k,i,l)*t2a(d,a,n,j)          !cnbkildanj      (-1.000)
     &     + u34(c,n,a,k,i,l)*t2a(d,b,n,j)          !cnakildbnj      (+1.000)
     &     + u34(b,n,c,k,i,l)*t2a(d,a,n,j)          !bnckildanj      (+1.000)
     &     - u34(a,n,c,k,i,l)*t2a(d,b,n,j)          !anckildbnj      (-1.000)
     &     - u34(b,n,a,k,i,l)*t2a(d,c,n,j)          !bnakildcnj      (-1.000)
     &     + u34(a,n,b,k,i,l)*t2a(d,c,n,j)          !anbkildcnj      (+1.000)
     &     + u34(d,n,a,k,j,l)*t2a(c,b,n,i)          !dnakjlcbni      (+1.000)
     &     - u34(d,n,b,k,j,l)*t2a(c,a,n,i)          !dnbkjlcani      (-1.000)
     &     + u34(d,n,c,k,j,l)*t2a(b,a,n,i)          !dnckjlbani      (+1.000)
     &     - u34(c,n,a,k,j,l)*t2a(d,b,n,i)          !cnakjldbni      (-1.000)
     &     + u34(c,n,b,k,j,l)*t2a(d,a,n,i)          !cnbkjldani      (+1.000)
     &     + u34(b,n,a,k,j,l)*t2a(d,c,n,i)          !bnakjldcni      (+1.000)
     &     - u34(a,n,b,k,j,l)*t2a(d,c,n,i)          !anbkjldcni      (-1.000)
     &     - u34(b,n,c,k,j,l)*t2a(d,a,n,i)          !bnckjldani      (-1.000)
     &     + u34(a,n,c,k,j,l)*t2a(d,b,n,i)          !anckjldbni      (+1.000)
     &     - u34(c,n,d,k,j,l)*t2a(b,a,n,i)          !cndkjlbani      (-1.000)
     &     + u34(b,n,d,k,j,l)*t2a(c,a,n,i)          !bndkjlcani      (+1.000)
     &     - u34(a,n,d,k,j,l)*t2a(c,b,n,i)          !andkjlcbni      (-1.000)       
             enddo
             v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call
!     & sum34512678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104,-1.000)
!       call
!     & sum24513678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104, 1.000)
!       call
!     & sum23514678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104,-1.000)
!       call
!     & sum34521678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104, 1.000)
!       call
!     & sum24531678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104,-1.000)
!       call
!     & sum23541678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104, 1.000)
!       call
!     & sum14523678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104,-1.000)
!       call
!     & sum13524678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104, 1.000)
!       call
!     & sum14532678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104, 1.000)
!       call
!     & sum13542678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104,-1.000)
!       call
!     & sum12534678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104,-1.000)
!       call
!     & sum12543678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104, 1.000)
!       call
!     & sum34612578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104, 1.000)
!       call
!     & sum24613578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104,-1.000)
!       call
!     & sum23614578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104, 1.000)
!       call
!     & sum34621578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104,-1.000)
!       call
!     & sum24631578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104, 1.000)
!       call
!     & sum23641578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104,-1.000)
!       call
!     & sum14623578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104, 1.000)
!       call
!     & sum13624578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104,-1.000)
!       call
!     & sum14632578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104,-1.000)
!       call
!     & sum13642578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104, 1.000)
!       call
!     & sum12634578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104, 1.000)
!       call
!     & sum12643578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104,-1.000)
!       call
!     & sum23714568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104,-1.000)
!       call
!     & sum24713568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104, 1.000)
!       call
!     & sum34712568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104,-1.000)
!       call
!     & sum13724568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104, 1.000)
!       call
!     & sum14723568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104,-1.000)
!       call
!     & sum12734568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104,-1.000)
!       call
!     & sum12743568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104, 1.000)
!       call
!     & sum14732568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104, 1.000)
!       call
!     & sum13742568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104,-1.000)
!       call
!     & sum34721568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104, 1.000)
!       call
!     & sum24731568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104,-1.000)
!       call
!     & sum23741568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104, 1.000)
!       call
!     & sum34512687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104, 1.000)
!       call
!     & sum24513687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104,-1.000)
!       call
!     & sum23514687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104, 1.000)
!       call
!     & sum34521687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104,-1.000)
!       call
!     & sum24531687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104, 1.000)
!       call
!     & sum23541687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104,-1.000)
!       call
!     & sum14523687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104, 1.000)
!       call
!     & sum13524687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104,-1.000)
!       call
!     & sum14532687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104,-1.000)
!       call
!     & sum13542687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104, 1.000)
!       call
!     & sum12534687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104, 1.000)
!       call
!     & sum12543687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104,-1.000)
!       call
!     & sum34612587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104,-1.000)
!       call
!     & sum24613587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104, 1.000)
!       call
!     & sum23614587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104,-1.000)
!       call
!     & sum34621587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104, 1.000)
!       call
!     & sum24631587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104,-1.000)
!       call
!     & sum23641587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104, 1.000)
!       call
!     & sum14623587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104,-1.000)
!       call
!     & sum13624587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104, 1.000)
!       call
!     & sum14632587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104, 1.000)
!       call
!     & sum13642587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104,-1.000)
!       call
!     & sum12634587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104,-1.000)
!       call
!     & sum12643587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104, 1.000)
!       call
!     & sum23814567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104, 1.000)
!       call
!     & sum24813567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104,-1.000)
!       call
!     & sum34812567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104, 1.000)
!       call
!     & sum13824567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104,-1.000)
!       call
!     & sum14823567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104, 1.000)
!       call
!     & sum12834567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104, 1.000)
!       call
!     & sum12843567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104,-1.000)
!       call
!     & sum14832567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104,-1.000)
!       call
!     & sum13842567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104, 1.000)
!       call
!     & sum34821567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104,-1.000)
!       call
!     & sum24831567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104, 1.000)
!       call
!     & sum23841567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104,-1.000)
!       call
!     & sum34512786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104,-1.000)
!       call
!     & sum24513786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104, 1.000)
!       call
!     & sum23514786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104,-1.000)
!       call
!     & sum34521786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104, 1.000)
!       call
!     & sum24531786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104,-1.000)
!       call
!     & sum23541786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104, 1.000)
!       call
!     & sum14523786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104,-1.000)
!       call
!     & sum13524786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104, 1.000)
!       call
!     & sum14532786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104, 1.000)
!       call
!     & sum13542786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104,-1.000)
!       call
!     & sum12534786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104,-1.000)
!       call
!     & sum12543786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104, 1.000)
!       call
!     & sum34612785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104, 1.000)
!       call
!     & sum24613785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104,-1.000)
!       call
!     & sum23614785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104, 1.000)
!       call
!     & sum34621785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104,-1.000)
!       call
!     & sum24631785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104, 1.000)
!       call
!     & sum23641785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104,-1.000)
!       call
!     & sum14623785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104, 1.000)
!       call
!     & sum13624785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104,-1.000)
!       call
!     & sum14632785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104,-1.000)
!       call
!     & sum13642785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104, 1.000)
!       call
!     & sum12634785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104, 1.000)
!       call
!     & sum12643785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104,-1.000)
!       call
!     & sum34712586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104, 1.000)
!       call
!     & sum24713586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104,-1.000)
!       call
!     & sum23714586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104, 1.000)
!       call
!     & sum34721586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104,-1.000)
!       call
!     & sum24731586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104, 1.000)
!       call
!     & sum23741586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104,-1.000)
!       call
!     & sum14723586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104, 1.000)
!       call
!     & sum13724586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104,-1.000)
!       call
!     & sum14732586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104,-1.000)
!       call
!     & sum13742586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104, 1.000)
!       call
!     & sum12734586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104, 1.000)
!       call
!     & sum12743586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104,-1.000)
!       call
!     & sum23814576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104,-1.000)
!       call
!     & sum24813576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104, 1.000)
!       call
!     & sum34812576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104,-1.000)
!       call
!     & sum13824576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104, 1.000)
!       call
!     & sum14823576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104,-1.000)
!       call
!     & sum12834576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104,-1.000)
!       call
!     & sum12843576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104, 1.000)
!       call
!     & sum14832576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104, 1.000)
!       call
!     & sum13842576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104,-1.000)
!       call
!     & sum34821576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104, 1.000)
!       call
!     & sum24831576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104,-1.000)
!       call
!     & sum23841576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104, 1.000)
!       call
!     & sum34712685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104,-1.000)
!       call
!     & sum24713685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104, 1.000)
!       call
!     & sum23714685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104,-1.000)
!       call
!     & sum34721685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104, 1.000)
!       call
!     & sum24731685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104,-1.000)
!       call
!     & sum23741685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104, 1.000)
!       call
!     & sum14723685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104,-1.000)
!       call
!     & sum13724685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104, 1.000)
!       call
!     & sum14732685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104, 1.000)
!       call
!     & sum13742685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104,-1.000)
!       call
!     & sum12734685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104,-1.000)
!       call
!     & sum12743685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104, 1.000)
!       call
!     & sum23814675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104, 1.000)
!       call
!     & sum24813675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104,-1.000)
!       call
!     & sum34812675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104, 1.000)
!       call
!     & sum13824675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104,-1.000)
!       call
!     & sum14823675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104, 1.000)
!       call
!     & sum12834675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104, 1.000)
!       call
!     & sum12843675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104,-1.000)
!       call
!     & sum14832675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104,-1.000)
!       call
!     & sum13842675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104, 1.000)
!       call
!     & sum34821675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104,-1.000)
!       call
!     & sum24831675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104, 1.000)
!       call
!     & sum23841675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z104,-1.000)
!c
!       do i=n0+1,n1-3
!       do j=i+1,n1-2
!       do k=j+1,n1-1
!       do l=k+1,n1
!       do a=n1+1,n3-3
!       do b=a+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)
!     & -z104(b,a,l,d,c,k,j,i)       ! 34512678 (-1.000)
!     & +z104(c,a,l,d,b,k,j,i)       ! 24513678 (+1.000)
!     & -z104(c,b,l,d,a,k,j,i)       ! 23514678 (-1.000)
!     & +z104(b,a,l,c,d,k,j,i)       ! 34521678 (+1.000)
!     & -z104(c,a,l,b,d,k,j,i)       ! 24531678 (-1.000)
!     & +z104(c,b,l,a,d,k,j,i)       ! 23541678 (+1.000)
!     & -z104(d,a,l,c,b,k,j,i)       ! 14523678 (-1.000)
!     & +z104(d,b,l,c,a,k,j,i)       ! 13524678 (+1.000)
!     & +z104(d,a,l,b,c,k,j,i)       ! 14532678 (+1.000)
!     & -z104(d,b,l,a,c,k,j,i)       ! 13542678 (-1.000)
!     & -z104(d,c,l,b,a,k,j,i)       ! 12534678 (-1.000)
!     & +z104(d,c,l,a,b,k,j,i)       ! 12543678 (+1.000)
!     & +z104(b,a,k,d,c,l,j,i)       ! 34612578 (+1.000)
!     & -z104(c,a,k,d,b,l,j,i)       ! 24613578 (-1.000)
!     & +z104(c,b,k,d,a,l,j,i)       ! 23614578 (+1.000)
!     & -z104(b,a,k,c,d,l,j,i)       ! 34621578 (-1.000)
!     & +z104(c,a,k,b,d,l,j,i)       ! 24631578 (+1.000)
!     & -z104(c,b,k,a,d,l,j,i)       ! 23641578 (-1.000)
!     & +z104(d,a,k,c,b,l,j,i)       ! 14623578 (+1.000)
!     & -z104(d,b,k,c,a,l,j,i)       ! 13624578 (-1.000)
!     & -z104(d,a,k,b,c,l,j,i)       ! 14632578 (-1.000)
!     & +z104(d,b,k,a,c,l,j,i)       ! 13642578 (+1.000)
!     & +z104(d,c,k,b,a,l,j,i)       ! 12634578 (+1.000)
!     & -z104(d,c,k,a,b,l,j,i)       ! 12643578 (-1.000)
!     & -z104(c,b,j,d,a,l,k,i)       ! 23714568 (-1.000)
!     & +z104(c,a,j,d,b,l,k,i)       ! 24713568 (+1.000)
!     & -z104(b,a,j,d,c,l,k,i)       ! 34712568 (-1.000)
!     & +z104(d,b,j,c,a,l,k,i)       ! 13724568 (+1.000)
!     & -z104(d,a,j,c,b,l,k,i)       ! 14723568 (-1.000)
!     & -z104(d,c,j,b,a,l,k,i)       ! 12734568 (-1.000)
!     & +z104(d,c,j,a,b,l,k,i)       ! 12743568 (+1.000)
!     & +z104(d,a,j,b,c,l,k,i)       ! 14732568 (+1.000)
!     & -z104(d,b,j,a,c,l,k,i)       ! 13742568 (-1.000)
!     & +z104(b,a,j,c,d,l,k,i)       ! 34721568 (+1.000)
!     & -z104(c,a,j,b,d,l,k,i)       ! 24731568 (-1.000)
!     & +z104(c,b,j,a,d,l,k,i)       ! 23741568 (+1.000)
!     & +z104(b,a,l,d,c,k,i,j)       ! 34512687 (+1.000)
!     & -z104(c,a,l,d,b,k,i,j)       ! 24513687 (-1.000)
!     & +z104(c,b,l,d,a,k,i,j)       ! 23514687 (+1.000)
!     & -z104(b,a,l,c,d,k,i,j)       ! 34521687 (-1.000)
!     & +z104(c,a,l,b,d,k,i,j)       ! 24531687 (+1.000)
!     & -z104(c,b,l,a,d,k,i,j)       ! 23541687 (-1.000)
!     & +z104(d,a,l,c,b,k,i,j)       ! 14523687 (+1.000)
!     & -z104(d,b,l,c,a,k,i,j)       ! 13524687 (-1.000)
!     & -z104(d,a,l,b,c,k,i,j)       ! 14532687 (-1.000)
!     & +z104(d,b,l,a,c,k,i,j)       ! 13542687 (+1.000)
!     & +z104(d,c,l,b,a,k,i,j)       ! 12534687 (+1.000)
!     & -z104(d,c,l,a,b,k,i,j)       ! 12543687 (-1.000)
!     & -z104(b,a,k,d,c,l,i,j)       ! 34612587 (-1.000)
!     & +z104(c,a,k,d,b,l,i,j)       ! 24613587 (+1.000)
!     & -z104(c,b,k,d,a,l,i,j)       ! 23614587 (-1.000)
!     & +z104(b,a,k,c,d,l,i,j)       ! 34621587 (+1.000)
!     & -z104(c,a,k,b,d,l,i,j)       ! 24631587 (-1.000)
!     & +z104(c,b,k,a,d,l,i,j)       ! 23641587 (+1.000)
!     & -z104(d,a,k,c,b,l,i,j)       ! 14623587 (-1.000)
!     & +z104(d,b,k,c,a,l,i,j)       ! 13624587 (+1.000)
!     & +z104(d,a,k,b,c,l,i,j)       ! 14632587 (+1.000)
!     & -z104(d,b,k,a,c,l,i,j)       ! 13642587 (-1.000)
!     & -z104(d,c,k,b,a,l,i,j)       ! 12634587 (-1.000)
!     & +z104(d,c,k,a,b,l,i,j)       ! 12643587 (+1.000)
!     & +z104(c,b,i,d,a,l,k,j)       ! 23814567 (+1.000)
!     & -z104(c,a,i,d,b,l,k,j)       ! 24813567 (-1.000)
!     & +z104(b,a,i,d,c,l,k,j)       ! 34812567 (+1.000)
!     & -z104(d,b,i,c,a,l,k,j)       ! 13824567 (-1.000)
!     & +z104(d,a,i,c,b,l,k,j)       ! 14823567 (+1.000)
!     & +z104(d,c,i,b,a,l,k,j)       ! 12834567 (+1.000)
!     & -z104(d,c,i,a,b,l,k,j)       ! 12843567 (-1.000)
!     & -z104(d,a,i,b,c,l,k,j)       ! 14832567 (-1.000)
!     & +z104(d,b,i,a,c,l,k,j)       ! 13842567 (+1.000)
!     & -z104(b,a,i,c,d,l,k,j)       ! 34821567 (-1.000)
!     & +z104(c,a,i,b,d,l,k,j)       ! 24831567 (+1.000)
!     & -z104(c,b,i,a,d,l,k,j)       ! 23841567 (-1.000)
!     & -z104(b,a,l,d,c,j,i,k)       ! 34512786 (-1.000)
!     & +z104(c,a,l,d,b,j,i,k)       ! 24513786 (+1.000)
!     & -z104(c,b,l,d,a,j,i,k)       ! 23514786 (-1.000)
!     & +z104(b,a,l,c,d,j,i,k)       ! 34521786 (+1.000)
!     & -z104(c,a,l,b,d,j,i,k)       ! 24531786 (-1.000)
!     & +z104(c,b,l,a,d,j,i,k)       ! 23541786 (+1.000)
!     & -z104(d,a,l,c,b,j,i,k)       ! 14523786 (-1.000)
!     & +z104(d,b,l,c,a,j,i,k)       ! 13524786 (+1.000)
!     & +z104(d,a,l,b,c,j,i,k)       ! 14532786 (+1.000)
!     & -z104(d,b,l,a,c,j,i,k)       ! 13542786 (-1.000)
!     & -z104(d,c,l,b,a,j,i,k)       ! 12534786 (-1.000)
!     & +z104(d,c,l,a,b,j,i,k)       ! 12543786 (+1.000)
!     & +z104(b,a,k,d,c,j,i,l)       ! 34612785 (+1.000)
!     & -z104(c,a,k,d,b,j,i,l)       ! 24613785 (-1.000)
!     & +z104(c,b,k,d,a,j,i,l)       ! 23614785 (+1.000)
!     & -z104(b,a,k,c,d,j,i,l)       ! 34621785 (-1.000)
!     & +z104(c,a,k,b,d,j,i,l)       ! 24631785 (+1.000)
!     & -z104(c,b,k,a,d,j,i,l)       ! 23641785 (-1.000)
!     & +z104(d,a,k,c,b,j,i,l)       ! 14623785 (+1.000)
!     & -z104(d,b,k,c,a,j,i,l)       ! 13624785 (-1.000)
!     & -z104(d,a,k,b,c,j,i,l)       ! 14632785 (-1.000)
!     & +z104(d,b,k,a,c,j,i,l)       ! 13642785 (+1.000)
!     & +z104(d,c,k,b,a,j,i,l)       ! 12634785 (+1.000)
!     & -z104(d,c,k,a,b,j,i,l)       ! 12643785 (-1.000)
!     & +z104(b,a,j,d,c,l,i,k)       ! 34712586 (+1.000)
!     & -z104(c,a,j,d,b,l,i,k)       ! 24713586 (-1.000)
!     & +z104(c,b,j,d,a,l,i,k)       ! 23714586 (+1.000)
!     & -z104(b,a,j,c,d,l,i,k)       ! 34721586 (-1.000)
!     & +z104(c,a,j,b,d,l,i,k)       ! 24731586 (+1.000)
!     & -z104(c,b,j,a,d,l,i,k)       ! 23741586 (-1.000)
!     & +z104(d,a,j,c,b,l,i,k)       ! 14723586 (+1.000)
!     & -z104(d,b,j,c,a,l,i,k)       ! 13724586 (-1.000)
!     & -z104(d,a,j,b,c,l,i,k)       ! 14732586 (-1.000)
!     & +z104(d,b,j,a,c,l,i,k)       ! 13742586 (+1.000)
!     & +z104(d,c,j,b,a,l,i,k)       ! 12734586 (+1.000)
!     & -z104(d,c,j,a,b,l,i,k)       ! 12743586 (-1.000)
!     & -z104(c,b,i,d,a,l,j,k)       ! 23814576 (-1.000)
!     & +z104(c,a,i,d,b,l,j,k)       ! 24813576 (+1.000)
!     & -z104(b,a,i,d,c,l,j,k)       ! 34812576 (-1.000)
!     & +z104(d,b,i,c,a,l,j,k)       ! 13824576 (+1.000)
!     & -z104(d,a,i,c,b,l,j,k)       ! 14823576 (-1.000)
!     & -z104(d,c,i,b,a,l,j,k)       ! 12834576 (-1.000)
!     & +z104(d,c,i,a,b,l,j,k)       ! 12843576 (+1.000)
!     & +z104(d,a,i,b,c,l,j,k)       ! 14832576 (+1.000)
!     & -z104(d,b,i,a,c,l,j,k)       ! 13842576 (-1.000)
!     & +z104(b,a,i,c,d,l,j,k)       ! 34821576 (+1.000)
!     & -z104(c,a,i,b,d,l,j,k)       ! 24831576 (-1.000)
!     & +z104(c,b,i,a,d,l,j,k)       ! 23841576 (+1.000)
!     & -z104(b,a,j,d,c,k,i,l)       ! 34712685 (-1.000)
!     & +z104(c,a,j,d,b,k,i,l)       ! 24713685 (+1.000)
!     & -z104(c,b,j,d,a,k,i,l)       ! 23714685 (-1.000)
!     & +z104(b,a,j,c,d,k,i,l)       ! 34721685 (+1.000)
!     & -z104(c,a,j,b,d,k,i,l)       ! 24731685 (-1.000)
!     & +z104(c,b,j,a,d,k,i,l)       ! 23741685 (+1.000)
!     & -z104(d,a,j,c,b,k,i,l)       ! 14723685 (-1.000)
!     & +z104(d,b,j,c,a,k,i,l)       ! 13724685 (+1.000)
!     & +z104(d,a,j,b,c,k,i,l)       ! 14732685 (+1.000)
!     & -z104(d,b,j,a,c,k,i,l)       ! 13742685 (-1.000)
!     & -z104(d,c,j,b,a,k,i,l)       ! 12734685 (-1.000)
!     & +z104(d,c,j,a,b,k,i,l)       ! 12743685 (+1.000)
!     & +z104(c,b,i,d,a,k,j,l)       ! 23814675 (+1.000)
!     & -z104(c,a,i,d,b,k,j,l)       ! 24813675 (-1.000)
!     & +z104(b,a,i,d,c,k,j,l)       ! 34812675 (+1.000)
!     & -z104(d,b,i,c,a,k,j,l)       ! 13824675 (-1.000)
!     & +z104(d,a,i,c,b,k,j,l)       ! 14823675 (+1.000)
!     & +z104(d,c,i,b,a,k,j,l)       ! 12834675 (+1.000)
!     & -z104(d,c,i,a,b,k,j,l)       ! 12843675 (-1.000)
!     & -z104(d,a,i,b,c,k,j,l)       ! 14832675 (-1.000)
!     & +z104(d,b,i,a,c,k,j,l)       ! 13842675 (+1.000)
!     & -z104(b,a,i,c,d,k,j,l)       ! 34821675 (-1.000)
!     & +z104(c,a,i,b,d,k,j,l)       ! 24831675 (+1.000)
!     & -z104(c,b,i,a,d,k,j,l)       ! 23841675 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z104)
       deallocate(u34)
c
       allocate(d1(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder4312(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n1,n3,n0,n1,n0,n1,intr,d1)
       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
       allocate(s17(n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1
       i2=k1*k1
       i3=k3*k3
       call egemm(i1,i2,i3,d1,d2,s17)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x5(n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       x5=0.0d0
       call sum3421(n0,n1,n0,n1,n0,n1,n0,n1,x5,s17, 0.500)
c
       allocate(d1(n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       call reorder3412(n0,n1,n0,n1,n0,n1,n0,n1,
     & n0,n1,n0,n1,n0,n1,n0,n1,s17,d1)
       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
       allocate(u31(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1*k1
       i2=k1*k3*k3
       i3=k1
       call egemm(i1,i2,i3,d1,d2,u31)
       deallocate(d1)
       deallocate(d2)
c
!       allocate(f1(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
!       call reorder412356(n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,u31,f1)
!       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
!       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
!       allocate(z99(n1+1:n3,n1+1:n3,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
!       i1=k1*k1*k1*k3*k3
!       i2=k1*k3*k3
!       i3=k1
!       call egemm(i1,i2,i3,f1,d2,z99)
!       deallocate(f1)
!       deallocate(d2)
c
       do i=n0+1,n1-3;do j=i+1,n1-2;do k=j+1,n1-1;do l=k+1,n1
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-3;do b=a+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
          if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do n=n0+1,n1
             sum=sum
     &     + (u31(b,a,l,n,j,i)*t2a(d,c,n,k)         !balnjidcnk      (+0.500)
     &     - u31(c,a,l,n,j,i)*t2a(d,b,n,k)          !calnjidbnk      (-0.500)
     &     - u31(d,a,k,n,j,i)*t2a(c,b,n,l)          !daknjicbnl      (-0.500)
     &     + u31(d,a,l,n,j,i)*t2a(c,b,n,k)          !dalnjicbnk      (+0.500)
     &     + u31(c,a,k,n,j,i)*t2a(d,b,n,l)          !caknjidbnl      (+0.500)
     &     - u31(b,a,k,n,j,i)*t2a(d,c,n,l)          !baknjidcnl      (-0.500)
     &     - u31(b,a,l,n,k,i)*t2a(d,c,n,j)          !balnkidcnj      (-0.500)
     &     + u31(c,a,l,n,k,i)*t2a(d,b,n,j)          !calnkidbnj      (+0.500)
     &     + u31(d,a,j,n,k,i)*t2a(c,b,n,l)          !dajnkicbnl      (+0.500)
     &     - u31(d,a,l,n,k,i)*t2a(c,b,n,j)          !dalnkicbnj      (-0.500)
     &     - u31(c,a,j,n,k,i)*t2a(d,b,n,l)          !cajnkidbnl      (-0.500)
     &     + u31(b,a,j,n,k,i)*t2a(d,c,n,l)          !bajnkidcnl      (+0.500)
     &     + u31(b,a,l,n,k,j)*t2a(d,c,n,i)          !balnkjdcni      (+0.500)
     &     - u31(c,a,l,n,k,j)*t2a(d,b,n,i)          !calnkjdbni      (-0.500)
     &     - u31(d,a,i,n,k,j)*t2a(c,b,n,l)          !dainkjcbnl      (-0.500)
     &     + u31(d,a,l,n,k,j)*t2a(c,b,n,i)          !dalnkjcbni      (+0.500)
     &     + u31(c,a,i,n,k,j)*t2a(d,b,n,l)          !cainkjdbnl      (+0.500)
     &     - u31(b,a,i,n,k,j)*t2a(d,c,n,l)          !bainkjdcnl      (-0.500)
     &     + u31(b,a,k,n,l,i)*t2a(d,c,n,j)          !baknlidcnj      (+0.500)
     &     - u31(c,a,k,n,l,i)*t2a(d,b,n,j)          !caknlidbnj      (-0.500)
     &     - u31(d,a,j,n,l,i)*t2a(c,b,n,k)          !dajnlicbnk      (-0.500)
     &     + u31(d,a,k,n,l,i)*t2a(c,b,n,j)          !daknlicbnj      (+0.500)
     &     + u31(c,a,j,n,l,i)*t2a(d,b,n,k)          !cajnlidbnk      (+0.500)
     &     - u31(b,a,j,n,l,i)*t2a(d,c,n,k)          !bajnlidcnk      (-0.500)
     &     - u31(b,a,k,n,l,j)*t2a(d,c,n,i)          !baknljdcni      (-0.500)
     &     + u31(c,a,k,n,l,j)*t2a(d,b,n,i)          !caknljdbni      (+0.500)
     &     + u31(d,a,i,n,l,j)*t2a(c,b,n,k)          !dainljcbnk      (+0.500)
     &     - u31(d,a,k,n,l,j)*t2a(c,b,n,i)          !daknljcbni      (-0.500)
     &     - u31(c,a,i,n,l,j)*t2a(d,b,n,k)          !cainljdbnk      (-0.500)
     &     + u31(b,a,i,n,l,j)*t2a(d,c,n,k)          !bainljdcnk      (+0.500)
     &     + u31(b,a,j,n,l,k)*t2a(d,c,n,i)          !bajnlkdcni      (+0.500)
     &     - u31(c,a,j,n,l,k)*t2a(d,b,n,i)          !cajnlkdbni      (-0.500)
     &     - u31(d,a,i,n,l,k)*t2a(c,b,n,j)          !dainlkcbnj      (-0.500)
     &     + u31(d,a,j,n,l,k)*t2a(c,b,n,i)          !dajnlkcbni      (+0.500)
     &     + u31(c,a,i,n,l,k)*t2a(d,b,n,j)          !cainlkdbnj      (+0.500)
     &     - u31(b,a,i,n,l,k)*t2a(d,c,n,j))/2.0d0   !bainlkdcnj      (-0.500)       
             enddo
             v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call
!     & sum12634578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z99, 0.500)
!       call
!     & sum13624578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z99,-0.500)
!       call
!     & sum23514678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z99,-0.500)
!       call
!     & sum23614578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z99, 0.500)
!       call
!     & sum13524678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z99, 0.500)
!       call
!     & sum12534678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z99,-0.500)
!       call
!     & sum12734568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z99,-0.500)
!       call
!     & sum13724568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z99, 0.500)
!       call
!     & sum23514768(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z99, 0.500)
!       call
!     & sum23714568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z99,-0.500)
!       call
!     & sum13524768(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z99,-0.500)
!       call
!     & sum12534768(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z99, 0.500)
!       call
!     & sum12834567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z99, 0.500)
!       call
!     & sum13824567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z99,-0.500)
!       call
!     & sum23514867(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z99,-0.500)
!       call
!     & sum23814567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z99, 0.500)
!       call
!     & sum13524867(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z99, 0.500)
!       call
!     & sum12534867(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z99,-0.500)
!       call
!     & sum12734658(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z99, 0.500)
!       call
!     & sum13724658(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z99,-0.500)
!       call
!     & sum23614758(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z99,-0.500)
!       call
!     & sum23714658(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z99, 0.500)
!       call
!     & sum13624758(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z99, 0.500)
!       call
!     & sum12634758(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z99,-0.500)
!       call
!     & sum12834657(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z99,-0.500)
!       call
!     & sum13824657(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z99, 0.500)
!       call
!     & sum23614857(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z99, 0.500)
!       call
!     & sum23814657(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z99,-0.500)
!       call
!     & sum13624857(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z99,-0.500)
!       call
!     & sum12634857(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z99, 0.500)
!       call
!     & sum12834756(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z99, 0.500)
!       call
!     & sum13824756(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z99,-0.500)
!       call
!     & sum23714856(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z99,-0.500)
!       call
!     & sum23814756(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z99, 0.500)
!       call
!     & sum13724856(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z99, 0.500)
!       call
!     & sum12734856(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z99,-0.500)
!c
!       do i=n0+1,n1-3
!       do j=i+1,n1-2
!       do k=j+1,n1-1
!       do l=k+1,n1
!       do a=n1+1,n3-3
!       do b=a+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)
!     & +(z99(d,c,k,b,a,l,j,i)       ! 12634578 (+0.500)
!     & -z99(d,b,k,c,a,l,j,i)       ! 13624578 (-0.500)
!     & -z99(c,b,l,d,a,k,j,i)       ! 23514678 (-0.500)
!     & +z99(c,b,k,d,a,l,j,i)       ! 23614578 (+0.500)
!     & +z99(d,b,l,c,a,k,j,i)       ! 13524678 (+0.500)
!     & -z99(d,c,l,b,a,k,j,i)       ! 12534678 (-0.500)
!     & -z99(d,c,j,b,a,l,k,i)       ! 12734568 (-0.500)
!     & +z99(d,b,j,c,a,l,k,i)       ! 13724568 (+0.500)
!     & +z99(c,b,l,d,a,j,k,i)       ! 23514768 (+0.500)
!     & -z99(c,b,j,d,a,l,k,i)       ! 23714568 (-0.500)
!     & -z99(d,b,l,c,a,j,k,i)       ! 13524768 (-0.500)
!     & +z99(d,c,l,b,a,j,k,i)       ! 12534768 (+0.500)
!     & +z99(d,c,i,b,a,l,k,j)       ! 12834567 (+0.500)
!     & -z99(d,b,i,c,a,l,k,j)       ! 13824567 (-0.500)
!     & -z99(c,b,l,d,a,i,k,j)       ! 23514867 (-0.500)
!     & +z99(c,b,i,d,a,l,k,j)       ! 23814567 (+0.500)
!     & +z99(d,b,l,c,a,i,k,j)       ! 13524867 (+0.500)
!     & -z99(d,c,l,b,a,i,k,j)       ! 12534867 (-0.500)
!     & +z99(d,c,j,b,a,k,l,i)       ! 12734658 (+0.500)
!     & -z99(d,b,j,c,a,k,l,i)       ! 13724658 (-0.500)
!     & -z99(c,b,k,d,a,j,l,i)       ! 23614758 (-0.500)
!     & +z99(c,b,j,d,a,k,l,i)       ! 23714658 (+0.500)
!     & +z99(d,b,k,c,a,j,l,i)       ! 13624758 (+0.500)
!     & -z99(d,c,k,b,a,j,l,i)       ! 12634758 (-0.500)
!     & -z99(d,c,i,b,a,k,l,j)       ! 12834657 (-0.500)
!     & +z99(d,b,i,c,a,k,l,j)       ! 13824657 (+0.500)
!     & +z99(c,b,k,d,a,i,l,j)       ! 23614857 (+0.500)
!     & -z99(c,b,i,d,a,k,l,j)       ! 23814657 (-0.500)
!     & -z99(d,b,k,c,a,i,l,j)       ! 13624857 (-0.500)
!     & +z99(d,c,k,b,a,i,l,j)       ! 12634857 (+0.500)
!     & +z99(d,c,i,b,a,j,l,k)       ! 12834756 (+0.500)
!     & -z99(d,b,i,c,a,j,l,k)       ! 13824756 (-0.500)
!     & -z99(c,b,j,d,a,i,l,k)       ! 23714856 (-0.500)
!     & +z99(c,b,i,d,a,j,l,k)       ! 23814756 (+0.500)
!     & +z99(d,b,j,c,a,i,l,k)       ! 13724856 (+0.500)
!     & -z99(d,c,j,b,a,i,l,k))/2.0d0! 12734856 (-0.500)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z99)
       deallocate(u31)
c
       allocate(d1(n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       call reorder3412(n0,n1,n0,n1,n0,n1,n0,n1,
     & n0,n1,n0,n1,n0,n1,n0,n1,s17,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s42(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1*k1
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s42)
       deallocate(d1)
       deallocate(b2)
       deallocate(s17)
c
       call sum2134(n0,n1,n1,n3,n0,n1,n0,n1,x1,s42,-0.500)
       deallocate(s42)
c
       allocate(d1(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder4321(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n1,n3,n0,n1,n0,n1,intr,d1)
       allocate(h2(n1+1:n3,n1+1:n3,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,
     & n0+1:n1,n0+1:n1))
       call reorder12534678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,
     & n0,n1,
     & n1,n3,n1,n3,n0,n1,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,t4a,h2)
       allocate(u11(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1
       i2=k1*k1*k1*k3*k3
       i3=k1*k3*k3
       call egemm(i1,i2,i3,d1,h2,u11)
       deallocate(d1)
       deallocate(h2)
c
!       allocate(f1(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
!       call reorder612345(n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,u11,f1)
!       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
!       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
!       allocate(z42(n1+1:n3,n1+1:n3,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
!       i1=k1*k1*k1*k3*k3
!       i2=k1*k3*k3
!       i3=k1
!       call egemm(i1,i2,i3,f1,d2,z42)
!       deallocate(f1)
!       deallocate(d2)
c
       do i=n0+1,n1-3;do j=i+1,n1-2;do k=j+1,n1-1;do l=k+1,n1
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-3;do b=a+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
          if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1                           !top 2 switched
             sum=sum
     &     + (u11(c,a,l,k,j,m)*t2a(d,b,m,i)         !calkjmdbmi      (+0.500)
     &     - u11(b,a,l,k,j,m)*t2a(d,c,m,i)          !balkjmdcmi      (-0.500)
     &     - u11(c,b,l,k,j,m)*t2a(d,a,m,i)          !cblkjmdami      (-0.500)
     &     - u11(d,a,l,k,j,m)*t2a(c,b,m,i)          !dalkjmcbmi      (-0.500)
     &     + u11(d,b,l,k,j,m)*t2a(c,a,m,i)          !dblkjmcami      (+0.500)
     &     - u11(d,c,l,k,j,m)*t2a(b,a,m,i)          !dclkjmbami      (-0.500)
     &     + u11(b,a,l,k,i,m)*t2a(d,c,m,j)          !balkimdcmj      (+0.500)
     &     - u11(c,a,l,k,i,m)*t2a(d,b,m,j)          !calkimdbmj      (-0.500)
     &     + u11(c,b,l,k,i,m)*t2a(d,a,m,j)          !cblkimdamj      (+0.500)
     &     + u11(d,a,l,k,i,m)*t2a(c,b,m,j)          !dalkimcbmj      (+0.500)
     &     - u11(d,b,l,k,i,m)*t2a(c,a,m,j)          !dblkimcamj      (-0.500)
     &     + u11(d,c,l,k,i,m)*t2a(b,a,m,j)          !dclkimbamj      (+0.500)
     &     - u11(b,a,l,j,i,m)*t2a(d,c,m,k)          !baljimdcmk      (-0.500)
     &     + u11(c,a,l,j,i,m)*t2a(d,b,m,k)          !caljimdbmk      (+0.500)
     &     - u11(c,b,l,j,i,m)*t2a(d,a,m,k)          !cbljimdamk      (-0.500)
     &     - u11(d,a,l,j,i,m)*t2a(c,b,m,k)          !daljimcbmk      (-0.500)
     &     + u11(d,b,l,j,i,m)*t2a(c,a,m,k)          !dbljimcamk      (+0.500)
     &     - u11(d,c,l,j,i,m)*t2a(b,a,m,k)          !dcljimbamk      (-0.500)
     &     + u11(b,a,k,j,i,m)*t2a(d,c,m,l)          !bakjimdcml      (+0.500)
     &     - u11(c,a,k,j,i,m)*t2a(d,b,m,l)          !cakjimdbml      (-0.500)
     &     + u11(c,b,k,j,i,m)*t2a(d,a,m,l)          !cbkjimdaml      (+0.500)
     &     + u11(d,a,k,j,i,m)*t2a(c,b,m,l)          !dakjimcbml      (+0.500)
     &     - u11(d,b,k,j,i,m)*t2a(c,a,m,l)          !dbkjimcaml      (-0.500)
     &     + u11(d,c,k,j,i,m)*t2a(b,a,m,l))/2.0d0   !dckjimbaml      (+0.500)       
             enddo
             v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call
!     & sum12834567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z42,-0.500)
!       call
!     & sum13824567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z42, 0.500)
!       call
!     & sum14823567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z42,-0.500)
!       call
!     & sum23814567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z42,-0.500)
!       call
!     & sum24813567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z42, 0.500)
!       call
!     & sum34812567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z42,-0.500)
!       call
!     & sum12734568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z42, 0.500)
!       call
!     & sum13724568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z42,-0.500)
!       call
!     & sum14723568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z42, 0.500)
!       call
!     & sum23714568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z42, 0.500)
!       call
!     & sum24713568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z42,-0.500)
!       call
!     & sum34712568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z42, 0.500)
!       call
!     & sum12634578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z42,-0.500)
!       call
!     & sum13624578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z42, 0.500)
!       call
!     & sum14623578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z42,-0.500)
!       call
!     & sum23614578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z42,-0.500)
!       call
!     & sum24613578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z42, 0.500)
!       call
!     & sum34612578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z42,-0.500)
!       call
!     & sum12534678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z42, 0.500)
!       call
!     & sum13524678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z42,-0.500)
!       call
!     & sum14523678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z42, 0.500)
!       call
!     & sum23514678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z42, 0.500)
!       call
!     & sum24513678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z42,-0.500)
!       call
!     & sum34512678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z42, 0.500)
!c
!       do i=n0+1,n1-3
!       do j=i+1,n1-2
!       do k=j+1,n1-1
!       do l=k+1,n1
!       do a=n1+1,n3-3
!       do b=a+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)
!     & +(z42(d,b,i,c,a,l,k,j)      ! 13824567 (+0.500) top two switched
!     & -z42(d,c,i,b,a,l,k,j)       ! 12834567 (-0.500)
!     & -z42(d,a,i,c,b,l,k,j)       ! 14823567 (-0.500)
!     & -z42(c,b,i,d,a,l,k,j)       ! 23814567 (-0.500)
!     & +z42(c,a,i,d,b,l,k,j)       ! 24813567 (+0.500)
!     & -z42(b,a,i,d,c,l,k,j)       ! 34812567 (-0.500)
!     & +z42(d,c,j,b,a,l,k,i)       ! 12734568 (+0.500)
!     & -z42(d,b,j,c,a,l,k,i)       ! 13724568 (-0.500)
!     & +z42(d,a,j,c,b,l,k,i)       ! 14723568 (+0.500)
!     & +z42(c,b,j,d,a,l,k,i)       ! 23714568 (+0.500)
!     & -z42(c,a,j,d,b,l,k,i)       ! 24713568 (-0.500)
!     & +z42(b,a,j,d,c,l,k,i)       ! 34712568 (+0.500)
!     & -z42(d,c,k,b,a,l,j,i)       ! 12634578 (-0.500)
!     & +z42(d,b,k,c,a,l,j,i)       ! 13624578 (+0.500)
!     & -z42(d,a,k,c,b,l,j,i)       ! 14623578 (-0.500)
!     & -z42(c,b,k,d,a,l,j,i)       ! 23614578 (-0.500)
!     & +z42(c,a,k,d,b,l,j,i)       ! 24613578 (+0.500)
!     & -z42(b,a,k,d,c,l,j,i)       ! 34612578 (-0.500)
!     & +z42(d,c,l,b,a,k,j,i)       ! 12534678 (+0.500)
!     & -z42(d,b,l,c,a,k,j,i)       ! 13524678 (-0.500)
!     & +z42(d,a,l,c,b,k,j,i)       ! 14523678 (+0.500)
!     & +z42(c,b,l,d,a,k,j,i)       ! 23514678 (+0.500)
!     & -z42(c,a,l,d,b,k,j,i)       ! 24513678 (-0.500)
!     & +z42(b,a,l,d,c,k,j,i))/2.0d0! 34512678 (+0.500)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z42)
       deallocate(u11)
c
       allocate(d1(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       call reorder3142(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n1,n3,n0,n1,intr,d1)
       allocate(d2(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       call reorder1324(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n0,n1,n1,n3,n0,n1,t2a,d2)
       allocate(s18(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       i1=k1*k3
       i2=k1*k3
       i3=k1*k3
       call egemm(i1,i2,i3,d1,d2,s18)
       deallocate(d1)
       deallocate(d2)
c
       call sum3421(n0,n1,n1,n3,n1,n3,n0,n1,x6,s18, 1.000)
c
       allocate(d1(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder4312(n1,n3,n0,n1,n1,n3,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,s18,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s44(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
       i1=k1*k3*k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s44)
       deallocate(d1)
       deallocate(b2)
c
       call sum2134(n1,n3,n1,n3,n1,n3,n0,n1,x10,s44,-1.000)
       deallocate(s44)
c
       allocate(d1(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       call reorder3412(n1,n3,n0,n1,n1,n3,n0,n1,
     & n1,n3,n0,n1,n1,n3,n0,n1,s18,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s40(n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1))
       i1=k1*k3*k1
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s40)
       deallocate(d1)
       deallocate(b2)
       deallocate(s18)
c
       call sum4123(n0,n1,n1,n3,n0,n1,n0,n1,x9,s40,-1.000)
       deallocate(s40)
c
       allocate(d1(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder4312(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n1,n3,n0,n1,n0,n1,intr,d1)
       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
       allocate(q7(n0+1:n1,n0+1:n1))
       i1=k1
       i2=k1
       i3=k1*k3*k3
       call egemm(i1,i2,i3,d1,d2,q7)
       deallocate(d1)
       deallocate(d2)
c
       call sum21(n0,n1,n0,n1,x3,q7,-0.500)
       deallocate(q7)
c
       allocate(d1(n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3))
       call reorder1243(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n0,n1,n1,n3,n1,n3,intr,d1)
       allocate(d2(n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3))
       call reorder4312(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n0,n1,n1,n3,n1,n3,t2a,d2)
       allocate(s19(n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3))
       i1=k3*k3
       i2=k3*k3
       i3=k1*k1
       call egemm(i1,i2,i3,d1,d2,s19)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x7(n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3))
       x7=0.0d0
       call sum3412(n1,n3,n1,n3,n1,n3,n1,n3,x7,s19, 0.500)
c
       allocate(d1(n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3))
       call reorder4312(n1,n3,n1,n3,n1,n3,n1,n3,
     & n1,n3,n1,n3,n1,n3,n1,n3,s19,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s41(n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3))
       i1=k3*k3*k3
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s41)
       deallocate(d1)
       deallocate(b2)
       deallocate(s19)
c
       call sum4123(n1,n3,n1,n3,n1,n3,n0,n1,x2,s41, 0.500)
       deallocate(s41)
c
       allocate(d1(n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3))
       call reorder3124(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n0,n1,n1,n3,intr,d1)
       allocate(d2(n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3))
       call reorder1432(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n0,n1,n0,n1,n1,n3,t2a,d2)
       allocate(q8(n1+1:n3,n1+1:n3))
       i1=k3
       i2=k3
       i3=k1*k1*k3
       call egemm(i1,i2,i3,d1,d2,q8)
       deallocate(d1)
       deallocate(d2)
c
       call sum21(n1,n3,n1,n3,x4,q8, 0.500)
       deallocate(q8)
c
       allocate(d1(n1+1:n3,n2+1:n3,n0+1:n1,n0+1:n2))
       call reorder3412(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n2,n3,n0,n1,n0,n2,intm,d1)
       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
       allocate(u12(n1+1:n3,n0+1:n1,n0+1:n1,n2+1:n3,n0+1:n1,n0+1:n2))
       i1=k2*k1*k4
       i2=k1*k1*k3
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u12)
       deallocate(d1)
       deallocate(d2)
c
!       allocate(f1(n0+1:n1,n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1,n0+1:n1))
!       call reorder546123(n1,n3,n0,n1,n0,n1,n2,n3,n0,n1,n0,n2,
!     & n0,n1,n2,n3,n0,n2,n1,n3,n0,n1,n0,n1,u12,f1)
!       allocate(h2(n0+1:n1,n2+1:n3,n0+1:n2,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!       call reorder61523478(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,
!     & n0,n1,n2,n3,n0,n2,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,t4b,h2)
!       allocate(z47(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
!       i1=k1*k1*k3
!       i2=k1*k1*k3*k3*k3
!       i3=k2*k4*k1
!       call egemm(i1,i2,i3,f1,h2,z47)
!       deallocate(f1)
!       deallocate(h2)
c
       do i=n0+1,n1-3;do j=i+1,n1-2;do k=j+1,n1-1;do l=k+1,n1
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-3;do b=a+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
          if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n2+1,n3;do m=n0+1,n1;do n=n0+1,n2
             sum=sum
     &     + u12(d,j,i,f,m,n)*t4b(f,c,b,a,n,m,l,k)  !djifmnfcbanmlk  (+1.000)
     &     - u12(c,j,i,f,m,n)*t4b(f,d,b,a,n,m,l,k)  !cjifmnfdbanmlk  (-1.000)
     &     + u12(b,j,i,f,m,n)*t4b(f,d,c,a,n,m,l,k)  !bjifmnfdcanmlk  (+1.000)
     &     - u12(a,j,i,f,m,n)*t4b(f,d,c,b,n,m,l,k)  !ajifmnfdcbnmlk  (-1.000)
     &     - u12(d,k,i,f,m,n)*t4b(f,c,b,a,n,m,l,j)  !dkifmnfcbanmlj  (-1.000)
     &     + u12(c,k,i,f,m,n)*t4b(f,d,b,a,n,m,l,j)  !ckifmnfdbanmlj  (+1.000)
     &     - u12(b,k,i,f,m,n)*t4b(f,d,c,a,n,m,l,j)  !bkifmnfdcanmlj  (-1.000)
     &     + u12(a,k,i,f,m,n)*t4b(f,d,c,b,n,m,l,j)  !akifmnfdcbnmlj  (+1.000)
     &     + u12(d,l,i,f,m,n)*t4b(f,c,b,a,n,m,k,j)  !dlifmnfcbanmkj  (+1.000)
     &     - u12(c,l,i,f,m,n)*t4b(f,d,b,a,n,m,k,j)  !clifmnfdbanmkj  (-1.000)
     &     + u12(b,l,i,f,m,n)*t4b(f,d,c,a,n,m,k,j)  !blifmnfdcanmkj  (+1.000)
     &     - u12(a,l,i,f,m,n)*t4b(f,d,c,b,n,m,k,j)  !alifmnfdcbnmkj  (-1.000)
     &     + u12(d,k,j,f,m,n)*t4b(f,c,b,a,n,m,l,i)  !dkjfmnfcbanmli  (+1.000)
     &     - u12(c,k,j,f,m,n)*t4b(f,d,b,a,n,m,l,i)  !ckjfmnfdbanmli  (-1.000)
     &     + u12(b,k,j,f,m,n)*t4b(f,d,c,a,n,m,l,i)  !bkjfmnfdcanmli  (+1.000)
     &     - u12(a,k,j,f,m,n)*t4b(f,d,c,b,n,m,l,i)  !akjfmnfdcbnmli  (-1.000)
     &     - u12(d,l,j,f,m,n)*t4b(f,c,b,a,n,m,k,i)  !dljfmnfcbanmki  (-1.000)
     &     + u12(c,l,j,f,m,n)*t4b(f,d,b,a,n,m,k,i)  !cljfmnfdbanmki  (+1.000)
     &     - u12(b,l,j,f,m,n)*t4b(f,d,c,a,n,m,k,i)  !bljfmnfdcanmki  (-1.000)
     &     + u12(a,l,j,f,m,n)*t4b(f,d,c,b,n,m,k,i)  !aljfmnfdcbnmki  (+1.000)
     &     + u12(d,l,k,f,m,n)*t4b(f,c,b,a,n,m,j,i)  !dlkfmnfcbanmji  (+1.000)
     &     - u12(c,l,k,f,m,n)*t4b(f,d,b,a,n,m,j,i)  !clkfmnfdbanmji  (-1.000)
     &     + u12(b,l,k,f,m,n)*t4b(f,d,c,a,n,m,j,i)  !blkfmnfdcanmji  (+1.000)
     &     - u12(a,l,k,f,m,n)*t4b(f,d,c,b,n,m,j,i)  !alkfmnfdcbnmji  (-1.000)       
             enddo;enddo;enddo
             v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call
!     & sum23456178(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z47, 1.000)
!       call
!     & sum13456278(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z47,-1.000)
!       call
!     & sum12456378(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z47, 1.000)
!       call
!     & sum12356478(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z47,-1.000)
!       call
!     & sum23457168(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z47,-1.000)
!       call
!     & sum13457268(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z47, 1.000)
!       call
!     & sum12457368(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z47,-1.000)
!       call
!     & sum12357468(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z47, 1.000)
!       call
!     & sum23467158(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z47, 1.000)
!       call
!     & sum13467258(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z47,-1.000)
!       call
!     & sum12467358(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z47, 1.000)
!       call
!     & sum12367458(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z47,-1.000)
!       call
!     & sum23458167(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z47, 1.000)
!       call
!     & sum13458267(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z47,-1.000)
!       call
!     & sum12458367(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z47, 1.000)
!       call
!     & sum12358467(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z47,-1.000)
!       call
!     & sum23468157(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z47,-1.000)
!       call
!     & sum13468257(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z47, 1.000)
!       call
!     & sum12468357(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z47,-1.000)
!       call
!     & sum12368457(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z47, 1.000)
!       call
!     & sum23478156(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z47, 1.000)
!       call
!     & sum13478256(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z47,-1.000)
!       call
!     & sum12478356(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z47, 1.000)
!       call
!     & sum12378456(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z47,-1.000)
!c
!       do i=n0+1,n1-3
!       do j=i+1,n1-2
!       do k=j+1,n1-1
!       do l=k+1,n1
!       do a=n1+1,n3-3
!       do b=a+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)
!     & +z47(c,b,a,l,k,d,j,i)       ! 23456178 (+1.000)
!     & -z47(d,b,a,l,k,c,j,i)       ! 13456278 (-1.000)
!     & +z47(d,c,a,l,k,b,j,i)       ! 12456378 (+1.000)
!     & -z47(d,c,b,l,k,a,j,i)       ! 12356478 (-1.000)
!     & -z47(c,b,a,l,j,d,k,i)       ! 23457168 (-1.000)
!     & +z47(d,b,a,l,j,c,k,i)       ! 13457268 (+1.000)
!     & -z47(d,c,a,l,j,b,k,i)       ! 12457368 (-1.000)
!     & +z47(d,c,b,l,j,a,k,i)       ! 12357468 (+1.000)
!     & +z47(c,b,a,k,j,d,l,i)       ! 23467158 (+1.000)
!     & -z47(d,b,a,k,j,c,l,i)       ! 13467258 (-1.000)
!     & +z47(d,c,a,k,j,b,l,i)       ! 12467358 (+1.000)
!     & -z47(d,c,b,k,j,a,l,i)       ! 12367458 (-1.000)
!     & +z47(c,b,a,l,i,d,k,j)       ! 23458167 (+1.000)
!     & -z47(d,b,a,l,i,c,k,j)       ! 13458267 (-1.000)
!     & +z47(d,c,a,l,i,b,k,j)       ! 12458367 (+1.000)
!     & -z47(d,c,b,l,i,a,k,j)       ! 12358467 (-1.000)
!     & -z47(c,b,a,k,i,d,l,j)       ! 23468157 (-1.000)
!     & +z47(d,b,a,k,i,c,l,j)       ! 13468257 (+1.000)
!     & -z47(d,c,a,k,i,b,l,j)       ! 12468357 (-1.000)
!     & +z47(d,c,b,k,i,a,l,j)       ! 12368457 (+1.000)
!     & +z47(c,b,a,j,i,d,l,k)       ! 23478156 (+1.000)
!     & -z47(d,b,a,j,i,c,l,k)       ! 13478256 (-1.000)
!     & +z47(d,c,a,j,i,b,l,k)       ! 12478356 (+1.000)
!     & -z47(d,c,b,j,i,a,l,k)       ! 12378456 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z47)
c
       allocate(f1(n2+1:n3,n0+1:n2,n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder465123(n1,n3,n0,n1,n0,n1,n2,n3,n0,n1,n0,n2,
     & n2,n3,n0,n2,n0,n1,n1,n3,n0,n1,n0,n1,u12,f1)
       allocate(d2(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       call reorder1324(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n0,n2,n1,n3,n0,n1,t2b,d2)
       allocate(u32(n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       i1=k1*k1*k3*k1
       i2=k1*k3
       i3=k2*k4
       call egemm(i1,i2,i3,f1,d2,u32)
       deallocate(f1)
       deallocate(d2)
c
!       allocate(f1(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
!       call reorder341256(n1,n3,n0,n1,n0,n1,n1,n3,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,u32,f1)
!       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
!       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
!       allocate(z100(n1+1:n3,n1+1:n3,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
!       i1=k1*k1*k1*k3*k3
!       i2=k1*k3*k3
!       i3=k1
!       call egemm(i1,i2,i3,f1,d2,z100)
!       deallocate(f1)
!       deallocate(d2)
c
       do i=n0+1,n1-3;do j=i+1,n1-2;do k=j+1,n1-1;do l=k+1,n1
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-3;do b=a+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
          if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1
             sum=sum
     &     - u32(a,l,m,d,j,i)*t2a(c,b,m,k)          !almdjicbmk      (-1.000)
     &     + u32(b,l,m,d,j,i)*t2a(c,a,m,k)          !blmdjicamk      (+1.000)
     &     - u32(c,l,m,d,j,i)*t2a(b,a,m,k)          !clmdjibamk      (-1.000)
     &     + u32(a,l,m,c,j,i)*t2a(d,b,m,k)          !almcjidbmk      (+1.000)
     &     - u32(b,l,m,c,j,i)*t2a(d,a,m,k)          !blmcjidamk      (-1.000)
     &     - u32(a,l,m,b,j,i)*t2a(d,c,m,k)          !almbjidcmk      (-1.000)
     &     + u32(b,l,m,a,j,i)*t2a(d,c,m,k)          !blmajidcmk      (+1.000)
     &     + u32(c,l,m,b,j,i)*t2a(d,a,m,k)          !clmbjidamk      (+1.000)
     &     - u32(c,l,m,a,j,i)*t2a(d,b,m,k)          !clmajidbmk      (-1.000)
     &     + u32(d,l,m,c,j,i)*t2a(b,a,m,k)          !dlmcjibamk      (+1.000)
     &     - u32(d,l,m,b,j,i)*t2a(c,a,m,k)          !dlmbjicamk      (-1.000)
     &     + u32(d,l,m,a,j,i)*t2a(c,b,m,k)          !dlmajicbmk      (+1.000)
     &     + u32(a,k,m,d,j,i)*t2a(c,b,m,l)          !akmdjicbml      (+1.000)
     &     - u32(b,k,m,d,j,i)*t2a(c,a,m,l)          !bkmdjicaml      (-1.000)
     &     + u32(c,k,m,d,j,i)*t2a(b,a,m,l)          !ckmdjibaml      (+1.000)
     &     - u32(a,k,m,c,j,i)*t2a(d,b,m,l)          !akmcjidbml      (-1.000)
     &     + u32(b,k,m,c,j,i)*t2a(d,a,m,l)          !bkmcjidaml      (+1.000)
     &     + u32(a,k,m,b,j,i)*t2a(d,c,m,l)          !akmbjidcml      (+1.000)
     &     - u32(b,k,m,a,j,i)*t2a(d,c,m,l)          !bkmajidcml      (-1.000)
     &     - u32(c,k,m,b,j,i)*t2a(d,a,m,l)          !ckmbjidaml      (-1.000)
     &     + u32(c,k,m,a,j,i)*t2a(d,b,m,l)          !ckmajidbml      (+1.000)
     &     - u32(d,k,m,c,j,i)*t2a(b,a,m,l)          !dkmcjibaml      (-1.000)
     &     + u32(d,k,m,b,j,i)*t2a(c,a,m,l)          !dkmbjicaml      (+1.000)
     &     - u32(d,k,m,a,j,i)*t2a(c,b,m,l)          !dkmajicbml      (-1.000)
     &     + u32(a,l,m,d,k,i)*t2a(c,b,m,j)          !almdkicbmj      (+1.000)
     &     - u32(b,l,m,d,k,i)*t2a(c,a,m,j)          !blmdkicamj      (-1.000)
     &     + u32(c,l,m,d,k,i)*t2a(b,a,m,j)          !clmdkibamj      (+1.000)
     &     - u32(a,l,m,c,k,i)*t2a(d,b,m,j)          !almckidbmj      (-1.000)
     &     + u32(b,l,m,c,k,i)*t2a(d,a,m,j)          !blmckidamj      (+1.000)
     &     + u32(a,l,m,b,k,i)*t2a(d,c,m,j)          !almbkidcmj      (+1.000)
     &     - u32(b,l,m,a,k,i)*t2a(d,c,m,j)          !blmakidcmj      (-1.000)
     &     - u32(c,l,m,b,k,i)*t2a(d,a,m,j)          !clmbkidamj      (-1.000)
     &     + u32(c,l,m,a,k,i)*t2a(d,b,m,j)          !clmakidbmj      (+1.000)
     &     - u32(d,l,m,c,k,i)*t2a(b,a,m,j)          !dlmckibamj      (-1.000)
     &     + u32(d,l,m,b,k,i)*t2a(c,a,m,j)          !dlmbkicamj      (+1.000)
     &     - u32(d,l,m,a,k,i)*t2a(c,b,m,j)          !dlmakicbmj      (-1.000)
     &     - u32(a,l,m,b,k,j)*t2a(d,c,m,i)          !almbkjdcmi      (-1.000)
     &     + u32(b,l,m,a,k,j)*t2a(d,c,m,i)          !blmakjdcmi      (+1.000)
     &     + u32(a,l,m,c,k,j)*t2a(d,b,m,i)          !almckjdbmi      (+1.000)
     &     - u32(b,l,m,c,k,j)*t2a(d,a,m,i)          !blmckjdami      (-1.000)
     &     - u32(c,l,m,a,k,j)*t2a(d,b,m,i)          !clmakjdbmi      (-1.000)
     &     + u32(c,l,m,b,k,j)*t2a(d,a,m,i)          !clmbkjdami      (+1.000)
     &     - u32(a,l,m,d,k,j)*t2a(c,b,m,i)          !almdkjcbmi      (-1.000)
     &     + u32(b,l,m,d,k,j)*t2a(c,a,m,i)          !blmdkjcami      (+1.000)
     &     - u32(c,l,m,d,k,j)*t2a(b,a,m,i)          !clmdkjbami      (-1.000)
     &     + u32(d,l,m,a,k,j)*t2a(c,b,m,i)          !dlmakjcbmi      (+1.000)
     &     - u32(d,l,m,b,k,j)*t2a(c,a,m,i)          !dlmbkjcami      (-1.000)
     &     + u32(d,l,m,c,k,j)*t2a(b,a,m,i)          !dlmckjbami      (+1.000)
     &     - u32(a,k,m,d,l,i)*t2a(c,b,m,j)          !akmdlicbmj      (-1.000)
     &     + u32(b,k,m,d,l,i)*t2a(c,a,m,j)          !bkmdlicamj      (+1.000)
     &     - u32(c,k,m,d,l,i)*t2a(b,a,m,j)          !ckmdlibamj      (-1.000)
     &     + u32(a,k,m,c,l,i)*t2a(d,b,m,j)          !akmclidbmj      (+1.000)
     &     - u32(b,k,m,c,l,i)*t2a(d,a,m,j)          !bkmclidamj      (-1.000)
     &     - u32(a,k,m,b,l,i)*t2a(d,c,m,j)          !akmblidcmj      (-1.000)
     &     + u32(b,k,m,a,l,i)*t2a(d,c,m,j)          !bkmalidcmj      (+1.000)
     &     + u32(c,k,m,b,l,i)*t2a(d,a,m,j)          !ckmblidamj      (+1.000)
     &     - u32(c,k,m,a,l,i)*t2a(d,b,m,j)          !ckmalidbmj      (-1.000)
     &     + u32(d,k,m,c,l,i)*t2a(b,a,m,j)          !dkmclibamj      (+1.000)
     &     - u32(d,k,m,b,l,i)*t2a(c,a,m,j)          !dkmblicamj      (-1.000)
     &     + u32(d,k,m,a,l,i)*t2a(c,b,m,j)          !dkmalicbmj      (+1.000)
     &     + u32(a,k,m,b,l,j)*t2a(d,c,m,i)          !akmbljdcmi      (+1.000)
     &     - u32(b,k,m,a,l,j)*t2a(d,c,m,i)          !bkmaljdcmi      (-1.000)
     &     - u32(a,k,m,c,l,j)*t2a(d,b,m,i)          !akmcljdbmi      (-1.000)
     &     + u32(b,k,m,c,l,j)*t2a(d,a,m,i)          !bkmcljdami      (+1.000)
     &     + u32(c,k,m,a,l,j)*t2a(d,b,m,i)          !ckmaljdbmi      (+1.000)
     &     - u32(c,k,m,b,l,j)*t2a(d,a,m,i)          !ckmbljdami      (-1.000)
     &     + u32(a,k,m,d,l,j)*t2a(c,b,m,i)          !akmdljcbmi      (+1.000)
     &     - u32(b,k,m,d,l,j)*t2a(c,a,m,i)          !bkmdljcami      (-1.000)
     &     + u32(c,k,m,d,l,j)*t2a(b,a,m,i)          !ckmdljbami      (+1.000)
     &     - u32(d,k,m,a,l,j)*t2a(c,b,m,i)          !dkmaljcbmi      (-1.000)
     &     + u32(d,k,m,b,l,j)*t2a(c,a,m,i)          !dkmbljcami      (+1.000)
     &     - u32(d,k,m,c,l,j)*t2a(b,a,m,i)          !dkmcljbami      (-1.000)
     &     - u32(a,j,m,d,k,i)*t2a(c,b,m,l)          !ajmdkicbml      (-1.000)
     &     + u32(b,j,m,d,k,i)*t2a(c,a,m,l)          !bjmdkicaml      (+1.000)
     &     - u32(c,j,m,d,k,i)*t2a(b,a,m,l)          !cjmdkibaml      (-1.000)
     &     + u32(a,j,m,c,k,i)*t2a(d,b,m,l)          !ajmckidbml      (+1.000)
     &     - u32(b,j,m,c,k,i)*t2a(d,a,m,l)          !bjmckidaml      (-1.000)
     &     - u32(a,j,m,b,k,i)*t2a(d,c,m,l)          !ajmbkidcml      (-1.000)
     &     + u32(b,j,m,a,k,i)*t2a(d,c,m,l)          !bjmakidcml      (+1.000)
     &     + u32(c,j,m,b,k,i)*t2a(d,a,m,l)          !cjmbkidaml      (+1.000)
     &     - u32(c,j,m,a,k,i)*t2a(d,b,m,l)          !cjmakidbml      (-1.000)
     &     + u32(d,j,m,c,k,i)*t2a(b,a,m,l)          !djmckibaml      (+1.000)
     &     - u32(d,j,m,b,k,i)*t2a(c,a,m,l)          !djmbkicaml      (-1.000)
     &     + u32(d,j,m,a,k,i)*t2a(c,b,m,l)          !djmakicbml      (+1.000)
     &     + u32(a,j,m,d,l,i)*t2a(c,b,m,k)          !ajmdlicbmk      (+1.000)
     &     - u32(b,j,m,d,l,i)*t2a(c,a,m,k)          !bjmdlicamk      (-1.000)
     &     + u32(c,j,m,d,l,i)*t2a(b,a,m,k)          !cjmdlibamk      (+1.000)
     &     - u32(a,j,m,c,l,i)*t2a(d,b,m,k)          !ajmclidbmk      (-1.000)
     &     + u32(b,j,m,c,l,i)*t2a(d,a,m,k)          !bjmclidamk      (+1.000)
     &     + u32(a,j,m,b,l,i)*t2a(d,c,m,k)          !ajmblidcmk      (+1.000)
     &     - u32(b,j,m,a,l,i)*t2a(d,c,m,k)          !bjmalidcmk      (-1.000)
     &     - u32(c,j,m,b,l,i)*t2a(d,a,m,k)          !cjmblidamk      (-1.000)
     &     + u32(c,j,m,a,l,i)*t2a(d,b,m,k)          !cjmalidbmk      (+1.000)
     &     - u32(d,j,m,c,l,i)*t2a(b,a,m,k)          !djmclibamk      (-1.000)
     &     + u32(d,j,m,b,l,i)*t2a(c,a,m,k)          !djmblicamk      (+1.000)
     &     - u32(d,j,m,a,l,i)*t2a(c,b,m,k)          !djmalicbmk      (-1.000)
     &     - u32(a,j,m,b,l,k)*t2a(d,c,m,i)          !ajmblkdcmi      (-1.000)
     &     + u32(b,j,m,a,l,k)*t2a(d,c,m,i)          !bjmalkdcmi      (+1.000)
     &     + u32(a,j,m,c,l,k)*t2a(d,b,m,i)          !ajmclkdbmi      (+1.000)
     &     - u32(b,j,m,c,l,k)*t2a(d,a,m,i)          !bjmclkdami      (-1.000)
     &     - u32(c,j,m,a,l,k)*t2a(d,b,m,i)          !cjmalkdbmi      (-1.000)
     &     + u32(c,j,m,b,l,k)*t2a(d,a,m,i)          !cjmblkdami      (+1.000)
     &     - u32(a,j,m,d,l,k)*t2a(c,b,m,i)          !ajmdlkcbmi      (-1.000)
     &     + u32(b,j,m,d,l,k)*t2a(c,a,m,i)          !bjmdlkcami      (+1.000)
     &     - u32(c,j,m,d,l,k)*t2a(b,a,m,i)          !cjmdlkbami      (-1.000)
     &     + u32(d,j,m,a,l,k)*t2a(c,b,m,i)          !djmalkcbmi      (+1.000)
     &     - u32(d,j,m,b,l,k)*t2a(c,a,m,i)          !djmblkcami      (-1.000)
     &     + u32(d,j,m,c,l,k)*t2a(b,a,m,i)          !djmclkbami      (+1.000)
     &     + u32(a,i,m,d,k,j)*t2a(c,b,m,l)          !aimdkjcbml      (+1.000)
     &     - u32(b,i,m,d,k,j)*t2a(c,a,m,l)          !bimdkjcaml      (-1.000)
     &     + u32(c,i,m,d,k,j)*t2a(b,a,m,l)          !cimdkjbaml      (+1.000)
     &     - u32(a,i,m,c,k,j)*t2a(d,b,m,l)          !aimckjdbml      (-1.000)
     &     + u32(b,i,m,c,k,j)*t2a(d,a,m,l)          !bimckjdaml      (+1.000)
     &     + u32(a,i,m,b,k,j)*t2a(d,c,m,l)          !aimbkjdcml      (+1.000)
     &     - u32(b,i,m,a,k,j)*t2a(d,c,m,l)          !bimakjdcml      (-1.000)
     &     - u32(c,i,m,b,k,j)*t2a(d,a,m,l)          !cimbkjdaml      (-1.000)
     &     + u32(c,i,m,a,k,j)*t2a(d,b,m,l)          !cimakjdbml      (+1.000)
     &     - u32(d,i,m,c,k,j)*t2a(b,a,m,l)          !dimckjbaml      (-1.000)
     &     + u32(d,i,m,b,k,j)*t2a(c,a,m,l)          !dimbkjcaml      (+1.000)
     &     - u32(d,i,m,a,k,j)*t2a(c,b,m,l)          !dimakjcbml      (-1.000)
     &     - u32(a,i,m,d,l,j)*t2a(c,b,m,k)          !aimdljcbmk      (-1.000)
     &     + u32(b,i,m,d,l,j)*t2a(c,a,m,k)          !bimdljcamk      (+1.000)
     &     - u32(c,i,m,d,l,j)*t2a(b,a,m,k)          !cimdljbamk      (-1.000)
     &     + u32(a,i,m,c,l,j)*t2a(d,b,m,k)          !aimcljdbmk      (+1.000)
     &     - u32(b,i,m,c,l,j)*t2a(d,a,m,k)          !bimcljdamk      (-1.000)
     &     - u32(a,i,m,b,l,j)*t2a(d,c,m,k)          !aimbljdcmk      (-1.000)
     &     + u32(b,i,m,a,l,j)*t2a(d,c,m,k)          !bimaljdcmk      (+1.000)
     &     + u32(c,i,m,b,l,j)*t2a(d,a,m,k)          !cimbljdamk      (+1.000)
     &     - u32(c,i,m,a,l,j)*t2a(d,b,m,k)          !cimaljdbmk      (-1.000)
     &     + u32(d,i,m,c,l,j)*t2a(b,a,m,k)          !dimcljbamk      (+1.000)
     &     - u32(d,i,m,b,l,j)*t2a(c,a,m,k)          !dimbljcamk      (-1.000)
     &     + u32(d,i,m,a,l,j)*t2a(c,b,m,k)          !dimaljcbmk      (+1.000)
     &     + u32(a,i,m,b,l,k)*t2a(d,c,m,j)          !aimblkdcmj      (+1.000)
     &     - u32(b,i,m,a,l,k)*t2a(d,c,m,j)          !bimalkdcmj      (-1.000)
     &     - u32(a,i,m,c,l,k)*t2a(d,b,m,j)          !aimclkdbmj      (-1.000)
     &     + u32(b,i,m,c,l,k)*t2a(d,a,m,j)          !bimclkdamj      (+1.000)
     &     + u32(c,i,m,a,l,k)*t2a(d,b,m,j)          !cimalkdbmj      (+1.000)
     &     - u32(c,i,m,b,l,k)*t2a(d,a,m,j)          !cimblkdamj      (-1.000)
     &     + u32(a,i,m,d,l,k)*t2a(c,b,m,j)          !aimdlkcbmj      (+1.000)
     &     - u32(b,i,m,d,l,k)*t2a(c,a,m,j)          !bimdlkcamj      (-1.000)
     &     + u32(c,i,m,d,l,k)*t2a(b,a,m,j)          !cimdlkbamj      (+1.000)
     &     - u32(d,i,m,a,l,k)*t2a(c,b,m,j)          !dimalkcbmj      (-1.000)
     &     + u32(d,i,m,b,l,k)*t2a(c,a,m,j)          !dimblkcamj      (+1.000)
     &     - u32(d,i,m,c,l,k)*t2a(b,a,m,j)          !dimclkbamj      (-1.000)      
             enddo
             v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call
!     & sum23614578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100,-1.000)
!       call
!     & sum24613578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100, 1.000)
!       call
!     & sum34612578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100,-1.000)
!       call
!     & sum13624578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100, 1.000)
!       call
!     & sum14623578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100,-1.000)
!       call
!     & sum12634578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100,-1.000)
!       call
!     & sum12643578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100, 1.000)
!       call
!     & sum14632578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100, 1.000)
!       call
!     & sum13642578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100,-1.000)
!       call
!     & sum34621578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100, 1.000)
!       call
!     & sum24631578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100,-1.000)
!       call
!     & sum23641578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100, 1.000)
!       call
!     & sum23514678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100, 1.000)
!       call
!     & sum24513678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100,-1.000)
!       call
!     & sum34512678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100, 1.000)
!       call
!     & sum13524678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100,-1.000)
!       call
!     & sum14523678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100, 1.000)
!       call
!     & sum12534678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100, 1.000)
!       call
!     & sum12543678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100,-1.000)
!       call
!     & sum14532678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100,-1.000)
!       call
!     & sum13542678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100, 1.000)
!       call
!     & sum34521678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100,-1.000)
!       call
!     & sum24531678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100, 1.000)
!       call
!     & sum23541678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100,-1.000)
!       call
!     & sum23714568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100, 1.000)
!       call
!     & sum24713568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100,-1.000)
!       call
!     & sum34712568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100, 1.000)
!       call
!     & sum13724568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100,-1.000)
!       call
!     & sum14723568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100, 1.000)
!       call
!     & sum12734568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100, 1.000)
!       call
!     & sum12743568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100,-1.000)
!       call
!     & sum14732568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100,-1.000)
!       call
!     & sum13742568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100, 1.000)
!       call
!     & sum34721568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100,-1.000)
!       call
!     & sum24731568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100, 1.000)
!       call
!     & sum23741568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100,-1.000)
!       call
!     & sum12834567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100,-1.000)
!       call
!     & sum12843567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100, 1.000)
!       call
!     & sum13824567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100, 1.000)
!       call
!     & sum14823567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100,-1.000)
!       call
!     & sum13842567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100,-1.000)
!       call
!     & sum14832567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100, 1.000)
!       call
!     & sum23814567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100,-1.000)
!       call
!     & sum24813567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100, 1.000)
!       call
!     & sum34812567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100,-1.000)
!       call
!     & sum23841567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100, 1.000)
!       call
!     & sum24831567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100,-1.000)
!       call
!     & sum34821567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100, 1.000)
!       call
!     & sum23714658(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100,-1.000)
!       call
!     & sum24713658(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100, 1.000)
!       call
!     & sum34712658(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100,-1.000)
!       call
!     & sum13724658(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100, 1.000)
!       call
!     & sum14723658(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100,-1.000)
!       call
!     & sum12734658(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100,-1.000)
!       call
!     & sum12743658(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100, 1.000)
!       call
!     & sum14732658(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100, 1.000)
!       call
!     & sum13742658(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100,-1.000)
!       call
!     & sum34721658(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100, 1.000)
!       call
!     & sum24731658(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100,-1.000)
!       call
!     & sum23741658(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100, 1.000)
!       call
!     & sum12834657(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100, 1.000)
!       call
!     & sum12843657(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100,-1.000)
!       call
!     & sum13824657(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100,-1.000)
!       call
!     & sum14823657(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100, 1.000)
!       call
!     & sum13842657(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100, 1.000)
!       call
!     & sum14832657(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100,-1.000)
!       call
!     & sum23814657(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100, 1.000)
!       call
!     & sum24813657(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100,-1.000)
!       call
!     & sum34812657(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100, 1.000)
!       call
!     & sum23841657(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100,-1.000)
!       call
!     & sum24831657(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100, 1.000)
!       call
!     & sum34821657(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100,-1.000)
!       call
!     & sum23514768(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100,-1.000)
!       call
!     & sum24513768(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100, 1.000)
!       call
!     & sum34512768(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100,-1.000)
!       call
!     & sum13524768(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100, 1.000)
!       call
!     & sum14523768(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100,-1.000)
!       call
!     & sum12534768(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100,-1.000)
!       call
!     & sum12543768(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100, 1.000)
!       call
!     & sum14532768(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100, 1.000)
!       call
!     & sum13542768(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100,-1.000)
!       call
!     & sum34521768(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100, 1.000)
!       call
!     & sum24531768(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100,-1.000)
!       call
!     & sum23541768(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100, 1.000)
!       call
!     & sum23614758(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100, 1.000)
!       call
!     & sum24613758(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100,-1.000)
!       call
!     & sum34612758(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100, 1.000)
!       call
!     & sum13624758(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100,-1.000)
!       call
!     & sum14623758(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100, 1.000)
!       call
!     & sum12634758(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100, 1.000)
!       call
!     & sum12643758(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100,-1.000)
!       call
!     & sum14632758(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100,-1.000)
!       call
!     & sum13642758(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100, 1.000)
!       call
!     & sum34621758(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100,-1.000)
!       call
!     & sum24631758(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100, 1.000)
!       call
!     & sum23641758(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100,-1.000)
!       call
!     & sum12834756(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100,-1.000)
!       call
!     & sum12843756(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100, 1.000)
!       call
!     & sum13824756(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100, 1.000)
!       call
!     & sum14823756(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100,-1.000)
!       call
!     & sum13842756(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100,-1.000)
!       call
!     & sum14832756(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100, 1.000)
!       call
!     & sum23814756(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100,-1.000)
!       call
!     & sum24813756(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100, 1.000)
!       call
!     & sum34812756(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100,-1.000)
!       call
!     & sum23841756(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100, 1.000)
!       call
!     & sum24831756(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100,-1.000)
!       call
!     & sum34821756(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100, 1.000)
!       call
!     & sum23514867(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100, 1.000)
!       call
!     & sum24513867(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100,-1.000)
!       call
!     & sum34512867(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100, 1.000)
!       call
!     & sum13524867(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100,-1.000)
!       call
!     & sum14523867(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100, 1.000)
!       call
!     & sum12534867(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100, 1.000)
!       call
!     & sum12543867(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100,-1.000)
!       call
!     & sum14532867(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100,-1.000)
!       call
!     & sum13542867(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100, 1.000)
!       call
!     & sum34521867(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100,-1.000)
!       call
!     & sum24531867(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100, 1.000)
!       call
!     & sum23541867(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100,-1.000)
!       call
!     & sum23614857(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100,-1.000)
!       call
!     & sum24613857(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100, 1.000)
!       call
!     & sum34612857(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100,-1.000)
!       call
!     & sum13624857(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100, 1.000)
!       call
!     & sum14623857(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100,-1.000)
!       call
!     & sum12634857(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100,-1.000)
!       call
!     & sum12643857(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100, 1.000)
!       call
!     & sum14632857(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100, 1.000)
!       call
!     & sum13642857(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100,-1.000)
!       call
!     & sum34621857(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100, 1.000)
!       call
!     & sum24631857(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100,-1.000)
!       call
!     & sum23641857(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100, 1.000)
!       call
!     & sum12734856(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100, 1.000)
!       call
!     & sum12743856(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100,-1.000)
!       call
!     & sum13724856(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100,-1.000)
!       call
!     & sum14723856(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100, 1.000)
!       call
!     & sum13742856(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100, 1.000)
!       call
!     & sum14732856(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100,-1.000)
!       call
!     & sum23714856(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100, 1.000)
!       call
!     & sum24713856(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100,-1.000)
!       call
!     & sum34712856(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100, 1.000)
!       call
!     & sum23741856(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100,-1.000)
!       call
!     & sum24731856(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100, 1.000)
!       call
!     & sum34721856(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z100,-1.000)
!c
!       do i=n0+1,n1-3
!       do j=i+1,n1-2
!       do k=j+1,n1-1
!       do l=k+1,n1
!       do a=n1+1,n3-3
!       do b=a+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)
!     & -z100(c,b,k,d,a,l,j,i)       ! 23614578 (-1.000)
!     & +z100(c,a,k,d,b,l,j,i)       ! 24613578 (+1.000)
!     & -z100(b,a,k,d,c,l,j,i)       ! 34612578 (-1.000)
!     & +z100(d,b,k,c,a,l,j,i)       ! 13624578 (+1.000)
!     & -z100(d,a,k,c,b,l,j,i)       ! 14623578 (-1.000)
!     & -z100(d,c,k,b,a,l,j,i)       ! 12634578 (-1.000)
!     & +z100(d,c,k,a,b,l,j,i)       ! 12643578 (+1.000)
!     & +z100(d,a,k,b,c,l,j,i)       ! 14632578 (+1.000)
!     & -z100(d,b,k,a,c,l,j,i)       ! 13642578 (-1.000)
!     & +z100(b,a,k,c,d,l,j,i)       ! 34621578 (+1.000)
!     & -z100(c,a,k,b,d,l,j,i)       ! 24631578 (-1.000)
!     & +z100(c,b,k,a,d,l,j,i)       ! 23641578 (+1.000)
!     & +z100(c,b,l,d,a,k,j,i)       ! 23514678 (+1.000)
!     & -z100(c,a,l,d,b,k,j,i)       ! 24513678 (-1.000)
!     & +z100(b,a,l,d,c,k,j,i)       ! 34512678 (+1.000)
!     & -z100(d,b,l,c,a,k,j,i)       ! 13524678 (-1.000)
!     & +z100(d,a,l,c,b,k,j,i)       ! 14523678 (+1.000)
!     & +z100(d,c,l,b,a,k,j,i)       ! 12534678 (+1.000)
!     & -z100(d,c,l,a,b,k,j,i)       ! 12543678 (-1.000)
!     & -z100(d,a,l,b,c,k,j,i)       ! 14532678 (-1.000)
!     & +z100(d,b,l,a,c,k,j,i)       ! 13542678 (+1.000)
!     & -z100(b,a,l,c,d,k,j,i)       ! 34521678 (-1.000)
!     & +z100(c,a,l,b,d,k,j,i)       ! 24531678 (+1.000)
!     & -z100(c,b,l,a,d,k,j,i)       ! 23541678 (-1.000)
!     & +z100(c,b,j,d,a,l,k,i)       ! 23714568 (+1.000)
!     & -z100(c,a,j,d,b,l,k,i)       ! 24713568 (-1.000)
!     & +z100(b,a,j,d,c,l,k,i)       ! 34712568 (+1.000)
!     & -z100(d,b,j,c,a,l,k,i)       ! 13724568 (-1.000)
!     & +z100(d,a,j,c,b,l,k,i)       ! 14723568 (+1.000)
!     & +z100(d,c,j,b,a,l,k,i)       ! 12734568 (+1.000)
!     & -z100(d,c,j,a,b,l,k,i)       ! 12743568 (-1.000)
!     & -z100(d,a,j,b,c,l,k,i)       ! 14732568 (-1.000)
!     & +z100(d,b,j,a,c,l,k,i)       ! 13742568 (+1.000)
!     & -z100(b,a,j,c,d,l,k,i)       ! 34721568 (-1.000)
!     & +z100(c,a,j,b,d,l,k,i)       ! 24731568 (+1.000)
!     & -z100(c,b,j,a,d,l,k,i)       ! 23741568 (-1.000)
!     & -z100(d,c,i,b,a,l,k,j)       ! 12834567 (-1.000)
!     & +z100(d,c,i,a,b,l,k,j)       ! 12843567 (+1.000)
!     & +z100(d,b,i,c,a,l,k,j)       ! 13824567 (+1.000)
!     & -z100(d,a,i,c,b,l,k,j)       ! 14823567 (-1.000)
!     & -z100(d,b,i,a,c,l,k,j)       ! 13842567 (-1.000)
!     & +z100(d,a,i,b,c,l,k,j)       ! 14832567 (+1.000)
!     & -z100(c,b,i,d,a,l,k,j)       ! 23814567 (-1.000)
!     & +z100(c,a,i,d,b,l,k,j)       ! 24813567 (+1.000)
!     & -z100(b,a,i,d,c,l,k,j)       ! 34812567 (-1.000)
!     & +z100(c,b,i,a,d,l,k,j)       ! 23841567 (+1.000)
!     & -z100(c,a,i,b,d,l,k,j)       ! 24831567 (-1.000)
!     & +z100(b,a,i,c,d,l,k,j)       ! 34821567 (+1.000)
!     & -z100(c,b,j,d,a,k,l,i)       ! 23714658 (-1.000)
!     & +z100(c,a,j,d,b,k,l,i)       ! 24713658 (+1.000)
!     & -z100(b,a,j,d,c,k,l,i)       ! 34712658 (-1.000)
!     & +z100(d,b,j,c,a,k,l,i)       ! 13724658 (+1.000)
!     & -z100(d,a,j,c,b,k,l,i)       ! 14723658 (-1.000)
!     & -z100(d,c,j,b,a,k,l,i)       ! 12734658 (-1.000)
!     & +z100(d,c,j,a,b,k,l,i)       ! 12743658 (+1.000)
!     & +z100(d,a,j,b,c,k,l,i)       ! 14732658 (+1.000)
!     & -z100(d,b,j,a,c,k,l,i)       ! 13742658 (-1.000)
!     & +z100(b,a,j,c,d,k,l,i)       ! 34721658 (+1.000)
!     & -z100(c,a,j,b,d,k,l,i)       ! 24731658 (-1.000)
!     & +z100(c,b,j,a,d,k,l,i)       ! 23741658 (+1.000)
!     & +z100(d,c,i,b,a,k,l,j)       ! 12834657 (+1.000)
!     & -z100(d,c,i,a,b,k,l,j)       ! 12843657 (-1.000)
!     & -z100(d,b,i,c,a,k,l,j)       ! 13824657 (-1.000)
!     & +z100(d,a,i,c,b,k,l,j)       ! 14823657 (+1.000)
!     & +z100(d,b,i,a,c,k,l,j)       ! 13842657 (+1.000)
!     & -z100(d,a,i,b,c,k,l,j)       ! 14832657 (-1.000)
!     & +z100(c,b,i,d,a,k,l,j)       ! 23814657 (+1.000)
!     & -z100(c,a,i,d,b,k,l,j)       ! 24813657 (-1.000)
!     & +z100(b,a,i,d,c,k,l,j)       ! 34812657 (+1.000)
!     & -z100(c,b,i,a,d,k,l,j)       ! 23841657 (-1.000)
!     & +z100(c,a,i,b,d,k,l,j)       ! 24831657 (+1.000)
!     & -z100(b,a,i,c,d,k,l,j)       ! 34821657 (-1.000)
!     & -z100(c,b,l,d,a,j,k,i)       ! 23514768 (-1.000)
!     & +z100(c,a,l,d,b,j,k,i)       ! 24513768 (+1.000)
!     & -z100(b,a,l,d,c,j,k,i)       ! 34512768 (-1.000)
!     & +z100(d,b,l,c,a,j,k,i)       ! 13524768 (+1.000)
!     & -z100(d,a,l,c,b,j,k,i)       ! 14523768 (-1.000)
!     & -z100(d,c,l,b,a,j,k,i)       ! 12534768 (-1.000)
!     & +z100(d,c,l,a,b,j,k,i)       ! 12543768 (+1.000)
!     & +z100(d,a,l,b,c,j,k,i)       ! 14532768 (+1.000)
!     & -z100(d,b,l,a,c,j,k,i)       ! 13542768 (-1.000)
!     & +z100(b,a,l,c,d,j,k,i)       ! 34521768 (+1.000)
!     & -z100(c,a,l,b,d,j,k,i)       ! 24531768 (-1.000)
!     & +z100(c,b,l,a,d,j,k,i)       ! 23541768 (+1.000)
!     & +z100(c,b,k,d,a,j,l,i)       ! 23614758 (+1.000)
!     & -z100(c,a,k,d,b,j,l,i)       ! 24613758 (-1.000)
!     & +z100(b,a,k,d,c,j,l,i)       ! 34612758 (+1.000)
!     & -z100(d,b,k,c,a,j,l,i)       ! 13624758 (-1.000)
!     & +z100(d,a,k,c,b,j,l,i)       ! 14623758 (+1.000)
!     & +z100(d,c,k,b,a,j,l,i)       ! 12634758 (+1.000)
!     & -z100(d,c,k,a,b,j,l,i)       ! 12643758 (-1.000)
!     & -z100(d,a,k,b,c,j,l,i)       ! 14632758 (-1.000)
!     & +z100(d,b,k,a,c,j,l,i)       ! 13642758 (+1.000)
!     & -z100(b,a,k,c,d,j,l,i)       ! 34621758 (-1.000)
!     & +z100(c,a,k,b,d,j,l,i)       ! 24631758 (+1.000)
!     & -z100(c,b,k,a,d,j,l,i)       ! 23641758 (-1.000)
!     & -z100(d,c,i,b,a,j,l,k)       ! 12834756 (-1.000)
!     & +z100(d,c,i,a,b,j,l,k)       ! 12843756 (+1.000)
!     & +z100(d,b,i,c,a,j,l,k)       ! 13824756 (+1.000)
!     & -z100(d,a,i,c,b,j,l,k)       ! 14823756 (-1.000)
!     & -z100(d,b,i,a,c,j,l,k)       ! 13842756 (-1.000)
!     & +z100(d,a,i,b,c,j,l,k)       ! 14832756 (+1.000)
!     & -z100(c,b,i,d,a,j,l,k)       ! 23814756 (-1.000)
!     & +z100(c,a,i,d,b,j,l,k)       ! 24813756 (+1.000)
!     & -z100(b,a,i,d,c,j,l,k)       ! 34812756 (-1.000)
!     & +z100(c,b,i,a,d,j,l,k)       ! 23841756 (+1.000)
!     & -z100(c,a,i,b,d,j,l,k)       ! 24831756 (-1.000)
!     & +z100(b,a,i,c,d,j,l,k)       ! 34821756 (+1.000)
!     & +z100(c,b,l,d,a,i,k,j)       ! 23514867 (+1.000)
!     & -z100(c,a,l,d,b,i,k,j)       ! 24513867 (-1.000)
!     & +z100(b,a,l,d,c,i,k,j)       ! 34512867 (+1.000)
!     & -z100(d,b,l,c,a,i,k,j)       ! 13524867 (-1.000)
!     & +z100(d,a,l,c,b,i,k,j)       ! 14523867 (+1.000)
!     & +z100(d,c,l,b,a,i,k,j)       ! 12534867 (+1.000)
!     & -z100(d,c,l,a,b,i,k,j)       ! 12543867 (-1.000)
!     & -z100(d,a,l,b,c,i,k,j)       ! 14532867 (-1.000)
!     & +z100(d,b,l,a,c,i,k,j)       ! 13542867 (+1.000)
!     & -z100(b,a,l,c,d,i,k,j)       ! 34521867 (-1.000)
!     & +z100(c,a,l,b,d,i,k,j)       ! 24531867 (+1.000)
!     & -z100(c,b,l,a,d,i,k,j)       ! 23541867 (-1.000)
!     & -z100(c,b,k,d,a,i,l,j)       ! 23614857 (-1.000)
!     & +z100(c,a,k,d,b,i,l,j)       ! 24613857 (+1.000)
!     & -z100(b,a,k,d,c,i,l,j)       ! 34612857 (-1.000)
!     & +z100(d,b,k,c,a,i,l,j)       ! 13624857 (+1.000)
!     & -z100(d,a,k,c,b,i,l,j)       ! 14623857 (-1.000)
!     & -z100(d,c,k,b,a,i,l,j)       ! 12634857 (-1.000)
!     & +z100(d,c,k,a,b,i,l,j)       ! 12643857 (+1.000)
!     & +z100(d,a,k,b,c,i,l,j)       ! 14632857 (+1.000)
!     & -z100(d,b,k,a,c,i,l,j)       ! 13642857 (-1.000)
!     & +z100(b,a,k,c,d,i,l,j)       ! 34621857 (+1.000)
!     & -z100(c,a,k,b,d,i,l,j)       ! 24631857 (-1.000)
!     & +z100(c,b,k,a,d,i,l,j)       ! 23641857 (+1.000)
!     & +z100(d,c,j,b,a,i,l,k)       ! 12734856 (+1.000)
!     & -z100(d,c,j,a,b,i,l,k)       ! 12743856 (-1.000)
!     & -z100(d,b,j,c,a,i,l,k)       ! 13724856 (-1.000)
!     & +z100(d,a,j,c,b,i,l,k)       ! 14723856 (+1.000)
!     & +z100(d,b,j,a,c,i,l,k)       ! 13742856 (+1.000)
!     & -z100(d,a,j,b,c,i,l,k)       ! 14732856 (-1.000)
!     & +z100(c,b,j,d,a,i,l,k)       ! 23714856 (+1.000)
!     & -z100(c,a,j,d,b,i,l,k)       ! 24713856 (-1.000)
!     & +z100(b,a,j,d,c,i,l,k)       ! 34712856 (+1.000)
!     & -z100(c,b,j,a,d,i,l,k)       ! 23741856 (-1.000)
!     & +z100(c,a,j,b,d,i,l,k)       ! 24731856 (+1.000)
!     & -z100(b,a,j,c,d,i,l,k)       ! 34721856 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z100)
       deallocate(u32)
c
c
       allocate(f1(n2+1:n3,n0+1:n2,n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder465123(n1,n3,n0,n1,n0,n1,n2,n3,n0,n1,n0,n2,
     & n2,n3,n0,n2,n0,n1,n1,n3,n0,n1,n0,n1,u12,f1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s48(n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       i1=k1*k1*k3*k1
       i3=k2*k4
       call egemm1(i1,i3,f1,b2,s48)
       deallocate(f1)
       deallocate(b2)
c
       x1=x1+s48
       deallocate(s48)
c
       allocate(f1(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n2))
       call reorder541236(n1,n3,n0,n1,n0,n1,n2,n3,n0,n1,n0,n2,
     & n0,n1,n2,n3,n1,n3,n0,n1,n0,n1,n0,n2,u12,f1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(u27(n1+1:n3,n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n2))
       i1=k2*k1*k1*k3*k4
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,f1,b2,u27)
       deallocate(f1)
       deallocate(b2)
       deallocate(u12)
c
!       allocate(f1(n0+1:n2,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!       call reorder621345(n1,n3,n2,n3,n1,n3,n0,n1,n0,n1,n0,n2,
!     & n0,n2,n2,n3,n1,n3,n1,n3,n0,n1,n0,n1,u27,f1)
!       allocate(f2(n0+1:n2,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!       call reorder412356(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
!     & n0,n2,n2,n3,n1,n3,n1,n3,n0,n1,n0,n1,t3b,f2)
!       allocate(z91(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!       i1=k1*k1*k3*k3
!       i2=k1*k1*k3*k3
!       i3=k4*k2
!       call egemm(i1,i2,i3,f1,f2,z91)
!       deallocate(f1)
!       deallocate(f2)
c
       do i=n0+1,n1-3;do j=i+1,n1-2;do k=j+1,n1-1;do l=k+1,n1
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-3;do b=a+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
          if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n2+1,n3;do n=n0+1,n2
             sum=sum
     &     - u27(d,f,c,j,i,n)*t3b(f,b,a,n,l,k)      !dfcjinfbanlk    (-1.000)
     &     + u27(d,f,b,j,i,n)*t3b(f,c,a,n,l,k)      !dfbjinfcanlk    (+1.000)
     &     - u27(d,f,a,j,i,n)*t3b(f,c,b,n,l,k)      !dfajinfcbnlk    (-1.000)
     &     + u27(c,f,d,j,i,n)*t3b(f,b,a,n,l,k)      !cfdjinfbanlk    (+1.000)
     &     - u27(b,f,d,j,i,n)*t3b(f,c,a,n,l,k)      !bfdjinfcanlk    (-1.000)
     &     + u27(a,f,d,j,i,n)*t3b(f,c,b,n,l,k)      !afdjinfcbnlk    (+1.000)
     &     - u27(c,f,b,j,i,n)*t3b(f,d,a,n,l,k)      !cfbjinfdanlk    (-1.000)
     &     + u27(c,f,a,j,i,n)*t3b(f,d,b,n,l,k)      !cfajinfdbnlk    (+1.000)
     &     + u27(b,f,c,j,i,n)*t3b(f,d,a,n,l,k)      !bfcjinfdanlk    (+1.000)
     &     - u27(a,f,c,j,i,n)*t3b(f,d,b,n,l,k)      !afcjinfdbnlk    (-1.000)
     &     - u27(b,f,a,j,i,n)*t3b(f,d,c,n,l,k)      !bfajinfdcnlk    (-1.000)
     &     + u27(a,f,b,j,i,n)*t3b(f,d,c,n,l,k)      !afbjinfdcnlk    (+1.000)
     &     + u27(d,f,c,k,i,n)*t3b(f,b,a,n,l,j)      !dfckinfbanlj    (+1.000)
     &     - u27(d,f,b,k,i,n)*t3b(f,c,a,n,l,j)      !dfbkinfcanlj    (-1.000)
     &     + u27(d,f,a,k,i,n)*t3b(f,c,b,n,l,j)      !dfakinfcbnlj    (+1.000)
     &     - u27(c,f,d,k,i,n)*t3b(f,b,a,n,l,j)      !cfdkinfbanlj    (-1.000)
     &     + u27(b,f,d,k,i,n)*t3b(f,c,a,n,l,j)      !bfdkinfcanlj    (+1.000)
     &     - u27(a,f,d,k,i,n)*t3b(f,c,b,n,l,j)      !afdkinfcbnlj    (-1.000)
     &     + u27(c,f,b,k,i,n)*t3b(f,d,a,n,l,j)      !cfbkinfdanlj    (+1.000)
     &     - u27(c,f,a,k,i,n)*t3b(f,d,b,n,l,j)      !cfakinfdbnlj    (-1.000)
     &     - u27(b,f,c,k,i,n)*t3b(f,d,a,n,l,j)      !bfckinfdanlj    (-1.000)
     &     + u27(a,f,c,k,i,n)*t3b(f,d,b,n,l,j)      !afckinfdbnlj    (+1.000)
     &     + u27(b,f,a,k,i,n)*t3b(f,d,c,n,l,j)      !bfakinfdcnlj    (+1.000)
     &     - u27(a,f,b,k,i,n)*t3b(f,d,c,n,l,j)      !afbkinfdcnlj    (-1.000)
     &     - u27(d,f,c,l,i,n)*t3b(f,b,a,n,k,j)      !dfclinfbankj    (-1.000)
     &     + u27(d,f,b,l,i,n)*t3b(f,c,a,n,k,j)      !dfblinfcankj    (+1.000)
     &     - u27(d,f,a,l,i,n)*t3b(f,c,b,n,k,j)      !dfalinfcbnkj    (-1.000)
     &     + u27(c,f,d,l,i,n)*t3b(f,b,a,n,k,j)      !cfdlinfbankj    (+1.000)
     &     - u27(b,f,d,l,i,n)*t3b(f,c,a,n,k,j)      !bfdlinfcankj    (-1.000)
     &     + u27(a,f,d,l,i,n)*t3b(f,c,b,n,k,j)      !afdlinfcbnkj    (+1.000)
     &     - u27(c,f,b,l,i,n)*t3b(f,d,a,n,k,j)      !cfblinfdankj    (-1.000)
     &     + u27(c,f,a,l,i,n)*t3b(f,d,b,n,k,j)      !cfalinfdbnkj    (+1.000)
     &     + u27(b,f,c,l,i,n)*t3b(f,d,a,n,k,j)      !bfclinfdankj    (+1.000)
     &     - u27(a,f,c,l,i,n)*t3b(f,d,b,n,k,j)      !afclinfdbnkj    (-1.000)
     &     - u27(b,f,a,l,i,n)*t3b(f,d,c,n,k,j)      !bfalinfdcnkj    (-1.000)
     &     + u27(a,f,b,l,i,n)*t3b(f,d,c,n,k,j)      !afblinfdcnkj    (+1.000)
     &     - u27(d,f,c,k,j,n)*t3b(f,b,a,n,l,i)      !dfckjnfbanli    (-1.000)
     &     + u27(d,f,b,k,j,n)*t3b(f,c,a,n,l,i)      !dfbkjnfcanli    (+1.000)
     &     - u27(d,f,a,k,j,n)*t3b(f,c,b,n,l,i)      !dfakjnfcbnli    (-1.000)
     &     + u27(c,f,d,k,j,n)*t3b(f,b,a,n,l,i)      !cfdkjnfbanli    (+1.000)
     &     - u27(b,f,d,k,j,n)*t3b(f,c,a,n,l,i)      !bfdkjnfcanli    (-1.000)
     &     + u27(a,f,d,k,j,n)*t3b(f,c,b,n,l,i)      !afdkjnfcbnli    (+1.000)
     &     - u27(c,f,b,k,j,n)*t3b(f,d,a,n,l,i)      !cfbkjnfdanli    (-1.000)
     &     + u27(c,f,a,k,j,n)*t3b(f,d,b,n,l,i)      !cfakjnfdbnli    (+1.000)
     &     + u27(b,f,c,k,j,n)*t3b(f,d,a,n,l,i)      !bfckjnfdanli    (+1.000)
     &     - u27(a,f,c,k,j,n)*t3b(f,d,b,n,l,i)      !afckjnfdbnli    (-1.000)
     &     - u27(b,f,a,k,j,n)*t3b(f,d,c,n,l,i)      !bfakjnfdcnli    (-1.000)
     &     + u27(a,f,b,k,j,n)*t3b(f,d,c,n,l,i)      !afbkjnfdcnli    (+1.000)
     &     + u27(d,f,c,l,j,n)*t3b(f,b,a,n,k,i)      !dfcljnfbanki    (+1.000)
     &     - u27(d,f,b,l,j,n)*t3b(f,c,a,n,k,i)      !dfbljnfcanki    (-1.000)
     &     + u27(d,f,a,l,j,n)*t3b(f,c,b,n,k,i)      !dfaljnfcbnki    (+1.000)
     &     - u27(c,f,d,l,j,n)*t3b(f,b,a,n,k,i)      !cfdljnfbanki    (-1.000)
     &     + u27(b,f,d,l,j,n)*t3b(f,c,a,n,k,i)      !bfdljnfcanki    (+1.000)
     &     - u27(a,f,d,l,j,n)*t3b(f,c,b,n,k,i)      !afdljnfcbnki    (-1.000)
     &     + u27(c,f,b,l,j,n)*t3b(f,d,a,n,k,i)      !cfbljnfdanki    (+1.000)
     &     - u27(c,f,a,l,j,n)*t3b(f,d,b,n,k,i)      !cfaljnfdbnki    (-1.000)
     &     - u27(b,f,c,l,j,n)*t3b(f,d,a,n,k,i)      !bfcljnfdanki    (-1.000)
     &     + u27(a,f,c,l,j,n)*t3b(f,d,b,n,k,i)      !afcljnfdbnki    (+1.000)
     &     + u27(b,f,a,l,j,n)*t3b(f,d,c,n,k,i)      !bfaljnfdcnki    (+1.000)
     &     - u27(a,f,b,l,j,n)*t3b(f,d,c,n,k,i)      !afbljnfdcnki    (-1.000)
     &     - u27(d,f,c,l,k,n)*t3b(f,b,a,n,j,i)      !dfclknfbanji    (-1.000)
     &     + u27(d,f,b,l,k,n)*t3b(f,c,a,n,j,i)      !dfblknfcanji    (+1.000)
     &     - u27(d,f,a,l,k,n)*t3b(f,c,b,n,j,i)      !dfalknfcbnji    (-1.000)
     &     + u27(c,f,d,l,k,n)*t3b(f,b,a,n,j,i)      !cfdlknfbanji    (+1.000)
     &     - u27(b,f,d,l,k,n)*t3b(f,c,a,n,j,i)      !bfdlknfcanji    (-1.000)
     &     + u27(a,f,d,l,k,n)*t3b(f,c,b,n,j,i)      !afdlknfcbnji    (+1.000)
     &     - u27(c,f,b,l,k,n)*t3b(f,d,a,n,j,i)      !cfblknfdanji    (-1.000)
     &     + u27(c,f,a,l,k,n)*t3b(f,d,b,n,j,i)      !cfalknfdbnji    (+1.000)
     &     + u27(b,f,c,l,k,n)*t3b(f,d,a,n,j,i)      !bfclknfdanji    (+1.000)
     &     - u27(a,f,c,l,k,n)*t3b(f,d,b,n,j,i)      !afclknfdbnji    (-1.000)
     &     - u27(b,f,a,l,k,n)*t3b(f,d,c,n,j,i)      !bfalknfdcnji    (-1.000)
     &     + u27(a,f,b,l,k,n)*t3b(f,d,c,n,j,i)      !afblknfdcnji    (+1.000)       
             enddo;enddo
             v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call
!     & sum34561278(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z91,-1.000)
!       call
!     & sum24561378(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z91, 1.000)
!       call
!     & sum23561478(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z91,-1.000)
!       call
!     & sum34562178(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z91, 1.000)
!       call
!     & sum24563178(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z91,-1.000)
!       call
!     & sum23564178(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z91, 1.000)
!       call
!     & sum14562378(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z91,-1.000)
!       call
!     & sum13562478(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z91, 1.000)
!       call
!     & sum14563278(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z91, 1.000)
!       call
!     & sum13564278(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z91,-1.000)
!       call
!     & sum12563478(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z91,-1.000)
!       call
!     & sum12564378(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z91, 1.000)
!       call
!     & sum34571268(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z91, 1.000)
!       call
!     & sum24571368(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z91,-1.000)
!       call
!     & sum23571468(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z91, 1.000)
!       call
!     & sum34572168(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z91,-1.000)
!       call
!     & sum24573168(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z91, 1.000)
!       call
!     & sum23574168(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z91,-1.000)
!       call
!     & sum14572368(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z91, 1.000)
!       call
!     & sum13572468(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z91,-1.000)
!       call
!     & sum14573268(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z91,-1.000)
!       call
!     & sum13574268(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z91, 1.000)
!       call
!     & sum12573468(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z91, 1.000)
!       call
!     & sum12574368(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z91,-1.000)
!       call
!     & sum34671258(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z91,-1.000)
!       call
!     & sum24671358(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z91, 1.000)
!       call
!     & sum23671458(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z91,-1.000)
!       call
!     & sum34672158(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z91, 1.000)
!       call
!     & sum24673158(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z91,-1.000)
!       call
!     & sum23674158(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z91, 1.000)
!       call
!     & sum14672358(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z91,-1.000)
!       call
!     & sum13672458(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z91, 1.000)
!       call
!     & sum14673258(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z91, 1.000)
!       call
!     & sum13674258(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z91,-1.000)
!       call
!     & sum12673458(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z91,-1.000)
!       call
!     & sum12674358(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z91, 1.000)
!       call
!     & sum34581267(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z91,-1.000)
!       call
!     & sum24581367(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z91, 1.000)
!       call
!     & sum23581467(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z91,-1.000)
!       call
!     & sum34582167(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z91, 1.000)
!       call
!     & sum24583167(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z91,-1.000)
!       call
!     & sum23584167(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z91, 1.000)
!       call
!     & sum14582367(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z91,-1.000)
!       call
!     & sum13582467(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z91, 1.000)
!       call
!     & sum14583267(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z91, 1.000)
!       call
!     & sum13584267(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z91,-1.000)
!       call
!     & sum12583467(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z91,-1.000)
!       call
!     & sum12584367(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z91, 1.000)
!       call
!     & sum34681257(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z91, 1.000)
!       call
!     & sum24681357(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z91,-1.000)
!       call
!     & sum23681457(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z91, 1.000)
!       call
!     & sum34682157(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z91,-1.000)
!       call
!     & sum24683157(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z91, 1.000)
!       call
!     & sum23684157(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z91,-1.000)
!       call
!     & sum14682357(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z91, 1.000)
!       call
!     & sum13682457(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z91,-1.000)
!       call
!     & sum14683257(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z91,-1.000)
!       call
!     & sum13684257(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z91, 1.000)
!       call
!     & sum12683457(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z91, 1.000)
!       call
!     & sum12684357(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z91,-1.000)
!       call
!     & sum34781256(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z91,-1.000)
!       call
!     & sum24781356(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z91, 1.000)
!       call
!     & sum23781456(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z91,-1.000)
!       call
!     & sum34782156(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z91, 1.000)
!       call
!     & sum24783156(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z91,-1.000)
!       call
!     & sum23784156(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z91, 1.000)
!       call
!     & sum14782356(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z91,-1.000)
!       call
!     & sum13782456(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z91, 1.000)
!       call
!     & sum14783256(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z91, 1.000)
!       call
!     & sum13784256(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z91,-1.000)
!       call
!     & sum12783456(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z91,-1.000)
!       call
!     & sum12784356(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z91, 1.000)
!c
!       do i=n0+1,n1-3
!       do j=i+1,n1-2
!       do k=j+1,n1-1
!       do l=k+1,n1
!       do a=n1+1,n3-3
!       do b=a+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)
!     & -z91(b,a,l,k,d,c,j,i)       ! 34561278 (-1.000)
!     & +z91(c,a,l,k,d,b,j,i)       ! 24561378 (+1.000)
!     & -z91(c,b,l,k,d,a,j,i)       ! 23561478 (-1.000)
!     & +z91(b,a,l,k,c,d,j,i)       ! 34562178 (+1.000)
!     & -z91(c,a,l,k,b,d,j,i)       ! 24563178 (-1.000)
!     & +z91(c,b,l,k,a,d,j,i)       ! 23564178 (+1.000)
!     & -z91(d,a,l,k,c,b,j,i)       ! 14562378 (-1.000)
!     & +z91(d,b,l,k,c,a,j,i)       ! 13562478 (+1.000)
!     & +z91(d,a,l,k,b,c,j,i)       ! 14563278 (+1.000)
!     & -z91(d,b,l,k,a,c,j,i)       ! 13564278 (-1.000)
!     & -z91(d,c,l,k,b,a,j,i)       ! 12563478 (-1.000)
!     & +z91(d,c,l,k,a,b,j,i)       ! 12564378 (+1.000)
!     & +z91(b,a,l,j,d,c,k,i)       ! 34571268 (+1.000)
!     & -z91(c,a,l,j,d,b,k,i)       ! 24571368 (-1.000)
!     & +z91(c,b,l,j,d,a,k,i)       ! 23571468 (+1.000)
!     & -z91(b,a,l,j,c,d,k,i)       ! 34572168 (-1.000)
!     & +z91(c,a,l,j,b,d,k,i)       ! 24573168 (+1.000)
!     & -z91(c,b,l,j,a,d,k,i)       ! 23574168 (-1.000)
!     & +z91(d,a,l,j,c,b,k,i)       ! 14572368 (+1.000)
!     & -z91(d,b,l,j,c,a,k,i)       ! 13572468 (-1.000)
!     & -z91(d,a,l,j,b,c,k,i)       ! 14573268 (-1.000)
!     & +z91(d,b,l,j,a,c,k,i)       ! 13574268 (+1.000)
!     & +z91(d,c,l,j,b,a,k,i)       ! 12573468 (+1.000)
!     & -z91(d,c,l,j,a,b,k,i)       ! 12574368 (-1.000)
!     & -z91(b,a,k,j,d,c,l,i)       ! 34671258 (-1.000)
!     & +z91(c,a,k,j,d,b,l,i)       ! 24671358 (+1.000)
!     & -z91(c,b,k,j,d,a,l,i)       ! 23671458 (-1.000)
!     & +z91(b,a,k,j,c,d,l,i)       ! 34672158 (+1.000)
!     & -z91(c,a,k,j,b,d,l,i)       ! 24673158 (-1.000)
!     & +z91(c,b,k,j,a,d,l,i)       ! 23674158 (+1.000)
!     & -z91(d,a,k,j,c,b,l,i)       ! 14672358 (-1.000)
!     & +z91(d,b,k,j,c,a,l,i)       ! 13672458 (+1.000)
!     & +z91(d,a,k,j,b,c,l,i)       ! 14673258 (+1.000)
!     & -z91(d,b,k,j,a,c,l,i)       ! 13674258 (-1.000)
!     & -z91(d,c,k,j,b,a,l,i)       ! 12673458 (-1.000)
!     & +z91(d,c,k,j,a,b,l,i)       ! 12674358 (+1.000)
!     & -z91(b,a,l,i,d,c,k,j)       ! 34581267 (-1.000)
!     & +z91(c,a,l,i,d,b,k,j)       ! 24581367 (+1.000)
!     & -z91(c,b,l,i,d,a,k,j)       ! 23581467 (-1.000)
!     & +z91(b,a,l,i,c,d,k,j)       ! 34582167 (+1.000)
!     & -z91(c,a,l,i,b,d,k,j)       ! 24583167 (-1.000)
!     & +z91(c,b,l,i,a,d,k,j)       ! 23584167 (+1.000)
!     & -z91(d,a,l,i,c,b,k,j)       ! 14582367 (-1.000)
!     & +z91(d,b,l,i,c,a,k,j)       ! 13582467 (+1.000)
!     & +z91(d,a,l,i,b,c,k,j)       ! 14583267 (+1.000)
!     & -z91(d,b,l,i,a,c,k,j)       ! 13584267 (-1.000)
!     & -z91(d,c,l,i,b,a,k,j)       ! 12583467 (-1.000)
!     & +z91(d,c,l,i,a,b,k,j)       ! 12584367 (+1.000)
!     & +z91(b,a,k,i,d,c,l,j)       ! 34681257 (+1.000)
!     & -z91(c,a,k,i,d,b,l,j)       ! 24681357 (-1.000)
!     & +z91(c,b,k,i,d,a,l,j)       ! 23681457 (+1.000)
!     & -z91(b,a,k,i,c,d,l,j)       ! 34682157 (-1.000)
!     & +z91(c,a,k,i,b,d,l,j)       ! 24683157 (+1.000)
!     & -z91(c,b,k,i,a,d,l,j)       ! 23684157 (-1.000)
!     & +z91(d,a,k,i,c,b,l,j)       ! 14682357 (+1.000)
!     & -z91(d,b,k,i,c,a,l,j)       ! 13682457 (-1.000)
!     & -z91(d,a,k,i,b,c,l,j)       ! 14683257 (-1.000)
!     & +z91(d,b,k,i,a,c,l,j)       ! 13684257 (+1.000)
!     & +z91(d,c,k,i,b,a,l,j)       ! 12683457 (+1.000)
!     & -z91(d,c,k,i,a,b,l,j)       ! 12684357 (-1.000)
!     & -z91(b,a,j,i,d,c,l,k)       ! 34781256 (-1.000)
!     & +z91(c,a,j,i,d,b,l,k)       ! 24781356 (+1.000)
!     & -z91(c,b,j,i,d,a,l,k)       ! 23781456 (-1.000)
!     & +z91(b,a,j,i,c,d,l,k)       ! 34782156 (+1.000)
!     & -z91(c,a,j,i,b,d,l,k)       ! 24783156 (-1.000)
!     & +z91(c,b,j,i,a,d,l,k)       ! 23784156 (+1.000)
!     & -z91(d,a,j,i,c,b,l,k)       ! 14782356 (-1.000)
!     & +z91(d,b,j,i,c,a,l,k)       ! 13782456 (+1.000)
!     & +z91(d,a,j,i,b,c,l,k)       ! 14783256 (+1.000)
!     & -z91(d,b,j,i,a,c,l,k)       ! 13784256 (-1.000)
!     & -z91(d,c,j,i,b,a,l,k)       ! 12783456 (-1.000)
!     & +z91(d,c,j,i,a,b,l,k)       ! 12784356 (+1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z91)
       deallocate(u27)
c
       allocate(d1(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder4321(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n0,n2,n0,n1,intm,d1)
       allocate(h2(n2+1:n3,n1+1:n3,n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n1,
     & n0+1:n1,n0+1:n1))
       call reorder12534678(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n0,n1,
     & n2,n3,n1,n3,n0,n2,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,t4b,h2)
       allocate(u13(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1
       i2=k1*k1*k1*k3*k3
       i3=k2*k3*k4
       call egemm(i1,i2,i3,d1,h2,u13)
       deallocate(d1)
       deallocate(h2)
c
!       allocate(f1(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
!       call reorder612345(n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,u13,f1)
!       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
!       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
!       allocate(z48(n1+1:n3,n1+1:n3,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
!       i1=k1*k1*k1*k3*k3
!       i2=k1*k3*k3
!       i3=k1
!       call egemm(i1,i2,i3,f1,d2,z48)
!       deallocate(f1)
!       deallocate(d2)
c
       do i=n0+1,n1-3;do j=i+1,n1-2;do k=j+1,n1-1;do l=k+1,n1
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-3;do b=a+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
          if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1
             sum=sum
     &     - u13(b,a,l,k,j,m)*t2a(d,c,m,i)          !balkjmdcmi      (-1.000)
     &     + u13(c,a,l,k,j,m)*t2a(d,b,m,i)          !calkjmdbmi      (+1.000)
     &     - u13(c,b,l,k,j,m)*t2a(d,a,m,i)          !cblkjmdami      (-1.000)
     &     - u13(d,a,l,k,j,m)*t2a(c,b,m,i)          !dalkjmcbmi      (-1.000)
     &     + u13(d,b,l,k,j,m)*t2a(c,a,m,i)          !dblkjmcami      (+1.000)
     &     - u13(d,c,l,k,j,m)*t2a(b,a,m,i)          !dclkjmbami      (-1.000)
     &     + u13(b,a,l,k,i,m)*t2a(d,c,m,j)          !balkimdcmj      (+1.000)
     &     - u13(c,a,l,k,i,m)*t2a(d,b,m,j)          !calkimdbmj      (-1.000)
     &     + u13(c,b,l,k,i,m)*t2a(d,a,m,j)          !cblkimdamj      (+1.000)
     &     + u13(d,a,l,k,i,m)*t2a(c,b,m,j)          !dalkimcbmj      (+1.000)
     &     - u13(d,b,l,k,i,m)*t2a(c,a,m,j)          !dblkimcamj      (-1.000)
     &     + u13(d,c,l,k,i,m)*t2a(b,a,m,j)          !dclkimbamj      (+1.000)
     &     - u13(b,a,l,j,i,m)*t2a(d,c,m,k)          !baljimdcmk      (-1.000)
     &     + u13(c,a,l,j,i,m)*t2a(d,b,m,k)          !caljimdbmk      (+1.000)
     &     - u13(c,b,l,j,i,m)*t2a(d,a,m,k)          !cbljimdamk      (-1.000)
     &     - u13(d,a,l,j,i,m)*t2a(c,b,m,k)          !daljimcbmk      (-1.000)
     &     + u13(d,b,l,j,i,m)*t2a(c,a,m,k)          !dbljimcamk      (+1.000)
     &     - u13(d,c,l,j,i,m)*t2a(b,a,m,k)          !dcljimbamk      (-1.000)
     &     + u13(b,a,k,j,i,m)*t2a(d,c,m,l)          !bakjimdcml      (+1.000)
     &     - u13(c,a,k,j,i,m)*t2a(d,b,m,l)          !cakjimdbml      (-1.000)
     &     + u13(c,b,k,j,i,m)*t2a(d,a,m,l)          !cbkjimdaml      (+1.000)
     &     + u13(d,a,k,j,i,m)*t2a(c,b,m,l)          !dakjimcbml      (+1.000)
     &     - u13(d,b,k,j,i,m)*t2a(c,a,m,l)          !dbkjimcaml      (-1.000)
     &     + u13(d,c,k,j,i,m)*t2a(b,a,m,l)          !dckjimbaml      (+1.000)       
             enddo
             v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call
!     & sum12834567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z48,-1.000)
!       call
!     & sum13824567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z48, 1.000)
!       call
!     & sum14823567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z48,-1.000)
!       call
!     & sum23814567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z48,-1.000)
!       call
!     & sum24813567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z48, 1.000)
!       call
!     & sum34812567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z48,-1.000)
!       call
!     & sum12734568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z48, 1.000)
!       call
!     & sum13724568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z48,-1.000)
!       call
!     & sum14723568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z48, 1.000)
!       call
!     & sum23714568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z48, 1.000)
!       call
!     & sum24713568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z48,-1.000)
!       call
!     & sum34712568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z48, 1.000)
!       call
!     & sum12634578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z48,-1.000)
!       call
!     & sum13624578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z48, 1.000)
!       call
!     & sum14623578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z48,-1.000)
!       call
!     & sum23614578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z48,-1.000)
!       call
!     & sum24613578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z48, 1.000)
!       call
!     & sum34612578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z48,-1.000)
!       call
!     & sum12534678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z48, 1.000)
!       call
!     & sum13524678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z48,-1.000)
!       call
!     & sum14523678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z48, 1.000)
!       call
!     & sum23514678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z48, 1.000)
!       call
!     & sum24513678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z48,-1.000)
!       call
!     & sum34512678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z48, 1.000)
!c
!       do i=n0+1,n1-3
!       do j=i+1,n1-2
!       do k=j+1,n1-1
!       do l=k+1,n1
!       do a=n1+1,n3-3
!       do b=a+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)
!     & -z48(d,c,i,b,a,l,k,j)       ! 12834567 (-1.000)
!     & +z48(d,b,i,c,a,l,k,j)       ! 13824567 (+1.000)
!     & -z48(d,a,i,c,b,l,k,j)       ! 14823567 (-1.000)
!     & -z48(c,b,i,d,a,l,k,j)       ! 23814567 (-1.000)
!     & +z48(c,a,i,d,b,l,k,j)       ! 24813567 (+1.000)
!     & -z48(b,a,i,d,c,l,k,j)       ! 34812567 (-1.000)
!     & +z48(d,c,j,b,a,l,k,i)       ! 12734568 (+1.000)
!     & -z48(d,b,j,c,a,l,k,i)       ! 13724568 (-1.000)
!     & +z48(d,a,j,c,b,l,k,i)       ! 14723568 (+1.000)
!     & +z48(c,b,j,d,a,l,k,i)       ! 23714568 (+1.000)
!     & -z48(c,a,j,d,b,l,k,i)       ! 24713568 (-1.000)
!     & +z48(b,a,j,d,c,l,k,i)       ! 34712568 (+1.000)
!     & -z48(d,c,k,b,a,l,j,i)       ! 12634578 (-1.000)
!     & +z48(d,b,k,c,a,l,j,i)       ! 13624578 (+1.000)
!     & -z48(d,a,k,c,b,l,j,i)       ! 14623578 (-1.000)
!     & -z48(c,b,k,d,a,l,j,i)       ! 23614578 (-1.000)
!     & +z48(c,a,k,d,b,l,j,i)       ! 24613578 (+1.000)
!     & -z48(b,a,k,d,c,l,j,i)       ! 34612578 (-1.000)
!     & +z48(d,c,l,b,a,k,j,i)       ! 12534678 (+1.000)
!     & -z48(d,b,l,c,a,k,j,i)       ! 13524678 (-1.000)
!     & +z48(d,a,l,c,b,k,j,i)       ! 14523678 (+1.000)
!     & +z48(c,b,l,d,a,k,j,i)       ! 23514678 (+1.000)
!     & -z48(c,a,l,d,b,k,j,i)       ! 24513678 (-1.000)
!     & +z48(b,a,l,d,c,k,j,i)       ! 34512678 (+1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z48)
       deallocate(u13)
c
       allocate(d1(n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2))
       call reorder3142(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n2,n3,n0,n2,intm,d1)
       allocate(d2(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       call reorder1324(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n0,n1,n1,n3,n0,n1,t2a,d2)
       allocate(s20(n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2))
       i1=k2*k4
       i2=k1*k3
       i3=k1*k3
       call egemm(i1,i2,i3,d1,d2,s20)
       deallocate(d1)
       deallocate(d2)
c
       call sum3421(n0,n2,n2,n3,n1,n3,n0,n1,x8,s20, 1.000)
       deallocate(s20)
c
       allocate(d1(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       call reorder4213(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n0,n1,n0,n1,intm,d1)
       allocate(d2(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       call reorder1324(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n0,n2,n1,n3,n0,n1,t2b,d2)
       allocate(s21(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1
       i2=k1*k3
       i3=k2*k4
       call egemm(i1,i2,i3,d1,d2,s21)
       deallocate(d1)
       deallocate(d2)
c
       call sum2314(n0,n1,n1,n3,n0,n1,n0,n1,x9,s21,-1.000)
       deallocate(s21)
c
       allocate(d1(n2+1:n3,n0+1:n2,n1+1:n3,n1+1:n3))
       call reorder4231(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n1,n3,n1,n3,intm,d1)
       allocate(d2(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       call reorder1324(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n0,n2,n1,n3,n0,n1,t2b,d2)
       allocate(s22(n1+1:n3,n0+1:n1,n1+1:n3,n1+1:n3))
       i1=k3*k3
       i2=k1*k3
       i3=k2*k4
       call egemm(i1,i2,i3,d1,d2,s22)
       deallocate(d1)
       deallocate(d2)
c
       call sum3412(n1,n3,n1,n3,n1,n3,n0,n1,x10,s22, 1.000)
       deallocate(s22)
c
       allocate(d1(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       call reorder4231(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n1,n3,n0,n1,intm,d1)
       allocate(d2(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       call reorder1324(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n0,n2,n1,n3,n0,n1,t2b,d2)
       allocate(s23(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       i1=k1*k3
       i2=k1*k3
       i3=k2*k4
       call egemm(i1,i2,i3,d1,d2,s23)
       deallocate(d1)
       deallocate(d2)
c
       call sum3421(n0,n1,n1,n3,n1,n3,n0,n1,x6,s23, 1.000)
c
       allocate(d1(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder4312(n1,n3,n0,n1,n1,n3,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,s23,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s47(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
       i1=k1*k3*k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s47)
       deallocate(d1)
       deallocate(b2)
c
       call sum2134(n1,n3,n1,n3,n1,n3,n0,n1,x10,s47,-1.000)
       deallocate(s47)
c
!       allocate(f2(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
!       call reorder123456(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,
!     & n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,t3a,f2)
!       allocate(z11(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
!       i1=k1*k3*k3
!       i2=k1*k1*k1*k3*k3
!       i3=k3
!       call egemm(i1,i2,i3,x10,f2,z11)
!       deallocate(f2)
c
       do i=n0+1,n1-3;do j=i+1,n1-2;do k=j+1,n1-1;do l=k+1,n1
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-3;do b=a+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
          if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n1+1,n3
             sum=sum
     &     + x10(e,d,c,i)*t3a(e,b,a,l,k,j)          !edciebalkj      (+1.000)
     &     - x10(e,d,b,i)*t3a(e,c,a,l,k,j)          !edbiecalkj      (-1.000)
     &     + x10(e,d,a,i)*t3a(e,c,b,l,k,j)          !edaiecblkj      (+1.000)
     &     - x10(e,c,d,i)*t3a(e,b,a,l,k,j)          !ecdiebalkj      (-1.000)
     &     + x10(e,b,d,i)*t3a(e,c,a,l,k,j)          !ebdiecalkj      (+1.000)
     &     - x10(e,a,d,i)*t3a(e,c,b,l,k,j)          !eadiecblkj      (-1.000)
     &     + x10(e,c,b,i)*t3a(e,d,a,l,k,j)          !ecbiedalkj      (+1.000)
     &     - x10(e,c,a,i)*t3a(e,d,b,l,k,j)          !ecaiedblkj      (-1.000)
     &     - x10(e,b,c,i)*t3a(e,d,a,l,k,j)          !ebciedalkj      (-1.000)
     &     + x10(e,a,c,i)*t3a(e,d,b,l,k,j)          !eaciedblkj      (+1.000)
     &     + x10(e,b,a,i)*t3a(e,d,c,l,k,j)          !ebaiedclkj      (+1.000)
     &     - x10(e,a,b,i)*t3a(e,d,c,l,k,j)          !eabiedclkj      (-1.000)
     &     - x10(e,d,c,j)*t3a(e,b,a,l,k,i)          !edcjebalki      (-1.000)
     &     + x10(e,d,b,j)*t3a(e,c,a,l,k,i)          !edbjecalki      (+1.000)
     &     - x10(e,d,a,j)*t3a(e,c,b,l,k,i)          !edajecblki      (-1.000)
     &     + x10(e,c,d,j)*t3a(e,b,a,l,k,i)          !ecdjebalki      (+1.000)
     &     - x10(e,b,d,j)*t3a(e,c,a,l,k,i)          !ebdjecalki      (-1.000)
     &     + x10(e,a,d,j)*t3a(e,c,b,l,k,i)          !eadjecblki      (+1.000)
     &     - x10(e,c,b,j)*t3a(e,d,a,l,k,i)          !ecbjedalki      (-1.000)
     &     + x10(e,c,a,j)*t3a(e,d,b,l,k,i)          !ecajedblki      (+1.000)
     &     + x10(e,b,c,j)*t3a(e,d,a,l,k,i)          !ebcjedalki      (+1.000)
     &     - x10(e,a,c,j)*t3a(e,d,b,l,k,i)          !eacjedblki      (-1.000)
     &     - x10(e,b,a,j)*t3a(e,d,c,l,k,i)          !ebajedclki      (-1.000)
     &     + x10(e,a,b,j)*t3a(e,d,c,l,k,i)          !eabjedclki      (+1.000)
     &     + x10(e,d,c,k)*t3a(e,b,a,l,j,i)          !edckebalji      (+1.000)
     &     - x10(e,d,b,k)*t3a(e,c,a,l,j,i)          !edbkecalji      (-1.000)
     &     + x10(e,d,a,k)*t3a(e,c,b,l,j,i)          !edakecblji      (+1.000)
     &     - x10(e,c,d,k)*t3a(e,b,a,l,j,i)          !ecdkebalji      (-1.000)
     &     + x10(e,b,d,k)*t3a(e,c,a,l,j,i)          !ebdkecalji      (+1.000)
     &     - x10(e,a,d,k)*t3a(e,c,b,l,j,i)          !eadkecblji      (-1.000)
     &     + x10(e,c,b,k)*t3a(e,d,a,l,j,i)          !ecbkedalji      (+1.000)
     &     - x10(e,c,a,k)*t3a(e,d,b,l,j,i)          !ecakedblji      (-1.000)
     &     - x10(e,b,c,k)*t3a(e,d,a,l,j,i)          !ebckedalji      (-1.000)
     &     + x10(e,a,c,k)*t3a(e,d,b,l,j,i)          !eackedblji      (+1.000)
     &     + x10(e,b,a,k)*t3a(e,d,c,l,j,i)          !ebakedclji      (+1.000)
     &     - x10(e,a,b,k)*t3a(e,d,c,l,j,i)          !eabkedclji      (-1.000)
     &     - x10(e,d,c,l)*t3a(e,b,a,k,j,i)          !edclebakji      (-1.000)
     &     + x10(e,d,b,l)*t3a(e,c,a,k,j,i)          !edblecakji      (+1.000)
     &     - x10(e,d,a,l)*t3a(e,c,b,k,j,i)          !edalecbkji      (-1.000)
     &     + x10(e,c,d,l)*t3a(e,b,a,k,j,i)          !ecdlebakji      (+1.000)
     &     - x10(e,b,d,l)*t3a(e,c,a,k,j,i)          !ebdlecakji      (-1.000)
     &     + x10(e,a,d,l)*t3a(e,c,b,k,j,i)          !eadlecbkji      (+1.000)
     &     - x10(e,c,b,l)*t3a(e,d,a,k,j,i)          !ecbledakji      (-1.000)
     &     + x10(e,c,a,l)*t3a(e,d,b,k,j,i)          !ecaledbkji      (+1.000)
     &     + x10(e,b,c,l)*t3a(e,d,a,k,j,i)          !ebcledakji      (+1.000)
     &     - x10(e,a,c,l)*t3a(e,d,b,k,j,i)          !eacledbkji      (-1.000)
     &     - x10(e,b,a,l)*t3a(e,d,c,k,j,i)          !ebaledckji      (-1.000)
     &     + x10(e,a,b,l)*t3a(e,d,c,k,j,i)          !eabledckji      (+1.000)       
             enddo
             v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call
!     & sum34567128(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z11, 1.000)
!       call
!     & sum24567138(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z11,-1.000)
!       call
!     & sum23567148(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z11, 1.000)
!       call
!     & sum34567218(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z11,-1.000)
!       call
!     & sum24567318(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z11, 1.000)
!       call
!     & sum23567418(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z11,-1.000)
!       call
!     & sum14567238(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z11, 1.000)
!       call
!     & sum13567248(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z11,-1.000)
!       call
!     & sum14567328(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z11,-1.000)
!       call
!     & sum13567428(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z11, 1.000)
!       call
!     & sum12567348(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z11, 1.000)
!       call
!     & sum12567438(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z11,-1.000)
!       call
!     & sum34568127(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z11,-1.000)
!       call
!     & sum24568137(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z11, 1.000)
!       call
!     & sum23568147(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z11,-1.000)
!       call
!     & sum34568217(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z11, 1.000)
!       call
!     & sum24568317(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z11,-1.000)
!       call
!     & sum23568417(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z11, 1.000)
!       call
!     & sum14568237(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z11,-1.000)
!       call
!     & sum13568247(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z11, 1.000)
!       call
!     & sum14568327(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z11, 1.000)
!       call
!     & sum13568427(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z11,-1.000)
!       call
!     & sum12568347(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z11,-1.000)
!       call
!     & sum12568437(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z11, 1.000)
!       call
!     & sum34578126(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z11, 1.000)
!       call
!     & sum24578136(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z11,-1.000)
!       call
!     & sum23578146(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z11, 1.000)
!       call
!     & sum34578216(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z11,-1.000)
!       call
!     & sum24578316(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z11, 1.000)
!       call
!     & sum23578416(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z11,-1.000)
!       call
!     & sum14578236(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z11, 1.000)
!       call
!     & sum13578246(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z11,-1.000)
!       call
!     & sum14578326(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z11,-1.000)
!       call
!     & sum13578426(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z11, 1.000)
!       call
!     & sum12578346(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z11, 1.000)
!       call
!     & sum12578436(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z11,-1.000)
!       call
!     & sum34678125(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z11,-1.000)
!       call
!     & sum24678135(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z11, 1.000)
!       call
!     & sum23678145(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z11,-1.000)
!       call
!     & sum34678215(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z11, 1.000)
!       call
!     & sum24678315(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z11,-1.000)
!       call
!     & sum23678415(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z11, 1.000)
!       call
!     & sum14678235(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z11,-1.000)
!       call
!     & sum13678245(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z11, 1.000)
!       call
!     & sum14678325(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z11, 1.000)
!       call
!     & sum13678425(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z11,-1.000)
!       call
!     & sum12678345(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z11,-1.000)
!       call
!     & sum12678435(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z11, 1.000)
!c
!       do i=n0+1,n1-3
!       do j=i+1,n1-2
!       do k=j+1,n1-1
!       do l=k+1,n1
!       do a=n1+1,n3-3
!       do b=a+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)
!     & +z11(b,a,l,k,j,d,c,i)       ! 34567128 (+1.000)
!     & -z11(c,a,l,k,j,d,b,i)       ! 24567138 (-1.000)
!     & +z11(c,b,l,k,j,d,a,i)       ! 23567148 (+1.000)
!     & -z11(b,a,l,k,j,c,d,i)       ! 34567218 (-1.000)
!     & +z11(c,a,l,k,j,b,d,i)       ! 24567318 (+1.000)
!     & -z11(c,b,l,k,j,a,d,i)       ! 23567418 (-1.000)
!     & +z11(d,a,l,k,j,c,b,i)       ! 14567238 (+1.000)
!     & -z11(d,b,l,k,j,c,a,i)       ! 13567248 (-1.000)
!     & -z11(d,a,l,k,j,b,c,i)       ! 14567328 (-1.000)
!     & +z11(d,b,l,k,j,a,c,i)       ! 13567428 (+1.000)
!     & +z11(d,c,l,k,j,b,a,i)       ! 12567348 (+1.000)
!     & -z11(d,c,l,k,j,a,b,i)       ! 12567438 (-1.000)
!     & -z11(b,a,l,k,i,d,c,j)       ! 34568127 (-1.000)
!     & +z11(c,a,l,k,i,d,b,j)       ! 24568137 (+1.000)
!     & -z11(c,b,l,k,i,d,a,j)       ! 23568147 (-1.000)
!     & +z11(b,a,l,k,i,c,d,j)       ! 34568217 (+1.000)
!     & -z11(c,a,l,k,i,b,d,j)       ! 24568317 (-1.000)
!     & +z11(c,b,l,k,i,a,d,j)       ! 23568417 (+1.000)
!     & -z11(d,a,l,k,i,c,b,j)       ! 14568237 (-1.000)
!     & +z11(d,b,l,k,i,c,a,j)       ! 13568247 (+1.000)
!     & +z11(d,a,l,k,i,b,c,j)       ! 14568327 (+1.000)
!     & -z11(d,b,l,k,i,a,c,j)       ! 13568427 (-1.000)
!     & -z11(d,c,l,k,i,b,a,j)       ! 12568347 (-1.000)
!     & +z11(d,c,l,k,i,a,b,j)       ! 12568437 (+1.000)
!     & +z11(b,a,l,j,i,d,c,k)       ! 34578126 (+1.000)
!     & -z11(c,a,l,j,i,d,b,k)       ! 24578136 (-1.000)
!     & +z11(c,b,l,j,i,d,a,k)       ! 23578146 (+1.000)
!     & -z11(b,a,l,j,i,c,d,k)       ! 34578216 (-1.000)
!     & +z11(c,a,l,j,i,b,d,k)       ! 24578316 (+1.000)
!     & -z11(c,b,l,j,i,a,d,k)       ! 23578416 (-1.000)
!     & +z11(d,a,l,j,i,c,b,k)       ! 14578236 (+1.000)
!     & -z11(d,b,l,j,i,c,a,k)       ! 13578246 (-1.000)
!     & -z11(d,a,l,j,i,b,c,k)       ! 14578326 (-1.000)
!     & +z11(d,b,l,j,i,a,c,k)       ! 13578426 (+1.000)
!     & +z11(d,c,l,j,i,b,a,k)       ! 12578346 (+1.000)
!     & -z11(d,c,l,j,i,a,b,k)       ! 12578436 (-1.000)
!     & -z11(b,a,k,j,i,d,c,l)       ! 34678125 (-1.000)
!     & +z11(c,a,k,j,i,d,b,l)       ! 24678135 (+1.000)
!     & -z11(c,b,k,j,i,d,a,l)       ! 23678145 (-1.000)
!     & +z11(b,a,k,j,i,c,d,l)       ! 34678215 (+1.000)
!     & -z11(c,a,k,j,i,b,d,l)       ! 24678315 (-1.000)
!     & +z11(c,b,k,j,i,a,d,l)       ! 23678415 (+1.000)
!     & -z11(d,a,k,j,i,c,b,l)       ! 14678235 (-1.000)
!     & +z11(d,b,k,j,i,c,a,l)       ! 13678245 (+1.000)
!     & +z11(d,a,k,j,i,b,c,l)       ! 14678325 (+1.000)
!     & -z11(d,b,k,j,i,a,c,l)       ! 13678425 (-1.000)
!     & -z11(d,c,k,j,i,b,a,l)       ! 12678345 (-1.000)
!     & +z11(d,c,k,j,i,a,b,l)       ! 12678435 (+1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z11)
       deallocate(x10)
c
       allocate(d1(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       call reorder3412(n1,n3,n0,n1,n1,n3,n0,n1,
     & n1,n3,n0,n1,n1,n3,n0,n1,s23,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s46(n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1))
       i1=k1*k3*k1
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s46)
       deallocate(d1)
       deallocate(b2)
       deallocate(s23)
c
       call sum4123(n0,n1,n1,n3,n0,n1,n0,n1,x9,s46,-1.000)
       deallocate(s46)
c
!       allocate(f2(n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!       call reorder412356(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,t3a,f2)
!       allocate(z10(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
!       i1=k1*k1*k3
!       i2=k1*k1*k3*k3*k3
!       i3=k1
!       call egemm(i1,i2,i3,x9,f2,z10)
!       deallocate(f2)
c
       do i=n0+1,n1-3;do j=i+1,n1-2;do k=j+1,n1-1;do l=k+1,n1
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-3;do b=a+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
          if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1
             sum=sum
     &     + x9(m,d,j,i)*t3a(c,b,a,m,l,k)           !mdjicbamlk      (+1.000)
     &     - x9(m,c,j,i)*t3a(d,b,a,m,l,k)           !mcjidbamlk      (-1.000)
     &     + x9(m,b,j,i)*t3a(d,c,a,m,l,k)           !mbjidcamlk      (+1.000)
     &     - x9(m,a,j,i)*t3a(d,c,b,m,l,k)           !majidcbmlk      (-1.000)
     &     - x9(m,d,k,i)*t3a(c,b,a,m,l,j)           !mdkicbamlj      (-1.000)
     &     + x9(m,c,k,i)*t3a(d,b,a,m,l,j)           !mckidbamlj      (+1.000)
     &     - x9(m,b,k,i)*t3a(d,c,a,m,l,j)           !mbkidcamlj      (-1.000)
     &     + x9(m,a,k,i)*t3a(d,c,b,m,l,j)           !makidcbmlj      (+1.000)
     &     + x9(m,d,l,i)*t3a(c,b,a,m,k,j)           !mdlicbamkj      (+1.000)
     &     - x9(m,c,l,i)*t3a(d,b,a,m,k,j)           !mclidbamkj      (-1.000)
     &     + x9(m,b,l,i)*t3a(d,c,a,m,k,j)           !mblidcamkj      (+1.000)
     &     - x9(m,a,l,i)*t3a(d,c,b,m,k,j)           !malidcbmkj      (-1.000)
     &     - x9(m,d,i,j)*t3a(c,b,a,m,l,k)           !mdijcbamlk      (-1.000)
     &     + x9(m,c,i,j)*t3a(d,b,a,m,l,k)           !mcijdbamlk      (+1.000)
     &     - x9(m,b,i,j)*t3a(d,c,a,m,l,k)           !mbijdcamlk      (-1.000)
     &     + x9(m,a,i,j)*t3a(d,c,b,m,l,k)           !maijdcbmlk      (+1.000)
     &     + x9(m,d,i,k)*t3a(c,b,a,m,l,j)           !mdikcbamlj      (+1.000)
     &     - x9(m,c,i,k)*t3a(d,b,a,m,l,j)           !mcikdbamlj      (-1.000)
     &     + x9(m,b,i,k)*t3a(d,c,a,m,l,j)           !mbikdcamlj      (+1.000)
     &     - x9(m,a,i,k)*t3a(d,c,b,m,l,j)           !maikdcbmlj      (-1.000)
     &     - x9(m,d,i,l)*t3a(c,b,a,m,k,j)           !mdilcbamkj      (-1.000)
     &     + x9(m,c,i,l)*t3a(d,b,a,m,k,j)           !mcildbamkj      (+1.000)
     &     - x9(m,b,i,l)*t3a(d,c,a,m,k,j)           !mbildcamkj      (-1.000)
     &     + x9(m,a,i,l)*t3a(d,c,b,m,k,j)           !maildcbmkj      (+1.000)
     &     + x9(m,d,k,j)*t3a(c,b,a,m,l,i)           !mdkjcbamli      (+1.000)
     &     - x9(m,c,k,j)*t3a(d,b,a,m,l,i)           !mckjdbamli      (-1.000)
     &     + x9(m,b,k,j)*t3a(d,c,a,m,l,i)           !mbkjdcamli      (+1.000)
     &     - x9(m,a,k,j)*t3a(d,c,b,m,l,i)           !makjdcbmli      (-1.000)
     &     - x9(m,d,l,j)*t3a(c,b,a,m,k,i)           !mdljcbamki      (-1.000)
     &     + x9(m,c,l,j)*t3a(d,b,a,m,k,i)           !mcljdbamki      (+1.000)
     &     - x9(m,b,l,j)*t3a(d,c,a,m,k,i)           !mbljdcamki      (-1.000)
     &     + x9(m,a,l,j)*t3a(d,c,b,m,k,i)           !maljdcbmki      (+1.000)
     &     - x9(m,d,j,k)*t3a(c,b,a,m,l,i)           !mdjkcbamli      (-1.000)
     &     + x9(m,c,j,k)*t3a(d,b,a,m,l,i)           !mcjkdbamli      (+1.000)
     &     - x9(m,b,j,k)*t3a(d,c,a,m,l,i)           !mbjkdcamli      (-1.000)
     &     + x9(m,a,j,k)*t3a(d,c,b,m,l,i)           !majkdcbmli      (+1.000)
     &     + x9(m,d,j,l)*t3a(c,b,a,m,k,i)           !mdjlcbamki      (+1.000)
     &     - x9(m,c,j,l)*t3a(d,b,a,m,k,i)           !mcjldbamki      (-1.000)
     &     + x9(m,b,j,l)*t3a(d,c,a,m,k,i)           !mbjldcamki      (+1.000)
     &     - x9(m,a,j,l)*t3a(d,c,b,m,k,i)           !majldcbmki      (-1.000)
     &     + x9(m,d,l,k)*t3a(c,b,a,m,j,i)           !mdlkcbamji      (+1.000)
     &     - x9(m,c,l,k)*t3a(d,b,a,m,j,i)           !mclkdbamji      (-1.000)
     &     + x9(m,b,l,k)*t3a(d,c,a,m,j,i)           !mblkdcamji      (+1.000)
     &     - x9(m,a,l,k)*t3a(d,c,b,m,j,i)           !malkdcbmji      (-1.000)
     &     - x9(m,d,k,l)*t3a(c,b,a,m,j,i)           !mdklcbamji      (-1.000)
     &     + x9(m,c,k,l)*t3a(d,b,a,m,j,i)           !mckldbamji      (+1.000)
     &     - x9(m,b,k,l)*t3a(d,c,a,m,j,i)           !mbkldcamji      (-1.000)
     &     + x9(m,a,k,l)*t3a(d,c,b,m,j,i)           !makldcbmji      (+1.000)       
             enddo
             v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call
!     & sum23456178(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z10, 1.000)
!       call
!     & sum13456278(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z10,-1.000)
!       call
!     & sum12456378(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z10, 1.000)
!       call
!     & sum12356478(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z10,-1.000)
!       call
!     & sum23457168(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z10,-1.000)
!       call
!     & sum13457268(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z10, 1.000)
!       call
!     & sum12457368(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z10,-1.000)
!       call
!     & sum12357468(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z10, 1.000)
!       call
!     & sum23467158(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z10, 1.000)
!       call
!     & sum13467258(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z10,-1.000)
!       call
!     & sum12467358(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z10, 1.000)
!       call
!     & sum12367458(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z10,-1.000)
!       call
!     & sum23456187(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z10,-1.000)
!       call
!     & sum13456287(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z10, 1.000)
!       call
!     & sum12456387(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z10,-1.000)
!       call
!     & sum12356487(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z10, 1.000)
!       call
!     & sum23457186(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z10, 1.000)
!       call
!     & sum13457286(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z10,-1.000)
!       call
!     & sum12457386(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z10, 1.000)
!       call
!     & sum12357486(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z10,-1.000)
!       call
!     & sum23467185(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z10,-1.000)
!       call
!     & sum13467285(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z10, 1.000)
!       call
!     & sum12467385(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z10,-1.000)
!       call
!     & sum12367485(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z10, 1.000)
!       call
!     & sum23458167(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z10, 1.000)
!       call
!     & sum13458267(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z10,-1.000)
!       call
!     & sum12458367(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z10, 1.000)
!       call
!     & sum12358467(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z10,-1.000)
!       call
!     & sum23468157(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z10,-1.000)
!       call
!     & sum13468257(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z10, 1.000)
!       call
!     & sum12468357(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z10,-1.000)
!       call
!     & sum12368457(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z10, 1.000)
!       call
!     & sum23458176(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z10,-1.000)
!       call
!     & sum13458276(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z10, 1.000)
!       call
!     & sum12458376(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z10,-1.000)
!       call
!     & sum12358476(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z10, 1.000)
!       call
!     & sum23468175(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z10, 1.000)
!       call
!     & sum13468275(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z10,-1.000)
!       call
!     & sum12468375(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z10, 1.000)
!       call
!     & sum12368475(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z10,-1.000)
!       call
!     & sum23478156(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z10, 1.000)
!       call
!     & sum13478256(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z10,-1.000)
!       call
!     & sum12478356(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z10, 1.000)
!       call
!     & sum12378456(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z10,-1.000)
!       call
!     & sum23478165(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z10,-1.000)
!       call
!     & sum13478265(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z10, 1.000)
!       call
!     & sum12478365(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z10,-1.000)
!       call
!     & sum12378465(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z10, 1.000)
!c
!       do i=n0+1,n1-3
!       do j=i+1,n1-2
!       do k=j+1,n1-1
!       do l=k+1,n1
!       do a=n1+1,n3-3
!       do b=a+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)
!     & +z10(c,b,a,l,k,d,j,i)       ! 23456178 (+1.000)
!     & -z10(d,b,a,l,k,c,j,i)       ! 13456278 (-1.000)
!     & +z10(d,c,a,l,k,b,j,i)       ! 12456378 (+1.000)
!     & -z10(d,c,b,l,k,a,j,i)       ! 12356478 (-1.000)
!     & -z10(c,b,a,l,j,d,k,i)       ! 23457168 (-1.000)
!     & +z10(d,b,a,l,j,c,k,i)       ! 13457268 (+1.000)
!     & -z10(d,c,a,l,j,b,k,i)       ! 12457368 (-1.000)
!     & +z10(d,c,b,l,j,a,k,i)       ! 12357468 (+1.000)
!     & +z10(c,b,a,k,j,d,l,i)       ! 23467158 (+1.000)
!     & -z10(d,b,a,k,j,c,l,i)       ! 13467258 (-1.000)
!     & +z10(d,c,a,k,j,b,l,i)       ! 12467358 (+1.000)
!     & -z10(d,c,b,k,j,a,l,i)       ! 12367458 (-1.000)
!     & -z10(c,b,a,l,k,d,i,j)       ! 23456187 (-1.000)
!     & +z10(d,b,a,l,k,c,i,j)       ! 13456287 (+1.000)
!     & -z10(d,c,a,l,k,b,i,j)       ! 12456387 (-1.000)
!     & +z10(d,c,b,l,k,a,i,j)       ! 12356487 (+1.000)
!     & +z10(c,b,a,l,j,d,i,k)       ! 23457186 (+1.000)
!     & -z10(d,b,a,l,j,c,i,k)       ! 13457286 (-1.000)
!     & +z10(d,c,a,l,j,b,i,k)       ! 12457386 (+1.000)
!     & -z10(d,c,b,l,j,a,i,k)       ! 12357486 (-1.000)
!     & -z10(c,b,a,k,j,d,i,l)       ! 23467185 (-1.000)
!     & +z10(d,b,a,k,j,c,i,l)       ! 13467285 (+1.000)
!     & -z10(d,c,a,k,j,b,i,l)       ! 12467385 (-1.000)
!     & +z10(d,c,b,k,j,a,i,l)       ! 12367485 (+1.000)
!     & +z10(c,b,a,l,i,d,k,j)       ! 23458167 (+1.000)
!     & -z10(d,b,a,l,i,c,k,j)       ! 13458267 (-1.000)
!     & +z10(d,c,a,l,i,b,k,j)       ! 12458367 (+1.000)
!     & -z10(d,c,b,l,i,a,k,j)       ! 12358467 (-1.000)
!     & -z10(c,b,a,k,i,d,l,j)       ! 23468157 (-1.000)
!     & +z10(d,b,a,k,i,c,l,j)       ! 13468257 (+1.000)
!     & -z10(d,c,a,k,i,b,l,j)       ! 12468357 (-1.000)
!     & +z10(d,c,b,k,i,a,l,j)       ! 12368457 (+1.000)
!     & -z10(c,b,a,l,i,d,j,k)       ! 23458176 (-1.000)
!     & +z10(d,b,a,l,i,c,j,k)       ! 13458276 (+1.000)
!     & -z10(d,c,a,l,i,b,j,k)       ! 12458376 (-1.000)
!     & +z10(d,c,b,l,i,a,j,k)       ! 12358476 (+1.000)
!     & +z10(c,b,a,k,i,d,j,l)       ! 23468175 (+1.000)
!     & -z10(d,b,a,k,i,c,j,l)       ! 13468275 (-1.000)
!     & +z10(d,c,a,k,i,b,j,l)       ! 12468375 (+1.000)
!     & -z10(d,c,b,k,i,a,j,l)       ! 12368475 (-1.000)
!     & +z10(c,b,a,j,i,d,l,k)       ! 23478156 (+1.000)
!     & -z10(d,b,a,j,i,c,l,k)       ! 13478256 (-1.000)
!     & +z10(d,c,a,j,i,b,l,k)       ! 12478356 (+1.000)
!     & -z10(d,c,b,j,i,a,l,k)       ! 12378456 (-1.000)
!     & -z10(c,b,a,j,i,d,k,l)       ! 23478165 (-1.000)
!     & +z10(d,b,a,j,i,c,k,l)       ! 13478265 (+1.000)
!     & -z10(d,c,a,j,i,b,k,l)       ! 12478365 (-1.000)
!     & +z10(d,c,b,j,i,a,k,l)       ! 12378465 (+1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z10)
       deallocate(x9)
c
c
       allocate(d1(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder4321(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n0,n2,n0,n1,intm,d1)
       allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
       allocate(q9(n0+1:n1,n0+1:n1))
       i1=k1
       i2=k1
       i3=k2*k3*k4
       call egemm(i1,i2,i3,d1,d2,q9)
       deallocate(d1)
       deallocate(d2)
c
       call sum21(n0,n1,n0,n1,x3,q9, 1.000)
       deallocate(q9)
c
       allocate(d1(n2+1:n3,n0+1:n1,n0+1:n2,n1+1:n3))
       call reorder4123(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n1,n0,n2,n1,n3,intm,d1)
       allocate(d2(n2+1:n3,n0+1:n1,n0+1:n2,n1+1:n3))
       call reorder1432(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n0,n1,n0,n2,n1,n3,t2b,d2)
       allocate(q10(n1+1:n3,n1+1:n3))
       i1=k3
       i2=k3
       i3=k2*k1*k4
       call egemm(i1,i2,i3,d1,d2,q10)
       deallocate(d1)
       deallocate(d2)
c
       call sum21(n1,n3,n1,n3,x4,q10,-1.000)
       deallocate(q10)
c
       allocate(d1(n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2))
       call reorder3142(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n2,n3,n0,n2,intb,d1)
       allocate(d2(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       call reorder1324(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n0,n2,n1,n3,n0,n1,t2b,d2)
       allocate(s24(n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2))
       i1=k2*k4
       i2=k1*k3
       i3=k2*k4
       call egemm(i1,i2,i3,d1,d2,s24)
       deallocate(d1)
       deallocate(d2)
c
       call sum3421(n0,n2,n2,n3,n1,n3,n0,n1,x8,s24, 1.000)
       deallocate(s24)
c
       allocate(d1(n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3))
       call reorder3124(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n0,n1,n1,n3,intr,d1)
       allocate(f2(n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder154236(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,
     & n1,n3,n0,n1,n0,n1,n1,n3,n1,n3,n0,n1,t3a,f2)
       allocate(s25(n1+1:n3,n1+1:n3,n0+1:n1,n1+1:n3))
       i1=k3
       i2=k1*k3*k3
       i3=k1*k1*k3
       call egemm(i1,i2,i3,d1,f2,s25)
       deallocate(d1)
       deallocate(f2)
c
!       allocate(d1(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
!       call reorder4123(n1,n3,n1,n3,n0,n1,n1,n3,
!     & n1,n3,n1,n3,n1,n3,n0,n1,s25,d1)
!       allocate(f2(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
!       call reorder123456(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,
!     & n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,t3a,f2)
!       allocate(z56(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
!       i1=k1*k3*k3
!       i2=k1*k1*k1*k3*k3
!       i3=k3
!       call egemm(i1,i2,i3,d1,f2,z56)
!       deallocate(d1)
!       deallocate(f2)
c
       do i=n0+1,n1-3;do j=i+1,n1-2;do k=j+1,n1-1;do l=k+1,n1
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-3;do b=a+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
          if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n1+1,n3
             sum=sum                           !top 2 switched
     &     + (s25(c,a,l,f)*t3a(f,d,b,k,j,i)         !calffdbkji      (+0.500)
     &     - s25(b,a,l,f)*t3a(f,d,c,k,j,i)          !balffdckji      (-0.500)
     &     - s25(c,b,l,f)*t3a(f,d,a,k,j,i)          !cblffdakji      (-0.500)
     &     - s25(d,a,l,f)*t3a(f,c,b,k,j,i)          !dalffcbkji      (-0.500)
     &     + s25(d,b,l,f)*t3a(f,c,a,k,j,i)          !dblffcakji      (+0.500)
     &     - s25(d,c,l,f)*t3a(f,b,a,k,j,i)          !dclffbakji      (-0.500)
     &     + s25(b,a,k,f)*t3a(f,d,c,l,j,i)          !bakffdclji      (+0.500)
     &     - s25(c,a,k,f)*t3a(f,d,b,l,j,i)          !cakffdblji      (-0.500)
     &     + s25(c,b,k,f)*t3a(f,d,a,l,j,i)          !cbkffdalji      (+0.500)
     &     + s25(d,a,k,f)*t3a(f,c,b,l,j,i)          !dakffcblji      (+0.500)
     &     - s25(d,b,k,f)*t3a(f,c,a,l,j,i)          !dbkffcalji      (-0.500)
     &     + s25(d,c,k,f)*t3a(f,b,a,l,j,i)          !dckffbalji      (+0.500)
     &     - s25(b,a,j,f)*t3a(f,d,c,l,k,i)          !bajffdclki      (-0.500)
     &     + s25(c,a,j,f)*t3a(f,d,b,l,k,i)          !cajffdblki      (+0.500)
     &     - s25(c,b,j,f)*t3a(f,d,a,l,k,i)          !cbjffdalki      (-0.500)
     &     - s25(d,a,j,f)*t3a(f,c,b,l,k,i)          !dajffcblki      (-0.500)
     &     + s25(d,b,j,f)*t3a(f,c,a,l,k,i)          !dbjffcalki      (+0.500)
     &     - s25(d,c,j,f)*t3a(f,b,a,l,k,i)          !dcjffbalki      (-0.500)
     &     + s25(d,c,i,f)*t3a(f,b,a,l,k,j)          !dciffbalkj      (+0.500)
     &     - s25(d,b,i,f)*t3a(f,c,a,l,k,j)          !dbiffcalkj      (-0.500)
     &     + s25(d,a,i,f)*t3a(f,c,b,l,k,j)          !daiffcblkj      (+0.500)
     &     + s25(c,b,i,f)*t3a(f,d,a,l,k,j)          !cbiffdalkj      (+0.500)
     &     - s25(c,a,i,f)*t3a(f,d,b,l,k,j)          !caiffdblkj      (-0.500)
     &     + s25(b,a,i,f)*t3a(f,d,c,l,k,j))/2.0d0   !baiffdclkj      (+0.500)      
             enddo
             v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call
!     & sum12678345(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z56,-0.500)
!       call
!     & sum13678245(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z56, 0.500)
!       call
!     & sum14678235(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z56,-0.500)
!       call
!     & sum23678145(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z56,-0.500)
!       call
!     & sum24678135(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z56, 0.500)
!       call
!     & sum34678125(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z56,-0.500)
!       call
!     & sum12578346(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z56, 0.500)
!       call
!     & sum13578246(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z56,-0.500)
!       call
!     & sum14578236(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z56, 0.500)
!       call
!     & sum23578146(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z56, 0.500)
!       call
!     & sum24578136(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z56,-0.500)
!       call
!     & sum34578126(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z56, 0.500)
!       call
!     & sum12568347(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z56,-0.500)
!       call
!     & sum13568247(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z56, 0.500)
!       call
!     & sum14568237(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z56,-0.500)
!       call
!     & sum23568147(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z56,-0.500)
!       call
!     & sum24568137(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z56, 0.500)
!       call
!     & sum34568127(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z56,-0.500)
!       call
!     & sum34567128(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z56, 0.500)
!       call
!     & sum24567138(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z56,-0.500)
!       call
!     & sum23567148(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z56, 0.500)
!       call
!     & sum14567238(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z56, 0.500)
!       call
!     & sum13567248(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z56,-0.500)
!       call
!     & sum12567348(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z56, 0.500)
!c
!       do i=n0+1,n1-3
!       do j=i+1,n1-2
!       do k=j+1,n1-1
!       do l=k+1,n1
!       do a=n1+1,n3-3
!       do b=a+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)
!     & +(z56(d,b,k,j,i,c,a,l)      ! 13678245 (+0.500) top two switched
!     & -z56(d,c,k,j,i,b,a,l)       ! 12678345 (-0.500)
!     & -z56(d,a,k,j,i,c,b,l)       ! 14678235 (-0.500)
!     & -z56(c,b,k,j,i,d,a,l)       ! 23678145 (-0.500)
!     & +z56(c,a,k,j,i,d,b,l)       ! 24678135 (+0.500)
!     & -z56(b,a,k,j,i,d,c,l)       ! 34678125 (-0.500)
!     & +z56(d,c,l,j,i,b,a,k)       ! 12578346 (+0.500)
!     & -z56(d,b,l,j,i,c,a,k)       ! 13578246 (-0.500)
!     & +z56(d,a,l,j,i,c,b,k)       ! 14578236 (+0.500)
!     & +z56(c,b,l,j,i,d,a,k)       ! 23578146 (+0.500)
!     & -z56(c,a,l,j,i,d,b,k)       ! 24578136 (-0.500)
!     & +z56(b,a,l,j,i,d,c,k)       ! 34578126 (+0.500)
!     & -z56(d,c,l,k,i,b,a,j)       ! 12568347 (-0.500)
!     & +z56(d,b,l,k,i,c,a,j)       ! 13568247 (+0.500)
!     & -z56(d,a,l,k,i,c,b,j)       ! 14568237 (-0.500)
!     & -z56(c,b,l,k,i,d,a,j)       ! 23568147 (-0.500)
!     & +z56(c,a,l,k,i,d,b,j)       ! 24568137 (+0.500)
!     & -z56(b,a,l,k,i,d,c,j)       ! 34568127 (-0.500)
!     & +z56(b,a,l,k,j,d,c,i)       ! 34567128 (+0.500)
!     & -z56(c,a,l,k,j,d,b,i)       ! 24567138 (-0.500)
!     & +z56(c,b,l,k,j,d,a,i)       ! 23567148 (+0.500)
!     & +z56(d,a,l,k,j,c,b,i)       ! 14567238 (+0.500)
!     & -z56(d,b,l,k,j,c,a,i)       ! 13567248 (-0.500)
!     & +z56(d,c,l,k,j,b,a,i))/2.0d0! 12567348 (+0.500)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z56)
       deallocate(s25)
c
       allocate(d1(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder4312(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n1,n3,n0,n1,n0,n1,intr,d1)
       allocate(f2(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       call reorder123456(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,
     & n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,t3a,f2)
       allocate(u14(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1
       i2=k1*k1*k1*k3
       i3=k3*k3
       call egemm(i1,i2,i3,d1,f2,u14)
       deallocate(d1)
       deallocate(f2)
c
!       allocate(f1(n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
!       call reorder651234(n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,n0,n1,
!     & n0,n1,n0,n1,n1,n3,n0,n1,n0,n1,n0,n1,u14,f1)
!       allocate(f2(n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
!       call reorder451236(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,
!     & n0,n1,n0,n1,n1,n3,n1,n3,n1,n3,n0,n1,t3a,f2)
!       allocate(z57(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
!       i1=k1*k1*k1*k3
!       i2=k1*k3*k3*k3
!       i3=k1*k1
!       call egemm(i1,i2,i3,f1,f2,z57)
!       deallocate(f1)
!       deallocate(f2)
c
       do i=n0+1,n1-3;do j=i+1,n1-2;do k=j+1,n1-1;do l=k+1,n1
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-3;do b=a+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
          if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1;do n=n0+1,n1
             sum=sum                                   !top 2 switched
     &     + (u14(c,k,j,i,m,n)*t3a(d,b,a,n,m,l)        !ckjimndbanml    (+0.250)
     &     - u14(d,k,j,i,m,n)*t3a(c,b,a,n,m,l)         !dkjimncbanml    (-0.250)
     &     - u14(b,k,j,i,m,n)*t3a(d,c,a,n,m,l)         !bkjimndcanml    (-0.250)
     &     + u14(a,k,j,i,m,n)*t3a(d,c,b,n,m,l)         !akjimndcbnml    (+0.250)
     &     + u14(d,l,j,i,m,n)*t3a(c,b,a,n,m,k)         !dljimncbanmk    (+0.250)
     &     - u14(c,l,j,i,m,n)*t3a(d,b,a,n,m,k)         !cljimndbanmk    (-0.250)
     &     + u14(b,l,j,i,m,n)*t3a(d,c,a,n,m,k)         !bljimndcanmk    (+0.250)
     &     - u14(a,l,j,i,m,n)*t3a(d,c,b,n,m,k)         !aljimndcbnmk    (-0.250)
     &     - u14(d,l,k,i,m,n)*t3a(c,b,a,n,m,j)         !dlkimncbanmj    (-0.250)
     &     + u14(c,l,k,i,m,n)*t3a(d,b,a,n,m,j)         !clkimndbanmj    (+0.250)
     &     - u14(b,l,k,i,m,n)*t3a(d,c,a,n,m,j)         !blkimndcanmj    (-0.250)
     &     + u14(a,l,k,i,m,n)*t3a(d,c,b,n,m,j)         !alkimndcbnmj    (+0.250)
     &     - u14(a,l,k,j,m,n)*t3a(d,c,b,n,m,i)         !alkjmndcbnmi    (-0.250)
     &     + u14(b,l,k,j,m,n)*t3a(d,c,a,n,m,i)         !blkjmndcanmi    (+0.250)
     &     - u14(c,l,k,j,m,n)*t3a(d,b,a,n,m,i)         !clkjmndbanmi    (-0.250)
     &     + u14(d,l,k,j,m,n)*t3a(c,b,a,n,m,i))/4.0d0  !dlkjmncbanmi    (+0.250)
             enddo;enddo
             v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call
!     & sum23451678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z57,-0.250)
!       call
!     & sum13452678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z57, 0.250)
!       call
!     & sum12453678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z57,-0.250)
!       call
!     & sum12354678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z57, 0.250)
!       call
!     & sum23461578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z57, 0.250)
!       call
!     & sum13462578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z57,-0.250)
!       call
!     & sum12463578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z57, 0.250)
!       call
!     & sum12364578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z57,-0.250)
!       call
!     & sum23471568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z57,-0.250)
!       call
!     & sum13472568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z57, 0.250)
!       call
!     & sum12473568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z57,-0.250)
!       call
!     & sum12374568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z57, 0.250)
!       call
!     & sum12384567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z57,-0.250)
!       call
!     & sum12483567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z57, 0.250)
!       call
!     & sum13482567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z57,-0.250)
!       call
!     & sum23481567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z57, 0.250)
!c
!       do i=n0+1,n1-3
!       do j=i+1,n1-2
!       do k=j+1,n1-1
!       do l=k+1,n1
!       do a=n1+1,n3-3
!       do b=a+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)
!     & +(z57(d,b,a,l,c,k,j,i)      !     13452678 (+0.250) top two switched
!     & -z57(c,b,a,l,d,k,j,i)       !     23451678 (-0.250)
!     & -z57(d,c,a,l,b,k,j,i)       !     12453678 (-0.250)
!     & +z57(d,c,b,l,a,k,j,i)       !     12354678 (+0.250)
!     & +z57(c,b,a,k,d,l,j,i)       !     23461578 (+0.250)
!     & -z57(d,b,a,k,c,l,j,i)       !     13462578 (-0.250)
!     & +z57(d,c,a,k,b,l,j,i)       !     12463578 (+0.250)
!     & -z57(d,c,b,k,a,l,j,i)       !     12364578 (-0.250)
!     & -z57(c,b,a,j,d,l,k,i)       !     23471568 (-0.250)
!     & +z57(d,b,a,j,c,l,k,i)       !     13472568 (+0.250)
!     & -z57(d,c,a,j,b,l,k,i)       !     12473568 (-0.250)
!     & +z57(d,c,b,j,a,l,k,i)       !     12374568 (+0.250)
!     & -z57(d,c,b,i,a,l,k,j)       !     12384567 (-0.250)
!     & +z57(d,c,a,i,b,l,k,j)       !     12483567 (+0.250)
!     & -z57(d,b,a,i,c,l,k,j)       !     13482567 (-0.250)
!     & +z57(c,b,a,i,d,l,k,j))/4.0d0!     23481567 (+0.250)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z57)
c
       allocate(f1(n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       call reorder561234(n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,n0,n1,
     & n0,n1,n0,n1,n1,n3,n0,n1,n0,n1,n0,n1,u14,f1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(u25(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1*k1*k3*k1
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,f1,b2,u25)
       deallocate(f1)
       deallocate(b2)
       deallocate(u14)
c
!       allocate(f1(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
!       call reorder213456(n1,n3,n0,n1,n1,n3,n0,n1,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,u25,f1)
!       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
!       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
!       allocate(z87(n1+1:n3,n1+1:n3,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
!       i1=k1*k1*k1*k3*k3
!       i2=k1*k3*k3
!       i3=k1
!       call egemm(i1,i2,i3,f1,d2,z87)
!       deallocate(f1)
!       deallocate(d2)
c
       do i=n0+1,n1-3;do j=i+1,n1-2;do k=j+1,n1-1;do l=k+1,n1
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-3;do b=a+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
          if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do n=n0+1,n1
             sum=sum
     &     + (u25(d,n,a,l,k,j)*t2a(c,b,n,i)         !dnalkjcbni      (+0.500)
     &     - u25(d,n,b,l,k,j)*t2a(c,a,n,i)          !dnblkjcani      (-0.500)
     &     + u25(d,n,c,l,k,j)*t2a(b,a,n,i)          !dnclkjbani      (+0.500)
     &     - u25(c,n,a,l,k,j)*t2a(d,b,n,i)          !cnalkjdbni      (-0.500)
     &     + u25(c,n,b,l,k,j)*t2a(d,a,n,i)          !cnblkjdani      (+0.500)
     &     + u25(b,n,a,l,k,j)*t2a(d,c,n,i)          !bnalkjdcni      (+0.500)
     &     - u25(a,n,b,l,k,j)*t2a(d,c,n,i)          !anblkjdcni      (-0.500)
     &     - u25(b,n,c,l,k,j)*t2a(d,a,n,i)          !bnclkjdani      (-0.500)
     &     + u25(a,n,c,l,k,j)*t2a(d,b,n,i)          !anclkjdbni      (+0.500)
     &     - u25(c,n,d,l,k,j)*t2a(b,a,n,i)          !cndlkjbani      (-0.500)
     &     + u25(b,n,d,l,k,j)*t2a(c,a,n,i)          !bndlkjcani      (+0.500)
     &     - u25(a,n,d,l,k,j)*t2a(c,b,n,i)          !andlkjcbni      (-0.500)
     &     - u25(d,n,a,l,k,i)*t2a(c,b,n,j)          !dnalkicbnj      (-0.500)
     &     + u25(d,n,b,l,k,i)*t2a(c,a,n,j)          !dnblkicanj      (+0.500)
     &     - u25(d,n,c,l,k,i)*t2a(b,a,n,j)          !dnclkibanj      (-0.500)
     &     + u25(c,n,a,l,k,i)*t2a(d,b,n,j)          !cnalkidbnj      (+0.500)
     &     - u25(c,n,b,l,k,i)*t2a(d,a,n,j)          !cnblkidanj      (-0.500)
     &     - u25(b,n,a,l,k,i)*t2a(d,c,n,j)          !bnalkidcnj      (-0.500)
     &     + u25(a,n,b,l,k,i)*t2a(d,c,n,j)          !anblkidcnj      (+0.500)
     &     + u25(b,n,c,l,k,i)*t2a(d,a,n,j)          !bnclkidanj      (+0.500)
     &     - u25(a,n,c,l,k,i)*t2a(d,b,n,j)          !anclkidbnj      (-0.500)
     &     + u25(c,n,d,l,k,i)*t2a(b,a,n,j)          !cndlkibanj      (+0.500)
     &     - u25(b,n,d,l,k,i)*t2a(c,a,n,j)          !bndlkicanj      (-0.500)
     &     + u25(a,n,d,l,k,i)*t2a(c,b,n,j)          !andlkicbnj      (+0.500)
     &     + u25(d,n,a,l,j,i)*t2a(c,b,n,k)          !dnaljicbnk      (+0.500)
     &     - u25(d,n,b,l,j,i)*t2a(c,a,n,k)          !dnbljicank      (-0.500)
     &     + u25(d,n,c,l,j,i)*t2a(b,a,n,k)          !dncljibank      (+0.500)
     &     - u25(c,n,a,l,j,i)*t2a(d,b,n,k)          !cnaljidbnk      (-0.500)
     &     + u25(c,n,b,l,j,i)*t2a(d,a,n,k)          !cnbljidank      (+0.500)
     &     + u25(b,n,a,l,j,i)*t2a(d,c,n,k)          !bnaljidcnk      (+0.500)
     &     - u25(a,n,b,l,j,i)*t2a(d,c,n,k)          !anbljidcnk      (-0.500)
     &     - u25(b,n,c,l,j,i)*t2a(d,a,n,k)          !bncljidank      (-0.500)
     &     + u25(a,n,c,l,j,i)*t2a(d,b,n,k)          !ancljidbnk      (+0.500)
     &     - u25(c,n,d,l,j,i)*t2a(b,a,n,k)          !cndljibank      (-0.500)
     &     + u25(b,n,d,l,j,i)*t2a(c,a,n,k)          !bndljicank      (+0.500)
     &     - u25(a,n,d,l,j,i)*t2a(c,b,n,k)          !andljicbnk      (-0.500)
     &     - u25(d,n,a,k,j,i)*t2a(c,b,n,l)          !dnakjicbnl      (-0.500)
     &     + u25(d,n,b,k,j,i)*t2a(c,a,n,l)          !dnbkjicanl      (+0.500)
     &     - u25(d,n,c,k,j,i)*t2a(b,a,n,l)          !dnckjibanl      (-0.500)
     &     + u25(c,n,a,k,j,i)*t2a(d,b,n,l)          !cnakjidbnl      (+0.500)
     &     - u25(c,n,b,k,j,i)*t2a(d,a,n,l)          !cnbkjidanl      (-0.500)
     &     - u25(b,n,a,k,j,i)*t2a(d,c,n,l)          !bnakjidcnl      (-0.500)
     &     + u25(a,n,b,k,j,i)*t2a(d,c,n,l)          !anbkjidcnl      (+0.500)
     &     + u25(b,n,c,k,j,i)*t2a(d,a,n,l)          !bnckjidanl      (+0.500)
     &     - u25(a,n,c,k,j,i)*t2a(d,b,n,l)          !anckjidbnl      (-0.500)
     &     + u25(c,n,d,k,j,i)*t2a(b,a,n,l)          !cndkjibanl      (+0.500)
     &     - u25(b,n,d,k,j,i)*t2a(c,a,n,l)          !bndkjicanl      (-0.500)
     &     + u25(a,n,d,k,j,i)*t2a(c,b,n,l))/2.0d0   !andkjicbnl      (+0.500)       
             enddo
             v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call
!     & sum23814567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z87, 0.500)
!       call
!     & sum24813567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z87,-0.500)
!       call
!     & sum34812567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z87, 0.500)
!       call
!     & sum13824567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z87,-0.500)
!       call
!     & sum14823567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z87, 0.500)
!       call
!     & sum12834567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z87, 0.500)
!       call
!     & sum12843567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z87,-0.500)
!       call
!     & sum14832567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z87,-0.500)
!       call
!     & sum13842567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z87, 0.500)
!       call
!     & sum34821567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z87,-0.500)
!       call
!     & sum24831567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z87, 0.500)
!       call
!     & sum23841567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z87,-0.500)
!       call
!     & sum23714568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z87,-0.500)
!       call
!     & sum24713568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z87, 0.500)
!       call
!     & sum34712568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z87,-0.500)
!       call
!     & sum13724568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z87, 0.500)
!       call
!     & sum14723568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z87,-0.500)
!       call
!     & sum12734568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z87,-0.500)
!       call
!     & sum12743568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z87, 0.500)
!       call
!     & sum14732568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z87, 0.500)
!       call
!     & sum13742568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z87,-0.500)
!       call
!     & sum34721568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z87, 0.500)
!       call
!     & sum24731568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z87,-0.500)
!       call
!     & sum23741568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z87, 0.500)
!       call
!     & sum23614578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z87, 0.500)
!       call
!     & sum24613578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z87,-0.500)
!       call
!     & sum34612578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z87, 0.500)
!       call
!     & sum13624578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z87,-0.500)
!       call
!     & sum14623578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z87, 0.500)
!       call
!     & sum12634578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z87, 0.500)
!       call
!     & sum12643578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z87,-0.500)
!       call
!     & sum14632578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z87,-0.500)
!       call
!     & sum13642578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z87, 0.500)
!       call
!     & sum34621578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z87,-0.500)
!       call
!     & sum24631578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z87, 0.500)
!       call
!     & sum23641578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z87,-0.500)
!       call
!     & sum23514678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z87,-0.500)
!       call
!     & sum24513678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z87, 0.500)
!       call
!     & sum34512678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z87,-0.500)
!       call
!     & sum13524678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z87, 0.500)
!       call
!     & sum14523678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z87,-0.500)
!       call
!     & sum12534678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z87,-0.500)
!       call
!     & sum12543678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z87, 0.500)
!       call
!     & sum14532678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z87, 0.500)
!       call
!     & sum13542678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z87,-0.500)
!       call
!     & sum34521678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z87, 0.500)
!       call
!     & sum24531678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z87,-0.500)
!       call
!     & sum23541678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z87, 0.500)
!c
!       do i=n0+1,n1-3
!       do j=i+1,n1-2
!       do k=j+1,n1-1
!       do l=k+1,n1
!       do a=n1+1,n3-3
!       do b=a+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)
!     & +(z87(c,b,i,d,a,l,k,j)      ! 23814567 (+0.500)
!     & -z87(c,a,i,d,b,l,k,j)       ! 24813567 (-0.500)
!     & +z87(b,a,i,d,c,l,k,j)       ! 34812567 (+0.500)
!     & -z87(d,b,i,c,a,l,k,j)       ! 13824567 (-0.500)
!     & +z87(d,a,i,c,b,l,k,j)       ! 14823567 (+0.500)
!     & +z87(d,c,i,b,a,l,k,j)       ! 12834567 (+0.500)
!     & -z87(d,c,i,a,b,l,k,j)       ! 12843567 (-0.500)
!     & -z87(d,a,i,b,c,l,k,j)       ! 14832567 (-0.500)
!     & +z87(d,b,i,a,c,l,k,j)       ! 13842567 (+0.500)
!     & -z87(b,a,i,c,d,l,k,j)       ! 34821567 (-0.500)
!     & +z87(c,a,i,b,d,l,k,j)       ! 24831567 (+0.500)
!     & -z87(c,b,i,a,d,l,k,j)       ! 23841567 (-0.500)
!     & -z87(c,b,j,d,a,l,k,i)       ! 23714568 (-0.500)
!     & +z87(c,a,j,d,b,l,k,i)       ! 24713568 (+0.500)
!     & -z87(b,a,j,d,c,l,k,i)       ! 34712568 (-0.500)
!     & +z87(d,b,j,c,a,l,k,i)       ! 13724568 (+0.500)
!     & -z87(d,a,j,c,b,l,k,i)       ! 14723568 (-0.500)
!     & -z87(d,c,j,b,a,l,k,i)       ! 12734568 (-0.500)
!     & +z87(d,c,j,a,b,l,k,i)       ! 12743568 (+0.500)
!     & +z87(d,a,j,b,c,l,k,i)       ! 14732568 (+0.500)
!     & -z87(d,b,j,a,c,l,k,i)       ! 13742568 (-0.500)
!     & +z87(b,a,j,c,d,l,k,i)       ! 34721568 (+0.500)
!     & -z87(c,a,j,b,d,l,k,i)       ! 24731568 (-0.500)
!     & +z87(c,b,j,a,d,l,k,i)       ! 23741568 (+0.500)
!     & +z87(c,b,k,d,a,l,j,i)       ! 23614578 (+0.500)
!     & -z87(c,a,k,d,b,l,j,i)       ! 24613578 (-0.500)
!     & +z87(b,a,k,d,c,l,j,i)       ! 34612578 (+0.500)
!     & -z87(d,b,k,c,a,l,j,i)       ! 13624578 (-0.500)
!     & +z87(d,a,k,c,b,l,j,i)       ! 14623578 (+0.500)
!     & +z87(d,c,k,b,a,l,j,i)       ! 12634578 (+0.500)
!     & -z87(d,c,k,a,b,l,j,i)       ! 12643578 (-0.500)
!     & -z87(d,a,k,b,c,l,j,i)       ! 14632578 (-0.500)
!     & +z87(d,b,k,a,c,l,j,i)       ! 13642578 (+0.500)
!     & -z87(b,a,k,c,d,l,j,i)       ! 34621578 (-0.500)
!     & +z87(c,a,k,b,d,l,j,i)       ! 24631578 (+0.500)
!     & -z87(c,b,k,a,d,l,j,i)       ! 23641578 (-0.500)
!     & -z87(c,b,l,d,a,k,j,i)       ! 23514678 (-0.500)
!     & +z87(c,a,l,d,b,k,j,i)       ! 24513678 (+0.500)
!     & -z87(b,a,l,d,c,k,j,i)       ! 34512678 (-0.500)
!     & +z87(d,b,l,c,a,k,j,i)       ! 13524678 (+0.500)
!     & -z87(d,a,l,c,b,k,j,i)       ! 14523678 (-0.500)
!     & -z87(d,c,l,b,a,k,j,i)       ! 12534678 (-0.500)
!     & +z87(d,c,l,a,b,k,j,i)       ! 12543678 (+0.500)
!     & +z87(d,a,l,b,c,k,j,i)       ! 14532678 (+0.500)
!     & -z87(d,b,l,a,c,k,j,i)       ! 13542678 (-0.500)
!     & +z87(b,a,l,c,d,k,j,i)       ! 34521678 (+0.500)
!     & -z87(c,a,l,b,d,k,j,i)       ! 24531678 (-0.500)
!     & +z87(c,b,l,a,d,k,j,i))/2.0d0! 23541678 (+0.500)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z87)
       deallocate(u25)
c
       allocate(d1(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder4312(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n1,n3,n0,n1,n0,n1,intr,d1)
       allocate(f2(n1+1:n3,n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder124356(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n1,n3,n0,n1,n0,n1,t3a,f2)
       allocate(s26(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1
       i2=k1*k1*k3
       i3=k1*k3*k3
       call egemm(i1,i2,i3,d1,f2,s26)
       deallocate(d1)
       deallocate(f2)
c
!       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
!       call reorder4123(n1,n3,n0,n1,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n0,n1,n0,n1,s26,d1)
!       allocate(f2(n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!       call reorder412356(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,t3a,f2)
!       allocate(z58(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
!       i1=k1*k1*k3
!       i2=k1*k1*k3*k3*k3
!       i3=k1
!       call egemm(i1,i2,i3,d1,f2,z58)
!       deallocate(d1)
!       deallocate(f2)
c
       do i=n0+1,n1-3;do j=i+1,n1-2;do k=j+1,n1-1;do l=k+1,n1
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-3;do b=a+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
          if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do n=n0+1,n1
             sum=sum
     &     + (s26(a,l,k,n)*t3a(d,c,b,n,j,i)         !alkndcbnji      (+0.500)
     &     - s26(b,l,k,n)*t3a(d,c,a,n,j,i)          !blkndcanji      (-0.500)
     &     + s26(c,l,k,n)*t3a(d,b,a,n,j,i)          !clkndbanji      (+0.500)
     &     - s26(d,j,i,n)*t3a(c,b,a,n,l,k)          !djincbanlk      (-0.500)
     &     - s26(d,l,k,n)*t3a(c,b,a,n,j,i)          !dlkncbanji      (-0.500)
     &     + s26(c,j,i,n)*t3a(d,b,a,n,l,k)          !cjindbanlk      (+0.500)
     &     - s26(b,j,i,n)*t3a(d,c,a,n,l,k)          !bjindcanlk      (-0.500)
     &     + s26(a,j,i,n)*t3a(d,c,b,n,l,k)          !ajindcbnlk      (+0.500)
     &     - s26(a,l,j,n)*t3a(d,c,b,n,k,i)          !aljndcbnki      (-0.500)
     &     + s26(b,l,j,n)*t3a(d,c,a,n,k,i)          !bljndcanki      (+0.500)
     &     - s26(c,l,j,n)*t3a(d,b,a,n,k,i)          !cljndbanki      (-0.500)
     &     + s26(d,k,i,n)*t3a(c,b,a,n,l,j)          !dkincbanlj      (+0.500)
     &     + s26(d,l,j,n)*t3a(c,b,a,n,k,i)          !dljncbanki      (+0.500)
     &     - s26(c,k,i,n)*t3a(d,b,a,n,l,j)          !ckindbanlj      (-0.500)
     &     + s26(b,k,i,n)*t3a(d,c,a,n,l,j)          !bkindcanlj      (+0.500)
     &     - s26(a,k,i,n)*t3a(d,c,b,n,l,j)          !akindcbnlj      (-0.500)
     &     + s26(a,k,j,n)*t3a(d,c,b,n,l,i)          !akjndcbnli      (+0.500)
     &     - s26(b,k,j,n)*t3a(d,c,a,n,l,i)          !bkjndcanli      (-0.500)
     &     + s26(c,k,j,n)*t3a(d,b,a,n,l,i)          !ckjndbanli      (+0.500)
     &     - s26(d,l,i,n)*t3a(c,b,a,n,k,j)          !dlincbankj      (-0.500)
     &     - s26(d,k,j,n)*t3a(c,b,a,n,l,i)          !dkjncbanli      (-0.500)
     &     + s26(c,l,i,n)*t3a(d,b,a,n,k,j)          !clindbankj      (+0.500)
     &     - s26(b,l,i,n)*t3a(d,c,a,n,k,j)          !blindcankj      (-0.500)
     &     + s26(a,l,i,n)*t3a(d,c,b,n,k,j))/2.0d0   !alindcbnkj      (+0.500)       
             enddo
             v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call
!     & sum12378456(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z58, 0.500)
!       call
!     & sum12478356(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z58,-0.500)
!       call
!     & sum13478256(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z58, 0.500)
!       call
!     & sum23456178(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z58,-0.500)
!       call
!     & sum23478156(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z58,-0.500)
!       call
!     & sum13456278(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z58, 0.500)
!       call
!     & sum12456378(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z58,-0.500)
!       call
!     & sum12356478(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z58, 0.500)
!       call
!     & sum12368457(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z58,-0.500)
!       call
!     & sum12468357(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z58, 0.500)
!       call
!     & sum13468257(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z58,-0.500)
!       call
!     & sum23457168(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z58, 0.500)
!       call
!     & sum23468157(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z58, 0.500)
!       call
!     & sum13457268(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z58,-0.500)
!       call
!     & sum12457368(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z58, 0.500)
!       call
!     & sum12357468(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z58,-0.500)
!       call
!     & sum12358467(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z58, 0.500)
!       call
!     & sum12458367(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z58,-0.500)
!       call
!     & sum13458267(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z58, 0.500)
!       call
!     & sum23467158(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z58,-0.500)
!       call
!     & sum23458167(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z58,-0.500)
!       call
!     & sum13467258(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z58, 0.500)
!       call
!     & sum12467358(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z58,-0.500)
!       call
!     & sum12367458(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z58, 0.500)
!c
!       do i=n0+1,n1-3
!       do j=i+1,n1-2
!       do k=j+1,n1-1
!       do l=k+1,n1
!       do a=n1+1,n3-3
!       do b=a+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)
!     & +(z58(d,c,b,j,i,a,l,k)      ! 12378456 (+0.500)
!     & -z58(d,c,a,j,i,b,l,k)       ! 12478356 (-0.500)
!     & +z58(d,b,a,j,i,c,l,k)       ! 13478256 (+0.500)
!     & -z58(c,b,a,l,k,d,j,i)       ! 23456178 (-0.500)
!     & -z58(c,b,a,j,i,d,l,k)       ! 23478156 (-0.500)
!     & +z58(d,b,a,l,k,c,j,i)       ! 13456278 (+0.500)
!     & -z58(d,c,a,l,k,b,j,i)       ! 12456378 (-0.500)
!     & +z58(d,c,b,l,k,a,j,i)       ! 12356478 (+0.500)
!     & -z58(d,c,b,k,i,a,l,j)       ! 12368457 (-0.500)
!     & +z58(d,c,a,k,i,b,l,j)       ! 12468357 (+0.500)
!     & -z58(d,b,a,k,i,c,l,j)       ! 13468257 (-0.500)
!     & +z58(c,b,a,l,j,d,k,i)       ! 23457168 (+0.500)
!     & +z58(c,b,a,k,i,d,l,j)       ! 23468157 (+0.500)
!     & -z58(d,b,a,l,j,c,k,i)       ! 13457268 (-0.500)
!     & +z58(d,c,a,l,j,b,k,i)       ! 12457368 (+0.500)
!     & -z58(d,c,b,l,j,a,k,i)       ! 12357468 (-0.500)
!     & +z58(d,c,b,l,i,a,k,j)       ! 12358467 (+0.500)
!     & -z58(d,c,a,l,i,b,k,j)       ! 12458367 (-0.500)
!     & +z58(d,b,a,l,i,c,k,j)       ! 13458267 (+0.500)
!     & -z58(c,b,a,k,j,d,l,i)       ! 23467158 (-0.500)
!     & -z58(c,b,a,l,i,d,k,j)       ! 23458167 (-0.500)
!     & +z58(d,b,a,k,j,c,l,i)       ! 13467258 (+0.500)
!     & -z58(d,c,a,k,j,b,l,i)       ! 12467358 (-0.500)
!     & +z58(d,c,b,k,j,a,l,i))/2.0d0! 12367458 (+0.500)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z58)
       deallocate(s26)
c
       allocate(d1(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       call reorder3142(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n1,n3,n0,n1,intr,d1)
       allocate(f2(n1+1:n3,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder142356(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,
     & n1,n3,n0,n1,n1,n3,n1,n3,n0,n1,n0,n1,t3a,f2)
       allocate(u15(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1))
       i1=k1*k3
       i2=k1*k1*k3*k3
       i3=k1*k3
       call egemm(i1,i2,i3,d1,f2,u15)
       deallocate(d1)
       deallocate(f2)
c
!       allocate(f1(n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!       call reorder651234(n1,n3,n1,n3,n0,n1,n0,n1,n1,n3,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,u15,f1)
!       allocate(f2(n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!       call reorder412356(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,t3a,f2)
!       allocate(z59(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!       i1=k1*k1*k3*k3
!       i2=k1*k1*k3*k3
!       i3=k3*k1
!       call egemm(i1,i2,i3,f1,f2,z59)
!       deallocate(f1)
!       deallocate(f2)
!c
       do i=n0+1,n1-3;do j=i+1,n1-2;do k=j+1,n1-1;do l=k+1,n1
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-3;do b=a+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
          if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n1+1,n3;do n=n0+1,n1
             sum=sum
     &     + u15(b,a,l,k,f,n)*t3a(f,d,c,n,j,i)      !balkfnfdcnji    (+1.000)
     &     - u15(c,a,l,k,f,n)*t3a(f,d,b,n,j,i)      !calkfnfdbnji    (-1.000)
     &     + u15(d,a,j,i,f,n)*t3a(f,c,b,n,l,k)      !dajifnfcbnlk    (+1.000)
     &     + u15(d,a,l,k,f,n)*t3a(f,c,b,n,j,i)      !dalkfnfcbnji    (+1.000)
     &     - u15(c,a,j,i,f,n)*t3a(f,d,b,n,l,k)      !cajifnfdbnlk    (-1.000)
     &     + u15(b,a,j,i,f,n)*t3a(f,d,c,n,l,k)      !bajifnfdcnlk    (+1.000)
     &     - u15(b,a,l,j,f,n)*t3a(f,d,c,n,k,i)      !baljfnfdcnki    (-1.000)
     &     + u15(c,a,l,j,f,n)*t3a(f,d,b,n,k,i)      !caljfnfdbnki    (+1.000)
     &     - u15(d,a,k,i,f,n)*t3a(f,c,b,n,l,j)      !dakifnfcbnlj    (-1.000)
     &     - u15(d,a,l,j,f,n)*t3a(f,c,b,n,k,i)      !daljfnfcbnki    (-1.000)
     &     + u15(c,a,k,i,f,n)*t3a(f,d,b,n,l,j)      !cakifnfdbnlj    (+1.000)
     &     - u15(b,a,k,i,f,n)*t3a(f,d,c,n,l,j)      !bakifnfdcnlj    (-1.000)
     &     + u15(b,a,k,j,f,n)*t3a(f,d,c,n,l,i)      !bakjfnfdcnli    (+1.000)
     &     - u15(c,a,k,j,f,n)*t3a(f,d,b,n,l,i)      !cakjfnfdbnli    (-1.000)
     &     + u15(d,a,l,i,f,n)*t3a(f,c,b,n,k,j)      !dalifnfcbnkj    (+1.000)
     &     + u15(d,a,k,j,f,n)*t3a(f,c,b,n,l,i)      !dakjfnfcbnli    (+1.000)
     &     - u15(c,a,l,i,f,n)*t3a(f,d,b,n,k,j)      !califnfdbnkj    (-1.000)
     &     + u15(b,a,l,i,f,n)*t3a(f,d,c,n,k,j)      !balifnfdcnkj    (+1.000)       
             enddo;enddo
             v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call
!     & sum12783456(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z59, 1.000)
!       call
!     & sum13782456(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z59,-1.000)
!       call
!     & sum23561478(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z59, 1.000)
!       call
!     & sum23781456(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z59, 1.000)
!       call
!     & sum13562478(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z59,-1.000)
!       call
!     & sum12563478(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z59, 1.000)
!       call
!     & sum12683457(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z59,-1.000)
!       call
!     & sum13682457(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z59, 1.000)
!       call
!     & sum23571468(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z59,-1.000)
!       call
!     & sum23681457(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z59,-1.000)
!       call
!     & sum13572468(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z59, 1.000)
!       call
!     & sum12573468(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z59,-1.000)
!       call
!     & sum12583467(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z59, 1.000)
!       call
!     & sum13582467(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z59,-1.000)
!       call
!     & sum23671458(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z59, 1.000)
!       call
!     & sum23581467(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z59, 1.000)
!       call
!     & sum13672458(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z59,-1.000)
!       call
!     & sum12673458(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z59, 1.000)
!c
!       do i=n0+1,n1-3
!       do j=i+1,n1-2
!       do k=j+1,n1-1
!       do l=k+1,n1
!       do a=n1+1,n3-3
!       do b=a+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)
!     & +z59(d,c,j,i,b,a,l,k)       ! 12783456 (+1.000)
!     & -z59(d,b,j,i,c,a,l,k)       ! 13782456 (-1.000)
!     & +z59(c,b,l,k,d,a,j,i)       ! 23561478 (+1.000)
!     & +z59(c,b,j,i,d,a,l,k)       ! 23781456 (+1.000)
!     & -z59(d,b,l,k,c,a,j,i)       ! 13562478 (-1.000)
!     & +z59(d,c,l,k,b,a,j,i)       ! 12563478 (+1.000)
!     & -z59(d,c,k,i,b,a,l,j)       ! 12683457 (-1.000)
!     & +z59(d,b,k,i,c,a,l,j)       ! 13682457 (+1.000)
!     & -z59(c,b,l,j,d,a,k,i)       ! 23571468 (-1.000)
!     & -z59(c,b,k,i,d,a,l,j)       ! 23681457 (-1.000)
!     & +z59(d,b,l,j,c,a,k,i)       ! 13572468 (+1.000)
!     & -z59(d,c,l,j,b,a,k,i)       ! 12573468 (-1.000)
!     & +z59(d,c,l,i,b,a,k,j)       ! 12583467 (+1.000)
!     & -z59(d,b,l,i,c,a,k,j)       ! 13582467 (-1.000)
!     & +z59(c,b,k,j,d,a,l,i)       ! 23671458 (+1.000)
!     & +z59(c,b,l,i,d,a,k,j)       ! 23581467 (+1.000)
!     & -z59(d,b,k,j,c,a,l,i)       ! 13672458 (-1.000)
!     & +z59(d,c,k,j,b,a,l,i)       ! 12673458 (+1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z59)
c
       allocate(f1(n1+1:n3,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder561234(n1,n3,n1,n3,n0,n1,n0,n1,n1,n3,n0,n1,
     & n1,n3,n0,n1,n1,n3,n1,n3,n0,n1,n0,n1,u15,f1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(u23(n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       i1=k1*k1*k3*k3*k1
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,f1,b2,u23)
       deallocate(f1)
       deallocate(b2)
       deallocate(u15)
c
!       allocate(f1(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
!       call reorder234561(n0,n1,n0,n1,n1,n3,n1,n3,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,u23,f1)
!       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
!       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
!       allocate(z81(n1+1:n3,n1+1:n3,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
!       i1=k1*k1*k1*k3*k3
!       i2=k1*k3*k3
!       i3=k1
!       call egemm(i1,i2,i3,f1,d2,z81)
!       deallocate(f1)
!       deallocate(d2)
c
       do i=n0+1,n1-3;do j=i+1,n1-2;do k=j+1,n1-1;do l=k+1,n1
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-3;do b=a+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
          if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1
             sum=sum
     &     + u23(i,m,b,a,l,k)*t2a(d,c,m,j)          !imbalkdcmj      (+1.000)
     &     - u23(i,m,c,a,l,k)*t2a(d,b,m,j)          !imcalkdbmj      (-1.000)
     &     + u23(i,m,c,b,l,k)*t2a(d,a,m,j)          !imcblkdamj      (+1.000)
     &     + u23(i,m,d,a,l,k)*t2a(c,b,m,j)          !imdalkcbmj      (+1.000)
     &     - u23(i,m,d,b,l,k)*t2a(c,a,m,j)          !imdblkcamj      (-1.000)
     &     + u23(i,m,d,c,l,k)*t2a(b,a,m,j)          !imdclkbamj      (+1.000)
     &     - u23(i,m,b,a,l,j)*t2a(d,c,m,k)          !imbaljdcmk      (-1.000)
     &     + u23(i,m,c,a,l,j)*t2a(d,b,m,k)          !imcaljdbmk      (+1.000)
     &     - u23(i,m,c,b,l,j)*t2a(d,a,m,k)          !imcbljdamk      (-1.000)
     &     - u23(i,m,d,a,l,j)*t2a(c,b,m,k)          !imdaljcbmk      (-1.000)
     &     + u23(i,m,d,b,l,j)*t2a(c,a,m,k)          !imdbljcamk      (+1.000)
     &     - u23(i,m,d,c,l,j)*t2a(b,a,m,k)          !imdcljbamk      (-1.000)
     &     + u23(i,m,b,a,k,j)*t2a(d,c,m,l)          !imbakjdcml      (+1.000)
     &     - u23(i,m,c,a,k,j)*t2a(d,b,m,l)          !imcakjdbml      (-1.000)
     &     + u23(i,m,c,b,k,j)*t2a(d,a,m,l)          !imcbkjdaml      (+1.000)
     &     + u23(i,m,d,a,k,j)*t2a(c,b,m,l)          !imdakjcbml      (+1.000)
     &     - u23(i,m,d,b,k,j)*t2a(c,a,m,l)          !imdbkjcaml      (-1.000)
     &     + u23(i,m,d,c,k,j)*t2a(b,a,m,l)          !imdckjbaml      (+1.000)
     &     - u23(j,m,b,a,l,k)*t2a(d,c,m,i)          !jmbalkdcmi      (-1.000)
     &     + u23(j,m,c,a,l,k)*t2a(d,b,m,i)          !jmcalkdbmi      (+1.000)
     &     - u23(j,m,c,b,l,k)*t2a(d,a,m,i)          !jmcblkdami      (-1.000)
     &     - u23(j,m,d,a,l,k)*t2a(c,b,m,i)          !jmdalkcbmi      (-1.000)
     &     + u23(j,m,d,b,l,k)*t2a(c,a,m,i)          !jmdblkcami      (+1.000)
     &     - u23(j,m,d,c,l,k)*t2a(b,a,m,i)          !jmdclkbami      (-1.000)
     &     + u23(k,m,b,a,l,j)*t2a(d,c,m,i)          !kmbaljdcmi      (+1.000)
     &     - u23(k,m,c,a,l,j)*t2a(d,b,m,i)          !kmcaljdbmi      (-1.000)
     &     + u23(k,m,c,b,l,j)*t2a(d,a,m,i)          !kmcbljdami      (+1.000)
     &     + u23(k,m,d,a,l,j)*t2a(c,b,m,i)          !kmdaljcbmi      (+1.000)
     &     - u23(k,m,d,b,l,j)*t2a(c,a,m,i)          !kmdbljcami      (-1.000)
     &     + u23(k,m,d,c,l,j)*t2a(b,a,m,i)          !kmdcljbami      (+1.000)
     &     - u23(l,m,b,a,k,j)*t2a(d,c,m,i)          !lmbakjdcmi      (-1.000)
     &     + u23(l,m,c,a,k,j)*t2a(d,b,m,i)          !lmcakjdbmi      (+1.000)
     &     - u23(l,m,c,b,k,j)*t2a(d,a,m,i)          !lmcbkjdami      (-1.000)
     &     - u23(l,m,d,a,k,j)*t2a(c,b,m,i)          !lmdakjcbmi      (-1.000)
     &     + u23(l,m,d,b,k,j)*t2a(c,a,m,i)          !lmdbkjcami      (+1.000)
     &     - u23(l,m,d,c,k,j)*t2a(b,a,m,i)          !lmdckjbami      (-1.000)
     &     + u23(j,m,b,a,l,i)*t2a(d,c,m,k)          !jmbalidcmk      (+1.000)
     &     - u23(j,m,c,a,l,i)*t2a(d,b,m,k)          !jmcalidbmk      (-1.000)
     &     + u23(j,m,c,b,l,i)*t2a(d,a,m,k)          !jmcblidamk      (+1.000)
     &     + u23(j,m,d,a,l,i)*t2a(c,b,m,k)          !jmdalicbmk      (+1.000)
     &     - u23(j,m,d,b,l,i)*t2a(c,a,m,k)          !jmdblicamk      (-1.000)
     &     + u23(j,m,d,c,l,i)*t2a(b,a,m,k)          !jmdclibamk      (+1.000)
     &     - u23(j,m,b,a,k,i)*t2a(d,c,m,l)          !jmbakidcml      (-1.000)
     &     + u23(j,m,c,a,k,i)*t2a(d,b,m,l)          !jmcakidbml      (+1.000)
     &     - u23(j,m,c,b,k,i)*t2a(d,a,m,l)          !jmcbkidaml      (-1.000)
     &     - u23(j,m,d,a,k,i)*t2a(c,b,m,l)          !jmdakicbml      (-1.000)
     &     + u23(j,m,d,b,k,i)*t2a(c,a,m,l)          !jmdbkicaml      (+1.000)
     &     - u23(j,m,d,c,k,i)*t2a(b,a,m,l)          !jmdckibaml      (-1.000)
     &     - u23(k,m,b,a,l,i)*t2a(d,c,m,j)          !kmbalidcmj      (-1.000)
     &     + u23(k,m,c,a,l,i)*t2a(d,b,m,j)          !kmcalidbmj      (+1.000)
     &     - u23(k,m,c,b,l,i)*t2a(d,a,m,j)          !kmcblidamj      (-1.000)
     &     - u23(k,m,d,a,l,i)*t2a(c,b,m,j)          !kmdalicbmj      (-1.000)
     &     + u23(k,m,d,b,l,i)*t2a(c,a,m,j)          !kmdblicamj      (+1.000)
     &     - u23(k,m,d,c,l,i)*t2a(b,a,m,j)          !kmdclibamj      (-1.000)
     &     + u23(l,m,b,a,k,i)*t2a(d,c,m,j)          !lmbakidcmj      (+1.000)
     &     - u23(l,m,c,a,k,i)*t2a(d,b,m,j)          !lmcakidbmj      (-1.000)
     &     + u23(l,m,c,b,k,i)*t2a(d,a,m,j)          !lmcbkidamj      (+1.000)
     &     + u23(l,m,d,a,k,i)*t2a(c,b,m,j)          !lmdakicbmj      (+1.000)
     &     - u23(l,m,d,b,k,i)*t2a(c,a,m,j)          !lmdbkicamj      (-1.000)
     &     + u23(l,m,d,c,k,i)*t2a(b,a,m,j)          !lmdckibamj      (+1.000)
     &     + u23(k,m,b,a,j,i)*t2a(d,c,m,l)          !kmbajidcml      (+1.000)
     &     - u23(k,m,c,a,j,i)*t2a(d,b,m,l)          !kmcajidbml      (-1.000)
     &     + u23(k,m,c,b,j,i)*t2a(d,a,m,l)          !kmcbjidaml      (+1.000)
     &     + u23(k,m,d,a,j,i)*t2a(c,b,m,l)          !kmdajicbml      (+1.000)
     &     - u23(k,m,d,b,j,i)*t2a(c,a,m,l)          !kmdbjicaml      (-1.000)
     &     + u23(k,m,d,c,j,i)*t2a(b,a,m,l)          !kmdcjibaml      (+1.000)
     &     - u23(l,m,b,a,j,i)*t2a(d,c,m,k)          !lmbajidcmk      (-1.000)
     &     + u23(l,m,c,a,j,i)*t2a(d,b,m,k)          !lmcajidbmk      (+1.000)
     &     - u23(l,m,c,b,j,i)*t2a(d,a,m,k)          !lmcbjidamk      (-1.000)
     &     - u23(l,m,d,a,j,i)*t2a(c,b,m,k)          !lmdajicbmk      (-1.000)
     &     + u23(l,m,d,b,j,i)*t2a(c,a,m,k)          !lmdbjicamk      (+1.000)
     &     - u23(l,m,d,c,j,i)*t2a(b,a,m,k)          !lmdcjibamk      (-1.000)       
             enddo
             v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call
!     & sum12734568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z81, 1.000)
!       call
!     & sum13724568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z81,-1.000)
!       call
!     & sum14723568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z81, 1.000)
!       call
!     & sum23714568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z81, 1.000)
!       call
!     & sum24713568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z81,-1.000)
!       call
!     & sum34712568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z81, 1.000)
!       call
!     & sum12634578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z81,-1.000)
!       call
!     & sum13624578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z81, 1.000)
!       call
!     & sum14623578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z81,-1.000)
!       call
!     & sum23614578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z81,-1.000)
!       call
!     & sum24613578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z81, 1.000)
!       call
!     & sum34612578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z81,-1.000)
!       call
!     & sum12534678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z81, 1.000)
!       call
!     & sum13524678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z81,-1.000)
!       call
!     & sum14523678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z81, 1.000)
!       call
!     & sum23514678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z81, 1.000)
!       call
!     & sum24513678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z81,-1.000)
!       call
!     & sum34512678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z81, 1.000)
!       call
!     & sum12834567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z81,-1.000)
!       call
!     & sum13824567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z81, 1.000)
!       call
!     & sum14823567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z81,-1.000)
!       call
!     & sum23814567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z81,-1.000)
!       call
!     & sum24813567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z81, 1.000)
!       call
!     & sum34812567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z81,-1.000)
!       call
!     & sum12834576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z81, 1.000)
!       call
!     & sum13824576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z81,-1.000)
!       call
!     & sum14823576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z81, 1.000)
!       call
!     & sum23814576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z81, 1.000)
!       call
!     & sum24813576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z81,-1.000)
!       call
!     & sum34812576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z81, 1.000)
!       call
!     & sum12834675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z81,-1.000)
!       call
!     & sum13824675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z81, 1.000)
!       call
!     & sum14823675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z81,-1.000)
!       call
!     & sum23814675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z81,-1.000)
!       call
!     & sum24813675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z81, 1.000)
!       call
!     & sum34812675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z81,-1.000)
!       call
!     & sum12634587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z81, 1.000)
!       call
!     & sum13624587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z81,-1.000)
!       call
!     & sum14623587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z81, 1.000)
!       call
!     & sum23614587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z81, 1.000)
!       call
!     & sum24613587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z81,-1.000)
!       call
!     & sum34612587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z81, 1.000)
!       call
!     & sum12534687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z81,-1.000)
!       call
!     & sum13524687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z81, 1.000)
!       call
!     & sum14523687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z81,-1.000)
!       call
!     & sum23514687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z81,-1.000)
!       call
!     & sum24513687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z81, 1.000)
!       call
!     & sum34512687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z81,-1.000)
!       call
!     & sum12734586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z81,-1.000)
!       call
!     & sum13724586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z81, 1.000)
!       call
!     & sum14723586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z81,-1.000)
!       call
!     & sum23714586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z81,-1.000)
!       call
!     & sum24713586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z81, 1.000)
!       call
!     & sum34712586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z81,-1.000)
!       call
!     & sum12734685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z81, 1.000)
!       call
!     & sum13724685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z81,-1.000)
!       call
!     & sum14723685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z81, 1.000)
!       call
!     & sum23714685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z81, 1.000)
!       call
!     & sum24713685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z81,-1.000)
!       call
!     & sum34712685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z81, 1.000)
!       call
!     & sum12534786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z81, 1.000)
!       call
!     & sum13524786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z81,-1.000)
!       call
!     & sum14523786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z81, 1.000)
!       call
!     & sum23514786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z81, 1.000)
!       call
!     & sum24513786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z81,-1.000)
!       call
!     & sum34512786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z81, 1.000)
!       call
!     & sum12634785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z81,-1.000)
!       call
!     & sum13624785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z81, 1.000)
!       call
!     & sum14623785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z81,-1.000)
!       call
!     & sum23614785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z81,-1.000)
!       call
!     & sum24613785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z81, 1.000)
!       call
!     & sum34612785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z81,-1.000)
!c
!       do i=n0+1,n1-3
!       do j=i+1,n1-2
!       do k=j+1,n1-1
!       do l=k+1,n1
!       do a=n1+1,n3-3
!       do b=a+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)
!     & +z81(d,c,j,b,a,l,k,i)       ! 12734568 (+1.000)
!     & -z81(d,b,j,c,a,l,k,i)       ! 13724568 (-1.000)
!     & +z81(d,a,j,c,b,l,k,i)       ! 14723568 (+1.000)
!     & +z81(c,b,j,d,a,l,k,i)       ! 23714568 (+1.000)
!     & -z81(c,a,j,d,b,l,k,i)       ! 24713568 (-1.000)
!     & +z81(b,a,j,d,c,l,k,i)       ! 34712568 (+1.000)
!     & -z81(d,c,k,b,a,l,j,i)       ! 12634578 (-1.000)
!     & +z81(d,b,k,c,a,l,j,i)       ! 13624578 (+1.000)
!     & -z81(d,a,k,c,b,l,j,i)       ! 14623578 (-1.000)
!     & -z81(c,b,k,d,a,l,j,i)       ! 23614578 (-1.000)
!     & +z81(c,a,k,d,b,l,j,i)       ! 24613578 (+1.000)
!     & -z81(b,a,k,d,c,l,j,i)       ! 34612578 (-1.000)
!     & +z81(d,c,l,b,a,k,j,i)       ! 12534678 (+1.000)
!     & -z81(d,b,l,c,a,k,j,i)       ! 13524678 (-1.000)
!     & +z81(d,a,l,c,b,k,j,i)       ! 14523678 (+1.000)
!     & +z81(c,b,l,d,a,k,j,i)       ! 23514678 (+1.000)
!     & -z81(c,a,l,d,b,k,j,i)       ! 24513678 (-1.000)
!     & +z81(b,a,l,d,c,k,j,i)       ! 34512678 (+1.000)
!     & -z81(d,c,i,b,a,l,k,j)       ! 12834567 (-1.000)
!     & +z81(d,b,i,c,a,l,k,j)       ! 13824567 (+1.000)
!     & -z81(d,a,i,c,b,l,k,j)       ! 14823567 (-1.000)
!     & -z81(c,b,i,d,a,l,k,j)       ! 23814567 (-1.000)
!     & +z81(c,a,i,d,b,l,k,j)       ! 24813567 (+1.000)
!     & -z81(b,a,i,d,c,l,k,j)       ! 34812567 (-1.000)
!     & +z81(d,c,i,b,a,l,j,k)       ! 12834576 (+1.000)
!     & -z81(d,b,i,c,a,l,j,k)       ! 13824576 (-1.000)
!     & +z81(d,a,i,c,b,l,j,k)       ! 14823576 (+1.000)
!     & +z81(c,b,i,d,a,l,j,k)       ! 23814576 (+1.000)
!     & -z81(c,a,i,d,b,l,j,k)       ! 24813576 (-1.000)
!     & +z81(b,a,i,d,c,l,j,k)       ! 34812576 (+1.000)
!     & -z81(d,c,i,b,a,k,j,l)       ! 12834675 (-1.000)
!     & +z81(d,b,i,c,a,k,j,l)       ! 13824675 (+1.000)
!     & -z81(d,a,i,c,b,k,j,l)       ! 14823675 (-1.000)
!     & -z81(c,b,i,d,a,k,j,l)       ! 23814675 (-1.000)
!     & +z81(c,a,i,d,b,k,j,l)       ! 24813675 (+1.000)
!     & -z81(b,a,i,d,c,k,j,l)       ! 34812675 (-1.000)
!     & +z81(d,c,k,b,a,l,i,j)       ! 12634587 (+1.000)
!     & -z81(d,b,k,c,a,l,i,j)       ! 13624587 (-1.000)
!     & +z81(d,a,k,c,b,l,i,j)       ! 14623587 (+1.000)
!     & +z81(c,b,k,d,a,l,i,j)       ! 23614587 (+1.000)
!     & -z81(c,a,k,d,b,l,i,j)       ! 24613587 (-1.000)
!     & +z81(b,a,k,d,c,l,i,j)       ! 34612587 (+1.000)
!     & -z81(d,c,l,b,a,k,i,j)       ! 12534687 (-1.000)
!     & +z81(d,b,l,c,a,k,i,j)       ! 13524687 (+1.000)
!     & -z81(d,a,l,c,b,k,i,j)       ! 14523687 (-1.000)
!     & -z81(c,b,l,d,a,k,i,j)       ! 23514687 (-1.000)
!     & +z81(c,a,l,d,b,k,i,j)       ! 24513687 (+1.000)
!     & -z81(b,a,l,d,c,k,i,j)       ! 34512687 (-1.000)
!     & -z81(d,c,j,b,a,l,i,k)       ! 12734586 (-1.000)
!     & +z81(d,b,j,c,a,l,i,k)       ! 13724586 (+1.000)
!     & -z81(d,a,j,c,b,l,i,k)       ! 14723586 (-1.000)
!     & -z81(c,b,j,d,a,l,i,k)       ! 23714586 (-1.000)
!     & +z81(c,a,j,d,b,l,i,k)       ! 24713586 (+1.000)
!     & -z81(b,a,j,d,c,l,i,k)       ! 34712586 (-1.000)
!     & +z81(d,c,j,b,a,k,i,l)       ! 12734685 (+1.000)
!     & -z81(d,b,j,c,a,k,i,l)       ! 13724685 (-1.000)
!     & +z81(d,a,j,c,b,k,i,l)       ! 14723685 (+1.000)
!     & +z81(c,b,j,d,a,k,i,l)       ! 23714685 (+1.000)
!     & -z81(c,a,j,d,b,k,i,l)       ! 24713685 (-1.000)
!     & +z81(b,a,j,d,c,k,i,l)       ! 34712685 (+1.000)
!     & +z81(d,c,l,b,a,j,i,k)       ! 12534786 (+1.000)
!     & -z81(d,b,l,c,a,j,i,k)       ! 13524786 (-1.000)
!     & +z81(d,a,l,c,b,j,i,k)       ! 14523786 (+1.000)
!     & +z81(c,b,l,d,a,j,i,k)       ! 23514786 (+1.000)
!     & -z81(c,a,l,d,b,j,i,k)       ! 24513786 (-1.000)
!     & +z81(b,a,l,d,c,j,i,k)       ! 34512786 (+1.000)
!     & -z81(d,c,k,b,a,j,i,l)       ! 12634785 (-1.000)
!     & +z81(d,b,k,c,a,j,i,l)       ! 13624785 (+1.000)
!     & -z81(d,a,k,c,b,j,i,l)       ! 14623785 (-1.000)
!     & -z81(c,b,k,d,a,j,i,l)       ! 23614785 (-1.000)
!     & +z81(c,a,k,d,b,j,i,l)       ! 24613785 (+1.000)
!     & -z81(b,a,k,d,c,j,i,l)       ! 34612785 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z81)
       deallocate(u23)
c
       allocate(d1(n2+1:n3,n0+1:n1,n0+1:n2,n1+1:n3))
       call reorder4123(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n1,n0,n2,n1,n3,intm,d1)
       allocate(f2(n2+1:n3,n0+1:n1,n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder154236(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n2,n3,n0,n1,n0,n2,n1,n3,n1,n3,n0,n1,t3b,f2)
       allocate(s27(n1+1:n3,n1+1:n3,n0+1:n1,n1+1:n3))
       i1=k3
       i2=k1*k3*k3
       i3=k2*k1*k4
       call egemm(i1,i2,i3,d1,f2,s27)
       deallocate(d1)
       deallocate(f2)
c
!       allocate(d1(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
!       call reorder4123(n1,n3,n1,n3,n0,n1,n1,n3,
!     & n1,n3,n1,n3,n1,n3,n0,n1,s27,d1)
!       allocate(f2(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
!       call reorder123456(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,
!     & n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,t3a,f2)
!       allocate(z60(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
!       i1=k1*k3*k3
!       i2=k1*k1*k1*k3*k3
!       i3=k3
!       call egemm(i1,i2,i3,d1,f2,z60)
!       deallocate(d1)
!       deallocate(f2)
c
       do i=n0+1,n1-3;do j=i+1,n1-2;do k=j+1,n1-1;do l=k+1,n1
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-3;do b=a+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
          if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n1+1,n3
             sum=sum
     &     + s27(b,a,l,e)*t3a(e,d,c,k,j,i)          !baleedckji      (+1.000)
     &     - s27(c,a,l,e)*t3a(e,d,b,k,j,i)          !caleedbkji      (-1.000)
     &     + s27(c,b,l,e)*t3a(e,d,a,k,j,i)          !cbleedakji      (+1.000)
     &     + s27(d,a,l,e)*t3a(e,c,b,k,j,i)          !daleecbkji      (+1.000)
     &     - s27(d,b,l,e)*t3a(e,c,a,k,j,i)          !dbleecakji      (-1.000)
     &     + s27(d,c,l,e)*t3a(e,b,a,k,j,i)          !dcleebakji      (+1.000)
     &     - s27(b,a,k,e)*t3a(e,d,c,l,j,i)          !bakeedclji      (-1.000)
     &     + s27(c,a,k,e)*t3a(e,d,b,l,j,i)          !cakeedblji      (+1.000)
     &     - s27(c,b,k,e)*t3a(e,d,a,l,j,i)          !cbkeedalji      (-1.000)
     &     - s27(d,a,k,e)*t3a(e,c,b,l,j,i)          !dakeecblji      (-1.000)
     &     + s27(d,b,k,e)*t3a(e,c,a,l,j,i)          !dbkeecalji      (+1.000)
     &     - s27(d,c,k,e)*t3a(e,b,a,l,j,i)          !dckeebalji      (-1.000)
     &     + s27(b,a,j,e)*t3a(e,d,c,l,k,i)          !bajeedclki      (+1.000)
     &     - s27(c,a,j,e)*t3a(e,d,b,l,k,i)          !cajeedblki      (-1.000)
     &     + s27(c,b,j,e)*t3a(e,d,a,l,k,i)          !cbjeedalki      (+1.000)
     &     + s27(d,a,j,e)*t3a(e,c,b,l,k,i)          !dajeecblki      (+1.000)
     &     - s27(d,b,j,e)*t3a(e,c,a,l,k,i)          !dbjeecalki      (-1.000)
     &     + s27(d,c,j,e)*t3a(e,b,a,l,k,i)          !dcjeebalki      (+1.000)
     &     - s27(b,a,i,e)*t3a(e,d,c,l,k,j)          !baieedclkj      (-1.000)
     &     + s27(c,a,i,e)*t3a(e,d,b,l,k,j)          !caieedblkj      (+1.000)
     &     - s27(c,b,i,e)*t3a(e,d,a,l,k,j)          !cbieedalkj      (-1.000)
     &     - s27(d,a,i,e)*t3a(e,c,b,l,k,j)          !daieecblkj      (-1.000)
     &     + s27(d,b,i,e)*t3a(e,c,a,l,k,j)          !dbieecalkj      (+1.000)
     &     - s27(d,c,i,e)*t3a(e,b,a,l,k,j)          !dcieebalkj      (-1.000)      
             enddo
             v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call
!     & sum12678345(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z60, 1.000)
!       call
!     & sum13678245(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z60,-1.000)
!       call
!     & sum14678235(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z60, 1.000)
!       call
!     & sum23678145(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z60, 1.000)
!       call
!     & sum24678135(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z60,-1.000)
!       call
!     & sum34678125(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z60, 1.000)
!       call
!     & sum12578346(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z60,-1.000)
!       call
!     & sum13578246(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z60, 1.000)
!       call
!     & sum14578236(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z60,-1.000)
!       call
!     & sum23578146(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z60,-1.000)
!       call
!     & sum24578136(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z60, 1.000)
!       call
!     & sum34578126(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z60,-1.000)
!       call
!     & sum12568347(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z60, 1.000)
!       call
!     & sum13568247(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z60,-1.000)
!       call
!     & sum14568237(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z60, 1.000)
!       call
!     & sum23568147(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z60, 1.000)
!       call
!     & sum24568137(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z60,-1.000)
!       call
!     & sum34568127(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z60, 1.000)
!       call
!     & sum12567348(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z60,-1.000)
!       call
!     & sum13567248(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z60, 1.000)
!       call
!     & sum14567238(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z60,-1.000)
!       call
!     & sum23567148(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z60,-1.000)
!       call
!     & sum24567138(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z60, 1.000)
!       call
!     & sum34567128(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z60,-1.000)
!c
!       do i=n0+1,n1-3
!       do j=i+1,n1-2
!       do k=j+1,n1-1
!       do l=k+1,n1
!       do a=n1+1,n3-3
!       do b=a+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)
!     & +z60(d,c,k,j,i,b,a,l)       ! 12678345 (+1.000)
!     & -z60(d,b,k,j,i,c,a,l)       ! 13678245 (-1.000)
!     & +z60(d,a,k,j,i,c,b,l)       ! 14678235 (+1.000)
!     & +z60(c,b,k,j,i,d,a,l)       ! 23678145 (+1.000)
!     & -z60(c,a,k,j,i,d,b,l)       ! 24678135 (-1.000)
!     & +z60(b,a,k,j,i,d,c,l)       ! 34678125 (+1.000)
!     & -z60(d,c,l,j,i,b,a,k)       ! 12578346 (-1.000)
!     & +z60(d,b,l,j,i,c,a,k)       ! 13578246 (+1.000)
!     & -z60(d,a,l,j,i,c,b,k)       ! 14578236 (-1.000)
!     & -z60(c,b,l,j,i,d,a,k)       ! 23578146 (-1.000)
!     & +z60(c,a,l,j,i,d,b,k)       ! 24578136 (+1.000)
!     & -z60(b,a,l,j,i,d,c,k)       ! 34578126 (-1.000)
!     & +z60(d,c,l,k,i,b,a,j)       ! 12568347 (+1.000)
!     & -z60(d,b,l,k,i,c,a,j)       ! 13568247 (-1.000)
!     & +z60(d,a,l,k,i,c,b,j)       ! 14568237 (+1.000)
!     & +z60(c,b,l,k,i,d,a,j)       ! 23568147 (+1.000)
!     & -z60(c,a,l,k,i,d,b,j)       ! 24568137 (-1.000)
!     & +z60(b,a,l,k,i,d,c,j)       ! 34568127 (+1.000)
!     & -z60(d,c,l,k,j,b,a,i)       ! 12567348 (-1.000)
!     & +z60(d,b,l,k,j,c,a,i)       ! 13567248 (+1.000)
!     & -z60(d,a,l,k,j,c,b,i)       ! 14567238 (-1.000)
!     & -z60(c,b,l,k,j,d,a,i)       ! 23567148 (-1.000)
!     & +z60(c,a,l,k,j,d,b,i)       ! 24567138 (+1.000)
!     & -z60(b,a,l,k,j,d,c,i)       ! 34567128 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z60)
       deallocate(s27)
c
       allocate(d1(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder4321(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n0,n2,n0,n1,intm,d1)
       allocate(f2(n2+1:n3,n1+1:n3,n0+1:n2,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder124356(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n2,n3,n1,n3,n0,n2,n1,n3,n0,n1,n0,n1,t3b,f2)
       allocate(s28(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1
       i2=k1*k1*k3
       i3=k2*k3*k4
       call egemm(i1,i2,i3,d1,f2,s28)
       deallocate(d1)
       deallocate(f2)
c
!       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
!       call reorder4123(n1,n3,n0,n1,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n0,n1,n0,n1,s28,d1)
!       allocate(f2(n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!       call reorder412356(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,t3a,f2)
!       allocate(z61(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
!       i1=k1*k1*k3
!       i2=k1*k1*k3*k3*k3
!       i3=k1
!       call egemm(i1,i2,i3,d1,f2,z61)
!       deallocate(d1)
!       deallocate(f2)
c
       do i=n0+1,n1-3;do j=i+1,n1-2;do k=j+1,n1-1;do l=k+1,n1
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-3;do b=a+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
          if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1
             sum=sum
     &     - s28(a,l,k,m)*t3a(d,c,b,m,j,i)          !alkmdcbmji      (-1.000)
     &     + s28(b,l,k,m)*t3a(d,c,a,m,j,i)          !blkmdcamji      (+1.000)
     &     - s28(c,l,k,m)*t3a(d,b,a,m,j,i)          !clkmdbamji      (-1.000)
     &     + s28(d,l,k,m)*t3a(c,b,a,m,j,i)          !dlkmcbamji      (+1.000)
     &     + s28(a,l,j,m)*t3a(d,c,b,m,k,i)          !aljmdcbmki      (+1.000)
     &     - s28(b,l,j,m)*t3a(d,c,a,m,k,i)          !bljmdcamki      (-1.000)
     &     + s28(c,l,j,m)*t3a(d,b,a,m,k,i)          !cljmdbamki      (+1.000)
     &     - s28(d,l,j,m)*t3a(c,b,a,m,k,i)          !dljmcbamki      (-1.000)
     &     - s28(a,k,j,m)*t3a(d,c,b,m,l,i)          !akjmdcbmli      (-1.000)
     &     + s28(b,k,j,m)*t3a(d,c,a,m,l,i)          !bkjmdcamli      (+1.000)
     &     - s28(c,k,j,m)*t3a(d,b,a,m,l,i)          !ckjmdbamli      (-1.000)
     &     + s28(d,k,j,m)*t3a(c,b,a,m,l,i)          !dkjmcbamli      (+1.000)
     &     - s28(a,l,i,m)*t3a(d,c,b,m,k,j)          !alimdcbmkj      (-1.000)
     &     + s28(b,l,i,m)*t3a(d,c,a,m,k,j)          !blimdcamkj      (+1.000)
     &     - s28(c,l,i,m)*t3a(d,b,a,m,k,j)          !climdbamkj      (-1.000)
     &     + s28(d,l,i,m)*t3a(c,b,a,m,k,j)          !dlimcbamkj      (+1.000)
     &     + s28(a,k,i,m)*t3a(d,c,b,m,l,j)          !akimdcbmlj      (+1.000)
     &     - s28(b,k,i,m)*t3a(d,c,a,m,l,j)          !bkimdcamlj      (-1.000)
     &     + s28(c,k,i,m)*t3a(d,b,a,m,l,j)          !ckimdbamlj      (+1.000)
     &     - s28(d,k,i,m)*t3a(c,b,a,m,l,j)          !dkimcbamlj      (-1.000)
     &     - s28(a,j,i,m)*t3a(d,c,b,m,l,k)          !ajimdcbmlk      (-1.000)
     &     + s28(b,j,i,m)*t3a(d,c,a,m,l,k)          !bjimdcamlk      (+1.000)
     &     - s28(c,j,i,m)*t3a(d,b,a,m,l,k)          !cjimdbamlk      (-1.000)
     &     + s28(d,j,i,m)*t3a(c,b,a,m,l,k)          !djimcbamlk      (+1.000)       
             enddo
             v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call
!     & sum12378456(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z61,-1.000)
!       call
!     & sum12478356(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z61, 1.000)
!       call
!     & sum13478256(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z61,-1.000)
!       call
!     & sum23478156(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z61, 1.000)
!       call
!     & sum12368457(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z61, 1.000)
!       call
!     & sum12468357(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z61,-1.000)
!       call
!     & sum13468257(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z61, 1.000)
!       call
!     & sum23468157(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z61,-1.000)
!       call
!     & sum12358467(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z61,-1.000)
!       call
!     & sum12458367(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z61, 1.000)
!       call
!     & sum13458267(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z61,-1.000)
!       call
!     & sum23458167(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z61, 1.000)
!       call
!     & sum12367458(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z61,-1.000)
!       call
!     & sum12467358(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z61, 1.000)
!       call
!     & sum13467258(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z61,-1.000)
!       call
!     & sum23467158(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z61, 1.000)
!       call
!     & sum12357468(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z61, 1.000)
!       call
!     & sum12457368(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z61,-1.000)
!       call
!     & sum13457268(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z61, 1.000)
!       call
!     & sum23457168(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z61,-1.000)
!       call
!     & sum12356478(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z61,-1.000)
!       call
!     & sum12456378(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z61, 1.000)
!       call
!     & sum13456278(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z61,-1.000)
!       call
!     & sum23456178(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z61, 1.000)
!c
!       do i=n0+1,n1-3
!       do j=i+1,n1-2
!       do k=j+1,n1-1
!       do l=k+1,n1
!       do a=n1+1,n3-3
!       do b=a+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)
!     & -z61(d,c,b,j,i,a,l,k)       ! 12378456 (-1.000)
!     & +z61(d,c,a,j,i,b,l,k)       ! 12478356 (+1.000)
!     & -z61(d,b,a,j,i,c,l,k)       ! 13478256 (-1.000)
!     & +z61(c,b,a,j,i,d,l,k)       ! 23478156 (+1.000)
!     & +z61(d,c,b,k,i,a,l,j)       ! 12368457 (+1.000)
!     & -z61(d,c,a,k,i,b,l,j)       ! 12468357 (-1.000)
!     & +z61(d,b,a,k,i,c,l,j)       ! 13468257 (+1.000)
!     & -z61(c,b,a,k,i,d,l,j)       ! 23468157 (-1.000)
!     & -z61(d,c,b,l,i,a,k,j)       ! 12358467 (-1.000)
!     & +z61(d,c,a,l,i,b,k,j)       ! 12458367 (+1.000)
!     & -z61(d,b,a,l,i,c,k,j)       ! 13458267 (-1.000)
!     & +z61(c,b,a,l,i,d,k,j)       ! 23458167 (+1.000)
!     & -z61(d,c,b,k,j,a,l,i)       ! 12367458 (-1.000)
!     & +z61(d,c,a,k,j,b,l,i)       ! 12467358 (+1.000)
!     & -z61(d,b,a,k,j,c,l,i)       ! 13467258 (-1.000)
!     & +z61(c,b,a,k,j,d,l,i)       ! 23467158 (+1.000)
!     & +z61(d,c,b,l,j,a,k,i)       ! 12357468 (+1.000)
!     & -z61(d,c,a,l,j,b,k,i)       ! 12457368 (-1.000)
!     & +z61(d,b,a,l,j,c,k,i)       ! 13457268 (+1.000)
!     & -z61(c,b,a,l,j,d,k,i)       ! 23457168 (-1.000)
!     & -z61(d,c,b,l,k,a,j,i)       ! 12356478 (-1.000)
!     & +z61(d,c,a,l,k,b,j,i)       ! 12456378 (+1.000)
!     & -z61(d,b,a,l,k,c,j,i)       ! 13456278 (-1.000)
!     & +z61(c,b,a,l,k,d,j,i)       ! 23456178 (+1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z61)
       deallocate(s28)
c
c
       allocate(d1(n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2))
       call reorder3142(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n2,n3,n0,n2,intm,d1)
       allocate(f2(n1+1:n3,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder142356(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,
     & n1,n3,n0,n1,n1,n3,n1,n3,n0,n1,n0,n1,t3a,f2)
       allocate(u16(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n2+1:n3,n0+1:n2))
       i1=k2*k4
       i2=k1*k1*k3*k3
       i3=k1*k3
       call egemm(i1,i2,i3,d1,f2,u16)
       deallocate(d1)
       deallocate(f2)
c
!       allocate(f1(n0+1:n2,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!       call reorder651234(n1,n3,n1,n3,n0,n1,n0,n1,n2,n3,n0,n2,
!     & n0,n2,n2,n3,n1,n3,n1,n3,n0,n1,n0,n1,u16,f1)
!       allocate(f2(n0+1:n2,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!       call reorder412356(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
!     & n0,n2,n2,n3,n1,n3,n1,n3,n0,n1,n0,n1,t3b,f2)
!       allocate(z62(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!       i1=k1*k1*k3*k3
!       i2=k1*k1*k3*k3
!       i3=k4*k2
!       call egemm(i1,i2,i3,f1,f2,z62)
!       deallocate(f1)
!       deallocate(f2)
c
       do i=n0+1,n1-3;do j=i+1,n1-2;do k=j+1,n1-1;do l=k+1,n1
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-3;do b=a+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
          if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n2+1,n3;do n=n0+1,n2
             sum=sum
     &     + u16(d,c,j,i,f,n)*t3b(f,b,a,n,l,k)      !dcjifnfbanlk    (+1.000)
     &     - u16(d,b,j,i,f,n)*t3b(f,c,a,n,l,k)      !dbjifnfcanlk    (-1.000)
     &     + u16(d,a,j,i,f,n)*t3b(f,c,b,n,l,k)      !dajifnfcbnlk    (+1.000)
     &     + u16(c,b,j,i,f,n)*t3b(f,d,a,n,l,k)      !cbjifnfdanlk    (+1.000)
     &     - u16(c,a,j,i,f,n)*t3b(f,d,b,n,l,k)      !cajifnfdbnlk    (-1.000)
     &     + u16(b,a,j,i,f,n)*t3b(f,d,c,n,l,k)      !bajifnfdcnlk    (+1.000)
     &     - u16(d,c,k,i,f,n)*t3b(f,b,a,n,l,j)      !dckifnfbanlj    (-1.000)
     &     + u16(d,b,k,i,f,n)*t3b(f,c,a,n,l,j)      !dbkifnfcanlj    (+1.000)
     &     - u16(d,a,k,i,f,n)*t3b(f,c,b,n,l,j)      !dakifnfcbnlj    (-1.000)
     &     - u16(c,b,k,i,f,n)*t3b(f,d,a,n,l,j)      !cbkifnfdanlj    (-1.000)
     &     + u16(c,a,k,i,f,n)*t3b(f,d,b,n,l,j)      !cakifnfdbnlj    (+1.000)
     &     - u16(b,a,k,i,f,n)*t3b(f,d,c,n,l,j)      !bakifnfdcnlj    (-1.000)
     &     + u16(d,c,l,i,f,n)*t3b(f,b,a,n,k,j)      !dclifnfbankj    (+1.000)
     &     - u16(d,b,l,i,f,n)*t3b(f,c,a,n,k,j)      !dblifnfcankj    (-1.000)
     &     + u16(d,a,l,i,f,n)*t3b(f,c,b,n,k,j)      !dalifnfcbnkj    (+1.000)
     &     + u16(c,b,l,i,f,n)*t3b(f,d,a,n,k,j)      !cblifnfdankj    (+1.000)
     &     - u16(c,a,l,i,f,n)*t3b(f,d,b,n,k,j)      !califnfdbnkj    (-1.000)
     &     + u16(b,a,l,i,f,n)*t3b(f,d,c,n,k,j)      !balifnfdcnkj    (+1.000)
     &     + u16(d,c,k,j,f,n)*t3b(f,b,a,n,l,i)      !dckjfnfbanli    (+1.000)
     &     - u16(d,b,k,j,f,n)*t3b(f,c,a,n,l,i)      !dbkjfnfcanli    (-1.000)
     &     + u16(d,a,k,j,f,n)*t3b(f,c,b,n,l,i)      !dakjfnfcbnli    (+1.000)
     &     + u16(c,b,k,j,f,n)*t3b(f,d,a,n,l,i)      !cbkjfnfdanli    (+1.000)
     &     - u16(c,a,k,j,f,n)*t3b(f,d,b,n,l,i)      !cakjfnfdbnli    (-1.000)
     &     + u16(b,a,k,j,f,n)*t3b(f,d,c,n,l,i)      !bakjfnfdcnli    (+1.000)
     &     - u16(d,c,l,j,f,n)*t3b(f,b,a,n,k,i)      !dcljfnfbanki    (-1.000)
     &     + u16(d,b,l,j,f,n)*t3b(f,c,a,n,k,i)      !dbljfnfcanki    (+1.000)
     &     - u16(d,a,l,j,f,n)*t3b(f,c,b,n,k,i)      !daljfnfcbnki    (-1.000)
     &     - u16(c,b,l,j,f,n)*t3b(f,d,a,n,k,i)      !cbljfnfdanki    (-1.000)
     &     + u16(c,a,l,j,f,n)*t3b(f,d,b,n,k,i)      !caljfnfdbnki    (+1.000)
     &     - u16(b,a,l,j,f,n)*t3b(f,d,c,n,k,i)      !baljfnfdcnki    (-1.000)
     &     + u16(d,c,l,k,f,n)*t3b(f,b,a,n,j,i)      !dclkfnfbanji    (+1.000)
     &     - u16(d,b,l,k,f,n)*t3b(f,c,a,n,j,i)      !dblkfnfcanji    (-1.000)
     &     + u16(d,a,l,k,f,n)*t3b(f,c,b,n,j,i)      !dalkfnfcbnji    (+1.000)
     &     + u16(c,b,l,k,f,n)*t3b(f,d,a,n,j,i)      !cblkfnfdanji    (+1.000)
     &     - u16(c,a,l,k,f,n)*t3b(f,d,b,n,j,i)      !calkfnfdbnji    (-1.000)
     &     + u16(b,a,l,k,f,n)*t3b(f,d,c,n,j,i)      !balkfnfdcnji    (+1.000)       
             enddo;enddo
             v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call
!     & sum34561278(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z62, 1.000)
!       call
!     & sum24561378(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z62,-1.000)
!       call
!     & sum23561478(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z62, 1.000)
!       call
!     & sum14562378(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z62, 1.000)
!       call
!     & sum13562478(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z62,-1.000)
!       call
!     & sum12563478(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z62, 1.000)
!       call
!     & sum34571268(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z62,-1.000)
!       call
!     & sum24571368(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z62, 1.000)
!       call
!     & sum23571468(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z62,-1.000)
!       call
!     & sum14572368(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z62,-1.000)
!       call
!     & sum13572468(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z62, 1.000)
!       call
!     & sum12573468(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z62,-1.000)
!       call
!     & sum34671258(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z62, 1.000)
!       call
!     & sum24671358(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z62,-1.000)
!       call
!     & sum23671458(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z62, 1.000)
!       call
!     & sum14672358(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z62, 1.000)
!       call
!     & sum13672458(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z62,-1.000)
!       call
!     & sum12673458(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z62, 1.000)
!       call
!     & sum34581267(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z62, 1.000)
!       call
!     & sum24581367(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z62,-1.000)
!       call
!     & sum23581467(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z62, 1.000)
!       call
!     & sum14582367(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z62, 1.000)
!       call
!     & sum13582467(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z62,-1.000)
!       call
!     & sum12583467(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z62, 1.000)
!       call
!     & sum34681257(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z62,-1.000)
!       call
!     & sum24681357(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z62, 1.000)
!       call
!     & sum23681457(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z62,-1.000)
!       call
!     & sum14682357(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z62,-1.000)
!       call
!     & sum13682457(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z62, 1.000)
!       call
!     & sum12683457(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z62,-1.000)
!       call
!     & sum34781256(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z62, 1.000)
!       call
!     & sum24781356(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z62,-1.000)
!       call
!     & sum23781456(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z62, 1.000)
!       call
!     & sum14782356(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z62, 1.000)
!       call
!     & sum13782456(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z62,-1.000)
!       call
!     & sum12783456(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z62, 1.000)
!c
!       do i=n0+1,n1-3
!       do j=i+1,n1-2
!       do k=j+1,n1-1
!       do l=k+1,n1
!       do a=n1+1,n3-3
!       do b=a+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)
!     & +z62(b,a,l,k,d,c,j,i)       ! 34561278 (+1.000)
!     & -z62(c,a,l,k,d,b,j,i)       ! 24561378 (-1.000)
!     & +z62(c,b,l,k,d,a,j,i)       ! 23561478 (+1.000)
!     & +z62(d,a,l,k,c,b,j,i)       ! 14562378 (+1.000)
!     & -z62(d,b,l,k,c,a,j,i)       ! 13562478 (-1.000)
!     & +z62(d,c,l,k,b,a,j,i)       ! 12563478 (+1.000)
!     & -z62(b,a,l,j,d,c,k,i)       ! 34571268 (-1.000)
!     & +z62(c,a,l,j,d,b,k,i)       ! 24571368 (+1.000)
!     & -z62(c,b,l,j,d,a,k,i)       ! 23571468 (-1.000)
!     & -z62(d,a,l,j,c,b,k,i)       ! 14572368 (-1.000)
!     & +z62(d,b,l,j,c,a,k,i)       ! 13572468 (+1.000)
!     & -z62(d,c,l,j,b,a,k,i)       ! 12573468 (-1.000)
!     & +z62(b,a,k,j,d,c,l,i)       ! 34671258 (+1.000)
!     & -z62(c,a,k,j,d,b,l,i)       ! 24671358 (-1.000)
!     & +z62(c,b,k,j,d,a,l,i)       ! 23671458 (+1.000)
!     & +z62(d,a,k,j,c,b,l,i)       ! 14672358 (+1.000)
!     & -z62(d,b,k,j,c,a,l,i)       ! 13672458 (-1.000)
!     & +z62(d,c,k,j,b,a,l,i)       ! 12673458 (+1.000)
!     & +z62(b,a,l,i,d,c,k,j)       ! 34581267 (+1.000)
!     & -z62(c,a,l,i,d,b,k,j)       ! 24581367 (-1.000)
!     & +z62(c,b,l,i,d,a,k,j)       ! 23581467 (+1.000)
!     & +z62(d,a,l,i,c,b,k,j)       ! 14582367 (+1.000)
!     & -z62(d,b,l,i,c,a,k,j)       ! 13582467 (-1.000)
!     & +z62(d,c,l,i,b,a,k,j)       ! 12583467 (+1.000)
!     & -z62(b,a,k,i,d,c,l,j)       ! 34681257 (-1.000)
!     & +z62(c,a,k,i,d,b,l,j)       ! 24681357 (+1.000)
!     & -z62(c,b,k,i,d,a,l,j)       ! 23681457 (-1.000)
!     & -z62(d,a,k,i,c,b,l,j)       ! 14682357 (-1.000)
!     & +z62(d,b,k,i,c,a,l,j)       ! 13682457 (+1.000)
!     & -z62(d,c,k,i,b,a,l,j)       ! 12683457 (-1.000)
!     & +z62(b,a,j,i,d,c,l,k)       ! 34781256 (+1.000)
!     & -z62(c,a,j,i,d,b,l,k)       ! 24781356 (-1.000)
!     & +z62(c,b,j,i,d,a,l,k)       ! 23781456 (+1.000)
!     & +z62(d,a,j,i,c,b,l,k)       ! 14782356 (+1.000)
!     & -z62(d,b,j,i,c,a,l,k)       ! 13782456 (-1.000)
!     & +z62(d,c,j,i,b,a,l,k)       ! 12783456 (+1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z62)
       deallocate(u16)
c
       allocate(d1(n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2))
       call reorder3142(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n2,n3,n0,n2,intb,d1)
       allocate(f2(n2+1:n3,n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder142356(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n2,n3,n0,n2,n1,n3,n1,n3,n0,n1,n0,n1,t3b,f2)
       allocate(u17(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n2+1:n3,n0+1:n2))
       i1=k2*k4
       i2=k1*k1*k3*k3
       i3=k2*k4
       call egemm(i1,i2,i3,d1,f2,u17)
       deallocate(d1)
       deallocate(f2)
c
!       allocate(f1(n0+1:n2,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!       call reorder651234(n1,n3,n1,n3,n0,n1,n0,n1,n2,n3,n0,n2,
!     & n0,n2,n2,n3,n1,n3,n1,n3,n0,n1,n0,n1,u17,f1)
!       allocate(f2(n0+1:n2,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!       call reorder412356(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
!     & n0,n2,n2,n3,n1,n3,n1,n3,n0,n1,n0,n1,t3b,f2)
!       allocate(z63(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!       i1=k1*k1*k3*k3
!       i2=k1*k1*k3*k3
!       i3=k4*k2
!       call egemm(i1,i2,i3,f1,f2,z63)
!       deallocate(f1)
!       deallocate(f2)
c
       do i=n0+1,n1-3;do j=i+1,n1-2;do k=j+1,n1-1;do l=k+1,n1
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-3;do b=a+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
          if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n2+1,n3;do n=n0+1,n2
             sum=sum
     &     + u17(b,a,l,k,f,n)*t3b(f,d,c,n,j,i)      !balkfnfdcnji    (+1.000)
     &     - u17(c,a,l,k,f,n)*t3b(f,d,b,n,j,i)      !calkfnfdbnji    (-1.000)
     &     + u17(d,a,j,i,f,n)*t3b(f,c,b,n,l,k)      !dajifnfcbnlk    (+1.000)
     &     + u17(d,a,l,k,f,n)*t3b(f,c,b,n,j,i)      !dalkfnfcbnji    (+1.000)
     &     - u17(c,a,j,i,f,n)*t3b(f,d,b,n,l,k)      !cajifnfdbnlk    (-1.000)
     &     + u17(b,a,j,i,f,n)*t3b(f,d,c,n,l,k)      !bajifnfdcnlk    (+1.000)
     &     - u17(b,a,l,j,f,n)*t3b(f,d,c,n,k,i)      !baljfnfdcnki    (-1.000)
     &     + u17(c,a,l,j,f,n)*t3b(f,d,b,n,k,i)      !caljfnfdbnki    (+1.000)
     &     - u17(d,a,k,i,f,n)*t3b(f,c,b,n,l,j)      !dakifnfcbnlj    (-1.000)
     &     - u17(d,a,l,j,f,n)*t3b(f,c,b,n,k,i)      !daljfnfcbnki    (-1.000)
     &     + u17(c,a,k,i,f,n)*t3b(f,d,b,n,l,j)      !cakifnfdbnlj    (+1.000)
     &     - u17(b,a,k,i,f,n)*t3b(f,d,c,n,l,j)      !bakifnfdcnlj    (-1.000)
     &     + u17(b,a,k,j,f,n)*t3b(f,d,c,n,l,i)      !bakjfnfdcnli    (+1.000)
     &     - u17(c,a,k,j,f,n)*t3b(f,d,b,n,l,i)      !cakjfnfdbnli    (-1.000)
     &     + u17(d,a,l,i,f,n)*t3b(f,c,b,n,k,j)      !dalifnfcbnkj    (+1.000)
     &     + u17(d,a,k,j,f,n)*t3b(f,c,b,n,l,i)      !dakjfnfcbnli    (+1.000)
     &     - u17(c,a,l,i,f,n)*t3b(f,d,b,n,k,j)      !califnfdbnkj    (-1.000)
     &     + u17(b,a,l,i,f,n)*t3b(f,d,c,n,k,j)      !balifnfdcnkj    (+1.000)
             enddo;enddo
             v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call
!     & sum12783456(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z63, 1.000)
!       call
!     & sum13782456(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z63,-1.000)
!       call
!     & sum23561478(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z63, 1.000)
!       call
!     & sum23781456(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z63, 1.000)
!       call
!     & sum13562478(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z63,-1.000)
!       call
!     & sum12563478(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z63, 1.000)
!       call
!     & sum12683457(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z63,-1.000)
!       call
!     & sum13682457(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z63, 1.000)
!       call
!     & sum23571468(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z63,-1.000)
!       call
!     & sum23681457(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z63,-1.000)
!       call
!     & sum13572468(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z63, 1.000)
!       call
!     & sum12573468(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z63,-1.000)
!       call
!     & sum12583467(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z63, 1.000)
!       call
!     & sum13582467(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z63,-1.000)
!       call
!     & sum23671458(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z63, 1.000)
!       call
!     & sum23581467(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z63, 1.000)
!       call
!     & sum13672458(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z63,-1.000)
!       call
!     & sum12673458(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z63, 1.000)
!c
!       do i=n0+1,n1-3
!       do j=i+1,n1-2
!       do k=j+1,n1-1
!       do l=k+1,n1
!       do a=n1+1,n3-3
!       do b=a+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)
!     & +z63(d,c,j,i,b,a,l,k)       ! 12783456 (+1.000)
!     & -z63(d,b,j,i,c,a,l,k)       ! 13782456 (-1.000)
!     & +z63(c,b,l,k,d,a,j,i)       ! 23561478 (+1.000)
!     & +z63(c,b,j,i,d,a,l,k)       ! 23781456 (+1.000)
!     & -z63(d,b,l,k,c,a,j,i)       ! 13562478 (-1.000)
!     & +z63(d,c,l,k,b,a,j,i)       ! 12563478 (+1.000)
!     & -z63(d,c,k,i,b,a,l,j)       ! 12683457 (-1.000)
!     & +z63(d,b,k,i,c,a,l,j)       ! 13682457 (+1.000)
!     & -z63(c,b,l,j,d,a,k,i)       ! 23571468 (-1.000)
!     & -z63(c,b,k,i,d,a,l,j)       ! 23681457 (-1.000)
!     & +z63(d,b,l,j,c,a,k,i)       ! 13572468 (+1.000)
!     & -z63(d,c,l,j,b,a,k,i)       ! 12573468 (-1.000)
!     & +z63(d,c,l,i,b,a,k,j)       ! 12583467 (+1.000)
!     & -z63(d,b,l,i,c,a,k,j)       ! 13582467 (-1.000)
!     & +z63(c,b,k,j,d,a,l,i)       ! 23671458 (+1.000)
!     & +z63(c,b,l,i,d,a,k,j)       ! 23581467 (+1.000)
!     & -z63(d,b,k,j,c,a,l,i)       ! 13672458 (-1.000)
!     & +z63(d,c,k,j,b,a,l,i)       ! 12673458 (+1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z63)
       deallocate(u17)
c
       allocate(d1(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder3412(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n1,n3,n0,n1,n0,n1,intr,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s33(n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       i1=k1*k1*k3
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s33)
       deallocate(d1)
       deallocate(b2)
c
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder3214(n0,n1,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n0,n1,n0,n1,s33,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(q11(n0+1:n1,n0+1:n1))
       i1=k1*k1
       i3=k3*k1
       call egemm1(i1,i3,d1,b2,q11)
       deallocate(d1)
       deallocate(b2)
c
       call sum21(n0,n1,n0,n1,x3,q11,-1.000)
       deallocate(q11)
c
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder3214(n0,n1,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n0,n1,n0,n1,s33,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s35(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       i1=k1*k1*k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s35)
       deallocate(d1)
       deallocate(b2)
c
       call sum3241(n0,n1,n1,n3,n1,n3,n0,n1,x6,s35,-1.000)
c
       call sumx3142(n0,n3,n0,n1,n1,n3,n1,n3,n0,n1,x6,intr, 1.000)
c
!       allocate(h2(n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
!       call reorder51234678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,t4a,h2)
!       allocate(z6(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1))
!       i1=k1*k3
!       i2=k1*k1*k1*k3*k3*k3
!       i3=k3*k1
!       call egemm(i1,i2,i3,x6,h2,z6)
!       deallocate(h2)
c
       do i=n0+1,n1-3;do j=i+1,n1-2;do k=j+1,n1-1;do l=k+1,n1
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-3;do b=a+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
          if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n1+1,n3;do m=n0+1,n1
             sum=sum
     &     - x6(m,e,d,i)*t4a(e,c,b,a,m,l,k,j)       !mediecbamlkj    (-1.000)
     &     + x6(m,e,c,i)*t4a(e,d,b,a,m,l,k,j)       !meciedbamlkj    (+1.000)
     &     - x6(m,e,b,i)*t4a(e,d,c,a,m,l,k,j)       !mebiedcamlkj    (-1.000)
     &     + x6(m,e,a,i)*t4a(e,d,c,b,m,l,k,j)       !meaiedcbmlkj    (+1.000)
     &     + x6(m,e,d,j)*t4a(e,c,b,a,m,l,k,i)       !medjecbamlki    (+1.000)
     &     - x6(m,e,c,j)*t4a(e,d,b,a,m,l,k,i)       !mecjedbamlki    (-1.000)
     &     + x6(m,e,b,j)*t4a(e,d,c,a,m,l,k,i)       !mebjedcamlki    (+1.000)
     &     - x6(m,e,a,j)*t4a(e,d,c,b,m,l,k,i)       !meajedcbmlki    (-1.000)
     &     - x6(m,e,d,k)*t4a(e,c,b,a,m,l,j,i)       !medkecbamlji    (-1.000)
     &     + x6(m,e,c,k)*t4a(e,d,b,a,m,l,j,i)       !meckedbamlji    (+1.000)
     &     - x6(m,e,b,k)*t4a(e,d,c,a,m,l,j,i)       !mebkedcamlji    (-1.000)
     &     + x6(m,e,a,k)*t4a(e,d,c,b,m,l,j,i)       !meakedcbmlji    (+1.000)
     &     + x6(m,e,d,l)*t4a(e,c,b,a,m,k,j,i)       !medlecbamkji    (+1.000)
     &     - x6(m,e,c,l)*t4a(e,d,b,a,m,k,j,i)       !mecledbamkji    (-1.000)
     &     + x6(m,e,b,l)*t4a(e,d,c,a,m,k,j,i)       !mebledcamkji    (+1.000)
     &     - x6(m,e,a,l)*t4a(e,d,c,b,m,k,j,i)       !mealedcbmkji    (-1.000)
             enddo;enddo
             v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call
!     & sum23456718(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z6,-1.000)
!       call
!     & sum13456728(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z6, 1.000)
!       call
!     & sum12456738(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z6,-1.000)
!       call
!     & sum12356748(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z6, 1.000)
!       call
!     & sum23456817(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z6, 1.000)
!       call
!     & sum13456827(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z6,-1.000)
!       call
!     & sum12456837(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z6, 1.000)
!       call
!     & sum12356847(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z6,-1.000)
!       call
!     & sum23457816(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z6,-1.000)
!       call
!     & sum13457826(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z6, 1.000)
!       call
!     & sum12457836(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z6,-1.000)
!       call
!     & sum12357846(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z6, 1.000)
!       call
!     & sum23467815(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z6, 1.000)
!       call
!     & sum13467825(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z6,-1.000)
!       call
!     & sum12467835(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z6, 1.000)
!       call
!     & sum12367845(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z6,-1.000)
!c
!       do i=n0+1,n1-3
!       do j=i+1,n1-2
!       do k=j+1,n1-1
!       do l=k+1,n1
!       do a=n1+1,n3-3
!       do b=a+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)
!     & -z6(c,b,a,l,k,j,d,i)       ! 23456718 (-1.000)
!     & +z6(d,b,a,l,k,j,c,i)       ! 13456728 (+1.000)
!     & -z6(d,c,a,l,k,j,b,i)       ! 12456738 (-1.000)
!     & +z6(d,c,b,l,k,j,a,i)       ! 12356748 (+1.000)
!     & +z6(c,b,a,l,k,i,d,j)       ! 23456817 (+1.000)
!     & -z6(d,b,a,l,k,i,c,j)       ! 13456827 (-1.000)
!     & +z6(d,c,a,l,k,i,b,j)       ! 12456837 (+1.000)
!     & -z6(d,c,b,l,k,i,a,j)       ! 12356847 (-1.000)
!     & -z6(c,b,a,l,j,i,d,k)       ! 23457816 (-1.000)
!     & +z6(d,b,a,l,j,i,c,k)       ! 13457826 (+1.000)
!     & -z6(d,c,a,l,j,i,b,k)       ! 12457836 (-1.000)
!     & +z6(d,c,b,l,j,i,a,k)       ! 12357846 (+1.000)
!     & +z6(c,b,a,k,j,i,d,l)       ! 23467815 (+1.000)
!     & -z6(d,b,a,k,j,i,c,l)       ! 13467825 (-1.000)
!     & +z6(d,c,a,k,j,i,b,l)       ! 12467835 (+1.000)
!     & -z6(d,c,b,k,j,i,a,l)       ! 12367845 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z6)
       deallocate(x6)
c
       allocate(d1(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder4213(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,s35,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s51(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
       i1=k1*k3*k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s51)
       deallocate(d1)
       deallocate(b2)
       deallocate(s35)
c
       call sum2134(n1,n3,n1,n3,n1,n3,n0,n1,x2,s51, 1.000)
       deallocate(s51)
c
       allocate(d1(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       call reorder2314(n0,n1,n1,n3,n0,n1,n0,n1,
     & n1,n3,n0,n1,n0,n1,n0,n1,s33,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s34(n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1*k1
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s34)
       deallocate(d1)
       deallocate(b2)
       deallocate(s33)
c
       call sum3241(n0,n1,n0,n1,n0,n1,n0,n1,x5,s34, 1.000)
c
       call sumx2143(n0,n3,n0,n1,n0,n1,n0,n1,n0,n1,x5,intr, 1.000)
c
!       allocate(h2(n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!       call reorder56123478(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,
!     & n0,n1,n0,n1,n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,t4a,h2)
!       allocate(z5(n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
!       i1=k1*k1
!       i2=k1*k1*k3*k3*k3*k3
!       i3=k1*k1
!       call egemm(i1,i2,i3,x5,h2,z5)
!       deallocate(h2)
c
       do i=n0+1,n1-3;do j=i+1,n1-2;do k=j+1,n1-1;do l=k+1,n1
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-3;do b=a+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
          if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1;do n=n0+1,n1
             sum=sum
     &     + (x5(n,m,j,i)*t4a(d,c,b,a,n,m,l,k)      !nmjidcbanmlk    (+0.500)
     &     - x5(n,m,k,i)*t4a(d,c,b,a,n,m,l,j)       !nmkidcbanmlj    (-0.500)
     &     + x5(n,m,l,i)*t4a(d,c,b,a,n,m,k,j)       !nmlidcbanmkj    (+0.500)
     &     + x5(n,m,k,j)*t4a(d,c,b,a,n,m,l,i)       !nmkjdcbanmli    (+0.500)
     &     - x5(n,m,l,j)*t4a(d,c,b,a,n,m,k,i)       !nmljdcbanmki    (-0.500)
     &     + x5(n,m,l,k)*t4a(d,c,b,a,n,m,j,i))/2.0d0!nmlkdcbanmji    (+0.500)
             enddo;enddo
             v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       v4a=v4a+0.500*z5
!       call
!     & sum12345768(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z5,-0.500)
!       call
!     & sum12346758(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z5, 0.500)
!       call
!     & sum12345867(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z5, 0.500)
!       call
!     & sum12346857(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z5,-0.500)
!       call
!     & sum12347856(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z5, 0.500)
!c
!       do i=n0+1,n1-3
!       do j=i+1,n1-2
!       do k=j+1,n1-1
!       do l=k+1,n1
!       do a=n1+1,n3-3
!       do b=a+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)
!     & +(z5(d,c,b,a,l,k,j,i)      ! 12345678 (+0.500)
!     & -z5(d,c,b,a,l,j,k,i)       ! 12345768 (-0.500)
!     & +z5(d,c,b,a,k,j,l,i)       ! 12346758 (+0.500)
!     & +z5(d,c,b,a,l,i,k,j)       ! 12345867 (+0.500)
!     & -z5(d,c,b,a,k,i,l,j)       ! 12346857 (-0.500)
!     & +z5(d,c,b,a,j,i,l,k))/2.0d0! 12347856 (+0.500)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z5)
       deallocate(x5)
c
       allocate(d1(n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       call reorder2413(n0,n1,n0,n1,n0,n1,n0,n1,
     & n0,n1,n0,n1,n0,n1,n0,n1,s34,d1)
       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
       allocate(u33(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1*k1
       i2=k1*k3*k3
       i3=k1
       call egemm(i1,i2,i3,d1,d2,u33)
       deallocate(d1)
       deallocate(d2)
c
!       allocate(f1(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
!       call reorder412356(n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,u33,f1)
!       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
!       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
!       allocate(z103(n1+1:n3,n1+1:n3,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
!       i1=k1*k1*k1*k3*k3
!       i2=k1*k3*k3
!       i3=k1
!       call egemm(i1,i2,i3,f1,d2,z103)
!       deallocate(f1)
!       deallocate(d2)
c
       do i=n0+1,n1-3;do j=i+1,n1-2;do k=j+1,n1-1;do l=k+1,n1
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-3;do b=a+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
          if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do n=n0+1,n1
             sum=sum
     &     + u33(b,a,l,n,j,i)*t2a(d,c,n,k)          !balnjidcnk      (+1.000)
     &     - u33(c,a,l,n,j,i)*t2a(d,b,n,k)          !calnjidbnk      (-1.000)
     &     - u33(d,a,k,n,j,i)*t2a(c,b,n,l)          !daknjicbnl      (-1.000)
     &     + u33(d,a,l,n,j,i)*t2a(c,b,n,k)          !dalnjicbnk      (+1.000)
     &     + u33(c,a,k,n,j,i)*t2a(d,b,n,l)          !caknjidbnl      (+1.000)
     &     - u33(b,a,k,n,j,i)*t2a(d,c,n,l)          !baknjidcnl      (-1.000)
     &     - u33(b,a,l,n,k,i)*t2a(d,c,n,j)          !balnkidcnj      (-1.000)
     &     + u33(c,a,l,n,k,i)*t2a(d,b,n,j)          !calnkidbnj      (+1.000)
     &     + u33(d,a,j,n,k,i)*t2a(c,b,n,l)          !dajnkicbnl      (+1.000)
     &     - u33(d,a,l,n,k,i)*t2a(c,b,n,j)          !dalnkicbnj      (-1.000)
     &     - u33(c,a,j,n,k,i)*t2a(d,b,n,l)          !cajnkidbnl      (-1.000)
     &     + u33(b,a,j,n,k,i)*t2a(d,c,n,l)          !bajnkidcnl      (+1.000)
     &     + u33(b,a,k,n,l,i)*t2a(d,c,n,j)          !baknlidcnj      (+1.000)
     &     - u33(c,a,k,n,l,i)*t2a(d,b,n,j)          !caknlidbnj      (-1.000)
     &     - u33(d,a,j,n,l,i)*t2a(c,b,n,k)          !dajnlicbnk      (-1.000)
     &     + u33(d,a,k,n,l,i)*t2a(c,b,n,j)          !daknlicbnj      (+1.000)
     &     + u33(c,a,j,n,l,i)*t2a(d,b,n,k)          !cajnlidbnk      (+1.000)
     &     - u33(b,a,j,n,l,i)*t2a(d,c,n,k)          !bajnlidcnk      (-1.000)
     &     + u33(b,a,l,n,k,j)*t2a(d,c,n,i)          !balnkjdcni      (+1.000)
     &     - u33(c,a,l,n,k,j)*t2a(d,b,n,i)          !calnkjdbni      (-1.000)
     &     - u33(d,a,i,n,k,j)*t2a(c,b,n,l)          !dainkjcbnl      (-1.000)
     &     + u33(d,a,l,n,k,j)*t2a(c,b,n,i)          !dalnkjcbni      (+1.000)
     &     + u33(c,a,i,n,k,j)*t2a(d,b,n,l)          !cainkjdbnl      (+1.000)
     &     - u33(b,a,i,n,k,j)*t2a(d,c,n,l)          !bainkjdcnl      (-1.000)
     &     - u33(b,a,k,n,l,j)*t2a(d,c,n,i)          !baknljdcni      (-1.000)
     &     + u33(c,a,k,n,l,j)*t2a(d,b,n,i)          !caknljdbni      (+1.000)
     &     + u33(d,a,i,n,l,j)*t2a(c,b,n,k)          !dainljcbnk      (+1.000)
     &     - u33(d,a,k,n,l,j)*t2a(c,b,n,i)          !daknljcbni      (-1.000)
     &     - u33(c,a,i,n,l,j)*t2a(d,b,n,k)          !cainljdbnk      (-1.000)
     &     + u33(b,a,i,n,l,j)*t2a(d,c,n,k)          !bainljdcnk      (+1.000)
     &     + u33(b,a,j,n,l,k)*t2a(d,c,n,i)          !bajnlkdcni      (+1.000)
     &     - u33(c,a,j,n,l,k)*t2a(d,b,n,i)          !cajnlkdbni      (-1.000)
     &     - u33(d,a,i,n,l,k)*t2a(c,b,n,j)          !dainlkcbnj      (-1.000)
     &     + u33(d,a,j,n,l,k)*t2a(c,b,n,i)          !dajnlkcbni      (+1.000)
     &     + u33(c,a,i,n,l,k)*t2a(d,b,n,j)          !cainlkdbnj      (+1.000)
     &     - u33(b,a,i,n,l,k)*t2a(d,c,n,j)          !bainlkdcnj      (-1.000)
             enddo
             v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call
!     & sum12634578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z103, 1.000)
!       call
!     & sum13624578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z103,-1.000)
!       call
!     & sum23514678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z103,-1.000)
!       call
!     & sum23614578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z103, 1.000)
!       call
!     & sum13524678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z103, 1.000)
!       call
!     & sum12534678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z103,-1.000)
!       call
!     & sum12734568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z103,-1.000)
!       call
!     & sum13724568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z103, 1.000)
!       call
!     & sum23514768(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z103, 1.000)
!       call
!     & sum23714568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z103,-1.000)
!       call
!     & sum13524768(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z103,-1.000)
!       call
!     & sum12534768(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z103, 1.000)
!       call
!     & sum12734658(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z103, 1.000)
!       call
!     & sum13724658(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z103,-1.000)
!       call
!     & sum23614758(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z103,-1.000)
!       call
!     & sum23714658(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z103, 1.000)
!       call
!     & sum13624758(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z103, 1.000)
!       call
!     & sum12634758(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z103,-1.000)
!       call
!     & sum12834567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z103, 1.000)
!       call
!     & sum13824567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z103,-1.000)
!       call
!     & sum23514867(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z103,-1.000)
!       call
!     & sum23814567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z103, 1.000)
!       call
!     & sum13524867(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z103, 1.000)
!       call
!     & sum12534867(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z103,-1.000)
!       call
!     & sum12834657(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z103,-1.000)
!       call
!     & sum13824657(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z103, 1.000)
!       call
!     & sum23614857(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z103, 1.000)
!       call
!     & sum23814657(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z103,-1.000)
!       call
!     & sum13624857(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z103,-1.000)
!       call
!     & sum12634857(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z103, 1.000)
!       call
!     & sum12834756(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z103, 1.000)
!       call
!     & sum13824756(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z103,-1.000)
!       call
!     & sum23714856(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z103,-1.000)
!       call
!     & sum23814756(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z103, 1.000)
!       call
!     & sum13724856(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z103, 1.000)
!       call
!     & sum12734856(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z103,-1.000)
!c
!       do i=n0+1,n1-3
!       do j=i+1,n1-2
!       do k=j+1,n1-1
!       do l=k+1,n1
!       do a=n1+1,n3-3
!       do b=a+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)
!     & +z103(d,c,k,b,a,l,j,i)       ! 12634578 (+1.000)
!     & -z103(d,b,k,c,a,l,j,i)       ! 13624578 (-1.000)
!     & -z103(c,b,l,d,a,k,j,i)       ! 23514678 (-1.000)
!     & +z103(c,b,k,d,a,l,j,i)       ! 23614578 (+1.000)
!     & +z103(d,b,l,c,a,k,j,i)       ! 13524678 (+1.000)
!     & -z103(d,c,l,b,a,k,j,i)       ! 12534678 (-1.000)
!     & -z103(d,c,j,b,a,l,k,i)       ! 12734568 (-1.000)
!     & +z103(d,b,j,c,a,l,k,i)       ! 13724568 (+1.000)
!     & +z103(c,b,l,d,a,j,k,i)       ! 23514768 (+1.000)
!     & -z103(c,b,j,d,a,l,k,i)       ! 23714568 (-1.000)
!     & -z103(d,b,l,c,a,j,k,i)       ! 13524768 (-1.000)
!     & +z103(d,c,l,b,a,j,k,i)       ! 12534768 (+1.000)
!     & +z103(d,c,j,b,a,k,l,i)       ! 12734658 (+1.000)
!     & -z103(d,b,j,c,a,k,l,i)       ! 13724658 (-1.000)
!     & -z103(c,b,k,d,a,j,l,i)       ! 23614758 (-1.000)
!     & +z103(c,b,j,d,a,k,l,i)       ! 23714658 (+1.000)
!     & +z103(d,b,k,c,a,j,l,i)       ! 13624758 (+1.000)
!     & -z103(d,c,k,b,a,j,l,i)       ! 12634758 (-1.000)
!     & +z103(d,c,i,b,a,l,k,j)       ! 12834567 (+1.000)
!     & -z103(d,b,i,c,a,l,k,j)       ! 13824567 (-1.000)
!     & -z103(c,b,l,d,a,i,k,j)       ! 23514867 (-1.000)
!     & +z103(c,b,i,d,a,l,k,j)       ! 23814567 (+1.000)
!     & +z103(d,b,l,c,a,i,k,j)       ! 13524867 (+1.000)
!     & -z103(d,c,l,b,a,i,k,j)       ! 12534867 (-1.000)
!     & -z103(d,c,i,b,a,k,l,j)       ! 12834657 (-1.000)
!     & +z103(d,b,i,c,a,k,l,j)       ! 13824657 (+1.000)
!     & +z103(c,b,k,d,a,i,l,j)       ! 23614857 (+1.000)
!     & -z103(c,b,i,d,a,k,l,j)       ! 23814657 (-1.000)
!     & -z103(d,b,k,c,a,i,l,j)       ! 13624857 (-1.000)
!     & +z103(d,c,k,b,a,i,l,j)       ! 12634857 (+1.000)
!     & +z103(d,c,i,b,a,j,l,k)       ! 12834756 (+1.000)
!     & -z103(d,b,i,c,a,j,l,k)       ! 13824756 (-1.000)
!     & -z103(c,b,j,d,a,i,l,k)       ! 23714856 (-1.000)
!     & +z103(c,b,i,d,a,j,l,k)       ! 23814756 (+1.000)
!     & +z103(d,b,j,c,a,i,l,k)       ! 13724856 (+1.000)
!     & -z103(d,c,j,b,a,i,l,k)       ! 12734856 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z103)
       deallocate(u33)
c
       allocate(d1(n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       call reorder2413(n0,n1,n0,n1,n0,n1,n0,n1,
     & n0,n1,n0,n1,n0,n1,n0,n1,s34,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s50(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1*k1
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s50)
       deallocate(d1)
       deallocate(b2)
       deallocate(s34)
c
       call sum2134(n0,n1,n1,n3,n0,n1,n0,n1,x1,s50,-1.000)
       deallocate(s50)
c
       call sumx2143(n0,n3,n0,n1,n1,n3,n0,n1,n0,n1,x1,intr, 1.000)
c
!       allocate(f2(n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!       call reorder412356(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,t3a,f2)
!       allocate(z1(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
!       i1=k1*k1*k3
!       i2=k1*k1*k3*k3*k3
!       i3=k1
!       call egemm(i1,i2,i3,x1,f2,z1)
!       deallocate(f2)
c
       do i=n0+1,n1-3;do j=i+1,n1-2;do k=j+1,n1-1;do l=k+1,n1
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-3;do b=a+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
          if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1
             sum=sum
     &     + x1(m,d,j,i)*t3a(c,b,a,m,l,k)           !mdjicbamlk      (+1.000)
     &     - x1(m,c,j,i)*t3a(d,b,a,m,l,k)           !mcjidbamlk      (-1.000)
     &     + x1(m,b,j,i)*t3a(d,c,a,m,l,k)           !mbjidcamlk      (+1.000)
     &     - x1(m,a,j,i)*t3a(d,c,b,m,l,k)           !majidcbmlk      (-1.000)
     &     - x1(m,d,k,i)*t3a(c,b,a,m,l,j)           !mdkicbamlj      (-1.000)
     &     + x1(m,c,k,i)*t3a(d,b,a,m,l,j)           !mckidbamlj      (+1.000)
     &     - x1(m,b,k,i)*t3a(d,c,a,m,l,j)           !mbkidcamlj      (-1.000)
     &     + x1(m,a,k,i)*t3a(d,c,b,m,l,j)           !makidcbmlj      (+1.000)
     &     + x1(m,d,l,i)*t3a(c,b,a,m,k,j)           !mdlicbamkj      (+1.000)
     &     - x1(m,c,l,i)*t3a(d,b,a,m,k,j)           !mclidbamkj      (-1.000)
     &     + x1(m,b,l,i)*t3a(d,c,a,m,k,j)           !mblidcamkj      (+1.000)
     &     - x1(m,a,l,i)*t3a(d,c,b,m,k,j)           !malidcbmkj      (-1.000)
     &     + x1(m,d,k,j)*t3a(c,b,a,m,l,i)           !mdkjcbamli      (+1.000)
     &     - x1(m,c,k,j)*t3a(d,b,a,m,l,i)           !mckjdbamli      (-1.000)
     &     + x1(m,b,k,j)*t3a(d,c,a,m,l,i)           !mbkjdcamli      (+1.000)
     &     - x1(m,a,k,j)*t3a(d,c,b,m,l,i)           !makjdcbmli      (-1.000)
     &     - x1(m,d,l,j)*t3a(c,b,a,m,k,i)           !mdljcbamki      (-1.000)
     &     + x1(m,c,l,j)*t3a(d,b,a,m,k,i)           !mcljdbamki      (+1.000)
     &     - x1(m,b,l,j)*t3a(d,c,a,m,k,i)           !mbljdcamki      (-1.000)
     &     + x1(m,a,l,j)*t3a(d,c,b,m,k,i)           !maljdcbmki      (+1.000)
     &     + x1(m,d,l,k)*t3a(c,b,a,m,j,i)           !mdlkcbamji      (+1.000)
     &     - x1(m,c,l,k)*t3a(d,b,a,m,j,i)           !mclkdbamji      (-1.000)
     &     + x1(m,b,l,k)*t3a(d,c,a,m,j,i)           !mblkdcamji      (+1.000)
     &     - x1(m,a,l,k)*t3a(d,c,b,m,j,i)           !malkdcbmji      (-1.000)
             enddo
             v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call
!     & sum23456178(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z1, 1.000)
!       call
!     & sum13456278(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z1,-1.000)
!       call
!     & sum12456378(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z1, 1.000)
!       call
!     & sum12356478(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z1,-1.000)
!       call
!     & sum23457168(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z1,-1.000)
!       call
!     & sum13457268(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z1, 1.000)
!       call
!     & sum12457368(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z1,-1.000)
!       call
!     & sum12357468(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z1, 1.000)
!       call
!     & sum23467158(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z1, 1.000)
!       call
!     & sum13467258(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z1,-1.000)
!       call
!     & sum12467358(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z1, 1.000)
!       call
!     & sum12367458(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z1,-1.000)
!       call
!     & sum23458167(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z1, 1.000)
!       call
!     & sum13458267(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z1,-1.000)
!       call
!     & sum12458367(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z1, 1.000)
!       call
!     & sum12358467(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z1,-1.000)
!       call
!     & sum23468157(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z1,-1.000)
!       call
!     & sum13468257(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z1, 1.000)
!       call
!     & sum12468357(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z1,-1.000)
!       call
!     & sum12368457(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z1, 1.000)
!       call
!     & sum23478156(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z1, 1.000)
!       call
!     & sum13478256(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z1,-1.000)
!       call
!     & sum12478356(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z1, 1.000)
!       call
!     & sum12378456(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z1,-1.000)
c
!       do i=n0+1,n1-3
!       do j=i+1,n1-2
!       do k=j+1,n1-1
!       do l=k+1,n1
!       do a=n1+1,n3-3
!       do b=a+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)
!     & +z1(c,b,a,l,k,d,j,i)       ! 23456178 (+1.000)
!     & -z1(d,b,a,l,k,c,j,i)       ! 13456278 (-1.000)
!     & +z1(d,c,a,l,k,b,j,i)       ! 12456378 (+1.000)
!     & -z1(d,c,b,l,k,a,j,i)       ! 12356478 (-1.000)
!     & -z1(c,b,a,l,j,d,k,i)       ! 23457168 (-1.000)
!     & +z1(d,b,a,l,j,c,k,i)       ! 13457268 (+1.000)
!     & -z1(d,c,a,l,j,b,k,i)       ! 12457368 (-1.000)
!     & +z1(d,c,b,l,j,a,k,i)       ! 12357468 (+1.000)
!     & +z1(c,b,a,k,j,d,l,i)       ! 23467158 (+1.000)
!     & -z1(d,b,a,k,j,c,l,i)       ! 13467258 (-1.000)
!     & +z1(d,c,a,k,j,b,l,i)       ! 12467358 (+1.000)
!     & -z1(d,c,b,k,j,a,l,i)       ! 12367458 (-1.000)
!     & +z1(c,b,a,l,i,d,k,j)       ! 23458167 (+1.000)
!     & -z1(d,b,a,l,i,c,k,j)       ! 13458267 (-1.000)
!     & +z1(d,c,a,l,i,b,k,j)       ! 12458367 (+1.000)
!     & -z1(d,c,b,l,i,a,k,j)       ! 12358467 (-1.000)
!     & -z1(c,b,a,k,i,d,l,j)       ! 23468157 (-1.000)
!     & +z1(d,b,a,k,i,c,l,j)       ! 13468257 (+1.000)
!     & -z1(d,c,a,k,i,b,l,j)       ! 12468357 (-1.000)
!     & +z1(d,c,b,k,i,a,l,j)       ! 12368457 (+1.000)
!     & +z1(c,b,a,j,i,d,l,k)       ! 23478156 (+1.000)
!     & -z1(d,b,a,j,i,c,l,k)       ! 13478256 (-1.000)
!     & +z1(d,c,a,j,i,b,l,k)       ! 12478356 (+1.000)
!     & -z1(d,c,b,j,i,a,l,k)       ! 12378456 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z1)
       deallocate(x1)
c
       allocate(d1(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder1432(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n1,n3,n0,n1,intr,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s36(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
       i1=k1*k3*k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s36)
       deallocate(d1)
       deallocate(b2)
c
       allocate(d1(n1+1:n3,n0+1:n1,n1+1:n3,n1+1:n3))
       call reorder3421(n1,n3,n1,n3,n1,n3,n0,n1,
     & n1,n3,n0,n1,n1,n3,n1,n3,s36,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(q12(n1+1:n3,n1+1:n3))
       i1=k3*k3
       i3=k1*k3
       call egemm1(i1,i3,d1,b2,q12)
       deallocate(d1)
       deallocate(b2)
c
       x4=x4+q12
       deallocate(q12)
c
       allocate(d1(n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3))
       call reorder4231(n1,n3,n1,n3,n1,n3,n0,n1,
     & n0,n1,n1,n3,n1,n3,n1,n3,s36,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s37(n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3))
       i1=k3*k3*k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s37)
       deallocate(d1)
       deallocate(b2)
       deallocate(s36)
c
       call sum3124(n1,n3,n1,n3,n1,n3,n1,n3,x7,s37, 1.000)
       deallocate(s37)
c
       call sumx4321(n0,n3,n1,n3,n1,n3,n1,n3,n1,n3,x7,intr, 1.000)
c
!       allocate(h2(n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
!       call reorder12345678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,
!     & n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,t4a,h2)
!       allocate(z7(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3))
!       i1=k3*k3
!       i2=k1*k1*k1*k1*k3*k3
!       i3=k3*k3
!       call egemm(i1,i2,i3,x7,h2,z7)
!       deallocate(h2)
c
       do i=n0+1,n1-3;do j=i+1,n1-2;do k=j+1,n1-1;do l=k+1,n1
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-3;do b=a+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
          if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n1+1,n3;do f=n1+1,n3
             sum=sum
     &     + (x7(f,e,d,c)*t4a(f,e,b,a,l,k,j,i)      !fedcfebalkji    (+0.500)
     &     - x7(f,e,d,b)*t4a(f,e,c,a,l,k,j,i)       !fedbfecalkji    (-0.500)
     &     + x7(f,e,d,a)*t4a(f,e,c,b,l,k,j,i)       !fedafecblkji    (+0.500)
     &     + x7(f,e,c,b)*t4a(f,e,d,a,l,k,j,i)       !fecbfedalkji    (+0.500)
     &     - x7(f,e,c,a)*t4a(f,e,d,b,l,k,j,i)       !fecafedblkji    (-0.500)
     &     + x7(f,e,b,a)*t4a(f,e,d,c,l,k,j,i))/2.0d0!febafedclkji    (+0.500)
             enddo;enddo
             v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call
!     & sum34567812(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z7, 0.500)
!       call
!     & sum24567813(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z7,-0.500)
!       call
!     & sum23567814(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z7, 0.500)
!       call
!     & sum14567823(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z7, 0.500)
!       call
!     & sum13567824(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z7,-0.500)
!       call
!     & sum12567834(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z7, 0.500)
!c
!       do i=n0+1,n1-3
!       do j=i+1,n1-2
!       do k=j+1,n1-1
!       do l=k+1,n1
!       do a=n1+1,n3-3
!       do b=a+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)
!     & +(z7(b,a,l,k,j,i,d,c)      ! 34567812 (+0.500)
!     & -z7(c,a,l,k,j,i,d,b)       ! 24567813 (-0.500)
!     & +z7(c,b,l,k,j,i,d,a)       ! 23567814 (+0.500)
!     & +z7(d,a,l,k,j,i,c,b)       ! 14567823 (+0.500)
!     & -z7(d,b,l,k,j,i,c,a)       ! 13567824 (-0.500)
!     & +z7(d,c,l,k,j,i,b,a))/2.0d0! 12567834 (+0.500)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z7)
       deallocate(x7)
c
       allocate(d1(n1+1:n3,n2+1:n3,n0+1:n1,n0+1:n2))
       call reorder3412(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n2,n3,n0,n1,n0,n2,intm,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s38(n0+1:n1,n2+1:n3,n0+1:n1,n0+1:n2))
       i1=k2*k1*k4
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s38)
       deallocate(d1)
       deallocate(b2)
c
       allocate(d1(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       call reorder2431(n0,n1,n2,n3,n0,n1,n0,n2,
     & n2,n3,n0,n2,n0,n1,n0,n1,s38,d1)
       allocate(f2(n2+1:n3,n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder142356(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n2,n3,n0,n2,n1,n3,n1,n3,n0,n1,n0,n1,t3b,f2)
       allocate(u26(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1
       i2=k1*k1*k3*k3
       i3=k2*k4
       call egemm(i1,i2,i3,d1,f2,u26)
       deallocate(d1)
       deallocate(f2)
c
!       allocate(f1(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
!       call reorder512346(n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,u26,f1)
!       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
!       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
!       allocate(z90(n1+1:n3,n1+1:n3,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
!       i1=k1*k1*k1*k3*k3
!       i2=k1*k3*k3
!       i3=k1
!       call egemm(i1,i2,i3,f1,d2,z90)
!       deallocate(f1)
!       deallocate(d2)
c   
       do i=n0+1,n1-3;do j=i+1,n1-2;do k=j+1,n1-1;do l=k+1,n1
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-3;do b=a+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
          if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1
             sum=sum
     &     + u26(b,a,l,k,m,i)*t2a(d,c,m,j)          !balkmidcmj      (+1.000)
     &     - u26(c,a,l,k,m,i)*t2a(d,b,m,j)          !calkmidbmj      (-1.000)
     &     + u26(c,b,l,k,m,i)*t2a(d,a,m,j)          !cblkmidamj      (+1.000)
     &     + u26(d,a,l,k,m,i)*t2a(c,b,m,j)          !dalkmicbmj      (+1.000)
     &     - u26(d,b,l,k,m,i)*t2a(c,a,m,j)          !dblkmicamj      (-1.000)
     &     + u26(d,c,l,k,m,i)*t2a(b,a,m,j)          !dclkmibamj      (+1.000)
     &     - u26(b,a,l,j,m,i)*t2a(d,c,m,k)          !baljmidcmk      (-1.000)
     &     + u26(c,a,l,j,m,i)*t2a(d,b,m,k)          !caljmidbmk      (+1.000)
     &     - u26(c,b,l,j,m,i)*t2a(d,a,m,k)          !cbljmidamk      (-1.000)
     &     - u26(d,a,l,j,m,i)*t2a(c,b,m,k)          !daljmicbmk      (-1.000)
     &     + u26(d,b,l,j,m,i)*t2a(c,a,m,k)          !dbljmicamk      (+1.000)
     &     - u26(d,c,l,j,m,i)*t2a(b,a,m,k)          !dcljmibamk      (-1.000)
     &     + u26(b,a,k,j,m,i)*t2a(d,c,m,l)          !bakjmidcml      (+1.000)
     &     - u26(c,a,k,j,m,i)*t2a(d,b,m,l)          !cakjmidbml      (-1.000)
     &     + u26(c,b,k,j,m,i)*t2a(d,a,m,l)          !cbkjmidaml      (+1.000)
     &     + u26(d,a,k,j,m,i)*t2a(c,b,m,l)          !dakjmicbml      (+1.000)
     &     - u26(d,b,k,j,m,i)*t2a(c,a,m,l)          !dbkjmicaml      (-1.000)
     &     + u26(d,c,k,j,m,i)*t2a(b,a,m,l)          !dckjmibaml      (+1.000)
     &     - u26(b,a,l,k,m,j)*t2a(d,c,m,i)          !balkmjdcmi      (-1.000)
     &     + u26(c,a,l,k,m,j)*t2a(d,b,m,i)          !calkmjdbmi      (+1.000)
     &     - u26(c,b,l,k,m,j)*t2a(d,a,m,i)          !cblkmjdami      (-1.000)
     &     - u26(d,a,l,k,m,j)*t2a(c,b,m,i)          !dalkmjcbmi      (-1.000)
     &     + u26(d,b,l,k,m,j)*t2a(c,a,m,i)          !dblkmjcami      (+1.000)
     &     - u26(d,c,l,k,m,j)*t2a(b,a,m,i)          !dclkmjbami      (-1.000)
     &     + u26(b,a,l,j,m,k)*t2a(d,c,m,i)          !baljmkdcmi      (+1.000)
     &     - u26(c,a,l,j,m,k)*t2a(d,b,m,i)          !caljmkdbmi      (-1.000)
     &     + u26(c,b,l,j,m,k)*t2a(d,a,m,i)          !cbljmkdami      (+1.000)
     &     + u26(d,a,l,j,m,k)*t2a(c,b,m,i)          !daljmkcbmi      (+1.000)
     &     - u26(d,b,l,j,m,k)*t2a(c,a,m,i)          !dbljmkcami      (-1.000)
     &     + u26(d,c,l,j,m,k)*t2a(b,a,m,i)          !dcljmkbami      (+1.000)
     &     - u26(b,a,k,j,m,l)*t2a(d,c,m,i)          !bakjmldcmi      (-1.000)
     &     + u26(c,a,k,j,m,l)*t2a(d,b,m,i)          !cakjmldbmi      (+1.000)
     &     - u26(c,b,k,j,m,l)*t2a(d,a,m,i)          !cbkjmldami      (-1.000)
     &     - u26(d,a,k,j,m,l)*t2a(c,b,m,i)          !dakjmlcbmi      (-1.000)
     &     + u26(d,b,k,j,m,l)*t2a(c,a,m,i)          !dbkjmlcami      (+1.000)
     &     - u26(d,c,k,j,m,l)*t2a(b,a,m,i)          !dckjmlbami      (-1.000)
     &     + u26(b,a,l,i,m,j)*t2a(d,c,m,k)          !balimjdcmk      (+1.000)
     &     - u26(c,a,l,i,m,j)*t2a(d,b,m,k)          !calimjdbmk      (-1.000)
     &     + u26(c,b,l,i,m,j)*t2a(d,a,m,k)          !cblimjdamk      (+1.000)
     &     + u26(d,a,l,i,m,j)*t2a(c,b,m,k)          !dalimjcbmk      (+1.000)
     &     - u26(d,b,l,i,m,j)*t2a(c,a,m,k)          !dblimjcamk      (-1.000)
     &     + u26(d,c,l,i,m,j)*t2a(b,a,m,k)          !dclimjbamk      (+1.000)
     &     - u26(b,a,k,i,m,j)*t2a(d,c,m,l)          !bakimjdcml      (-1.000)
     &     + u26(c,a,k,i,m,j)*t2a(d,b,m,l)          !cakimjdbml      (+1.000)
     &     - u26(c,b,k,i,m,j)*t2a(d,a,m,l)          !cbkimjdaml      (-1.000)
     &     - u26(d,a,k,i,m,j)*t2a(c,b,m,l)          !dakimjcbml      (-1.000)
     &     + u26(d,b,k,i,m,j)*t2a(c,a,m,l)          !dbkimjcaml      (+1.000)
     &     - u26(d,c,k,i,m,j)*t2a(b,a,m,l)          !dckimjbaml      (-1.000)
     &     - u26(b,a,l,i,m,k)*t2a(d,c,m,j)          !balimkdcmj      (-1.000)
     &     + u26(c,a,l,i,m,k)*t2a(d,b,m,j)          !calimkdbmj      (+1.000)
     &     - u26(c,b,l,i,m,k)*t2a(d,a,m,j)          !cblimkdamj      (-1.000)
     &     - u26(d,a,l,i,m,k)*t2a(c,b,m,j)          !dalimkcbmj      (-1.000)
     &     + u26(d,b,l,i,m,k)*t2a(c,a,m,j)          !dblimkcamj      (+1.000)
     &     - u26(d,c,l,i,m,k)*t2a(b,a,m,j)          !dclimkbamj      (-1.000)
     &     + u26(b,a,k,i,m,l)*t2a(d,c,m,j)          !bakimldcmj      (+1.000)
     &     - u26(c,a,k,i,m,l)*t2a(d,b,m,j)          !cakimldbmj      (-1.000)
     &     + u26(c,b,k,i,m,l)*t2a(d,a,m,j)          !cbkimldamj      (+1.000)
     &     + u26(d,a,k,i,m,l)*t2a(c,b,m,j)          !dakimlcbmj      (+1.000)
     &     - u26(d,b,k,i,m,l)*t2a(c,a,m,j)          !dbkimlcamj      (-1.000)
     &     + u26(d,c,k,i,m,l)*t2a(b,a,m,j)          !dckimlbamj      (+1.000)
     &     + u26(b,a,j,i,m,k)*t2a(d,c,m,l)          !bajimkdcml      (+1.000)
     &     - u26(c,a,j,i,m,k)*t2a(d,b,m,l)          !cajimkdbml      (-1.000)
     &     + u26(c,b,j,i,m,k)*t2a(d,a,m,l)          !cbjimkdaml      (+1.000)
     &     + u26(d,a,j,i,m,k)*t2a(c,b,m,l)          !dajimkcbml      (+1.000)
     &     - u26(d,b,j,i,m,k)*t2a(c,a,m,l)          !dbjimkcaml      (-1.000)
     &     + u26(d,c,j,i,m,k)*t2a(b,a,m,l)          !dcjimkbaml      (+1.000)
     &     - u26(b,a,j,i,m,l)*t2a(d,c,m,k)          !bajimldcmk      (-1.000)
     &     + u26(c,a,j,i,m,l)*t2a(d,b,m,k)          !cajimldbmk      (+1.000)
     &     - u26(c,b,j,i,m,l)*t2a(d,a,m,k)          !cbjimldamk      (-1.000)
     &     - u26(d,a,j,i,m,l)*t2a(c,b,m,k)          !dajimlcbmk      (-1.000)
     &     + u26(d,b,j,i,m,l)*t2a(c,a,m,k)          !dbjimlcamk      (+1.000)
     &     - u26(d,c,j,i,m,l)*t2a(b,a,m,k)          !dcjimlbamk      (-1.000)
             enddo
             v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call
!     & sum12734568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z90, 1.000)
!       call
!     & sum13724568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z90,-1.000)
!       call
!     & sum14723568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z90, 1.000)
!       call
!     & sum23714568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z90, 1.000)
!       call
!     & sum24713568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z90,-1.000)
!       call
!     & sum34712568(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z90, 1.000)
!       call
!     & sum12634578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z90,-1.000)
!       call
!     & sum13624578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z90, 1.000)
!       call
!     & sum14623578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z90,-1.000)
!       call
!     & sum23614578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z90,-1.000)
!       call
!     & sum24613578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z90, 1.000)
!       call
!     & sum34612578(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z90,-1.000)
!       call
!     & sum12534678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z90, 1.000)
!       call
!     & sum13524678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z90,-1.000)
!       call
!     & sum14523678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z90, 1.000)
!       call
!     & sum23514678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z90, 1.000)
!       call
!     & sum24513678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z90,-1.000)
!       call
!     & sum34512678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z90, 1.000)
!       call
!     & sum12834567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z90,-1.000)
!       call
!     & sum13824567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z90, 1.000)
!       call
!     & sum14823567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z90,-1.000)
!       call
!     & sum23814567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z90,-1.000)
!       call
!     & sum24813567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z90, 1.000)
!       call
!     & sum34812567(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z90,-1.000)
!       call
!     & sum12834576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z90, 1.000)
!       call
!     & sum13824576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z90,-1.000)
!       call
!     & sum14823576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z90, 1.000)
!       call
!     & sum23814576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z90, 1.000)
!       call
!     & sum24813576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z90,-1.000)
!       call
!     & sum34812576(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z90, 1.000)
!       call
!     & sum12834675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z90,-1.000)
!       call
!     & sum13824675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z90, 1.000)
!       call
!     & sum14823675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z90,-1.000)
!       call
!     & sum23814675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z90,-1.000)
!       call
!     & sum24813675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z90, 1.000)
!       call
!     & sum34812675(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z90,-1.000)
!       call
!     & sum12634587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z90, 1.000)
!       call
!     & sum13624587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z90,-1.000)
!       call
!     & sum14623587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z90, 1.000)
!       call
!     & sum23614587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z90, 1.000)
!       call
!     & sum24613587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z90,-1.000)
!       call
!     & sum34612587(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z90, 1.000)
!       call
!     & sum12534687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z90,-1.000)
!       call
!     & sum13524687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z90, 1.000)
!       call
!     & sum14523687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z90,-1.000)
!       call
!     & sum23514687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z90,-1.000)
!       call
!     & sum24513687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z90, 1.000)
!       call
!     & sum34512687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z90,-1.000)
!       call
!     & sum12734586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z90,-1.000)
!       call
!     & sum13724586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z90, 1.000)
!       call
!     & sum14723586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z90,-1.000)
!       call
!     & sum23714586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z90,-1.000)
!       call
!     & sum24713586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z90, 1.000)
!       call
!     & sum34712586(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z90,-1.000)
!       call
!     & sum12734685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z90, 1.000)
!       call
!     & sum13724685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z90,-1.000)
!       call
!     & sum14723685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z90, 1.000)
!       call
!     & sum23714685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z90, 1.000)
!       call
!     & sum24713685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z90,-1.000)
!       call
!     & sum34712685(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z90, 1.000)
!       call
!     & sum12534786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z90, 1.000)
!       call
!     & sum13524786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z90,-1.000)
!       call
!     & sum14523786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z90, 1.000)
!       call
!     & sum23514786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z90, 1.000)
!       call
!     & sum24513786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z90,-1.000)
!       call
!     & sum34512786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z90, 1.000)
!       call
!     & sum12634785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z90,-1.000)
!       call
!     & sum13624785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z90, 1.000)
!       call
!     & sum14623785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z90,-1.000)
!       call
!     & sum23614785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z90,-1.000)
!       call
!     & sum24613785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z90, 1.000)
!       call
!     & sum34612785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z90,-1.000)
!c
!       do i=n0+1,n1-3
!       do j=i+1,n1-2
!       do k=j+1,n1-1
!       do l=k+1,n1
!       do a=n1+1,n3-3
!       do b=a+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)
!     & +z90(d,c,j,b,a,l,k,i)       ! 12734568 (+1.000)
!     & -z90(d,b,j,c,a,l,k,i)       ! 13724568 (-1.000)
!     & +z90(d,a,j,c,b,l,k,i)       ! 14723568 (+1.000)
!     & +z90(c,b,j,d,a,l,k,i)       ! 23714568 (+1.000)
!     & -z90(c,a,j,d,b,l,k,i)       ! 24713568 (-1.000)
!     & +z90(b,a,j,d,c,l,k,i)       ! 34712568 (+1.000)
!     & -z90(d,c,k,b,a,l,j,i)       ! 12634578 (-1.000)
!     & +z90(d,b,k,c,a,l,j,i)       ! 13624578 (+1.000)
!     & -z90(d,a,k,c,b,l,j,i)       ! 14623578 (-1.000)
!     & -z90(c,b,k,d,a,l,j,i)       ! 23614578 (-1.000)
!     & +z90(c,a,k,d,b,l,j,i)       ! 24613578 (+1.000)
!     & -z90(b,a,k,d,c,l,j,i)       ! 34612578 (-1.000)
!     & +z90(d,c,l,b,a,k,j,i)       ! 12534678 (+1.000)
!     & -z90(d,b,l,c,a,k,j,i)       ! 13524678 (-1.000)
!     & +z90(d,a,l,c,b,k,j,i)       ! 14523678 (+1.000)
!     & +z90(c,b,l,d,a,k,j,i)       ! 23514678 (+1.000)
!     & -z90(c,a,l,d,b,k,j,i)       ! 24513678 (-1.000)
!     & +z90(b,a,l,d,c,k,j,i)       ! 34512678 (+1.000)
!     & -z90(d,c,i,b,a,l,k,j)       ! 12834567 (-1.000)
!     & +z90(d,b,i,c,a,l,k,j)       ! 13824567 (+1.000)
!     & -z90(d,a,i,c,b,l,k,j)       ! 14823567 (-1.000)
!     & -z90(c,b,i,d,a,l,k,j)       ! 23814567 (-1.000)
!     & +z90(c,a,i,d,b,l,k,j)       ! 24813567 (+1.000)
!     & -z90(b,a,i,d,c,l,k,j)       ! 34812567 (-1.000)
!     & +z90(d,c,i,b,a,l,j,k)       ! 12834576 (+1.000)
!     & -z90(d,b,i,c,a,l,j,k)       ! 13824576 (-1.000)
!     & +z90(d,a,i,c,b,l,j,k)       ! 14823576 (+1.000)
!     & +z90(c,b,i,d,a,l,j,k)       ! 23814576 (+1.000)
!     & -z90(c,a,i,d,b,l,j,k)       ! 24813576 (-1.000)
!     & +z90(b,a,i,d,c,l,j,k)       ! 34812576 (+1.000)
!     & -z90(d,c,i,b,a,k,j,l)       ! 12834675 (-1.000)
!     & +z90(d,b,i,c,a,k,j,l)       ! 13824675 (+1.000)
!     & -z90(d,a,i,c,b,k,j,l)       ! 14823675 (-1.000)
!     & -z90(c,b,i,d,a,k,j,l)       ! 23814675 (-1.000)
!     & +z90(c,a,i,d,b,k,j,l)       ! 24813675 (+1.000)
!     & -z90(b,a,i,d,c,k,j,l)       ! 34812675 (-1.000)
!     & +z90(d,c,k,b,a,l,i,j)       ! 12634587 (+1.000)
!     & -z90(d,b,k,c,a,l,i,j)       ! 13624587 (-1.000)
!     & +z90(d,a,k,c,b,l,i,j)       ! 14623587 (+1.000)
!     & +z90(c,b,k,d,a,l,i,j)       ! 23614587 (+1.000)
!     & -z90(c,a,k,d,b,l,i,j)       ! 24613587 (-1.000)
!     & +z90(b,a,k,d,c,l,i,j)       ! 34612587 (+1.000)
!     & -z90(d,c,l,b,a,k,i,j)       ! 12534687 (-1.000)
!     & +z90(d,b,l,c,a,k,i,j)       ! 13524687 (+1.000)
!     & -z90(d,a,l,c,b,k,i,j)       ! 14523687 (-1.000)
!     & -z90(c,b,l,d,a,k,i,j)       ! 23514687 (-1.000)
!     & +z90(c,a,l,d,b,k,i,j)       ! 24513687 (+1.000)
!     & -z90(b,a,l,d,c,k,i,j)       ! 34512687 (-1.000)
!     & -z90(d,c,j,b,a,l,i,k)       ! 12734586 (-1.000)
!     & +z90(d,b,j,c,a,l,i,k)       ! 13724586 (+1.000)
!     & -z90(d,a,j,c,b,l,i,k)       ! 14723586 (-1.000)
!     & -z90(c,b,j,d,a,l,i,k)       ! 23714586 (-1.000)
!     & +z90(c,a,j,d,b,l,i,k)       ! 24713586 (+1.000)
!     & -z90(b,a,j,d,c,l,i,k)       ! 34712586 (-1.000)
!     & +z90(d,c,j,b,a,k,i,l)       ! 12734685 (+1.000)
!     & -z90(d,b,j,c,a,k,i,l)       ! 13724685 (-1.000)
!     & +z90(d,a,j,c,b,k,i,l)       ! 14723685 (+1.000)
!     & +z90(c,b,j,d,a,k,i,l)       ! 23714685 (+1.000)
!     & -z90(c,a,j,d,b,k,i,l)       ! 24713685 (-1.000)
!     & +z90(b,a,j,d,c,k,i,l)       ! 34712685 (+1.000)
!     & +z90(d,c,l,b,a,j,i,k)       ! 12534786 (+1.000)
!     & -z90(d,b,l,c,a,j,i,k)       ! 13524786 (-1.000)
!     & +z90(d,a,l,c,b,j,i,k)       ! 14523786 (+1.000)
!     & +z90(c,b,l,d,a,j,i,k)       ! 23514786 (+1.000)
!     & -z90(c,a,l,d,b,j,i,k)       ! 24513786 (-1.000)
!     & +z90(b,a,l,d,c,j,i,k)       ! 34512786 (+1.000)
!     & -z90(d,c,k,b,a,j,i,l)       ! 12634785 (-1.000)
!     & +z90(d,b,k,c,a,j,i,l)       ! 13624785 (+1.000)
!     & -z90(d,a,k,c,b,j,i,l)       ! 14623785 (-1.000)
!     & -z90(c,b,k,d,a,j,i,l)       ! 23614785 (-1.000)
!     & +z90(c,a,k,d,b,j,i,l)       ! 24613785 (+1.000)
!     & -z90(b,a,k,d,c,j,i,l)       ! 34612785 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z90)
       deallocate(u26)
c
       allocate(d1(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       call reorder2431(n0,n1,n2,n3,n0,n1,n0,n2,
     & n2,n3,n0,n2,n0,n1,n0,n1,s38,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(q13(n0+1:n1,n0+1:n1))
       i1=k1*k1
       i3=k2*k4
       call egemm1(i1,i3,d1,b2,q13)
       deallocate(d1)
       deallocate(b2)
c
       x3=x3+q13
       deallocate(q13)
c
       call sumx12(0,n3,n0,n1,n0,n1,x3,fockr, 1.000)
c
!       allocate(h2(n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
!       call reorder51234678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,t4a,h2)
!       allocate(z3(n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
!       i1=k1
!       i2=k1*k1*k1*k3*k3*k3*k3
!       i3=k1
!       call egemm(i1,i2,i3,x3,h2,z3)
!       deallocate(h2)
c
       do i=n0+1,n1-3;do j=i+1,n1-2;do k=j+1,n1-1;do l=k+1,n1
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-3;do b=a+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
          if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1
             sum=sum
     &     + x3(m,i)*t4a(d,c,b,a,m,l,k,j)           !midcbamlkj      (+1.000)
     &     - x3(m,j)*t4a(d,c,b,a,m,l,k,i)           !mjdcbamlki      (-1.000)
     &     + x3(m,k)*t4a(d,c,b,a,m,l,j,i)           !mkdcbamlji      (+1.000)
     &     - x3(m,l)*t4a(d,c,b,a,m,k,j,i)           !mldcbamkji      (-1.000)
             enddo
             v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       v4a=v4a+z3
!       call
!     & sum12345687(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z3,-1.000)
!       call
!     & sum12345786(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z3, 1.000)
!       call
!     & sum12346785(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z3,-1.000)
!c
!       do i=n0+1,n1-3
!       do j=i+1,n1-2
!       do k=j+1,n1-1
!       do l=k+1,n1
!       do a=n1+1,n3-3
!       do b=a+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)
!     & +z3(d,c,b,a,l,k,j,i)       ! 12345678 (+1.000)
!     & -z3(d,c,b,a,l,k,i,j)       ! 12345687 (-1.000)
!     & +z3(d,c,b,a,l,j,i,k)       ! 12345786 (+1.000)
!     & -z3(d,c,b,a,k,j,i,l)       ! 12346785 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z3)
       deallocate(x3)
c
       allocate(d1(n0+1:n1,n2+1:n3,n0+1:n1,n0+1:n2))
       call reorder3214(n0,n1,n2,n3,n0,n1,n0,n2,
     & n0,n1,n2,n3,n0,n1,n0,n2,s38,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s39(n1+1:n3,n2+1:n3,n0+1:n1,n0+1:n2))
       i1=k2*k1*k4
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s39)
       deallocate(d1)
       deallocate(b2)
       deallocate(s38)
c
       call sum3241(n0,n2,n2,n3,n1,n3,n0,n1,x8,s39,-1.000)
       deallocate(s39)
c
       call sumx3142(n0,n3,n0,n2,n2,n3,n1,n3,n0,n1,x8,intm, 1.000)
c
!       allocate(h2(n0+1:n2,n2+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
!       call reorder51234678(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,
!     & n0,n2,n2,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,t4b,h2)
!       allocate(z8(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1))
!       i1=k1*k3
!       i2=k1*k1*k1*k3*k3*k3
!       i3=k4*k2
!       call egemm(i1,i2,i3,x8,h2,z8)
!       deallocate(h2)
c
       do i=n0+1,n1-3;do j=i+1,n1-2;do k=j+1,n1-1;do l=k+1,n1
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-3;do b=a+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
          if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n2+1,n3;do m=n0+1,n2
             sum=sum
     &     - x8(m,e,d,i)*t4b(e,c,b,a,m,l,k,j)       !mediecbamlkj    (-1.000)
     &     + x8(m,e,c,i)*t4b(e,d,b,a,m,l,k,j)       !meciedbamlkj    (+1.000)
     &     - x8(m,e,b,i)*t4b(e,d,c,a,m,l,k,j)       !mebiedcamlkj    (-1.000)
     &     + x8(m,e,a,i)*t4b(e,d,c,b,m,l,k,j)       !meaiedcbmlkj    (+1.000)
     &     + x8(m,e,d,j)*t4b(e,c,b,a,m,l,k,i)       !medjecbamlki    (+1.000)
     &     - x8(m,e,c,j)*t4b(e,d,b,a,m,l,k,i)       !mecjedbamlki    (-1.000)
     &     + x8(m,e,b,j)*t4b(e,d,c,a,m,l,k,i)       !mebjedcamlki    (+1.000)
     &     - x8(m,e,a,j)*t4b(e,d,c,b,m,l,k,i)       !meajedcbmlki    (-1.000)
     &     - x8(m,e,d,k)*t4b(e,c,b,a,m,l,j,i)       !medkecbamlji    (-1.000)
     &     + x8(m,e,c,k)*t4b(e,d,b,a,m,l,j,i)       !meckedbamlji    (+1.000)
     &     - x8(m,e,b,k)*t4b(e,d,c,a,m,l,j,i)       !mebkedcamlji    (-1.000)
     &     + x8(m,e,a,k)*t4b(e,d,c,b,m,l,j,i)       !meakedcbmlji    (+1.000)
     &     + x8(m,e,d,l)*t4b(e,c,b,a,m,k,j,i)       !medlecbamkji    (+1.000)
     &     - x8(m,e,c,l)*t4b(e,d,b,a,m,k,j,i)       !mecledbamkji    (-1.000)
     &     + x8(m,e,b,l)*t4b(e,d,c,a,m,k,j,i)       !mebledcamkji    (+1.000)
     &     - x8(m,e,a,l)*t4b(e,d,c,b,m,k,j,i)       !mealedcbmkji    (-1.000)
             enddo;enddo
             v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call
!     & sum23456718(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z8,-1.000)
!       call
!     & sum13456728(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z8, 1.000)
!       call
!     & sum12456738(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z8,-1.000)
!       call
!     & sum12356748(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z8, 1.000)
!       call
!     & sum23456817(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z8, 1.000)
!       call
!     & sum13456827(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z8,-1.000)
!       call
!     & sum12456837(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z8, 1.000)
!       call
!     & sum12356847(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z8,-1.000)
!       call
!     & sum23457816(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z8,-1.000)
!       call
!     & sum13457826(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z8, 1.000)
!       call
!     & sum12457836(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z8,-1.000)
!       call
!     & sum12357846(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z8, 1.000)
!       call
!     & sum23467815(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z8, 1.000)
!       call
!     & sum13467825(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z8,-1.000)
!       call
!     & sum12467835(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z8, 1.000)
!       call
!     & sum12367845(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z8,-1.000)
!c
!       do i=n0+1,n1-3
!       do j=i+1,n1-2
!       do k=j+1,n1-1
!       do l=k+1,n1
!       do a=n1+1,n3-3
!       do b=a+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)
!     & -z8(c,b,a,l,k,j,d,i)       ! 23456718 (-1.000)
!     & +z8(d,b,a,l,k,j,c,i)       ! 13456728 (+1.000)
!     & -z8(d,c,a,l,k,j,b,i)       ! 12456738 (-1.000)
!     & +z8(d,c,b,l,k,j,a,i)       ! 12356748 (+1.000)
!     & +z8(c,b,a,l,k,i,d,j)       ! 23456817 (+1.000)
!     & -z8(d,b,a,l,k,i,c,j)       ! 13456827 (-1.000)
!     & +z8(d,c,a,l,k,i,b,j)       ! 12456837 (+1.000)
!     & -z8(d,c,b,l,k,i,a,j)       ! 12356847 (-1.000)
!     & -z8(c,b,a,l,j,i,d,k)       ! 23457816 (-1.000)
!     & +z8(d,b,a,l,j,i,c,k)       ! 13457826 (+1.000)
!     & -z8(d,c,a,l,j,i,b,k)       ! 12457836 (-1.000)
!     & +z8(d,c,b,l,j,i,a,k)       ! 12357846 (+1.000)
!     & +z8(c,b,a,k,j,i,d,l)       ! 23467815 (+1.000)
!     & -z8(d,b,a,k,j,i,c,l)       ! 13467825 (-1.000)
!     & +z8(d,c,a,k,j,i,b,l)       ! 12467835 (+1.000)
!     & -z8(d,c,b,k,j,i,a,l)       ! 12367845 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z8)
       deallocate(x8)
c
c
       allocate(d1(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       call reorder4231(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n1,n3,n0,n1,intm,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(q14(n1+1:n3,n0+1:n1))
       i1=k1*k3
       i3=k2*k4
       call egemm1(i1,i3,d1,b2,q14)
       deallocate(d1)
       deallocate(b2)
c
       allocate(b1(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,q14,b1)
       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
       allocate(s49(n1+1:n3,n1+1:n3,n0+1:n1,n1+1:n3))
       i1=k3
       i2=k1*k3*k3
       i3=k1
       call egemm(i1,i2,i3,b1,d2,s49)
       deallocate(b1)
       deallocate(d2)
c
       call sum2341(n1,n3,n1,n3,n1,n3,n0,n1,x2,s49,-1.000)
       deallocate(s49)
c
       allocate(b1(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,q14,b1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(q15(n1+1:n3,n1+1:n3))
       i1=k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,b1,b2,q15)
       deallocate(b1)
       deallocate(b2)
       deallocate(q14)
c
       call sum21(n1,n3,n1,n3,x4,q15,-1.000)
       deallocate(q15)
c
       call sumx21(0,n3,n1,n3,n1,n3,x4,fockr, 1.000)
c
!       allocate(h2(n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
!       call reorder12345678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,
!     & n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,t4a,h2)
!       allocate(z4(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1,n1+1:n3))
!       i1=k3
!       i2=k1*k1*k1*k1*k3*k3*k3
!       i3=k3
!       call egemm(i1,i2,i3,x4,h2,z4)
!       deallocate(h2)
c
       do i=n0+1,n1-3;do j=i+1,n1-2;do k=j+1,n1-1;do l=k+1,n1
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-3;do b=a+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
          if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n1+1,n3
             sum=sum
     &     + x4(e,d)*t4a(e,c,b,a,l,k,j,i)           !edecbalkji      (+1.000)
     &     - x4(e,c)*t4a(e,d,b,a,l,k,j,i)           !ecedbalkji      (-1.000)
     &     + x4(e,b)*t4a(e,d,c,a,l,k,j,i)           !ebedcalkji      (+1.000)
     &     - x4(e,a)*t4a(e,d,c,b,l,k,j,i)           !eaedcblkji      (-1.000)
             enddo
             v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call
!     & sum23456781(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z4, 1.000)
!       call
!     & sum13456782(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z4,-1.000)
!       call
!     & sum12456783(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z4, 1.000)
!       call
!     & sum12356784(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z4,-1.000)
!c
!       do i=n0+1,n1-3
!       do j=i+1,n1-2
!       do k=j+1,n1-1
!       do l=k+1,n1
!       do a=n1+1,n3-3
!       do b=a+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)
!     & +z4(c,b,a,l,k,j,i,d)       ! 23456781 (+1.000)
!     & -z4(d,b,a,l,k,j,i,c)       ! 13456782 (-1.000)
!     & +z4(d,c,a,l,k,j,i,b)       ! 12456783 (+1.000)
!     & -z4(d,c,b,l,k,j,i,a)       ! 12356784 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z4)
       deallocate(x4)
c
       allocate(d1(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       call reorder3142(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n1,n3,n0,n1,intr,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(q16(n1+1:n3,n0+1:n1))
       i1=k1*k3
       i3=k1*k3
       call egemm1(i1,i3,d1,b2,q16)
       deallocate(d1)
       deallocate(b2)
c
       allocate(b1(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,q16,b1)
       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
       allocate(s45(n1+1:n3,n1+1:n3,n0+1:n1,n1+1:n3))
       i1=k3
       i2=k1*k3*k3
       i3=k1
       call egemm(i1,i2,i3,b1,d2,s45)
       deallocate(b1)
       deallocate(d2)
       deallocate(q16)
c
       call sum2341(n1,n3,n1,n3,n1,n3,n0,n1,x2,s45,-1.000)
       deallocate(s45)
c
       call sumx3241(n0,n3,n1,n3,n1,n3,n1,n3,n0,n1,x2,intr, 1.000)
c
!       allocate(f2(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
!       call reorder123456(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,
!     & n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,t3a,f2)
!       allocate(z2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
!       i1=k1*k3*k3
!       i2=k1*k1*k1*k3*k3
!       i3=k3
!       call egemm(i1,i2,i3,x2,f2,z2)
!       deallocate(f2)
c
       do i=n0+1,n1-3;do j=i+1,n1-2;do k=j+1,n1-1;do l=k+1,n1
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-3;do b=a+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
          if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n1+1,n3
             sum=sum
     &     + x2(e,d,c,i)*t3a(e,b,a,l,k,j)           !edciebalkj      (+1.000)
     &     - x2(e,d,b,i)*t3a(e,c,a,l,k,j)           !edbiecalkj      (-1.000)
     &     + x2(e,d,a,i)*t3a(e,c,b,l,k,j)           !edaiecblkj      (+1.000)
     &     + x2(e,c,b,i)*t3a(e,d,a,l,k,j)           !ecbiedalkj      (+1.000)
     &     - x2(e,c,a,i)*t3a(e,d,b,l,k,j)           !ecaiedblkj      (-1.000)
     &     + x2(e,b,a,i)*t3a(e,d,c,l,k,j)           !ebaiedclkj      (+1.000)
     &     - x2(e,d,c,j)*t3a(e,b,a,l,k,i)           !edcjebalki      (-1.000)
     &     + x2(e,d,b,j)*t3a(e,c,a,l,k,i)           !edbjecalki      (+1.000)
     &     - x2(e,d,a,j)*t3a(e,c,b,l,k,i)           !edajecblki      (-1.000)
     &     - x2(e,c,b,j)*t3a(e,d,a,l,k,i)           !ecbjedalki      (-1.000)
     &     + x2(e,c,a,j)*t3a(e,d,b,l,k,i)           !ecajedblki      (+1.000)
     &     - x2(e,b,a,j)*t3a(e,d,c,l,k,i)           !ebajedclki      (-1.000)
     &     + x2(e,d,c,k)*t3a(e,b,a,l,j,i)           !edckebalji      (+1.000)
     &     - x2(e,d,b,k)*t3a(e,c,a,l,j,i)           !edbkecalji      (-1.000)
     &     + x2(e,d,a,k)*t3a(e,c,b,l,j,i)           !edakecblji      (+1.000)
     &     + x2(e,c,b,k)*t3a(e,d,a,l,j,i)           !ecbkedalji      (+1.000)
     &     - x2(e,c,a,k)*t3a(e,d,b,l,j,i)           !ecakedblji      (-1.000)
     &     + x2(e,b,a,k)*t3a(e,d,c,l,j,i)           !ebakedclji      (+1.000)
     &     - x2(e,d,c,l)*t3a(e,b,a,k,j,i)           !edclebakji      (-1.000)
     &     + x2(e,d,b,l)*t3a(e,c,a,k,j,i)           !edblecakji      (+1.000)
     &     - x2(e,d,a,l)*t3a(e,c,b,k,j,i)           !edalecbkji      (-1.000)
     &     - x2(e,c,b,l)*t3a(e,d,a,k,j,i)           !ecbledakji      (-1.000)
     &     + x2(e,c,a,l)*t3a(e,d,b,k,j,i)           !ecaledbkji      (+1.000)
     &     - x2(e,b,a,l)*t3a(e,d,c,k,j,i)           !ebaledckji      (-1.000)
             enddo
             v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call
!     & sum34567128(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z2, 1.000)
!       call
!     & sum24567138(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z2,-1.000)
!       call
!     & sum23567148(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z2, 1.000)
!       call
!     & sum14567238(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z2, 1.000)
!       call
!     & sum13567248(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z2,-1.000)
!       call
!     & sum12567348(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z2, 1.000)
!       call
!     & sum34568127(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z2,-1.000)
!       call
!     & sum24568137(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z2, 1.000)
!       call
!     & sum23568147(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z2,-1.000)
!       call
!     & sum14568237(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z2,-1.000)
!       call
!     & sum13568247(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z2, 1.000)
!       call
!     & sum12568347(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z2,-1.000)
!       call
!     & sum34578126(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z2, 1.000)
!       call
!     & sum24578136(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z2,-1.000)
!       call
!     & sum23578146(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z2, 1.000)
!       call
!     & sum14578236(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z2, 1.000)
!       call
!     & sum13578246(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z2,-1.000)
!       call
!     & sum12578346(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z2, 1.000)
!       call
!     & sum34678125(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z2,-1.000)
!       call
!     & sum24678135(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z2, 1.000)
!       call
!     & sum23678145(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z2,-1.000)
!       call
!     & sum14678235(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z2,-1.000)
!       call
!     & sum13678245(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z2, 1.000)
!       call
!     & sum12678345(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,v4a,z2,-1.000)
!c
!       do i=n0+1,n1-3
!       do j=i+1,n1-2
!       do k=j+1,n1-1
!       do l=k+1,n1
!       do a=n1+1,n3-3
!       do b=a+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4a(d,c,b,a,l,k,j,i)=v4a(d,c,b,a,l,k,j,i)
!     & +z2(b,a,l,k,j,d,c,i)       ! 34567128 (+1.000)
!     & -z2(c,a,l,k,j,d,b,i)       ! 24567138 (-1.000)
!     & +z2(c,b,l,k,j,d,a,i)       ! 23567148 (+1.000)
!     & +z2(d,a,l,k,j,c,b,i)       ! 14567238 (+1.000)
!     & -z2(d,b,l,k,j,c,a,i)       ! 13567248 (-1.000)
!     & +z2(d,c,l,k,j,b,a,i)       ! 12567348 (+1.000)
!     & -z2(b,a,l,k,i,d,c,j)       ! 34568127 (-1.000)
!     & +z2(c,a,l,k,i,d,b,j)       ! 24568137 (+1.000)
!     & -z2(c,b,l,k,i,d,a,j)       ! 23568147 (-1.000)
!     & -z2(d,a,l,k,i,c,b,j)       ! 14568237 (-1.000)
!     & +z2(d,b,l,k,i,c,a,j)       ! 13568247 (+1.000)
!     & -z2(d,c,l,k,i,b,a,j)       ! 12568347 (-1.000)
!     & +z2(b,a,l,j,i,d,c,k)       ! 34578126 (+1.000)
!     & -z2(c,a,l,j,i,d,b,k)       ! 24578136 (-1.000)
!     & +z2(c,b,l,j,i,d,a,k)       ! 23578146 (+1.000)
!     & +z2(d,a,l,j,i,c,b,k)       ! 14578236 (+1.000)
!     & -z2(d,b,l,j,i,c,a,k)       ! 13578246 (-1.000)
!     & +z2(d,c,l,j,i,b,a,k)       ! 12578346 (+1.000)
!     & -z2(b,a,k,j,i,d,c,l)       ! 34678125 (-1.000)
!     & +z2(c,a,k,j,i,d,b,l)       ! 24678135 (+1.000)
!     & -z2(c,b,k,j,i,d,a,l)       ! 23678145 (-1.000)
!     & -z2(d,a,k,j,i,c,b,l)       ! 14678235 (-1.000)
!     & +z2(d,b,k,j,i,c,a,l)       ! 13678245 (+1.000)
!     & -z2(d,c,k,j,i,b,a,l)       ! 12678345 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z2)
       deallocate(x2)
c
c
       do i=n0+1,n1-3
       do j=i+1,n1-2
       do k=j+1,n1-1
       do l=k+1,n1
        if(indocc(l,k,j,i).eq.1)cycle
       do a=n1+1,n3-3
       do b=a+1,n3-2
       do c=b+1,n3-1
       do d=c+1,n3
          if(indunocc(d,c,b,a).eq.1)cycle
!
!        iocca=0
!        iunoa=0
!        if(i.gt.(n1-iactocca))iocca=iocca+1
!        if(j.gt.(n1-iactocca))iocca=iocca+1
!        if(k.gt.(n1-iactocca))iocca=iocca+1
!        if(l.gt.(n1-iactocca))iocca=iocca+1
!        if(iocca.lt.iactindq)cycle
!        if(a.lt.(n1+iactunoa+1))iunoa=iunoa+1
!        if(b.lt.(n1+iactunoa+1))iunoa=iunoa+1
!        if(c.lt.(n1+iactunoa+1))iunoa=iunoa+1
!        if(d.lt.(n1+iactunoa+1))iunoa=iunoa+1
!        if(iunoa.lt.iactindq)cycle
!
         coeleft=fockr(d,d)
     &          +fockr(c,c)
     &          +fockr(b,b)
     &          +fockr(a,a)
     &          -fockr(l,l)
     &          -fockr(k,k)
     &          -fockr(j,j)
     &          -fockr(i,i)
     &          +shift
         t4a(d,c,b,a,l,k,j,i)=t4a(d,c,b,a,l,k,j,i)-v4a(d,c,b,a,l,k,j,
     & i)/coeleft
         t4a(d,c,b,a,l,k,i,j)=-t4a(d,c,b,a,l,k,j,i)
         t4a(d,c,b,a,l,i,j,k)=-t4a(d,c,b,a,l,k,j,i)
         t4a(d,c,b,a,l,i,k,j)= t4a(d,c,b,a,l,k,j,i)
         t4a(d,c,b,a,i,k,j,l)=-t4a(d,c,b,a,l,k,j,i)
         t4a(d,c,b,a,i,k,l,j)= t4a(d,c,b,a,l,k,j,i)
         t4a(d,c,b,a,i,l,j,k)= t4a(d,c,b,a,l,k,j,i)
         t4a(d,c,b,a,i,l,k,j)=-t4a(d,c,b,a,l,k,j,i)
         t4a(d,c,b,a,l,j,k,i)=-t4a(d,c,b,a,l,k,j,i)
         t4a(d,c,b,a,l,j,i,k)= t4a(d,c,b,a,l,k,j,i)
         t4a(d,c,b,a,i,j,k,l)= t4a(d,c,b,a,l,k,j,i)
         t4a(d,c,b,a,i,j,l,k)=-t4a(d,c,b,a,l,k,j,i)
         t4a(d,c,b,a,j,k,l,i)=-t4a(d,c,b,a,l,k,j,i)
         t4a(d,c,b,a,j,k,i,l)= t4a(d,c,b,a,l,k,j,i)
         t4a(d,c,b,a,j,i,l,k)= t4a(d,c,b,a,l,k,j,i)
         t4a(d,c,b,a,j,i,k,l)=-t4a(d,c,b,a,l,k,j,i)
         t4a(d,c,b,a,j,l,k,i)= t4a(d,c,b,a,l,k,j,i)
         t4a(d,c,b,a,j,l,i,k)=-t4a(d,c,b,a,l,k,j,i)
         t4a(d,c,b,a,k,l,j,i)=-t4a(d,c,b,a,l,k,j,i)
         t4a(d,c,b,a,k,l,i,j)= t4a(d,c,b,a,l,k,j,i)
         t4a(d,c,b,a,k,i,j,l)= t4a(d,c,b,a,l,k,j,i)
         t4a(d,c,b,a,k,i,l,j)=-t4a(d,c,b,a,l,k,j,i)
         t4a(d,c,b,a,k,j,l,i)= t4a(d,c,b,a,l,k,j,i)
         t4a(d,c,b,a,k,j,i,l)=-t4a(d,c,b,a,l,k,j,i)
         t4a(d,c,a,b,l,k,j,i)=-t4a(d,c,b,a,l,k,j,i)
         t4a(d,c,a,b,l,k,i,j)= t4a(d,c,b,a,l,k,j,i)
         t4a(d,c,a,b,l,i,j,k)= t4a(d,c,b,a,l,k,j,i)
         t4a(d,c,a,b,l,i,k,j)=-t4a(d,c,b,a,l,k,j,i)
         t4a(d,c,a,b,i,k,j,l)= t4a(d,c,b,a,l,k,j,i)
         t4a(d,c,a,b,i,k,l,j)=-t4a(d,c,b,a,l,k,j,i)
         t4a(d,c,a,b,i,l,j,k)=-t4a(d,c,b,a,l,k,j,i)
         t4a(d,c,a,b,i,l,k,j)= t4a(d,c,b,a,l,k,j,i)
         t4a(d,c,a,b,l,j,k,i)= t4a(d,c,b,a,l,k,j,i)
         t4a(d,c,a,b,l,j,i,k)=-t4a(d,c,b,a,l,k,j,i)
         t4a(d,c,a,b,i,j,k,l)=-t4a(d,c,b,a,l,k,j,i)
         t4a(d,c,a,b,i,j,l,k)= t4a(d,c,b,a,l,k,j,i)
         t4a(d,c,a,b,j,k,l,i)= t4a(d,c,b,a,l,k,j,i)
         t4a(d,c,a,b,j,k,i,l)=-t4a(d,c,b,a,l,k,j,i)
         t4a(d,c,a,b,j,i,l,k)=-t4a(d,c,b,a,l,k,j,i)
         t4a(d,c,a,b,j,i,k,l)= t4a(d,c,b,a,l,k,j,i)
         t4a(d,c,a,b,j,l,k,i)=-t4a(d,c,b,a,l,k,j,i)
         t4a(d,c,a,b,j,l,i,k)= t4a(d,c,b,a,l,k,j,i)
         t4a(d,c,a,b,k,l,j,i)= t4a(d,c,b,a,l,k,j,i)
         t4a(d,c,a,b,k,l,i,j)=-t4a(d,c,b,a,l,k,j,i)
         t4a(d,c,a,b,k,i,j,l)=-t4a(d,c,b,a,l,k,j,i)
         t4a(d,c,a,b,k,i,l,j)= t4a(d,c,b,a,l,k,j,i)
         t4a(d,c,a,b,k,j,l,i)=-t4a(d,c,b,a,l,k,j,i)
         t4a(d,c,a,b,k,j,i,l)= t4a(d,c,b,a,l,k,j,i)
         t4a(d,a,b,c,l,k,j,i)=-t4a(d,c,b,a,l,k,j,i)
         t4a(d,a,b,c,l,k,i,j)= t4a(d,c,b,a,l,k,j,i)
         t4a(d,a,b,c,l,i,j,k)= t4a(d,c,b,a,l,k,j,i)
         t4a(d,a,b,c,l,i,k,j)=-t4a(d,c,b,a,l,k,j,i)
         t4a(d,a,b,c,i,k,j,l)= t4a(d,c,b,a,l,k,j,i)
         t4a(d,a,b,c,i,k,l,j)=-t4a(d,c,b,a,l,k,j,i)
         t4a(d,a,b,c,i,l,j,k)=-t4a(d,c,b,a,l,k,j,i)
         t4a(d,a,b,c,i,l,k,j)= t4a(d,c,b,a,l,k,j,i)
         t4a(d,a,b,c,l,j,k,i)= t4a(d,c,b,a,l,k,j,i)
         t4a(d,a,b,c,l,j,i,k)=-t4a(d,c,b,a,l,k,j,i)
         t4a(d,a,b,c,i,j,k,l)=-t4a(d,c,b,a,l,k,j,i)
         t4a(d,a,b,c,i,j,l,k)= t4a(d,c,b,a,l,k,j,i)
         t4a(d,a,b,c,j,k,l,i)= t4a(d,c,b,a,l,k,j,i)
         t4a(d,a,b,c,j,k,i,l)=-t4a(d,c,b,a,l,k,j,i)
         t4a(d,a,b,c,j,i,l,k)=-t4a(d,c,b,a,l,k,j,i)
         t4a(d,a,b,c,j,i,k,l)= t4a(d,c,b,a,l,k,j,i)
         t4a(d,a,b,c,j,l,k,i)=-t4a(d,c,b,a,l,k,j,i)
         t4a(d,a,b,c,j,l,i,k)= t4a(d,c,b,a,l,k,j,i)
         t4a(d,a,b,c,k,l,j,i)= t4a(d,c,b,a,l,k,j,i)
         t4a(d,a,b,c,k,l,i,j)=-t4a(d,c,b,a,l,k,j,i)
         t4a(d,a,b,c,k,i,j,l)=-t4a(d,c,b,a,l,k,j,i)
         t4a(d,a,b,c,k,i,l,j)= t4a(d,c,b,a,l,k,j,i)
         t4a(d,a,b,c,k,j,l,i)=-t4a(d,c,b,a,l,k,j,i)
         t4a(d,a,b,c,k,j,i,l)= t4a(d,c,b,a,l,k,j,i)
         t4a(d,a,c,b,l,k,j,i)= t4a(d,c,b,a,l,k,j,i)
         t4a(d,a,c,b,l,k,i,j)=-t4a(d,c,b,a,l,k,j,i)
         t4a(d,a,c,b,l,i,j,k)=-t4a(d,c,b,a,l,k,j,i)
         t4a(d,a,c,b,l,i,k,j)= t4a(d,c,b,a,l,k,j,i)
         t4a(d,a,c,b,i,k,j,l)=-t4a(d,c,b,a,l,k,j,i)
         t4a(d,a,c,b,i,k,l,j)= t4a(d,c,b,a,l,k,j,i)
         t4a(d,a,c,b,i,l,j,k)= t4a(d,c,b,a,l,k,j,i)
         t4a(d,a,c,b,i,l,k,j)=-t4a(d,c,b,a,l,k,j,i)
         t4a(d,a,c,b,l,j,k,i)=-t4a(d,c,b,a,l,k,j,i)
         t4a(d,a,c,b,l,j,i,k)= t4a(d,c,b,a,l,k,j,i)
         t4a(d,a,c,b,i,j,k,l)= t4a(d,c,b,a,l,k,j,i)
         t4a(d,a,c,b,i,j,l,k)=-t4a(d,c,b,a,l,k,j,i)
         t4a(d,a,c,b,j,k,l,i)=-t4a(d,c,b,a,l,k,j,i)
         t4a(d,a,c,b,j,k,i,l)= t4a(d,c,b,a,l,k,j,i)
         t4a(d,a,c,b,j,i,l,k)= t4a(d,c,b,a,l,k,j,i)
         t4a(d,a,c,b,j,i,k,l)=-t4a(d,c,b,a,l,k,j,i)
         t4a(d,a,c,b,j,l,k,i)= t4a(d,c,b,a,l,k,j,i)
         t4a(d,a,c,b,j,l,i,k)=-t4a(d,c,b,a,l,k,j,i)
         t4a(d,a,c,b,k,l,j,i)=-t4a(d,c,b,a,l,k,j,i)
         t4a(d,a,c,b,k,l,i,j)= t4a(d,c,b,a,l,k,j,i)
         t4a(d,a,c,b,k,i,j,l)= t4a(d,c,b,a,l,k,j,i)
         t4a(d,a,c,b,k,i,l,j)=-t4a(d,c,b,a,l,k,j,i)
         t4a(d,a,c,b,k,j,l,i)= t4a(d,c,b,a,l,k,j,i)
         t4a(d,a,c,b,k,j,i,l)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,c,b,d,l,k,j,i)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,c,b,d,l,k,i,j)= t4a(d,c,b,a,l,k,j,i)
         t4a(a,c,b,d,l,i,j,k)= t4a(d,c,b,a,l,k,j,i)
         t4a(a,c,b,d,l,i,k,j)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,c,b,d,i,k,j,l)= t4a(d,c,b,a,l,k,j,i)
         t4a(a,c,b,d,i,k,l,j)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,c,b,d,i,l,j,k)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,c,b,d,i,l,k,j)= t4a(d,c,b,a,l,k,j,i)
         t4a(a,c,b,d,l,j,k,i)= t4a(d,c,b,a,l,k,j,i)
         t4a(a,c,b,d,l,j,i,k)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,c,b,d,i,j,k,l)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,c,b,d,i,j,l,k)= t4a(d,c,b,a,l,k,j,i)
         t4a(a,c,b,d,j,k,l,i)= t4a(d,c,b,a,l,k,j,i)
         t4a(a,c,b,d,j,k,i,l)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,c,b,d,j,i,l,k)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,c,b,d,j,i,k,l)= t4a(d,c,b,a,l,k,j,i)
         t4a(a,c,b,d,j,l,k,i)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,c,b,d,j,l,i,k)= t4a(d,c,b,a,l,k,j,i)
         t4a(a,c,b,d,k,l,j,i)= t4a(d,c,b,a,l,k,j,i)
         t4a(a,c,b,d,k,l,i,j)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,c,b,d,k,i,j,l)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,c,b,d,k,i,l,j)= t4a(d,c,b,a,l,k,j,i)
         t4a(a,c,b,d,k,j,l,i)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,c,b,d,k,j,i,l)= t4a(d,c,b,a,l,k,j,i)
         t4a(a,c,d,b,l,k,j,i)= t4a(d,c,b,a,l,k,j,i)
         t4a(a,c,d,b,l,k,i,j)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,c,d,b,l,i,j,k)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,c,d,b,l,i,k,j)= t4a(d,c,b,a,l,k,j,i)
         t4a(a,c,d,b,i,k,j,l)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,c,d,b,i,k,l,j)= t4a(d,c,b,a,l,k,j,i)
         t4a(a,c,d,b,i,l,j,k)= t4a(d,c,b,a,l,k,j,i)
         t4a(a,c,d,b,i,l,k,j)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,c,d,b,l,j,k,i)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,c,d,b,l,j,i,k)= t4a(d,c,b,a,l,k,j,i)
         t4a(a,c,d,b,i,j,k,l)= t4a(d,c,b,a,l,k,j,i)
         t4a(a,c,d,b,i,j,l,k)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,c,d,b,j,k,l,i)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,c,d,b,j,k,i,l)= t4a(d,c,b,a,l,k,j,i)
         t4a(a,c,d,b,j,i,l,k)= t4a(d,c,b,a,l,k,j,i)
         t4a(a,c,d,b,j,i,k,l)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,c,d,b,j,l,k,i)= t4a(d,c,b,a,l,k,j,i)
         t4a(a,c,d,b,j,l,i,k)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,c,d,b,k,l,j,i)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,c,d,b,k,l,i,j)= t4a(d,c,b,a,l,k,j,i)
         t4a(a,c,d,b,k,i,j,l)= t4a(d,c,b,a,l,k,j,i)
         t4a(a,c,d,b,k,i,l,j)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,c,d,b,k,j,l,i)= t4a(d,c,b,a,l,k,j,i)
         t4a(a,c,d,b,k,j,i,l)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,d,b,c,l,k,j,i)= t4a(d,c,b,a,l,k,j,i)
         t4a(a,d,b,c,l,k,i,j)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,d,b,c,l,i,j,k)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,d,b,c,l,i,k,j)= t4a(d,c,b,a,l,k,j,i)
         t4a(a,d,b,c,i,k,j,l)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,d,b,c,i,k,l,j)= t4a(d,c,b,a,l,k,j,i)
         t4a(a,d,b,c,i,l,j,k)= t4a(d,c,b,a,l,k,j,i)
         t4a(a,d,b,c,i,l,k,j)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,d,b,c,l,j,k,i)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,d,b,c,l,j,i,k)= t4a(d,c,b,a,l,k,j,i)
         t4a(a,d,b,c,i,j,k,l)= t4a(d,c,b,a,l,k,j,i)
         t4a(a,d,b,c,i,j,l,k)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,d,b,c,j,k,l,i)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,d,b,c,j,k,i,l)= t4a(d,c,b,a,l,k,j,i)
         t4a(a,d,b,c,j,i,l,k)= t4a(d,c,b,a,l,k,j,i)
         t4a(a,d,b,c,j,i,k,l)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,d,b,c,j,l,k,i)= t4a(d,c,b,a,l,k,j,i)
         t4a(a,d,b,c,j,l,i,k)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,d,b,c,k,l,j,i)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,d,b,c,k,l,i,j)= t4a(d,c,b,a,l,k,j,i)
         t4a(a,d,b,c,k,i,j,l)= t4a(d,c,b,a,l,k,j,i)
         t4a(a,d,b,c,k,i,l,j)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,d,b,c,k,j,l,i)= t4a(d,c,b,a,l,k,j,i)
         t4a(a,d,b,c,k,j,i,l)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,d,c,b,l,k,j,i)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,d,c,b,l,k,i,j)= t4a(d,c,b,a,l,k,j,i)
         t4a(a,d,c,b,l,i,j,k)= t4a(d,c,b,a,l,k,j,i)
         t4a(a,d,c,b,l,i,k,j)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,d,c,b,i,k,j,l)= t4a(d,c,b,a,l,k,j,i)
         t4a(a,d,c,b,i,k,l,j)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,d,c,b,i,l,j,k)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,d,c,b,i,l,k,j)= t4a(d,c,b,a,l,k,j,i)
         t4a(a,d,c,b,l,j,k,i)= t4a(d,c,b,a,l,k,j,i)
         t4a(a,d,c,b,l,j,i,k)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,d,c,b,i,j,k,l)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,d,c,b,i,j,l,k)= t4a(d,c,b,a,l,k,j,i)
         t4a(a,d,c,b,j,k,l,i)= t4a(d,c,b,a,l,k,j,i)
         t4a(a,d,c,b,j,k,i,l)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,d,c,b,j,i,l,k)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,d,c,b,j,i,k,l)= t4a(d,c,b,a,l,k,j,i)
         t4a(a,d,c,b,j,l,k,i)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,d,c,b,j,l,i,k)= t4a(d,c,b,a,l,k,j,i)
         t4a(a,d,c,b,k,l,j,i)= t4a(d,c,b,a,l,k,j,i)
         t4a(a,d,c,b,k,l,i,j)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,d,c,b,k,i,j,l)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,d,c,b,k,i,l,j)= t4a(d,c,b,a,l,k,j,i)
         t4a(a,d,c,b,k,j,l,i)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,d,c,b,k,j,i,l)= t4a(d,c,b,a,l,k,j,i)
         t4a(d,b,c,a,l,k,j,i)=-t4a(d,c,b,a,l,k,j,i)
         t4a(d,b,c,a,l,k,i,j)= t4a(d,c,b,a,l,k,j,i)
         t4a(d,b,c,a,l,i,j,k)= t4a(d,c,b,a,l,k,j,i)
         t4a(d,b,c,a,l,i,k,j)=-t4a(d,c,b,a,l,k,j,i)
         t4a(d,b,c,a,i,k,j,l)= t4a(d,c,b,a,l,k,j,i)
         t4a(d,b,c,a,i,k,l,j)=-t4a(d,c,b,a,l,k,j,i)
         t4a(d,b,c,a,i,l,j,k)=-t4a(d,c,b,a,l,k,j,i)
         t4a(d,b,c,a,i,l,k,j)= t4a(d,c,b,a,l,k,j,i)
         t4a(d,b,c,a,l,j,k,i)= t4a(d,c,b,a,l,k,j,i)
         t4a(d,b,c,a,l,j,i,k)=-t4a(d,c,b,a,l,k,j,i)
         t4a(d,b,c,a,i,j,k,l)=-t4a(d,c,b,a,l,k,j,i)
         t4a(d,b,c,a,i,j,l,k)= t4a(d,c,b,a,l,k,j,i)
         t4a(d,b,c,a,j,k,l,i)= t4a(d,c,b,a,l,k,j,i)
         t4a(d,b,c,a,j,k,i,l)=-t4a(d,c,b,a,l,k,j,i)
         t4a(d,b,c,a,j,i,l,k)=-t4a(d,c,b,a,l,k,j,i)
         t4a(d,b,c,a,j,i,k,l)= t4a(d,c,b,a,l,k,j,i)
         t4a(d,b,c,a,j,l,k,i)=-t4a(d,c,b,a,l,k,j,i)
         t4a(d,b,c,a,j,l,i,k)= t4a(d,c,b,a,l,k,j,i)
         t4a(d,b,c,a,k,l,j,i)= t4a(d,c,b,a,l,k,j,i)
         t4a(d,b,c,a,k,l,i,j)=-t4a(d,c,b,a,l,k,j,i)
         t4a(d,b,c,a,k,i,j,l)=-t4a(d,c,b,a,l,k,j,i)
         t4a(d,b,c,a,k,i,l,j)= t4a(d,c,b,a,l,k,j,i)
         t4a(d,b,c,a,k,j,l,i)=-t4a(d,c,b,a,l,k,j,i)
         t4a(d,b,c,a,k,j,i,l)= t4a(d,c,b,a,l,k,j,i)
         t4a(d,b,a,c,l,k,j,i)= t4a(d,c,b,a,l,k,j,i)
         t4a(d,b,a,c,l,k,i,j)=-t4a(d,c,b,a,l,k,j,i)
         t4a(d,b,a,c,l,i,j,k)=-t4a(d,c,b,a,l,k,j,i)
         t4a(d,b,a,c,l,i,k,j)= t4a(d,c,b,a,l,k,j,i)
         t4a(d,b,a,c,i,k,j,l)=-t4a(d,c,b,a,l,k,j,i)
         t4a(d,b,a,c,i,k,l,j)= t4a(d,c,b,a,l,k,j,i)
         t4a(d,b,a,c,i,l,j,k)= t4a(d,c,b,a,l,k,j,i)
         t4a(d,b,a,c,i,l,k,j)=-t4a(d,c,b,a,l,k,j,i)
         t4a(d,b,a,c,l,j,k,i)=-t4a(d,c,b,a,l,k,j,i)
         t4a(d,b,a,c,l,j,i,k)= t4a(d,c,b,a,l,k,j,i)
         t4a(d,b,a,c,i,j,k,l)= t4a(d,c,b,a,l,k,j,i)
         t4a(d,b,a,c,i,j,l,k)=-t4a(d,c,b,a,l,k,j,i)
         t4a(d,b,a,c,j,k,l,i)=-t4a(d,c,b,a,l,k,j,i)
         t4a(d,b,a,c,j,k,i,l)= t4a(d,c,b,a,l,k,j,i)
         t4a(d,b,a,c,j,i,l,k)= t4a(d,c,b,a,l,k,j,i)
         t4a(d,b,a,c,j,i,k,l)=-t4a(d,c,b,a,l,k,j,i)
         t4a(d,b,a,c,j,l,k,i)= t4a(d,c,b,a,l,k,j,i)
         t4a(d,b,a,c,j,l,i,k)=-t4a(d,c,b,a,l,k,j,i)
         t4a(d,b,a,c,k,l,j,i)=-t4a(d,c,b,a,l,k,j,i)
         t4a(d,b,a,c,k,l,i,j)= t4a(d,c,b,a,l,k,j,i)
         t4a(d,b,a,c,k,i,j,l)= t4a(d,c,b,a,l,k,j,i)
         t4a(d,b,a,c,k,i,l,j)=-t4a(d,c,b,a,l,k,j,i)
         t4a(d,b,a,c,k,j,l,i)= t4a(d,c,b,a,l,k,j,i)
         t4a(d,b,a,c,k,j,i,l)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,b,c,d,l,k,j,i)= t4a(d,c,b,a,l,k,j,i)
         t4a(a,b,c,d,l,k,i,j)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,b,c,d,l,i,j,k)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,b,c,d,l,i,k,j)= t4a(d,c,b,a,l,k,j,i)
         t4a(a,b,c,d,i,k,j,l)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,b,c,d,i,k,l,j)= t4a(d,c,b,a,l,k,j,i)
         t4a(a,b,c,d,i,l,j,k)= t4a(d,c,b,a,l,k,j,i)
         t4a(a,b,c,d,i,l,k,j)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,b,c,d,l,j,k,i)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,b,c,d,l,j,i,k)= t4a(d,c,b,a,l,k,j,i)
         t4a(a,b,c,d,i,j,k,l)= t4a(d,c,b,a,l,k,j,i)
         t4a(a,b,c,d,i,j,l,k)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,b,c,d,j,k,l,i)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,b,c,d,j,k,i,l)= t4a(d,c,b,a,l,k,j,i)
         t4a(a,b,c,d,j,i,l,k)= t4a(d,c,b,a,l,k,j,i)
         t4a(a,b,c,d,j,i,k,l)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,b,c,d,j,l,k,i)= t4a(d,c,b,a,l,k,j,i)
         t4a(a,b,c,d,j,l,i,k)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,b,c,d,k,l,j,i)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,b,c,d,k,l,i,j)= t4a(d,c,b,a,l,k,j,i)
         t4a(a,b,c,d,k,i,j,l)= t4a(d,c,b,a,l,k,j,i)
         t4a(a,b,c,d,k,i,l,j)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,b,c,d,k,j,l,i)= t4a(d,c,b,a,l,k,j,i)
         t4a(a,b,c,d,k,j,i,l)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,b,d,c,l,k,j,i)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,b,d,c,l,k,i,j)= t4a(d,c,b,a,l,k,j,i)
         t4a(a,b,d,c,l,i,j,k)= t4a(d,c,b,a,l,k,j,i)
         t4a(a,b,d,c,l,i,k,j)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,b,d,c,i,k,j,l)= t4a(d,c,b,a,l,k,j,i)
         t4a(a,b,d,c,i,k,l,j)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,b,d,c,i,l,j,k)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,b,d,c,i,l,k,j)= t4a(d,c,b,a,l,k,j,i)
         t4a(a,b,d,c,l,j,k,i)= t4a(d,c,b,a,l,k,j,i)
         t4a(a,b,d,c,l,j,i,k)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,b,d,c,i,j,k,l)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,b,d,c,i,j,l,k)= t4a(d,c,b,a,l,k,j,i)
         t4a(a,b,d,c,j,k,l,i)= t4a(d,c,b,a,l,k,j,i)
         t4a(a,b,d,c,j,k,i,l)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,b,d,c,j,i,l,k)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,b,d,c,j,i,k,l)= t4a(d,c,b,a,l,k,j,i)
         t4a(a,b,d,c,j,l,k,i)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,b,d,c,j,l,i,k)= t4a(d,c,b,a,l,k,j,i)
         t4a(a,b,d,c,k,l,j,i)= t4a(d,c,b,a,l,k,j,i)
         t4a(a,b,d,c,k,l,i,j)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,b,d,c,k,i,j,l)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,b,d,c,k,i,l,j)= t4a(d,c,b,a,l,k,j,i)
         t4a(a,b,d,c,k,j,l,i)=-t4a(d,c,b,a,l,k,j,i)
         t4a(a,b,d,c,k,j,i,l)= t4a(d,c,b,a,l,k,j,i)
         t4a(b,c,d,a,l,k,j,i)=-t4a(d,c,b,a,l,k,j,i)
         t4a(b,c,d,a,l,k,i,j)= t4a(d,c,b,a,l,k,j,i)
         t4a(b,c,d,a,l,i,j,k)= t4a(d,c,b,a,l,k,j,i)
         t4a(b,c,d,a,l,i,k,j)=-t4a(d,c,b,a,l,k,j,i)
         t4a(b,c,d,a,i,k,j,l)= t4a(d,c,b,a,l,k,j,i)
         t4a(b,c,d,a,i,k,l,j)=-t4a(d,c,b,a,l,k,j,i)
         t4a(b,c,d,a,i,l,j,k)=-t4a(d,c,b,a,l,k,j,i)
         t4a(b,c,d,a,i,l,k,j)= t4a(d,c,b,a,l,k,j,i)
         t4a(b,c,d,a,l,j,k,i)= t4a(d,c,b,a,l,k,j,i)
         t4a(b,c,d,a,l,j,i,k)=-t4a(d,c,b,a,l,k,j,i)
         t4a(b,c,d,a,i,j,k,l)=-t4a(d,c,b,a,l,k,j,i)
         t4a(b,c,d,a,i,j,l,k)= t4a(d,c,b,a,l,k,j,i)
         t4a(b,c,d,a,j,k,l,i)= t4a(d,c,b,a,l,k,j,i)
         t4a(b,c,d,a,j,k,i,l)=-t4a(d,c,b,a,l,k,j,i)
         t4a(b,c,d,a,j,i,l,k)=-t4a(d,c,b,a,l,k,j,i)
         t4a(b,c,d,a,j,i,k,l)= t4a(d,c,b,a,l,k,j,i)
         t4a(b,c,d,a,j,l,k,i)=-t4a(d,c,b,a,l,k,j,i)
         t4a(b,c,d,a,j,l,i,k)= t4a(d,c,b,a,l,k,j,i)
         t4a(b,c,d,a,k,l,j,i)= t4a(d,c,b,a,l,k,j,i)
         t4a(b,c,d,a,k,l,i,j)=-t4a(d,c,b,a,l,k,j,i)
         t4a(b,c,d,a,k,i,j,l)=-t4a(d,c,b,a,l,k,j,i)
         t4a(b,c,d,a,k,i,l,j)= t4a(d,c,b,a,l,k,j,i)
         t4a(b,c,d,a,k,j,l,i)=-t4a(d,c,b,a,l,k,j,i)
         t4a(b,c,d,a,k,j,i,l)= t4a(d,c,b,a,l,k,j,i)
         t4a(b,c,a,d,l,k,j,i)= t4a(d,c,b,a,l,k,j,i)
         t4a(b,c,a,d,l,k,i,j)=-t4a(d,c,b,a,l,k,j,i)
         t4a(b,c,a,d,l,i,j,k)=-t4a(d,c,b,a,l,k,j,i)
         t4a(b,c,a,d,l,i,k,j)= t4a(d,c,b,a,l,k,j,i)
         t4a(b,c,a,d,i,k,j,l)=-t4a(d,c,b,a,l,k,j,i)
         t4a(b,c,a,d,i,k,l,j)= t4a(d,c,b,a,l,k,j,i)
         t4a(b,c,a,d,i,l,j,k)= t4a(d,c,b,a,l,k,j,i)
         t4a(b,c,a,d,i,l,k,j)=-t4a(d,c,b,a,l,k,j,i)
         t4a(b,c,a,d,l,j,k,i)=-t4a(d,c,b,a,l,k,j,i)
         t4a(b,c,a,d,l,j,i,k)= t4a(d,c,b,a,l,k,j,i)
         t4a(b,c,a,d,i,j,k,l)= t4a(d,c,b,a,l,k,j,i)
         t4a(b,c,a,d,i,j,l,k)=-t4a(d,c,b,a,l,k,j,i)
         t4a(b,c,a,d,j,k,l,i)=-t4a(d,c,b,a,l,k,j,i)
         t4a(b,c,a,d,j,k,i,l)= t4a(d,c,b,a,l,k,j,i)
         t4a(b,c,a,d,j,i,l,k)= t4a(d,c,b,a,l,k,j,i)
         t4a(b,c,a,d,j,i,k,l)=-t4a(d,c,b,a,l,k,j,i)
         t4a(b,c,a,d,j,l,k,i)= t4a(d,c,b,a,l,k,j,i)
         t4a(b,c,a,d,j,l,i,k)=-t4a(d,c,b,a,l,k,j,i)
         t4a(b,c,a,d,k,l,j,i)=-t4a(d,c,b,a,l,k,j,i)
         t4a(b,c,a,d,k,l,i,j)= t4a(d,c,b,a,l,k,j,i)
         t4a(b,c,a,d,k,i,j,l)= t4a(d,c,b,a,l,k,j,i)
         t4a(b,c,a,d,k,i,l,j)=-t4a(d,c,b,a,l,k,j,i)
         t4a(b,c,a,d,k,j,l,i)= t4a(d,c,b,a,l,k,j,i)
         t4a(b,c,a,d,k,j,i,l)=-t4a(d,c,b,a,l,k,j,i)
         t4a(b,a,d,c,l,k,j,i)= t4a(d,c,b,a,l,k,j,i)
         t4a(b,a,d,c,l,k,i,j)=-t4a(d,c,b,a,l,k,j,i)
         t4a(b,a,d,c,l,i,j,k)=-t4a(d,c,b,a,l,k,j,i)
         t4a(b,a,d,c,l,i,k,j)= t4a(d,c,b,a,l,k,j,i)
         t4a(b,a,d,c,i,k,j,l)=-t4a(d,c,b,a,l,k,j,i)
         t4a(b,a,d,c,i,k,l,j)= t4a(d,c,b,a,l,k,j,i)
         t4a(b,a,d,c,i,l,j,k)= t4a(d,c,b,a,l,k,j,i)
         t4a(b,a,d,c,i,l,k,j)=-t4a(d,c,b,a,l,k,j,i)
         t4a(b,a,d,c,l,j,k,i)=-t4a(d,c,b,a,l,k,j,i)
         t4a(b,a,d,c,l,j,i,k)= t4a(d,c,b,a,l,k,j,i)
         t4a(b,a,d,c,i,j,k,l)= t4a(d,c,b,a,l,k,j,i)
         t4a(b,a,d,c,i,j,l,k)=-t4a(d,c,b,a,l,k,j,i)
         t4a(b,a,d,c,j,k,l,i)=-t4a(d,c,b,a,l,k,j,i)
         t4a(b,a,d,c,j,k,i,l)= t4a(d,c,b,a,l,k,j,i)
         t4a(b,a,d,c,j,i,l,k)= t4a(d,c,b,a,l,k,j,i)
         t4a(b,a,d,c,j,i,k,l)=-t4a(d,c,b,a,l,k,j,i)
         t4a(b,a,d,c,j,l,k,i)= t4a(d,c,b,a,l,k,j,i)
         t4a(b,a,d,c,j,l,i,k)=-t4a(d,c,b,a,l,k,j,i)
         t4a(b,a,d,c,k,l,j,i)=-t4a(d,c,b,a,l,k,j,i)
         t4a(b,a,d,c,k,l,i,j)= t4a(d,c,b,a,l,k,j,i)
         t4a(b,a,d,c,k,i,j,l)= t4a(d,c,b,a,l,k,j,i)
         t4a(b,a,d,c,k,i,l,j)=-t4a(d,c,b,a,l,k,j,i)
         t4a(b,a,d,c,k,j,l,i)= t4a(d,c,b,a,l,k,j,i)
         t4a(b,a,d,c,k,j,i,l)=-t4a(d,c,b,a,l,k,j,i)
         t4a(b,a,c,d,l,k,j,i)=-t4a(d,c,b,a,l,k,j,i)
         t4a(b,a,c,d,l,k,i,j)= t4a(d,c,b,a,l,k,j,i)
         t4a(b,a,c,d,l,i,j,k)= t4a(d,c,b,a,l,k,j,i)
         t4a(b,a,c,d,l,i,k,j)=-t4a(d,c,b,a,l,k,j,i)
         t4a(b,a,c,d,i,k,j,l)= t4a(d,c,b,a,l,k,j,i)
         t4a(b,a,c,d,i,k,l,j)=-t4a(d,c,b,a,l,k,j,i)
         t4a(b,a,c,d,i,l,j,k)=-t4a(d,c,b,a,l,k,j,i)
         t4a(b,a,c,d,i,l,k,j)= t4a(d,c,b,a,l,k,j,i)
         t4a(b,a,c,d,l,j,k,i)= t4a(d,c,b,a,l,k,j,i)
         t4a(b,a,c,d,l,j,i,k)=-t4a(d,c,b,a,l,k,j,i)
         t4a(b,a,c,d,i,j,k,l)=-t4a(d,c,b,a,l,k,j,i)
         t4a(b,a,c,d,i,j,l,k)= t4a(d,c,b,a,l,k,j,i)
         t4a(b,a,c,d,j,k,l,i)= t4a(d,c,b,a,l,k,j,i)
         t4a(b,a,c,d,j,k,i,l)=-t4a(d,c,b,a,l,k,j,i)
         t4a(b,a,c,d,j,i,l,k)=-t4a(d,c,b,a,l,k,j,i)
         t4a(b,a,c,d,j,i,k,l)= t4a(d,c,b,a,l,k,j,i)
         t4a(b,a,c,d,j,l,k,i)=-t4a(d,c,b,a,l,k,j,i)
         t4a(b,a,c,d,j,l,i,k)= t4a(d,c,b,a,l,k,j,i)
         t4a(b,a,c,d,k,l,j,i)= t4a(d,c,b,a,l,k,j,i)
         t4a(b,a,c,d,k,l,i,j)=-t4a(d,c,b,a,l,k,j,i)
         t4a(b,a,c,d,k,i,j,l)=-t4a(d,c,b,a,l,k,j,i)
         t4a(b,a,c,d,k,i,l,j)= t4a(d,c,b,a,l,k,j,i)
         t4a(b,a,c,d,k,j,l,i)=-t4a(d,c,b,a,l,k,j,i)
         t4a(b,a,c,d,k,j,i,l)= t4a(d,c,b,a,l,k,j,i)
         t4a(b,d,c,a,l,k,j,i)= t4a(d,c,b,a,l,k,j,i)
         t4a(b,d,c,a,l,k,i,j)=-t4a(d,c,b,a,l,k,j,i)
         t4a(b,d,c,a,l,i,j,k)=-t4a(d,c,b,a,l,k,j,i)
         t4a(b,d,c,a,l,i,k,j)= t4a(d,c,b,a,l,k,j,i)
         t4a(b,d,c,a,i,k,j,l)=-t4a(d,c,b,a,l,k,j,i)
         t4a(b,d,c,a,i,k,l,j)= t4a(d,c,b,a,l,k,j,i)
         t4a(b,d,c,a,i,l,j,k)= t4a(d,c,b,a,l,k,j,i)
         t4a(b,d,c,a,i,l,k,j)=-t4a(d,c,b,a,l,k,j,i)
         t4a(b,d,c,a,l,j,k,i)=-t4a(d,c,b,a,l,k,j,i)
         t4a(b,d,c,a,l,j,i,k)= t4a(d,c,b,a,l,k,j,i)
         t4a(b,d,c,a,i,j,k,l)= t4a(d,c,b,a,l,k,j,i)
         t4a(b,d,c,a,i,j,l,k)=-t4a(d,c,b,a,l,k,j,i)
         t4a(b,d,c,a,j,k,l,i)=-t4a(d,c,b,a,l,k,j,i)
         t4a(b,d,c,a,j,k,i,l)= t4a(d,c,b,a,l,k,j,i)
         t4a(b,d,c,a,j,i,l,k)= t4a(d,c,b,a,l,k,j,i)
         t4a(b,d,c,a,j,i,k,l)=-t4a(d,c,b,a,l,k,j,i)
         t4a(b,d,c,a,j,l,k,i)= t4a(d,c,b,a,l,k,j,i)
         t4a(b,d,c,a,j,l,i,k)=-t4a(d,c,b,a,l,k,j,i)
         t4a(b,d,c,a,k,l,j,i)=-t4a(d,c,b,a,l,k,j,i)
         t4a(b,d,c,a,k,l,i,j)= t4a(d,c,b,a,l,k,j,i)
         t4a(b,d,c,a,k,i,j,l)= t4a(d,c,b,a,l,k,j,i)
         t4a(b,d,c,a,k,i,l,j)=-t4a(d,c,b,a,l,k,j,i)
         t4a(b,d,c,a,k,j,l,i)= t4a(d,c,b,a,l,k,j,i)
         t4a(b,d,c,a,k,j,i,l)=-t4a(d,c,b,a,l,k,j,i)
         t4a(b,d,a,c,l,k,j,i)=-t4a(d,c,b,a,l,k,j,i)
         t4a(b,d,a,c,l,k,i,j)= t4a(d,c,b,a,l,k,j,i)
         t4a(b,d,a,c,l,i,j,k)= t4a(d,c,b,a,l,k,j,i)
         t4a(b,d,a,c,l,i,k,j)=-t4a(d,c,b,a,l,k,j,i)
         t4a(b,d,a,c,i,k,j,l)= t4a(d,c,b,a,l,k,j,i)
         t4a(b,d,a,c,i,k,l,j)=-t4a(d,c,b,a,l,k,j,i)
         t4a(b,d,a,c,i,l,j,k)=-t4a(d,c,b,a,l,k,j,i)
         t4a(b,d,a,c,i,l,k,j)= t4a(d,c,b,a,l,k,j,i)
         t4a(b,d,a,c,l,j,k,i)= t4a(d,c,b,a,l,k,j,i)
         t4a(b,d,a,c,l,j,i,k)=-t4a(d,c,b,a,l,k,j,i)
         t4a(b,d,a,c,i,j,k,l)=-t4a(d,c,b,a,l,k,j,i)
         t4a(b,d,a,c,i,j,l,k)= t4a(d,c,b,a,l,k,j,i)
         t4a(b,d,a,c,j,k,l,i)= t4a(d,c,b,a,l,k,j,i)
         t4a(b,d,a,c,j,k,i,l)=-t4a(d,c,b,a,l,k,j,i)
         t4a(b,d,a,c,j,i,l,k)=-t4a(d,c,b,a,l,k,j,i)
         t4a(b,d,a,c,j,i,k,l)= t4a(d,c,b,a,l,k,j,i)
         t4a(b,d,a,c,j,l,k,i)=-t4a(d,c,b,a,l,k,j,i)
         t4a(b,d,a,c,j,l,i,k)= t4a(d,c,b,a,l,k,j,i)
         t4a(b,d,a,c,k,l,j,i)= t4a(d,c,b,a,l,k,j,i)
         t4a(b,d,a,c,k,l,i,j)=-t4a(d,c,b,a,l,k,j,i)
         t4a(b,d,a,c,k,i,j,l)=-t4a(d,c,b,a,l,k,j,i)
         t4a(b,d,a,c,k,i,l,j)= t4a(d,c,b,a,l,k,j,i)
         t4a(b,d,a,c,k,j,l,i)=-t4a(d,c,b,a,l,k,j,i)
         t4a(b,d,a,c,k,j,i,l)= t4a(d,c,b,a,l,k,j,i)
         t4a(c,d,b,a,l,k,j,i)=-t4a(d,c,b,a,l,k,j,i)
         t4a(c,d,b,a,l,k,i,j)= t4a(d,c,b,a,l,k,j,i)
         t4a(c,d,b,a,l,i,j,k)= t4a(d,c,b,a,l,k,j,i)
         t4a(c,d,b,a,l,i,k,j)=-t4a(d,c,b,a,l,k,j,i)
         t4a(c,d,b,a,i,k,j,l)= t4a(d,c,b,a,l,k,j,i)
         t4a(c,d,b,a,i,k,l,j)=-t4a(d,c,b,a,l,k,j,i)
         t4a(c,d,b,a,i,l,j,k)=-t4a(d,c,b,a,l,k,j,i)
         t4a(c,d,b,a,i,l,k,j)= t4a(d,c,b,a,l,k,j,i)
         t4a(c,d,b,a,l,j,k,i)= t4a(d,c,b,a,l,k,j,i)
         t4a(c,d,b,a,l,j,i,k)=-t4a(d,c,b,a,l,k,j,i)
         t4a(c,d,b,a,i,j,k,l)=-t4a(d,c,b,a,l,k,j,i)
         t4a(c,d,b,a,i,j,l,k)= t4a(d,c,b,a,l,k,j,i)
         t4a(c,d,b,a,j,k,l,i)= t4a(d,c,b,a,l,k,j,i)
         t4a(c,d,b,a,j,k,i,l)=-t4a(d,c,b,a,l,k,j,i)
         t4a(c,d,b,a,j,i,l,k)=-t4a(d,c,b,a,l,k,j,i)
         t4a(c,d,b,a,j,i,k,l)= t4a(d,c,b,a,l,k,j,i)
         t4a(c,d,b,a,j,l,k,i)=-t4a(d,c,b,a,l,k,j,i)
         t4a(c,d,b,a,j,l,i,k)= t4a(d,c,b,a,l,k,j,i)
         t4a(c,d,b,a,k,l,j,i)= t4a(d,c,b,a,l,k,j,i)
         t4a(c,d,b,a,k,l,i,j)=-t4a(d,c,b,a,l,k,j,i)
         t4a(c,d,b,a,k,i,j,l)=-t4a(d,c,b,a,l,k,j,i)
         t4a(c,d,b,a,k,i,l,j)= t4a(d,c,b,a,l,k,j,i)
         t4a(c,d,b,a,k,j,l,i)=-t4a(d,c,b,a,l,k,j,i)
         t4a(c,d,b,a,k,j,i,l)= t4a(d,c,b,a,l,k,j,i)
         t4a(c,d,a,b,l,k,j,i)= t4a(d,c,b,a,l,k,j,i)
         t4a(c,d,a,b,l,k,i,j)=-t4a(d,c,b,a,l,k,j,i)
         t4a(c,d,a,b,l,i,j,k)=-t4a(d,c,b,a,l,k,j,i)
         t4a(c,d,a,b,l,i,k,j)= t4a(d,c,b,a,l,k,j,i)
         t4a(c,d,a,b,i,k,j,l)=-t4a(d,c,b,a,l,k,j,i)
         t4a(c,d,a,b,i,k,l,j)= t4a(d,c,b,a,l,k,j,i)
         t4a(c,d,a,b,i,l,j,k)= t4a(d,c,b,a,l,k,j,i)
         t4a(c,d,a,b,i,l,k,j)=-t4a(d,c,b,a,l,k,j,i)
         t4a(c,d,a,b,l,j,k,i)=-t4a(d,c,b,a,l,k,j,i)
         t4a(c,d,a,b,l,j,i,k)= t4a(d,c,b,a,l,k,j,i)
         t4a(c,d,a,b,i,j,k,l)= t4a(d,c,b,a,l,k,j,i)
         t4a(c,d,a,b,i,j,l,k)=-t4a(d,c,b,a,l,k,j,i)
         t4a(c,d,a,b,j,k,l,i)=-t4a(d,c,b,a,l,k,j,i)
         t4a(c,d,a,b,j,k,i,l)= t4a(d,c,b,a,l,k,j,i)
         t4a(c,d,a,b,j,i,l,k)= t4a(d,c,b,a,l,k,j,i)
         t4a(c,d,a,b,j,i,k,l)=-t4a(d,c,b,a,l,k,j,i)
         t4a(c,d,a,b,j,l,k,i)= t4a(d,c,b,a,l,k,j,i)
         t4a(c,d,a,b,j,l,i,k)=-t4a(d,c,b,a,l,k,j,i)
         t4a(c,d,a,b,k,l,j,i)=-t4a(d,c,b,a,l,k,j,i)
         t4a(c,d,a,b,k,l,i,j)= t4a(d,c,b,a,l,k,j,i)
         t4a(c,d,a,b,k,i,j,l)= t4a(d,c,b,a,l,k,j,i)
         t4a(c,d,a,b,k,i,l,j)=-t4a(d,c,b,a,l,k,j,i)
         t4a(c,d,a,b,k,j,l,i)= t4a(d,c,b,a,l,k,j,i)
         t4a(c,d,a,b,k,j,i,l)=-t4a(d,c,b,a,l,k,j,i)
         t4a(c,a,b,d,l,k,j,i)= t4a(d,c,b,a,l,k,j,i)
         t4a(c,a,b,d,l,k,i,j)=-t4a(d,c,b,a,l,k,j,i)
         t4a(c,a,b,d,l,i,j,k)=-t4a(d,c,b,a,l,k,j,i)
         t4a(c,a,b,d,l,i,k,j)= t4a(d,c,b,a,l,k,j,i)
         t4a(c,a,b,d,i,k,j,l)=-t4a(d,c,b,a,l,k,j,i)
         t4a(c,a,b,d,i,k,l,j)= t4a(d,c,b,a,l,k,j,i)
         t4a(c,a,b,d,i,l,j,k)= t4a(d,c,b,a,l,k,j,i)
         t4a(c,a,b,d,i,l,k,j)=-t4a(d,c,b,a,l,k,j,i)
         t4a(c,a,b,d,l,j,k,i)=-t4a(d,c,b,a,l,k,j,i)
         t4a(c,a,b,d,l,j,i,k)= t4a(d,c,b,a,l,k,j,i)
         t4a(c,a,b,d,i,j,k,l)= t4a(d,c,b,a,l,k,j,i)
         t4a(c,a,b,d,i,j,l,k)=-t4a(d,c,b,a,l,k,j,i)
         t4a(c,a,b,d,j,k,l,i)=-t4a(d,c,b,a,l,k,j,i)
         t4a(c,a,b,d,j,k,i,l)= t4a(d,c,b,a,l,k,j,i)
         t4a(c,a,b,d,j,i,l,k)= t4a(d,c,b,a,l,k,j,i)
         t4a(c,a,b,d,j,i,k,l)=-t4a(d,c,b,a,l,k,j,i)
         t4a(c,a,b,d,j,l,k,i)= t4a(d,c,b,a,l,k,j,i)
         t4a(c,a,b,d,j,l,i,k)=-t4a(d,c,b,a,l,k,j,i)
         t4a(c,a,b,d,k,l,j,i)=-t4a(d,c,b,a,l,k,j,i)
         t4a(c,a,b,d,k,l,i,j)= t4a(d,c,b,a,l,k,j,i)
         t4a(c,a,b,d,k,i,j,l)= t4a(d,c,b,a,l,k,j,i)
         t4a(c,a,b,d,k,i,l,j)=-t4a(d,c,b,a,l,k,j,i)
         t4a(c,a,b,d,k,j,l,i)= t4a(d,c,b,a,l,k,j,i)
         t4a(c,a,b,d,k,j,i,l)=-t4a(d,c,b,a,l,k,j,i)
         t4a(c,a,d,b,l,k,j,i)=-t4a(d,c,b,a,l,k,j,i)
         t4a(c,a,d,b,l,k,i,j)= t4a(d,c,b,a,l,k,j,i)
         t4a(c,a,d,b,l,i,j,k)= t4a(d,c,b,a,l,k,j,i)
         t4a(c,a,d,b,l,i,k,j)=-t4a(d,c,b,a,l,k,j,i)
         t4a(c,a,d,b,i,k,j,l)= t4a(d,c,b,a,l,k,j,i)
         t4a(c,a,d,b,i,k,l,j)=-t4a(d,c,b,a,l,k,j,i)
         t4a(c,a,d,b,i,l,j,k)=-t4a(d,c,b,a,l,k,j,i)
         t4a(c,a,d,b,i,l,k,j)= t4a(d,c,b,a,l,k,j,i)
         t4a(c,a,d,b,l,j,k,i)= t4a(d,c,b,a,l,k,j,i)
         t4a(c,a,d,b,l,j,i,k)=-t4a(d,c,b,a,l,k,j,i)
         t4a(c,a,d,b,i,j,k,l)=-t4a(d,c,b,a,l,k,j,i)
         t4a(c,a,d,b,i,j,l,k)= t4a(d,c,b,a,l,k,j,i)
         t4a(c,a,d,b,j,k,l,i)= t4a(d,c,b,a,l,k,j,i)
         t4a(c,a,d,b,j,k,i,l)=-t4a(d,c,b,a,l,k,j,i)
         t4a(c,a,d,b,j,i,l,k)=-t4a(d,c,b,a,l,k,j,i)
         t4a(c,a,d,b,j,i,k,l)= t4a(d,c,b,a,l,k,j,i)
         t4a(c,a,d,b,j,l,k,i)=-t4a(d,c,b,a,l,k,j,i)
         t4a(c,a,d,b,j,l,i,k)= t4a(d,c,b,a,l,k,j,i)
         t4a(c,a,d,b,k,l,j,i)= t4a(d,c,b,a,l,k,j,i)
         t4a(c,a,d,b,k,l,i,j)=-t4a(d,c,b,a,l,k,j,i)
         t4a(c,a,d,b,k,i,j,l)=-t4a(d,c,b,a,l,k,j,i)
         t4a(c,a,d,b,k,i,l,j)= t4a(d,c,b,a,l,k,j,i)
         t4a(c,a,d,b,k,j,l,i)=-t4a(d,c,b,a,l,k,j,i)
         t4a(c,a,d,b,k,j,i,l)= t4a(d,c,b,a,l,k,j,i)
         t4a(c,b,d,a,l,k,j,i)= t4a(d,c,b,a,l,k,j,i)
         t4a(c,b,d,a,l,k,i,j)=-t4a(d,c,b,a,l,k,j,i)
         t4a(c,b,d,a,l,i,j,k)=-t4a(d,c,b,a,l,k,j,i)
         t4a(c,b,d,a,l,i,k,j)= t4a(d,c,b,a,l,k,j,i)
         t4a(c,b,d,a,i,k,j,l)=-t4a(d,c,b,a,l,k,j,i)
         t4a(c,b,d,a,i,k,l,j)= t4a(d,c,b,a,l,k,j,i)
         t4a(c,b,d,a,i,l,j,k)= t4a(d,c,b,a,l,k,j,i)
         t4a(c,b,d,a,i,l,k,j)=-t4a(d,c,b,a,l,k,j,i)
         t4a(c,b,d,a,l,j,k,i)=-t4a(d,c,b,a,l,k,j,i)
         t4a(c,b,d,a,l,j,i,k)= t4a(d,c,b,a,l,k,j,i)
         t4a(c,b,d,a,i,j,k,l)= t4a(d,c,b,a,l,k,j,i)
         t4a(c,b,d,a,i,j,l,k)=-t4a(d,c,b,a,l,k,j,i)
         t4a(c,b,d,a,j,k,l,i)=-t4a(d,c,b,a,l,k,j,i)
         t4a(c,b,d,a,j,k,i,l)= t4a(d,c,b,a,l,k,j,i)
         t4a(c,b,d,a,j,i,l,k)= t4a(d,c,b,a,l,k,j,i)
         t4a(c,b,d,a,j,i,k,l)=-t4a(d,c,b,a,l,k,j,i)
         t4a(c,b,d,a,j,l,k,i)= t4a(d,c,b,a,l,k,j,i)
         t4a(c,b,d,a,j,l,i,k)=-t4a(d,c,b,a,l,k,j,i)
         t4a(c,b,d,a,k,l,j,i)=-t4a(d,c,b,a,l,k,j,i)
         t4a(c,b,d,a,k,l,i,j)= t4a(d,c,b,a,l,k,j,i)
         t4a(c,b,d,a,k,i,j,l)= t4a(d,c,b,a,l,k,j,i)
         t4a(c,b,d,a,k,i,l,j)=-t4a(d,c,b,a,l,k,j,i)
         t4a(c,b,d,a,k,j,l,i)= t4a(d,c,b,a,l,k,j,i)
         t4a(c,b,d,a,k,j,i,l)=-t4a(d,c,b,a,l,k,j,i)
         t4a(c,b,a,d,l,k,j,i)=-t4a(d,c,b,a,l,k,j,i)
         t4a(c,b,a,d,l,k,i,j)= t4a(d,c,b,a,l,k,j,i)
         t4a(c,b,a,d,l,i,j,k)= t4a(d,c,b,a,l,k,j,i)
         t4a(c,b,a,d,l,i,k,j)=-t4a(d,c,b,a,l,k,j,i)
         t4a(c,b,a,d,i,k,j,l)= t4a(d,c,b,a,l,k,j,i)
         t4a(c,b,a,d,i,k,l,j)=-t4a(d,c,b,a,l,k,j,i)
         t4a(c,b,a,d,i,l,j,k)=-t4a(d,c,b,a,l,k,j,i)
         t4a(c,b,a,d,i,l,k,j)= t4a(d,c,b,a,l,k,j,i)
         t4a(c,b,a,d,l,j,k,i)= t4a(d,c,b,a,l,k,j,i)
         t4a(c,b,a,d,l,j,i,k)=-t4a(d,c,b,a,l,k,j,i)
         t4a(c,b,a,d,i,j,k,l)=-t4a(d,c,b,a,l,k,j,i)
         t4a(c,b,a,d,i,j,l,k)= t4a(d,c,b,a,l,k,j,i)
         t4a(c,b,a,d,j,k,l,i)= t4a(d,c,b,a,l,k,j,i)
         t4a(c,b,a,d,j,k,i,l)=-t4a(d,c,b,a,l,k,j,i)
         t4a(c,b,a,d,j,i,l,k)=-t4a(d,c,b,a,l,k,j,i)
         t4a(c,b,a,d,j,i,k,l)= t4a(d,c,b,a,l,k,j,i)
         t4a(c,b,a,d,j,l,k,i)=-t4a(d,c,b,a,l,k,j,i)
         t4a(c,b,a,d,j,l,i,k)= t4a(d,c,b,a,l,k,j,i)
         t4a(c,b,a,d,k,l,j,i)= t4a(d,c,b,a,l,k,j,i)
         t4a(c,b,a,d,k,l,i,j)=-t4a(d,c,b,a,l,k,j,i)
         t4a(c,b,a,d,k,i,j,l)=-t4a(d,c,b,a,l,k,j,i)
         t4a(c,b,a,d,k,i,l,j)= t4a(d,c,b,a,l,k,j,i)
         t4a(c,b,a,d,k,j,l,i)=-t4a(d,c,b,a,l,k,j,i)
         t4a(c,b,a,d,k,j,i,l)= t4a(d,c,b,a,l,k,j,i)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
       rewind(ta)
       write(ta)t4a
c
       end
