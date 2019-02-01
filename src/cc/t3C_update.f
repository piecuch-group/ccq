      subroutine t3c_update(n0,n1,n2,n3,k1,k2,k3,k4,lvl_q,shift,v3c,
     & fockr,fockb,intr,intb,intm,t1a,t1b,t2a,t2b,t2c,t3a,t3b,t3c,t3d,
     & iactocca,iactoccb,iactunoa,iactunob,iactindt,
     & t2diag3,t2diag4,t2diag5,t3diag1,t3diag2,t3diag3,t3diag4,t3diag5)

        implicit none

        integer n0, n1, n2, n3
        integer k1, k2, k3, k4

        integer a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p
        integer i3
        integer iactocca,iactoccb,iactunoa,iactunob,iactindt
        integer iocca,ioccb,iunoa,iunob
        integer,allocatable::indocc(:,:,:)
        integer,allocatable::indunocc(:,:,:)
        logical lvl_q
        real t2diag3,t2diag4,t2diag5
        real t3diag1,t3diag2,t3diag3,t3diag4,t3diag5
        real factor
        real*8 shift,pp,coeleft,time1,time2
        real*8 fockr(n3,n3)
        real*8 fockb(n3,n3)
        real*8 sum
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
        real*8 v3c(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1)

        real*8,allocatable::t4c(:,:,:,:,:,:,:,:)                     !ilias: if no quadruples comment out the following 2 lines
        real*8,allocatable::t4d(:,:,:,:,:,:,:,:)

        integer ta,tb,tc,td,te                                       !ilias: if no quadruples comment out the following 2 lines
        parameter(ta=29,tb=30,tc=31,td=32,te=33)

        integer i1,i2
        integer,allocatable::ind1(:,:,:,:)
        integer,allocatable::ind2(:,:,:,:)

        real*8,allocatable::b1(:,:)
        real*8,allocatable::b2(:,:)
        real*8,allocatable::d1(:,:,:,:)
        real*8,allocatable::d2(:,:,:,:)
        real*8,allocatable::f2(:,:,:,:,:,:)
        real*8,allocatable::h2(:,:,:,:,:,:,:,:)


        real*8,allocatable::s1(:,:,:,:)
        real*8,allocatable::s2(:,:,:,:)
        real*8,allocatable::s3(:,:,:,:)
        real*8,allocatable::s4(:,:,:,:)
        real*8,allocatable::s5(:,:,:,:)
        real*8,allocatable::s6(:,:,:,:)
        real*8,allocatable::q1(:,:)
        real*8,allocatable::q2(:,:)
        real*8,allocatable::s7(:,:,:,:)
        real*8,allocatable::q3(:,:)
        real*8,allocatable::s8(:,:,:,:)
        real*8,allocatable::q4(:,:)
        real*8,allocatable::s9(:,:,:,:)
        real*8,allocatable::s98(:,:,:,:)
        real*8,allocatable::s94(:,:,:,:)
        real*8,allocatable::s10(:,:,:,:)
        real*8,allocatable::s99(:,:,:,:)
        real*8,allocatable::s95(:,:,:,:)
        real*8,allocatable::s11(:,:,:,:)
        real*8,allocatable::s100(:,:,:,:)
        real*8,allocatable::q5(:,:)
        real*8,allocatable::s12(:,:,:,:)
        real*8,allocatable::s101(:,:,:,:)
        real*8,allocatable::q6(:,:)
        real*8,allocatable::s13(:,:,:,:)
        real*8,allocatable::s103(:,:,:,:)
        real*8,allocatable::s102(:,:,:,:)
        real*8,allocatable::s14(:,:,:,:)
        real*8,allocatable::s105(:,:,:,:)
        real*8,allocatable::s104(:,:,:,:)
        real*8,allocatable::s15(:,:,:,:)
        real*8,allocatable::s110(:,:,:,:)
        real*8,allocatable::q25(:,:)
        real*8,allocatable::s96(:,:,:,:)
        real*8,allocatable::s16(:,:,:,:)
        real*8,allocatable::s111(:,:,:,:)
        real*8,allocatable::q26(:,:)
        real*8,allocatable::q7(:,:)
        real*8,allocatable::s112(:,:,:,:)
        real*8,allocatable::s17(:,:,:,:)
        real*8,allocatable::s115(:,:,:,:)
        real*8,allocatable::s114(:,:,:,:)
        real*8,allocatable::s113(:,:,:,:)
        real*8,allocatable::q27(:,:)
        real*8,allocatable::s107(:,:,:,:)
        real*8,allocatable::s106(:,:,:,:)
        real*8,allocatable::s150(:,:,:,:)
        real*8,allocatable::s97(:,:,:,:)
        real*8,allocatable::s149(:,:,:,:)
        real*8,allocatable::s148(:,:,:,:)
        real*8,allocatable::s18(:,:,:,:)
        real*8,allocatable::s119(:,:,:,:)
        real*8,allocatable::s117(:,:,:,:)
        real*8,allocatable::s116(:,:,:,:)
        real*8,allocatable::q30(:,:)
        real*8,allocatable::s109(:,:,:,:)
        real*8,allocatable::s108(:,:,:,:)
        real*8,allocatable::s151(:,:,:,:)
        real*8,allocatable::q8(:,:)
        real*8,allocatable::s120(:,:,:,:)
        real*8,allocatable::s118(:,:,:,:)
        real*8,allocatable::q29(:,:)
        real*8,allocatable::q28(:,:)
        real*8,allocatable::s19(:,:,:,:)
        real*8,allocatable::s20(:,:,:,:)
        real*8,allocatable::s21(:,:,:,:)
        real*8,allocatable::s22(:,:,:,:)
        real*8,allocatable::s23(:,:,:,:)
        real*8,allocatable::s24(:,:,:,:)
        real*8,allocatable::s25(:,:,:,:)
        real*8,allocatable::s26(:,:,:,:)
        real*8,allocatable::s27(:,:,:,:)
        real*8,allocatable::s28(:,:,:,:)
        real*8,allocatable::s29(:,:,:,:)
        real*8,allocatable::s30(:,:,:,:)
        real*8,allocatable::q9(:,:)
        real*8,allocatable::q10(:,:)
        real*8,allocatable::s31(:,:,:,:)
        real*8,allocatable::s121(:,:,:,:)
        real*8,allocatable::s32(:,:,:,:)
        real*8,allocatable::q11(:,:)
        real*8,allocatable::s33(:,:,:,:)
        real*8,allocatable::s122(:,:,:,:)
        real*8,allocatable::s34(:,:,:,:)
        real*8,allocatable::q12(:,:)
        real*8,allocatable::s35(:,:,:,:)
        real*8,allocatable::s123(:,:,:,:)
        real*8,allocatable::s36(:,:,:,:)
        real*8,allocatable::s124(:,:,:,:)
        real*8,allocatable::q13(:,:)
        real*8,allocatable::s37(:,:,:,:)
        real*8,allocatable::s126(:,:,:,:)
        real*8,allocatable::s125(:,:,:,:)
        real*8,allocatable::s38(:,:,:,:)
        real*8,allocatable::q14(:,:)
        real*8,allocatable::s39(:,:,:,:)
        real*8,allocatable::s135(:,:,:,:)
        real*8,allocatable::s134(:,:,:,:)
        real*8,allocatable::s133(:,:,:,:)
        real*8,allocatable::s131(:,:,:,:)
        real*8,allocatable::s127(:,:,:,:)
        real*8,allocatable::s40(:,:,:,:)
        real*8,allocatable::s138(:,:,:,:)
        real*8,allocatable::s137(:,:,:,:)
        real*8,allocatable::s136(:,:,:,:)
        real*8,allocatable::s132(:,:,:,:)
        real*8,allocatable::q15(:,:)
        real*8,allocatable::s139(:,:,:,:)
        real*8,allocatable::s41(:,:,:,:)
        real*8,allocatable::s142(:,:,:,:)
        real*8,allocatable::s141(:,:,:,:)
        real*8,allocatable::s140(:,:,:,:)
        real*8,allocatable::q31(:,:)
        real*8,allocatable::s129(:,:,:,:)
        real*8,allocatable::s153(:,:,:,:)
        real*8,allocatable::s128(:,:,:,:)
        real*8,allocatable::s152(:,:,:,:)
        real*8,allocatable::s42(:,:,:,:)
        real*8,allocatable::s146(:,:,:,:)
        real*8,allocatable::s145(:,:,:,:)
        real*8,allocatable::s143(:,:,:,:)
        real*8,allocatable::q32(:,:)
        real*8,allocatable::s130(:,:,:,:)
        real*8,allocatable::q16(:,:)
        real*8,allocatable::s147(:,:,:,:)
        real*8,allocatable::s144(:,:,:,:)
        real*8,allocatable::s43(:,:,:,:)
        real*8,allocatable::s44(:,:,:,:)
        real*8,allocatable::s45(:,:,:,:)
        real*8,allocatable::q17(:,:)
        real*8,allocatable::q18(:,:)
        real*8,allocatable::s46(:,:,:,:)
        real*8,allocatable::s47(:,:,:,:)
        real*8,allocatable::s48(:,:,:,:)
        real*8,allocatable::s49(:,:,:,:)
        real*8,allocatable::s50(:,:,:,:)
        real*8,allocatable::s51(:,:,:,:)
        real*8,allocatable::s52(:,:,:,:)
        real*8,allocatable::s53(:,:,:,:)
        real*8,allocatable::s54(:,:,:,:)
        real*8,allocatable::s55(:,:,:,:)
        real*8,allocatable::s56(:,:,:,:)
        real*8,allocatable::s57(:,:,:,:)
        real*8,allocatable::s58(:,:,:,:)
        real*8,allocatable::s59(:,:,:,:)
        real*8,allocatable::s60(:,:,:,:)
        real*8,allocatable::s61(:,:,:,:)
        real*8,allocatable::s62(:,:,:,:)
        real*8,allocatable::s63(:,:,:,:)
        real*8,allocatable::s64(:,:,:,:)
        real*8,allocatable::s65(:,:,:,:)
        real*8,allocatable::s66(:,:,:,:)
        real*8,allocatable::s67(:,:,:,:)
        real*8,allocatable::s68(:,:,:,:)
        real*8,allocatable::s69(:,:,:,:)
        real*8,allocatable::s70(:,:,:,:)
        real*8,allocatable::s71(:,:,:,:)
        real*8,allocatable::s72(:,:,:,:)
        real*8,allocatable::s73(:,:,:,:)
        real*8,allocatable::s74(:,:,:,:)
        real*8,allocatable::s75(:,:,:,:)
        real*8,allocatable::s76(:,:,:,:)
        real*8,allocatable::s77(:,:,:,:)
        real*8,allocatable::s78(:,:,:,:)
        real*8,allocatable::q19(:,:)
        real*8,allocatable::s79(:,:,:,:)
        real*8,allocatable::s80(:,:,:,:)
        real*8,allocatable::s81(:,:,:,:)
        real*8,allocatable::q20(:,:)
        real*8,allocatable::s82(:,:,:,:)
        real*8,allocatable::q21(:,:)
        real*8,allocatable::q22(:,:)
        real*8,allocatable::s83(:,:,:,:)
        real*8,allocatable::s84(:,:,:,:)
        real*8,allocatable::s85(:,:,:,:)
        real*8,allocatable::s86(:,:,:,:)
        real*8,allocatable::s87(:,:,:,:)
        real*8,allocatable::s88(:,:,:,:)
        real*8,allocatable::s89(:,:,:,:)
        real*8,allocatable::s90(:,:,:,:)
        real*8,allocatable::s91(:,:,:,:)
        real*8,allocatable::s92(:,:,:,:)
        real*8,allocatable::q23(:,:)
        real*8,allocatable::s93(:,:,:,:)
        real*8,allocatable::q24(:,:)
        real*8,allocatable::x1(:,:,:,:)
        real*8,allocatable::z1(:,:,:,:,:,:)
        real*8,allocatable::x2(:,:,:,:)
        real*8,allocatable::z2(:,:,:,:,:,:)
        real*8,allocatable::x3(:,:,:,:)
        real*8,allocatable::z3(:,:,:,:,:,:)
        real*8,allocatable::x4(:,:,:,:)
        real*8,allocatable::z4(:,:,:,:,:,:)
        real*8,allocatable::x5(:,:,:,:)
        real*8,allocatable::z5(:,:,:,:,:,:)
        real*8,allocatable::x6(:,:,:,:)
        real*8,allocatable::z6(:,:,:,:,:,:)
        real*8,allocatable::x7(:,:,:,:)
        real*8,allocatable::z7(:,:,:,:,:,:)
        real*8,allocatable::x8(:,:)
        real*8,allocatable::z8(:,:,:,:,:,:)
        real*8,allocatable::x9(:,:)
        real*8,allocatable::z9(:,:,:,:,:,:)
        real*8,allocatable::x10(:,:)
        real*8,allocatable::z10(:,:,:,:,:,:)
        real*8,allocatable::x11(:,:)
        real*8,allocatable::z11(:,:,:,:,:,:)
        real*8,allocatable::x12(:,:,:,:)
        real*8,allocatable::z12(:,:,:,:,:,:)
        real*8,allocatable::x13(:,:,:,:)
        real*8,allocatable::z13(:,:,:,:,:,:)
        real*8,allocatable::x14(:,:,:,:)
        real*8,allocatable::z14(:,:,:,:,:,:)
        real*8,allocatable::x15(:,:,:,:)
        real*8,allocatable::z15(:,:,:,:,:,:)
        real*8,allocatable::x16(:,:,:,:)
        real*8,allocatable::z16(:,:,:,:,:,:)
        real*8,allocatable::x17(:,:,:,:)
        real*8,allocatable::z17(:,:,:,:,:,:)
        real*8,allocatable::x18(:,:,:,:)
        real*8,allocatable::z18(:,:,:,:,:,:)
        real*8,allocatable::x19(:,:,:,:)
        real*8,allocatable::z19(:,:,:,:,:,:)
        real*8,allocatable::x20(:,:,:,:)
        real*8,allocatable::z20(:,:,:,:,:,:)
        real*8,allocatable::x21(:,:)
        real*8,allocatable::z21(:,:,:,:,:,:)
        real*8,allocatable::x22(:,:,:,:)
        real*8,allocatable::z22(:,:,:,:,:,:)
        real*8,allocatable::x23(:,:,:,:)
        real*8,allocatable::z23(:,:,:,:,:,:)
        real*8,allocatable::x24(:,:,:,:)
        real*8,allocatable::z24(:,:,:,:,:,:)
        real*8,allocatable::x25(:,:,:,:)
        real*8,allocatable::z25(:,:,:,:,:,:)
        real*8,allocatable::x26(:,:)
        real*8,allocatable::z26(:,:,:,:,:,:)
        real*8,allocatable::x27(:,:,:,:)
        real*8,allocatable::z27(:,:,:,:,:,:)
        real*8,allocatable::x28(:,:,:,:)
        real*8,allocatable::z28(:,:,:,:,:,:)
        real*8,allocatable::x29(:,:,:,:)
        real*8,allocatable::z29(:,:,:,:,:,:)
        real*8,allocatable::x30(:,:,:,:)
        real*8,allocatable::z30(:,:,:,:,:,:)
        real*8,allocatable::x31(:,:,:,:)
        real*8,allocatable::z180(:,:,:,:,:,:)
        real*8,allocatable::x32(:,:,:,:)
        real*8,allocatable::z62(:,:,:,:,:,:)
        real*8,allocatable::x33(:,:,:,:)
        real*8,allocatable::z63(:,:,:,:,:,:)
        real*8,allocatable::z77(:,:,:,:,:,:)
        real*8,allocatable::z81(:,:,:,:,:,:)
        real*8,allocatable::x34(:,:,:,:)
        real*8,allocatable::z196(:,:,:,:,:,:)
        real*8,allocatable::x35(:,:,:,:)
        real*8,allocatable::z199(:,:,:,:,:,:)
        real*8,allocatable::z122(:,:,:,:,:,:)
        real*8,allocatable::z138(:,:,:,:,:,:)
        real*8,allocatable::z139(:,:,:,:,:,:)

        allocate(indocc(n0+1:n2,n0+1:n2,n0+1:n1))
        allocate(indunocc(n2+1:n3,n2+1:n3,n1+1:n3))
        indocc=0
        indunocc=0
        do i=n0+1,n1;do j=n0+1,n2-1;do k=j+1,n2
          iocca=0
          ioccb=0
          if(i.gt.(n1-iactocca))iocca=iocca+1
          if(j.gt.(n2-iactoccb))ioccb=ioccb+1
          if(k.gt.(n2-iactoccb))ioccb=ioccb+1
          if(iocca+ioccb.lt.iactindt)indocc(k,j,i)=1
        enddo;enddo;enddo
        do a=n1+1,n3;do b=n2+1,n3-1;do c=b+1,n3
          iunoa=0
          iunob=0
          if(a.lt.(n1+iactunoa+1))iunoa=iunoa+1
          if(b.lt.(n2+iactunob+1))iunob=iunob+1
          if(c.lt.(n2+iactunob+1))iunob=iunob+1
          if(iunoa+iunob.lt.iactindt)indunocc(c,b,a)=1
        enddo;enddo;enddo

        allocate(d1(n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2))
        call reorder3124(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n2,n3,n0,n2,intm,d1)
        allocate(b2(n1+1:n3,n0+1:n1))
        call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
        allocate(s1(n0+1:n1,n0+1:n1,n2+1:n3,n0+1:n2))
        i1=k2*k4*k1
        i2=k1
        i3=k3
        call egemm(i1,i2,i3,d1,b2,s1)
        deallocate(d1)
        deallocate(b2)

        allocate(x1(n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n1))
        x1=0.0d0
        call sum4123(n0,n1,n2,n3,n0,n2,n0,n1,x1,s1, 1.000)
        deallocate(s1)

        allocate(d1(n0+1:n1,n1+1:n3,n2+1:n3,n0+1:n2))
        call reorder1324(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n2,n3,n0,n2,intm,d1)
        allocate(b2(n0+1:n1,n1+1:n3))
        call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
        allocate(s2(n1+1:n3,n1+1:n3,n2+1:n3,n0+1:n2))
        i1=k2*k4*k3
        i2=k3
        i3=k1
        call egemm(i1,i2,i3,d1,b2,s2)
        deallocate(d1)
        deallocate(b2)

        allocate(x2(n1+1:n3,n2+1:n3,n1+1:n3,n0+1:n2))
        x2=0.0d0
        call sum3124(n1,n3,n2,n3,n1,n3,n0,n2,x2,s2,-1.000)
        deallocate(s2)

        allocate(d1(n0+1:n1,n0+1:n2,n0+1:n2,n0+1:n1))
        call reorder1243(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n0,n2,n0,n2,n0,n1,intm,d1)
        allocate(b2(n0+1:n1,n1+1:n3))
        call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
        allocate(s3(n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
        i1=k1*k2*k2
        i2=k3
        i3=k1
        call egemm(i1,i2,i3,d1,b2,s3)
        deallocate(d1)
        deallocate(b2)

        allocate(x5(n0+1:n2,n1+1:n3,n0+1:n2,n0+1:n1))
        x5=0.0d0
        call sum2134(n0,n2,n1,n3,n0,n2,n0,n1,x5,s3,-1.000)
        deallocate(s3)

        allocate(d1(n0+1:n1,n2+1:n3,n2+1:n3,n0+1:n1))
        call reorder1423(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n2,n3,n2,n3,n0,n1,intm,d1)
        allocate(b2(n0+1:n1,n1+1:n3))
        call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
        allocate(s4(n1+1:n3,n2+1:n3,n2+1:n3,n0+1:n1))
        i1=k1*k4*k4
        i2=k3
        i3=k1
        call egemm(i1,i2,i3,d1,b2,s4)
        deallocate(d1)
        deallocate(b2)

        allocate(x6(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n1))
        x6=0.0d0
        call sum3124(n2,n3,n2,n3,n1,n3,n0,n1,x6,s4,-1.000)
        deallocate(s4)

        allocate(d1(n1+1:n3,n0+1:n2,n1+1:n3,n0+1:n2))
        call reorder3214(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n2,n1,n3,n0,n2,intm,d1)
        allocate(b2(n1+1:n3,n0+1:n1))
        call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
        allocate(s5(n0+1:n1,n0+1:n2,n1+1:n3,n0+1:n2))
        i1=k2*k3*k2
        i2=k1
        i3=k3
        call egemm(i1,i2,i3,d1,b2,s5)
        deallocate(d1)
        deallocate(b2)

        call sum4123(n0,n2,n1,n3,n0,n2,n0,n1,x5,s5, 1.000)
        deallocate(s5)

        allocate(d1(n1+1:n3,n2+1:n3,n2+1:n3,n1+1:n3))
        call reorder3421(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n2,n3,n2,n3,n1,n3,intm,d1)
        allocate(b2(n1+1:n3,n0+1:n1))
        call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
        allocate(s6(n0+1:n1,n2+1:n3,n2+1:n3,n1+1:n3))
        i1=k3*k4*k4
        i2=k1
        i3=k3
        call egemm(i1,i2,i3,d1,b2,s6)
        deallocate(d1)
        deallocate(b2)

        call sum4123(n2,n3,n2,n3,n1,n3,n0,n1,x6,s6, 1.000)
        deallocate(s6)

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

        allocate(x8(n0+1:n1,n0+1:n1))
        x8=0.0d0
        call sum21(n0,n1,n0,n1,x8,q1, 1.000)
        deallocate(q1)

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

        allocate(x9(n1+1:n3,n1+1:n3))
        x9=0.0d0
        call sum21(n1,n3,n1,n3,x9,q2,-1.000)
        deallocate(q2)

        allocate(d1(n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1))
        call reorder1243(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n0,n1,n1,n3,n0,n1,intr,d1)
        allocate(b2(n0+1:n1,n1+1:n3))
        call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
        allocate(s7(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
        i1=k1*k3*k1
        i2=k3
        i3=k1
        call egemm(i1,i2,i3,d1,b2,s7)
        deallocate(d1)
        deallocate(b2)

        allocate(x12(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
        x12=0.0d0
        call sum3124(n0,n1,n1,n3,n1,n3,n0,n1,x12,s7,-1.000)
        deallocate(s7)

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

        x8=x8-q3
        deallocate(q3)

        allocate(d1(n1+1:n3,n0+1:n1,n1+1:n3,n1+1:n3))
        call reorder3241(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n1,n3,n1,n3,intr,d1)
        allocate(b2(n1+1:n3,n0+1:n1))
        call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
        allocate(s8(n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3))
        i1=k3*k3*k1
        i2=k1
        i3=k3
        call egemm(i1,i2,i3,d1,b2,s8)
        deallocate(d1)
        deallocate(b2)

        call sum4123(n0,n1,n1,n3,n1,n3,n0,n1,x12,s8, 1.000)
        deallocate(s8)

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

        x9=x9-q4
        deallocate(q4)

        allocate(d1(n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
        call reorder3214(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n2,n0,n1,n0,n2,intm,d1)
        allocate(b2(n1+1:n3,n0+1:n1))
        call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
        allocate(s9(n0+1:n1,n0+1:n2,n0+1:n1,n0+1:n2))
        i1=k2*k1*k2
        i2=k1
        i3=k3
        call egemm(i1,i2,i3,d1,b2,s9)
        deallocate(d1)
        deallocate(b2)

        allocate(x13(n0+1:n2,n0+1:n1,n0+1:n2,n0+1:n1))
        x13=0.0d0
        call sum4123(n0,n2,n0,n1,n0,n2,n0,n1,x13,s9, 1.000)

        allocate(d1(n0+1:n2,n0+1:n1,n0+1:n2,n0+1:n1))
        call reorder2341(n0,n1,n0,n2,n0,n1,n0,n2,
     & n0,n2,n0,n1,n0,n2,n0,n1,s9,d1)
        allocate(b2(n0+1:n2,n2+1:n3))
        call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
        allocate(s98(n2+1:n3,n0+1:n1,n0+1:n2,n0+1:n1))
        i1=k1*k2*k1
        i2=k4
        i3=k2
        call egemm(i1,i2,i3,d1,b2,s98)
        deallocate(d1)
        deallocate(b2)

        call sum2134(n0,n1,n2,n3,n0,n2,n0,n1,x1,s98,-1.000)
        deallocate(s98)

        allocate(d1(n0+1:n1,n0+1:n2,n0+1:n2,n0+1:n1))
        call reorder3241(n0,n1,n0,n2,n0,n1,n0,n2,
     & n0,n1,n0,n2,n0,n2,n0,n1,s9,d1)
        allocate(b2(n0+1:n1,n1+1:n3))
        call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
        allocate(s94(n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
        i1=k1*k2*k2
        i2=k3
        i3=k1
        call egemm(i1,i2,i3,d1,b2,s94)
        deallocate(d1)
        deallocate(b2)
        deallocate(s9)

        call sum2134(n0,n2,n1,n3,n0,n2,n0,n1,x5,s94,-1.000)
        deallocate(s94)

        allocate(d1(n1+1:n3,n0+1:n1,n2+1:n3,n2+1:n3))
        call reorder3142(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n2,n3,n2,n3,intm,d1)
        allocate(b2(n1+1:n3,n0+1:n1))
        call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
        allocate(s10(n0+1:n1,n0+1:n1,n2+1:n3,n2+1:n3))
        i1=k4*k4*k1
        i2=k1
        i3=k3
        call egemm(i1,i2,i3,d1,b2,s10)
        deallocate(d1)
        deallocate(b2)

        allocate(x14(n0+1:n1,n2+1:n3,n2+1:n3,n0+1:n1))
        x14=0.0d0
        call sum4123(n0,n1,n2,n3,n2,n3,n0,n1,x14,s10, 1.000)

        allocate(d1(n2+1:n3,n0+1:n1,n2+1:n3,n0+1:n1))
        call reorder3241(n0,n1,n0,n1,n2,n3,n2,n3,
     & n2,n3,n0,n1,n2,n3,n0,n1,s10,d1)
        allocate(b2(n2+1:n3,n0+1:n2))
        call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
        allocate(s99(n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n1))
        i1=k1*k4*k1
        i2=k2
        i3=k4
        call egemm(i1,i2,i3,d1,b2,s99)
        deallocate(d1)
        deallocate(b2)

        call sum3124(n0,n1,n2,n3,n0,n2,n0,n1,x1,s99, 1.000)
        deallocate(s99)

        allocate(d1(n0+1:n1,n2+1:n3,n2+1:n3,n0+1:n1))
        call reorder2341(n0,n1,n0,n1,n2,n3,n2,n3,
     & n0,n1,n2,n3,n2,n3,n0,n1,s10,d1)
        allocate(b2(n0+1:n1,n1+1:n3))
        call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
        allocate(s95(n1+1:n3,n2+1:n3,n2+1:n3,n0+1:n1))
        i1=k1*k4*k4
        i2=k3
        i3=k1
        call egemm(i1,i2,i3,d1,b2,s95)
        deallocate(d1)
        deallocate(b2)
        deallocate(s10)

        call sum3124(n2,n3,n2,n3,n1,n3,n0,n1,x6,s95,-1.000)
        deallocate(s95)

        allocate(d1(n0+1:n1,n0+1:n2,n1+1:n3,n0+1:n2))
        call reorder1234(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n0,n2,n1,n3,n0,n2,intm,d1)
        allocate(b2(n0+1:n1,n1+1:n3))
        call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
        allocate(s11(n1+1:n3,n0+1:n2,n1+1:n3,n0+1:n2))
        i1=k2*k3*k2
        i2=k3
        i3=k1
        call egemm(i1,i2,i3,d1,b2,s11)
        deallocate(d1)
        deallocate(b2)

        allocate(x15(n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n2))
        x15=0.0d0
        call sum3124(n0,n2,n1,n3,n1,n3,n0,n2,x15,s11,-1.000)

        allocate(d1(n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n2))
        call reorder2314(n1,n3,n0,n2,n1,n3,n0,n2,
     & n0,n2,n1,n3,n1,n3,n0,n2,s11,d1)
        allocate(b2(n0+1:n2,n2+1:n3))
        call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
        allocate(s100(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2))
        i1=k2*k3*k3
        i2=k4
        i3=k2
        call egemm(i1,i2,i3,d1,b2,s100)
        deallocate(d1)
        deallocate(b2)
        deallocate(s11)

        call sum2134(n1,n3,n2,n3,n1,n3,n0,n2,x2,s100, 1.000)
        deallocate(s100)

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

        allocate(x10(n0+1:n2,n0+1:n2))
        x10=0.0d0
        x10=x10+q5
        deallocate(q5)

        allocate(d1(n0+1:n1,n2+1:n3,n1+1:n3,n2+1:n3))
        call reorder1432(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n2,n3,n1,n3,n2,n3,intm,d1)
        allocate(b2(n0+1:n1,n1+1:n3))
        call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
        allocate(s12(n1+1:n3,n2+1:n3,n1+1:n3,n2+1:n3))
        i1=k4*k3*k4
        i2=k3
        i3=k1
        call egemm(i1,i2,i3,d1,b2,s12)
        deallocate(d1)
        deallocate(b2)

        allocate(x16(n2+1:n3,n1+1:n3,n2+1:n3,n1+1:n3))
        x16=0.0d0
        call sum4123(n2,n3,n1,n3,n2,n3,n1,n3,x16,s12,-1.000)

        allocate(d1(n2+1:n3,n1+1:n3,n2+1:n3,n1+1:n3))
        call reorder2341(n1,n3,n2,n3,n1,n3,n2,n3,
     & n2,n3,n1,n3,n2,n3,n1,n3,s12,d1)
        allocate(b2(n2+1:n3,n0+1:n2))
        call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
        allocate(s101(n0+1:n2,n1+1:n3,n2+1:n3,n1+1:n3))
        i1=k3*k4*k3
        i2=k2
        i3=k4
        call egemm(i1,i2,i3,d1,b2,s101)
        deallocate(d1)
        deallocate(b2)
        deallocate(s12)

        call sum4123(n1,n3,n2,n3,n1,n3,n0,n2,x2,s101,-1.000)
        deallocate(s101)

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

        allocate(x11(n2+1:n3,n2+1:n3))
        x11=0.0d0
        x11=x11+q6
        deallocate(q6)

        allocate(d1(n0+1:n1,n0+1:n2,n2+1:n3,n0+1:n1))
        call reorder1243(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n0,n2,n2,n3,n0,n1,intm,d1)
        allocate(b2(n0+1:n1,n1+1:n3))
        call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
        allocate(s13(n1+1:n3,n0+1:n2,n2+1:n3,n0+1:n1))
        i1=k1*k4*k2
        i2=k3
        i3=k1
        call egemm(i1,i2,i3,d1,b2,s13)
        deallocate(d1)
        deallocate(b2)

        allocate(x20(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
        x20=0.0d0
        call sum3124(n0,n2,n2,n3,n1,n3,n0,n1,x20,s13,-1.000)

        allocate(d1(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
        call reorder2314(n1,n3,n0,n2,n2,n3,n0,n1,
     & n0,n2,n2,n3,n1,n3,n0,n1,s13,d1)
        allocate(b2(n0+1:n2,n2+1:n3))
        call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
        allocate(s103(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n1))
        i1=k1*k3*k4
        i2=k4
        i3=k2
        call egemm(i1,i2,i3,d1,b2,s103)
        deallocate(d1)
        deallocate(b2)

        call sum2134(n2,n3,n2,n3,n1,n3,n0,n1,x6,s103, 1.000)
        deallocate(s103)

        allocate(d1(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
        call reorder3214(n1,n3,n0,n2,n2,n3,n0,n1,
     & n2,n3,n0,n2,n1,n3,n0,n1,s13,d1)
        allocate(b2(n2+1:n3,n0+1:n2))
        call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
        allocate(s102(n0+1:n2,n0+1:n2,n1+1:n3,n0+1:n1))
        i1=k1*k3*k2
        i2=k2
        i3=k4
        call egemm(i1,i2,i3,d1,b2,s102)
        deallocate(d1)
        deallocate(b2)
        deallocate(s13)

        call sum3124(n0,n2,n1,n3,n0,n2,n0,n1,x5,s102,-1.000)
        deallocate(s102)

        allocate(d1(n1+1:n3,n0+1:n2,n2+1:n3,n1+1:n3))
        call reorder3241(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n2,n2,n3,n1,n3,intm,d1)
        allocate(b2(n1+1:n3,n0+1:n1))
        call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
        allocate(s14(n0+1:n1,n0+1:n2,n2+1:n3,n1+1:n3))
        i1=k3*k4*k2
        i2=k1
        i3=k3
        call egemm(i1,i2,i3,d1,b2,s14)
        deallocate(d1)
        deallocate(b2)

        call sum4123(n0,n2,n2,n3,n1,n3,n0,n1,x20,s14, 1.000)

        allocate(d1(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
        call reorder2341(n0,n1,n0,n2,n2,n3,n1,n3,
     & n0,n2,n2,n3,n1,n3,n0,n1,s14,d1)
        allocate(b2(n0+1:n2,n2+1:n3))
        call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
        allocate(s105(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n1))
        i1=k1*k3*k4
        i2=k4
        i3=k2
        call egemm(i1,i2,i3,d1,b2,s105)
        deallocate(d1)
        deallocate(b2)

        call sum2134(n2,n3,n2,n3,n1,n3,n0,n1,x6,s105,-1.000)
        deallocate(s105)

        allocate(d1(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
        call reorder3241(n0,n1,n0,n2,n2,n3,n1,n3,
     & n2,n3,n0,n2,n1,n3,n0,n1,s14,d1)
        allocate(b2(n2+1:n3,n0+1:n2))
        call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
        allocate(s104(n0+1:n2,n0+1:n2,n1+1:n3,n0+1:n1))
        i1=k1*k3*k2
        i2=k2
        i3=k4
        call egemm(i1,i2,i3,d1,b2,s104)
        deallocate(d1)
        deallocate(b2)
        deallocate(s14)

        call sum3124(n0,n2,n1,n3,n0,n2,n0,n1,x5,s104, 1.000)
        deallocate(s104)

        allocate(d1(n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3))
        call reorder3214(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n0,n1,n1,n3,intr,d1)
        allocate(b2(n1+1:n3,n0+1:n1))
        call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
        allocate(s15(n0+1:n1,n0+1:n1,n0+1:n1,n1+1:n3))
        i1=k3*k1*k1
        i2=k1
        i3=k3
        call egemm(i1,i2,i3,d1,b2,s15)
        deallocate(d1)
        deallocate(b2)

        allocate(x22(n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1))
        x22=0.0d0
        call sum4123(n0,n1,n0,n1,n1,n3,n0,n1,x22,s15, 1.000)

        call sumx2143(n0,n3,n0,n1,n0,n1,n1,n3,n0,n1,x22,intr, 1.000)

        if (lvl_q) then
          allocate(t4c(n2+1:n3,n2+1:n3,n1+1:n3,n1+1:n3,                !ilias: if no quadruples comment out the following 4 lines
     & n0+1:n2,n0+1:n2,n0+1:n1,n0+1:n1))
          rewind(tc)
          read(tc)t4c

          do i=n0+1,n1;do j=n0+1,n2-1;do k=j+1,n2                      !ilias: if no quadruples comment out the following 12 lines
            if(indocc(k,j,i).eq.1)cycle
            do a=n1+1,n3;do b=n2+1,n3-1;do c=b+1,n3
              if(indunocc(c,b,a).eq.1)cycle
              sum=0.0d0
              do e=n1+1,n3;do m=n0+1,n1;do n=n0+1,n1
                sum=sum
     &     - (x22(n,m,e,i)*t4c(c,b,e,a,k,j,n,m))/2.0d0!icbakj(-0.500)
              enddo;enddo;enddo
              v3c(c,b,a,k,j,i)=v3c(c,b,a,k,j,i)+sum
            enddo;enddo;enddo
          enddo;enddo;enddo

          deallocate(t4c)                                              !ilias: if no quadruples comment out the following 2 lines
          deallocate(x22)
        endif

        allocate(d1(n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
        call reorder2431(n0,n1,n0,n1,n0,n1,n1,n3,
     & n0,n1,n1,n3,n0,n1,n0,n1,s15,d1)
        allocate(d2(n0+1:n1,n1+1:n3,n2+1:n3,n0+1:n2))
        call reorder4213(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n1,n3,n2,n3,n0,n2,t2b,d2)
        allocate(s110(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
        i1=k1*k1
        i2=k2*k4
        i3=k3*k1
        call egemm(i1,i2,i3,d1,d2,s110)
        deallocate(d1)
        deallocate(d2)

        call sum2314(n0,n1,n2,n3,n0,n2,n0,n1,x1,s110, 1.000)
        deallocate(s110)

        allocate(d1(n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
        call reorder3421(n0,n1,n0,n1,n0,n1,n1,n3,
     & n0,n1,n1,n3,n0,n1,n0,n1,s15,d1)
        allocate(b2(n0+1:n1,n1+1:n3))
        call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
        allocate(q25(n0+1:n1,n0+1:n1))
        i1=k1*k1
        i3=k3*k1
        call egemm1(i1,i3,d1,b2,q25)
        deallocate(d1)
        deallocate(b2)

        x8=x8-q25
        deallocate(q25)

        allocate(d1(n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1))
        call reorder3241(n0,n1,n0,n1,n0,n1,n1,n3,
     & n0,n1,n0,n1,n1,n3,n0,n1,s15,d1)
        allocate(b2(n0+1:n1,n1+1:n3))
        call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
        allocate(s96(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
        i1=k1*k3*k1
        i2=k3
        i3=k1
        call egemm(i1,i2,i3,d1,b2,s96)
        deallocate(d1)
        deallocate(b2)
        deallocate(s15)

        call sum3124(n0,n1,n1,n3,n1,n3,n0,n1,x12,s96,-1.000)
        deallocate(s96)

        allocate(d1(n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3))
        call reorder1243(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n0,n1,n1,n3,n1,n3,intr,d1)
        allocate(b2(n0+1:n1,n1+1:n3))
        call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
        allocate(s16(n1+1:n3,n0+1:n1,n1+1:n3,n1+1:n3))
        i1=k3*k3*k1
        i2=k3
        i3=k1
        call egemm(i1,i2,i3,d1,b2,s16)
        deallocate(d1)
        deallocate(b2)

        allocate(x23(n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3))
        x23=0.0d0
        call sum4123(n0,n1,n1,n3,n1,n3,n1,n3,x23,s16,-1.000)

        call sumx4132(n0,n3,n0,n1,n1,n3,n1,n3,n1,n3,x23,intr, 1.000)

        if (lvl_q) then
          allocate(t4c(n2+1:n3,n2+1:n3,n1+1:n3,n1+1:n3,                !ilias: if no quadruples comment out the following 4 lines
     & n0+1:n2,n0+1:n2,n0+1:n1,n0+1:n1))
          rewind(tc)
          read(tc)t4c

          do i=n0+1,n1;do j=n0+1,n2-1;do k=j+1,n2                      !ilias: if no quadruples comment out the following 12 lines
            if(indocc(k,j,i).eq.1)cycle
            do a=n1+1,n3;do b=n2+1,n3-1;do c=b+1,n3
              if(indunocc(c,b,a).eq.1)cycle
              sum=0.0d0
              do e=n1+1,n3;do f=n1+1,n3;do m=n0+1,n1
                sum=sum
     &     + (x23(m,f,e,a)*t4c(c,b,f,e,k,j,m,i))/2.0d0 !acbkji(+0.500)
              enddo;enddo;enddo
              v3c(c,b,a,k,j,i)=v3c(c,b,a,k,j,i)+sum
            enddo;enddo;enddo
          enddo;enddo;enddo

          deallocate(t4c)                                              !ilias: if no quadruples comment out the following 2 lines
          deallocate(x23)
        endif

        allocate(d1(n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3))
        call reorder2341(n1,n3,n0,n1,n1,n3,n1,n3,
     & n0,n1,n1,n3,n1,n3,n1,n3,s16,d1)
        allocate(d2(n0+1:n1,n1+1:n3,n2+1:n3,n0+1:n2))
        call reorder4213(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n1,n3,n2,n3,n0,n2,t2b,d2)
        allocate(s111(n2+1:n3,n0+1:n2,n1+1:n3,n1+1:n3))
        i1=k3*k3
        i2=k2*k4
        i3=k3*k1
        call egemm(i1,i2,i3,d1,d2,s111)
        deallocate(d1)
        deallocate(d2)

        call sum2413(n1,n3,n2,n3,n1,n3,n0,n2,x2,s111,-1.000)
        deallocate(s111)

        allocate(d1(n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3))
        call reorder2431(n1,n3,n0,n1,n1,n3,n1,n3,
     & n0,n1,n1,n3,n1,n3,n1,n3,s16,d1)
        allocate(b2(n0+1:n1,n1+1:n3))
        call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
        allocate(q26(n1+1:n3,n1+1:n3))
        i1=k3*k3
        i3=k3*k1
        call egemm1(i1,i3,d1,b2,q26)
        deallocate(d1)
        deallocate(b2)
        deallocate(s16)

        x9=x9+q26
        deallocate(q26)

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

        allocate(x21(n0+1:n1,n1+1:n3))
        x21=0.0d0
        x21=x21+q7

        allocate(b1(n1+1:n3,n0+1:n1))
        call reorder21(n0,n1,n1,n3,
     & n1,n3,n0,n1,q7,b1)
        allocate(d2(n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
        call reorder2134(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n2,n3,n0,n2,n0,n1,t2b,d2)
        allocate(s112(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
        i1=k1
        i2=k1*k2*k4
        i3=k3
        call egemm(i1,i2,i3,b1,d2,s112)
        deallocate(b1)
        deallocate(d2)
        deallocate(q7)

        call sum2341(n0,n1,n2,n3,n0,n2,n0,n1,x1,s112, 1.000)
        deallocate(s112)

        allocate(d1(n1+1:n3,n0+1:n2,n0+1:n1,n2+1:n3))
        call reorder3214(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n2,n0,n1,n2,n3,intm,d1)
        allocate(b2(n1+1:n3,n0+1:n1))
        call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
        allocate(s17(n0+1:n1,n0+1:n2,n0+1:n1,n2+1:n3))
        i1=k4*k1*k2
        i2=k1
        i3=k3
        call egemm(i1,i2,i3,d1,b2,s17)
        deallocate(d1)
        deallocate(b2)

        allocate(x27(n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n1))
        x27=0.0d0
        call sum4123(n0,n2,n0,n1,n2,n3,n0,n1,x27,s17, 1.000)

        call sumx2143(n0,n3,n0,n2,n0,n1,n2,n3,n0,n1,x27,intm, 1.000)

        if (lvl_q) then
          allocate(t4d(n2+1:n3,n2+1:n3,n2+1:n3,n1+1:n3,                !ilias: if no quadruples comment out the following 4 lines
     & n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n1))
          rewind(td)
          read(td)t4d

          do i=n0+1,n1;do j=n0+1,n2-1;do k=j+1,n2                      !ilias: if no quadruples comment out the following 12 lines
            if(indocc(k,j,i).eq.1)cycle
            do a=n1+1,n3;do b=n2+1,n3-1;do c=b+1,n3
              if(indunocc(c,b,a).eq.1)cycle
              sum=0.0d0
              do e=n2+1,n3;do m=n0+1,n1;do n=n0+1,n2
                sum=sum
     &     - x27(n,m,e,i)*t4d(e,c,b,a,n,k,j,m) !icbakj(-1.000)
              enddo;enddo;enddo
              v3c(c,b,a,k,j,i)=v3c(c,b,a,k,j,i)+sum
            enddo;enddo;enddo
          enddo;enddo;enddo

          deallocate(t4d)                                              !ilias: if no quadruples comment out the following 2 lines
          deallocate(x27)
        endif

        allocate(d1(n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n1))
        call reorder2341(n0,n1,n0,n2,n0,n1,n2,n3,
     & n0,n2,n0,n1,n2,n3,n0,n1,s17,d1)
        allocate(d2(n0+1:n2,n0+1:n1,n2+1:n3,n1+1:n3))
        call reorder3412(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n0,n1,n2,n3,n1,n3,t2b,d2)
        allocate(s115(n2+1:n3,n1+1:n3,n2+1:n3,n0+1:n1))
        i1=k1*k4
        i2=k3*k4
        i3=k1*k2
        call egemm(i1,i2,i3,d1,d2,s115)
        deallocate(d1)
        deallocate(d2)

        call sum2314(n2,n3,n2,n3,n1,n3,n0,n1,x6,s115, 1.000)
        deallocate(s115)

        allocate(d1(n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n1))
        call reorder3421(n0,n1,n0,n2,n0,n1,n2,n3,
     & n0,n1,n2,n3,n0,n2,n0,n1,s17,d1)
        allocate(d2(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
        call reorder4123(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n2,n3,n1,n3,n0,n2,t2b,d2)
        allocate(s114(n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
        i1=k1*k2
        i2=k2*k3
        i3=k4*k1
        call egemm(i1,i2,i3,d1,d2,s114)
        deallocate(d1)
        deallocate(d2)

        call sum2314(n0,n2,n1,n3,n0,n2,n0,n1,x5,s114,-1.000)
        deallocate(s114)

        allocate(d1(n0+1:n2,n2+1:n3,n0+1:n1,n0+1:n1))
        call reorder2431(n0,n1,n0,n2,n0,n1,n2,n3,
     & n0,n2,n2,n3,n0,n1,n0,n1,s17,d1)
        allocate(d2(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
        call reorder3124(n2,n3,n2,n3,n0,n2,n0,n2,
     & n0,n2,n2,n3,n2,n3,n0,n2,t2c,d2)
        allocate(s113(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
        i1=k1*k1
        i2=k2*k4
        i3=k4*k2
        call egemm(i1,i2,i3,d1,d2,s113)
        deallocate(d1)
        deallocate(d2)

        call sum2314(n0,n1,n2,n3,n0,n2,n0,n1,x1,s113, 1.000)
        deallocate(s113)

        allocate(d1(n0+1:n2,n2+1:n3,n0+1:n1,n0+1:n1))
        call reorder2431(n0,n1,n0,n2,n0,n1,n2,n3,
     & n0,n2,n2,n3,n0,n1,n0,n1,s17,d1)
        allocate(b2(n0+1:n2,n2+1:n3))
        call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
        allocate(q27(n0+1:n1,n0+1:n1))
        i1=k1*k1
        i3=k4*k2
        call egemm1(i1,i3,d1,b2,q27)
        deallocate(d1)
        deallocate(b2)

        x8=x8+q27
        deallocate(q27)

        allocate(d1(n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n1))
        call reorder2341(n0,n1,n0,n2,n0,n1,n2,n3,
     & n0,n2,n0,n1,n2,n3,n0,n1,s17,d1)
        allocate(b2(n0+1:n2,n2+1:n3))
        call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
        allocate(s107(n2+1:n3,n0+1:n1,n2+1:n3,n0+1:n1))
        i1=k1*k4*k1
        i2=k4
        i3=k2
        call egemm(i1,i2,i3,d1,b2,s107)
        deallocate(d1)
        deallocate(b2)

        call sum3124(n0,n1,n2,n3,n2,n3,n0,n1,x14,s107,-1.000)
        deallocate(s107)

        allocate(d1(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
        call reorder4231(n0,n1,n0,n2,n0,n1,n2,n3,
     & n2,n3,n0,n2,n0,n1,n0,n1,s17,d1)
        allocate(b2(n2+1:n3,n0+1:n2))
        call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
        allocate(s106(n0+1:n2,n0+1:n2,n0+1:n1,n0+1:n1))
        i1=k1*k1*k2
        i2=k2
        i3=k4
        call egemm(i1,i2,i3,d1,b2,s106)
        deallocate(d1)
        deallocate(b2)

        call sum3124(n0,n2,n0,n1,n0,n2,n0,n1,x13,s106, 1.000)

        allocate(d1(n0+1:n2,n0+1:n1,n0+1:n2,n0+1:n1))
        call reorder2314(n0,n2,n0,n2,n0,n1,n0,n1,
     & n0,n2,n0,n1,n0,n2,n0,n1,s106,d1)
        allocate(b2(n0+1:n2,n2+1:n3))
        call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
        allocate(s150(n2+1:n3,n0+1:n1,n0+1:n2,n0+1:n1))
        i1=k1*k2*k1
        i2=k4
        i3=k2
        call egemm(i1,i2,i3,d1,b2,s150)
        deallocate(d1)
        deallocate(b2)
        deallocate(s106)

        call sum2134(n0,n1,n2,n3,n0,n2,n0,n1,x1,s150,-1.000)
        deallocate(s150)

        allocate(d1(n0+1:n1,n0+1:n2,n2+1:n3,n0+1:n1))
        call reorder3241(n0,n1,n0,n2,n0,n1,n2,n3,
     & n0,n1,n0,n2,n2,n3,n0,n1,s17,d1)
        allocate(b2(n0+1:n1,n1+1:n3))
        call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
        allocate(s97(n1+1:n3,n0+1:n2,n2+1:n3,n0+1:n1))
        i1=k1*k4*k2
        i2=k3
        i3=k1
        call egemm(i1,i2,i3,d1,b2,s97)
        deallocate(d1)
        deallocate(b2)
        deallocate(s17)

        call sum3124(n0,n2,n2,n3,n1,n3,n0,n1,x20,s97,-1.000)

        allocate(d1(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
        call reorder2314(n1,n3,n0,n2,n2,n3,n0,n1,
     & n0,n2,n2,n3,n1,n3,n0,n1,s97,d1)
        allocate(b2(n0+1:n2,n2+1:n3))
        call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
        allocate(s149(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n1))
        i1=k1*k3*k4
        i2=k4
        i3=k2
        call egemm(i1,i2,i3,d1,b2,s149)
        deallocate(d1)
        deallocate(b2)

        call sum2134(n2,n3,n2,n3,n1,n3,n0,n1,x6,s149, 1.000)
        deallocate(s149)

        allocate(d1(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
        call reorder3214(n1,n3,n0,n2,n2,n3,n0,n1,
     & n2,n3,n0,n2,n1,n3,n0,n1,s97,d1)
        allocate(b2(n2+1:n3,n0+1:n2))
        call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
        allocate(s148(n0+1:n2,n0+1:n2,n1+1:n3,n0+1:n1))
        i1=k1*k3*k2
        i2=k2
        i3=k4
        call egemm(i1,i2,i3,d1,b2,s148)
        deallocate(d1)
        deallocate(b2)
        deallocate(s97)

        call sum3124(n0,n2,n1,n3,n0,n2,n0,n1,x5,s148,-1.000)
        deallocate(s148)

        allocate(d1(n0+1:n1,n0+1:n2,n2+1:n3,n1+1:n3))
        call reorder1243(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n0,n2,n2,n3,n1,n3,intm,d1)
        allocate(b2(n0+1:n1,n1+1:n3))
        call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
        allocate(s18(n1+1:n3,n0+1:n2,n2+1:n3,n1+1:n3))
        i1=k3*k4*k2
        i2=k3
        i3=k1
        call egemm(i1,i2,i3,d1,b2,s18)
        deallocate(d1)
        deallocate(b2)

        allocate(x28(n0+1:n2,n2+1:n3,n1+1:n3,n1+1:n3))
        x28=0.0d0
        call sum4123(n0,n2,n2,n3,n1,n3,n1,n3,x28,s18,-1.000)

        call sumx4132(n0,n3,n0,n2,n2,n3,n1,n3,n1,n3,x28,intm, 1.000)

        if (lvl_q) then
          allocate(t4d(n2+1:n3,n2+1:n3,n2+1:n3,n1+1:n3,                !ilias: if no quadruples comment out the following 4 lines
     & n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n1))
          rewind(td)
          read(td)t4d

          do i=n0+1,n1;do j=n0+1,n2-1;do k=j+1,n2                     !ilias: if no quadruples comment out the following 12 lines
            if(indocc(k,j,i).eq.1)cycle
            do a=n1+1,n3;do b=n2+1,n3-1;do c=b+1,n3
              if(indunocc(c,b,a).eq.1)cycle
              sum=0.0d0
              do e=n1+1,n3;do f=n2+1,n3;do m=n0+1,n2
                sum=sum
     &     + x28(m,f,e,a)*t4d(f,c,b,e,m,k,j,i) !acbkji(+1.000)
              enddo;enddo;enddo
              v3c(c,b,a,k,j,i)=v3c(c,b,a,k,j,i)+sum
            enddo;enddo;enddo
          enddo;enddo;enddo

          deallocate(t4d)                                              !ilias: if no quadruples comment out the following 2 lines
          deallocate(x28)
        endif

        allocate(d1(n0+1:n2,n1+1:n3,n2+1:n3,n1+1:n3))
        call reorder2431(n1,n3,n0,n2,n2,n3,n1,n3,
     & n0,n2,n1,n3,n2,n3,n1,n3,s18,d1)
        allocate(d2(n0+1:n2,n1+1:n3,n2+1:n3,n0+1:n1))
        call reorder3214(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n1,n3,n2,n3,n0,n1,t2b,d2)
        allocate(s119(n2+1:n3,n0+1:n1,n2+1:n3,n1+1:n3))
        i1=k3*k4
        i2=k1*k4
        i3=k3*k2
        call egemm(i1,i2,i3,d1,d2,s119)
        deallocate(d1)
        deallocate(d2)

        call sum2413(n2,n3,n2,n3,n1,n3,n0,n1,x6,s119, 1.000)
        deallocate(s119)

        allocate(d1(n2+1:n3,n1+1:n3,n0+1:n2,n1+1:n3))
        call reorder3421(n1,n3,n0,n2,n2,n3,n1,n3,
     & n2,n3,n1,n3,n0,n2,n1,n3,s18,d1)
        allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
        call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
        allocate(s117(n0+1:n2,n0+1:n1,n0+1:n2,n1+1:n3))
        i1=k3*k2
        i2=k1*k2
        i3=k3*k4
        call egemm(i1,i2,i3,d1,d2,s117)
        deallocate(d1)
        deallocate(d2)

        call sum3412(n0,n2,n1,n3,n0,n2,n0,n1,x5,s117,-1.000)
        deallocate(s117)

        allocate(d1(n0+1:n2,n2+1:n3,n1+1:n3,n1+1:n3))
        call reorder2341(n1,n3,n0,n2,n2,n3,n1,n3,
     & n0,n2,n2,n3,n1,n3,n1,n3,s18,d1)
        allocate(d2(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
        call reorder3124(n2,n3,n2,n3,n0,n2,n0,n2,
     & n0,n2,n2,n3,n2,n3,n0,n2,t2c,d2)
        allocate(s116(n2+1:n3,n0+1:n2,n1+1:n3,n1+1:n3))
        i1=k3*k3
        i2=k2*k4
        i3=k4*k2
        call egemm(i1,i2,i3,d1,d2,s116)
        deallocate(d1)
        deallocate(d2)

        call sum2413(n1,n3,n2,n3,n1,n3,n0,n2,x2,s116,-1.000)
        deallocate(s116)

        allocate(d1(n0+1:n2,n2+1:n3,n1+1:n3,n1+1:n3))
        call reorder2341(n1,n3,n0,n2,n2,n3,n1,n3,
     & n0,n2,n2,n3,n1,n3,n1,n3,s18,d1)
        allocate(b2(n0+1:n2,n2+1:n3))
        call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
        allocate(q30(n1+1:n3,n1+1:n3))
        i1=k3*k3
        i3=k4*k2
        call egemm1(i1,i3,d1,b2,q30)
        deallocate(d1)
        deallocate(b2)

        x9=x9-q30
        deallocate(q30)

        allocate(d1(n0+1:n2,n2+1:n3,n1+1:n3,n1+1:n3))
        call reorder2341(n1,n3,n0,n2,n2,n3,n1,n3,
     & n0,n2,n2,n3,n1,n3,n1,n3,s18,d1)
        allocate(b2(n0+1:n2,n2+1:n3))
        call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
        allocate(s109(n2+1:n3,n2+1:n3,n1+1:n3,n1+1:n3))
        i1=k3*k3*k4
        i2=k4
        i3=k2
        call egemm(i1,i2,i3,d1,b2,s109)
        deallocate(d1)
        deallocate(b2)

        call sum3124(n2,n3,n1,n3,n2,n3,n1,n3,x16,s109, 1.000)
        deallocate(s109)

        allocate(d1(n2+1:n3,n0+1:n2,n1+1:n3,n1+1:n3))
        call reorder3241(n1,n3,n0,n2,n2,n3,n1,n3,
     & n2,n3,n0,n2,n1,n3,n1,n3,s18,d1)
        allocate(b2(n2+1:n3,n0+1:n2))
        call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
        allocate(s108(n0+1:n2,n0+1:n2,n1+1:n3,n1+1:n3))
        i1=k3*k3*k2
        i2=k2
        i3=k4
        call egemm(i1,i2,i3,d1,b2,s108)
        deallocate(d1)
        deallocate(b2)
        deallocate(s18)

        call sum4123(n0,n2,n1,n3,n1,n3,n0,n2,x15,s108,-1.000)

        allocate(d1(n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n2))
        call reorder2341(n0,n2,n0,n2,n1,n3,n1,n3,
     & n0,n2,n1,n3,n1,n3,n0,n2,s108,d1)
        allocate(b2(n0+1:n2,n2+1:n3))
        call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
        allocate(s151(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2))
        i1=k2*k3*k3
        i2=k4
        i3=k2
        call egemm(i1,i2,i3,d1,b2,s151)
        deallocate(d1)
        deallocate(b2)
        deallocate(s108)

        call sum2134(n1,n3,n2,n3,n1,n3,n0,n2,x2,s151, 1.000)
        deallocate(s151)

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

        allocate(x26(n0+1:n2,n2+1:n3))
        x26=0.0d0
        x26=x26+q8

        allocate(b1(n2+1:n3,n0+1:n2))
        call reorder21(n0,n2,n2,n3,
     & n2,n3,n0,n2,q8,b1)
        allocate(d2(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
        call reorder1234(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n2,n3,n0,n2,n0,n2,t2c,d2)
        allocate(s120(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
        i1=k2
        i2=k2*k2*k4
        i3=k4
        call egemm(i1,i2,i3,b1,d2,s120)
        deallocate(b1)
        deallocate(d2)

        allocate(x31(n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2))
        x31=0.0d0
        call sum2341(n0,n2,n2,n3,n0,n2,n0,n2,x31,s120, 1.000)
        deallocate(s120)

        allocate(b1(n2+1:n3,n0+1:n2))
        call reorder21(n0,n2,n2,n3,
     & n2,n3,n0,n2,q8,b1)
        allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
        call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
        allocate(s118(n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
        i1=k2
        i2=k1*k2*k3
        i3=k4
        call egemm(i1,i2,i3,b1,d2,s118)
        deallocate(b1)
        deallocate(d2)

        call sum2341(n0,n2,n1,n3,n0,n2,n0,n1,x5,s118, 1.000)
        deallocate(s118)

        allocate(b2(n0+1:n2,n2+1:n3))
        call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
        allocate(q29(n2+1:n3,n2+1:n3))
        i1=k4
        i2=k4
        i3=k2
        call egemm(i1,i2,i3,q8,b2,q29)
        deallocate(b2)

        call sum21(n2,n3,n2,n3,x11,q29,-1.000)
        deallocate(q29)

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

        call sum21(n0,n2,n0,n2,x10,q28, 1.000)
        deallocate(q28)

        allocate(d1(n0+1:n2,n0+1:n1,n0+1:n2,n0+1:n1))
        call reorder2143(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n0,n1,n0,n2,n0,n1,intm,d1)
        allocate(b2(n0+1:n2,n2+1:n3))
        call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
        allocate(s19(n2+1:n3,n0+1:n1,n0+1:n2,n0+1:n1))
        i1=k1*k2*k1
        i2=k4
        i3=k2
        call egemm(i1,i2,i3,d1,b2,s19)
        deallocate(d1)
        deallocate(b2)

        call sum2134(n0,n1,n2,n3,n0,n2,n0,n1,x1,s19,-1.000)
        deallocate(s19)

        allocate(d1(n2+1:n3,n0+1:n1,n2+1:n3,n0+1:n1))
        call reorder4123(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n1,n2,n3,n0,n1,intm,d1)
        allocate(b2(n2+1:n3,n0+1:n2))
        call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
        allocate(s20(n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n1))
        i1=k1*k4*k1
        i2=k2
        i3=k4
        call egemm(i1,i2,i3,d1,b2,s20)
        deallocate(d1)
        deallocate(b2)

        call sum3124(n0,n1,n2,n3,n0,n2,n0,n1,x1,s20, 1.000)
        deallocate(s20)

        allocate(d1(n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n2))
        call reorder2314(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n1,n3,n1,n3,n0,n2,intm,d1)
        allocate(b2(n0+1:n2,n2+1:n3))
        call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
        allocate(s21(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2))
        i1=k2*k3*k3
        i2=k4
        i3=k2
        call egemm(i1,i2,i3,d1,b2,s21)
        deallocate(d1)
        deallocate(b2)

        call sum2134(n1,n3,n2,n3,n1,n3,n0,n2,x2,s21,-1.000)
        deallocate(s21)

        allocate(d1(n2+1:n3,n1+1:n3,n2+1:n3,n1+1:n3))
        call reorder4321(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n2,n3,n1,n3,intm,d1)
        allocate(b2(n2+1:n3,n0+1:n2))
        call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
        allocate(s22(n0+1:n2,n1+1:n3,n2+1:n3,n1+1:n3))
        i1=k3*k4*k3
        i2=k2
        i3=k4
        call egemm(i1,i2,i3,d1,b2,s22)
        deallocate(d1)
        deallocate(b2)

        call sum4123(n1,n3,n2,n3,n1,n3,n0,n2,x2,s22, 1.000)
        deallocate(s22)

        allocate(d1(n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n2))
        call reorder1243(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n0,n2,n0,n2,n0,n2,intb,d1)
        allocate(b2(n0+1:n2,n2+1:n3))
        call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
        allocate(s23(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
        i1=k2*k2*k2
        i2=k4
        i3=k2
        call egemm(i1,i2,i3,d1,b2,s23)
        deallocate(d1)
        deallocate(b2)

        allocate(x3(n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2))
        x3=0.0d0
        call sum2134(n0,n2,n2,n3,n0,n2,n0,n2,x3,s23,-1.000)
        deallocate(s23)

        allocate(d1(n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2))
        call reorder4213(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n2,n3,n0,n2,intb,d1)
        allocate(b2(n2+1:n3,n0+1:n2))
        call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
        allocate(s24(n0+1:n2,n0+1:n2,n2+1:n3,n0+1:n2))
        i1=k2*k4*k2
        i2=k2
        i3=k4
        call egemm(i1,i2,i3,d1,b2,s24)
        deallocate(d1)
        deallocate(b2)

        allocate(x32(n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2))
        x32=0.0d0
        call sum3124(n0,n2,n2,n3,n0,n2,n0,n2,x32,s24, 1.000)
        deallocate(s24)

        allocate(d1(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
        call reorder2413(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n2,n3,n0,n2,intb,d1)
        allocate(b2(n0+1:n2,n2+1:n3))
        call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
        allocate(s25(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
        i1=k2*k4*k4
        i2=k4
        i3=k2
        call egemm(i1,i2,i3,d1,b2,s25)
        deallocate(d1)
        deallocate(b2)

        allocate(x33(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
        x33=0.0d0
        call sum3124(n2,n3,n2,n3,n2,n3,n0,n2,x33,s25, 1.000)
        deallocate(s25)

        allocate(d1(n2+1:n3,n2+1:n3,n2+1:n3,n2+1:n3))
        call reorder3421(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n2,n3,n2,n3,n2,n3,intb,d1)
        allocate(b2(n2+1:n3,n0+1:n2))
        call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
        allocate(s26(n0+1:n2,n2+1:n3,n2+1:n3,n2+1:n3))
        i1=k4*k4*k4
        i2=k2
        i3=k4
        call egemm(i1,i2,i3,d1,b2,s26)
        deallocate(d1)
        deallocate(b2)

        allocate(x4(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
        x4=0.0d0
        call sum4123(n2,n3,n2,n3,n2,n3,n0,n2,x4,s26, 1.000)
        deallocate(s26)

        allocate(d1(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
        call reorder4213(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n1,n3,n0,n1,intm,d1)
        allocate(b2(n2+1:n3,n0+1:n2))
        call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
        allocate(s27(n0+1:n2,n0+1:n2,n1+1:n3,n0+1:n1))
        i1=k1*k3*k2
        i2=k2
        i3=k4
        call egemm(i1,i2,i3,d1,b2,s27)
        deallocate(d1)
        deallocate(b2)

        call sum3124(n0,n2,n1,n3,n0,n2,n0,n1,x5,s27, 1.000)
        deallocate(s27)

        allocate(d1(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
        call reorder2413(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n1,n3,n0,n1,intm,d1)
        allocate(b2(n0+1:n2,n2+1:n3))
        call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
        allocate(s28(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n1))
        i1=k1*k3*k4
        i2=k4
        i3=k2
        call egemm(i1,i2,i3,d1,b2,s28)
        deallocate(d1)
        deallocate(b2)

        call sum2134(n2,n3,n2,n3,n1,n3,n0,n1,x6,s28,-1.000)
        deallocate(s28)

        allocate(d1(n0+1:n2,n0+1:n1,n1+1:n3,n0+1:n2))
        call reorder2134(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n0,n1,n1,n3,n0,n2,intm,d1)
        allocate(b2(n0+1:n2,n2+1:n3))
        call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
        allocate(s29(n2+1:n3,n0+1:n1,n1+1:n3,n0+1:n2))
        i1=k2*k3*k1
        i2=k4
        i3=k2
        call egemm(i1,i2,i3,d1,b2,s29)
        deallocate(d1)
        deallocate(b2)

        allocate(x7(n0+1:n1,n1+1:n3,n2+1:n3,n0+1:n2))
        x7=0.0d0
        call sum3124(n0,n1,n1,n3,n2,n3,n0,n2,x7,s29,-1.000)
        deallocate(s29)

        allocate(d1(n2+1:n3,n0+1:n1,n1+1:n3,n2+1:n3))
        call reorder4132(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n1,n1,n3,n2,n3,intm,d1)
        allocate(b2(n2+1:n3,n0+1:n2))
        call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
        allocate(s30(n0+1:n2,n0+1:n1,n1+1:n3,n2+1:n3))
        i1=k4*k3*k1
        i2=k2
        i3=k4
        call egemm(i1,i2,i3,d1,b2,s30)
        deallocate(d1)
        deallocate(b2)

        call sum4123(n0,n1,n1,n3,n2,n3,n0,n2,x7,s30, 1.000)
        deallocate(s30)

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

        call sum21(n0,n2,n0,n2,x10,q9, 1.000)
        deallocate(q9)

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

        call sum21(n2,n3,n2,n3,x11,q10,-1.000)
        deallocate(q10)

        allocate(d1(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
        call reorder4213(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n0,n1,n0,n1,intm,d1)
        allocate(b2(n2+1:n3,n0+1:n2))
        call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
        allocate(s31(n0+1:n2,n0+1:n2,n0+1:n1,n0+1:n1))
        i1=k1*k1*k2
        i2=k2
        i3=k4
        call egemm(i1,i2,i3,d1,b2,s31)
        deallocate(d1)
        deallocate(b2)

        call sum3124(n0,n2,n0,n1,n0,n2,n0,n1,x13,s31, 1.000)

        allocate(d1(n0+1:n2,n0+1:n1,n0+1:n2,n0+1:n1))
        call reorder2314(n0,n2,n0,n2,n0,n1,n0,n1,
     & n0,n2,n0,n1,n0,n2,n0,n1,s31,d1)
        allocate(b2(n0+1:n2,n2+1:n3))
        call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
        allocate(s121(n2+1:n3,n0+1:n1,n0+1:n2,n0+1:n1))
        i1=k1*k2*k1
        i2=k4
        i3=k2
        call egemm(i1,i2,i3,d1,b2,s121)
        deallocate(d1)
        deallocate(b2)
        deallocate(s31)

        call sum2134(n0,n1,n2,n3,n0,n2,n0,n1,x1,s121,-1.000)
        deallocate(s121)

        allocate(d1(n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n1))
        call reorder2143(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n0,n1,n2,n3,n0,n1,intm,d1)
        allocate(b2(n0+1:n2,n2+1:n3))
        call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
        allocate(s32(n2+1:n3,n0+1:n1,n2+1:n3,n0+1:n1))
        i1=k1*k4*k1
        i2=k4
        i3=k2
        call egemm(i1,i2,i3,d1,b2,s32)
        deallocate(d1)
        deallocate(b2)

        call sum3124(n0,n1,n2,n3,n2,n3,n0,n1,x14,s32,-1.000)
        deallocate(s32)

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

        x8=x8+q11
        deallocate(q11)

        allocate(d1(n2+1:n3,n0+1:n2,n1+1:n3,n1+1:n3))
        call reorder4231(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n1,n3,n1,n3,intm,d1)
        allocate(b2(n2+1:n3,n0+1:n2))
        call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
        allocate(s33(n0+1:n2,n0+1:n2,n1+1:n3,n1+1:n3))
        i1=k3*k3*k2
        i2=k2
        i3=k4
        call egemm(i1,i2,i3,d1,b2,s33)
        deallocate(d1)
        deallocate(b2)

        call sum4123(n0,n2,n1,n3,n1,n3,n0,n2,x15,s33, 1.000)

        allocate(d1(n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n2))
        call reorder2341(n0,n2,n0,n2,n1,n3,n1,n3,
     & n0,n2,n1,n3,n1,n3,n0,n2,s33,d1)
        allocate(b2(n0+1:n2,n2+1:n3))
        call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
        allocate(s122(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2))
        i1=k2*k3*k3
        i2=k4
        i3=k2
        call egemm(i1,i2,i3,d1,b2,s122)
        deallocate(d1)
        deallocate(b2)
        deallocate(s33)

        call sum2134(n1,n3,n2,n3,n1,n3,n0,n2,x2,s122,-1.000)
        deallocate(s122)

        allocate(d1(n0+1:n2,n2+1:n3,n1+1:n3,n1+1:n3))
        call reorder2431(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n1,n3,n1,n3,intm,d1)
        allocate(b2(n0+1:n2,n2+1:n3))
        call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
        allocate(s34(n2+1:n3,n2+1:n3,n1+1:n3,n1+1:n3))
        i1=k3*k3*k4
        i2=k4
        i3=k2
        call egemm(i1,i2,i3,d1,b2,s34)
        deallocate(d1)
        deallocate(b2)

        call sum3124(n2,n3,n1,n3,n2,n3,n1,n3,x16,s34,-1.000)
        deallocate(s34)

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

        x9=x9+q12
        deallocate(q12)

        allocate(d1(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
        call reorder4213(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n0,n2,n0,n2,intb,d1)
        allocate(b2(n2+1:n3,n0+1:n2))
        call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
        allocate(s35(n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n2))
        i1=k2*k2*k2
        i2=k2
        i3=k4
        call egemm(i1,i2,i3,d1,b2,s35)
        deallocate(d1)
        deallocate(b2)


        do i=n0+1,n1;do j=n0+1,n2-1;do k=j+1,n2
          if(indocc(k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-1;do c=b+1,n3
            if(indunocc(c,b,a).eq.1)cycle
            sum=0.0d0
            do m=n0+1,n2;do n=n0+1,n2
              sum=sum
     &     + (s35(k,n,m,j)*t3c(c,b,a,n,m,i)      !kjcbai  (+0.500)
     &     - s35(j,n,m,k)*t3c(c,b,a,n,m,i))/2.0d0!jkcbai  (-0.500)
            enddo;enddo
            v3c(c,b,a,k,j,i)=v3c(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
        enddo;enddo;enddo


        allocate(d1(n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n2))
        call reorder3214(n0,n2,n0,n2,n0,n2,n0,n2,
     & n0,n2,n0,n2,n0,n2,n0,n2,s35,d1)
        allocate(b2(n0+1:n2,n2+1:n3))
        call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
        allocate(s123(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
        i1=k2*k2*k2
        i2=k4
        i3=k2
        call egemm(i1,i2,i3,d1,b2,s123)
        deallocate(d1)
        deallocate(b2)
        deallocate(s35)

        call sum2134(n0,n2,n2,n3,n0,n2,n0,n2,x32,s123,-1.000)
        deallocate(s123)

        allocate(d1(n0+1:n2,n0+1:n2,n2+1:n3,n0+1:n2))
        call reorder1243(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n0,n2,n2,n3,n0,n2,intb,d1)
        allocate(b2(n0+1:n2,n2+1:n3))
        call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
        allocate(s36(n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2))
        i1=k2*k4*k2
        i2=k4
        i3=k2
        call egemm(i1,i2,i3,d1,b2,s36)
        deallocate(d1)
        deallocate(b2)

        allocate(x18(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
        x18=0.0d0
        call sum3124(n0,n2,n2,n3,n2,n3,n0,n2,x18,s36,-1.000)

        allocate(d1(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
        call reorder2314(n2,n3,n0,n2,n2,n3,n0,n2,
     & n0,n2,n2,n3,n2,n3,n0,n2,s36,d1)
        allocate(b2(n0+1:n2,n2+1:n3))
        call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
        allocate(s124(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
        i1=k2*k4*k4
        i2=k4
        i3=k2
        call egemm(i1,i2,i3,d1,b2,s124)
        deallocate(d1)
        deallocate(b2)
        deallocate(s36)

        call sum2134(n2,n3,n2,n3,n2,n3,n0,n2,x4,s124, 1.000)
        deallocate(s124)

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

        x10=x10-q13
        deallocate(q13)

        allocate(d1(n2+1:n3,n0+1:n2,n2+1:n3,n2+1:n3))
        call reorder3241(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n2,n3,n2,n3,intb,d1)
        allocate(b2(n2+1:n3,n0+1:n2))
        call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
        allocate(s37(n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3))
        i1=k4*k4*k2
        i2=k2
        i3=k4
        call egemm(i1,i2,i3,d1,b2,s37)
        deallocate(d1)
        deallocate(b2)

        call sum4123(n0,n2,n2,n3,n2,n3,n0,n2,x18,s37, 1.000)

        allocate(d1(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
        call reorder2341(n0,n2,n0,n2,n2,n3,n2,n3,
     & n0,n2,n2,n3,n2,n3,n0,n2,s37,d1)
        allocate(b2(n0+1:n2,n2+1:n3))
        call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
        allocate(s126(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
        i1=k2*k4*k4
        i2=k4
        i3=k2
        call egemm(i1,i2,i3,d1,b2,s126)
        deallocate(d1)
        deallocate(b2)

        call sum3124(n2,n3,n2,n3,n2,n3,n0,n2,x33,s126, 1.000)
        deallocate(s126)

        allocate(d1(n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2))
        call reorder3241(n0,n2,n0,n2,n2,n3,n2,n3,
     & n2,n3,n0,n2,n2,n3,n0,n2,s37,d1)
        allocate(b2(n2+1:n3,n0+1:n2))
        call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
        allocate(s125(n0+1:n2,n0+1:n2,n2+1:n3,n0+1:n2))
        i1=k2*k4*k2
        i2=k2
        i3=k4
        call egemm(i1,i2,i3,d1,b2,s125)
        deallocate(d1)
        deallocate(b2)
        deallocate(s37)

        call sum3124(n0,n2,n2,n3,n0,n2,n0,n2,x3,s125, 1.000)
        deallocate(s125)

        allocate(d1(n0+1:n2,n2+1:n3,n2+1:n3,n2+1:n3))
        call reorder2431(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n2,n3,n2,n3,intb,d1)
        allocate(b2(n0+1:n2,n2+1:n3))
        call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
        allocate(s38(n2+1:n3,n2+1:n3,n2+1:n3,n2+1:n3))
        i1=k4*k4*k4
        i2=k4
        i3=k2
        call egemm(i1,i2,i3,d1,b2,s38)
        deallocate(d1)
        deallocate(b2)


        do i=n0+1,n1;do j=n0+1,n2-1;do k=j+1,n2
          if(indocc(k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-1;do c=b+1,n3
            if(indunocc(c,b,a).eq.1)cycle
            sum=0.0d0
            do e=n2+1,n3;do f=n2+1,n3
              sum=sum
     &     + (s38(b,f,e,c)*t3c(f,e,a,k,j,i)      !bcakji (+0.500)
     &     - s38(c,f,e,b)*t3c(f,e,a,k,j,i))/2.0d0!cbakji (-0.500)
            enddo;enddo
            v3c(c,b,a,k,j,i)=v3c(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
        enddo;enddo;enddo

        deallocate(s38)

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

        x11=x11-q14
        deallocate(q14)

        allocate(d1(n2+1:n3,n0+1:n2,n0+1:n1,n1+1:n3))
        call reorder4213(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n0,n1,n1,n3,intm,d1)
        allocate(b2(n2+1:n3,n0+1:n2))
        call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
        allocate(s39(n0+1:n2,n0+1:n2,n0+1:n1,n1+1:n3))
        i1=k3*k1*k2
        i2=k2
        i3=k4
        call egemm(i1,i2,i3,d1,b2,s39)
        deallocate(d1)
        deallocate(b2)

        allocate(x24(n0+1:n2,n0+1:n1,n1+1:n3,n0+1:n2))
        x24=0.0d0
        call sum4123(n0,n2,n0,n1,n1,n3,n0,n2,x24,s39, 1.000)

        call sumx2134(n0,n3,n0,n2,n0,n1,n1,n3,n0,n2,x24,intm, 1.000)

        if (lvl_q) then
          allocate(t4c(n2+1:n3,n2+1:n3,n1+1:n3,n1+1:n3,                !ilias: if no quadruples comment out the following 4 lines
     & n0+1:n2,n0+1:n2,n0+1:n1,n0+1:n1))
          rewind(tc)
          read(tc)t4c

          do i=n0+1,n1;do j=n0+1,n2-1;do k=j+1,n2                      !ilias: if no quadruples comment out the following 13 lines
            if(indocc(k,j,i).eq.1)cycle
            do a=n1+1,n3;do b=n2+1,n3-1;do c=b+1,n3
              if(indunocc(c,b,a).eq.1)cycle
              sum=0.0d0
              do e=n1+1,n3;do m=n0+1,n1;do n=n0+1,n2
                sum=sum
     &     + x24(n,m,e,j)*t4c(c,b,e,a,n,k,m,i) !jcbaki(+1.000)
     &     - x24(n,m,e,k)*t4c(c,b,e,a,n,j,m,i) !kcbaji(-1.000)
              enddo;enddo;enddo
              v3c(c,b,a,k,j,i)=v3c(c,b,a,k,j,i)+sum
            enddo;enddo;enddo
          enddo;enddo;enddo

          deallocate(t4c)                                              !ilias: if no quadruples comment out the following 2 lines
          deallocate(x24)
        endif

        allocate(d1(n0+1:n2,n1+1:n3,n0+1:n1,n0+1:n2))
        call reorder2431(n0,n2,n0,n2,n0,n1,n1,n3,
     & n0,n2,n1,n3,n0,n1,n0,n2,s39,d1)
        allocate(d2(n0+1:n2,n1+1:n3,n2+1:n3,n0+1:n1))
        call reorder3214(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n1,n3,n2,n3,n0,n1,t2b,d2)
        allocate(s135(n2+1:n3,n0+1:n1,n0+1:n1,n0+1:n2))
        i1=k2*k1
        i2=k1*k4
        i3=k3*k2
        call egemm(i1,i2,i3,d1,d2,s135)
        deallocate(d1)
        deallocate(d2)

        call sum2413(n0,n1,n2,n3,n0,n2,n0,n1,x1,s135,-1.000)
        deallocate(s135)

        allocate(d1(n0+1:n1,n1+1:n3,n0+1:n2,n0+1:n2))
        call reorder3421(n0,n2,n0,n2,n0,n1,n1,n3,
     & n0,n1,n1,n3,n0,n2,n0,n2,s39,d1)
        allocate(d2(n0+1:n1,n1+1:n3,n2+1:n3,n0+1:n2))
        call reorder4213(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n1,n3,n2,n3,n0,n2,t2b,d2)
        allocate(s134(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
        i1=k2*k2
        i2=k2*k4
        i3=k3*k1
        call egemm(i1,i2,i3,d1,d2,s134)
        deallocate(d1)
        deallocate(d2)

        allocate(x34(n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2))
        x34=0.0d0
        call sum2314(n0,n2,n2,n3,n0,n2,n0,n2,x34,s134, 1.000)
        deallocate(s134)

        allocate(d1(n0+1:n2,n0+1:n1,n1+1:n3,n0+1:n2))
        call reorder2341(n0,n2,n0,n2,n0,n1,n1,n3,
     & n0,n2,n0,n1,n1,n3,n0,n2,s39,d1)
        allocate(d2(n0+1:n2,n0+1:n1,n2+1:n3,n1+1:n3))
        call reorder3412(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n0,n1,n2,n3,n1,n3,t2b,d2)
        allocate(s133(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2))
        i1=k2*k3
        i2=k3*k4
        i3=k1*k2
        call egemm(i1,i2,i3,d1,d2,s133)
        deallocate(d1)
        deallocate(d2)

        call sum2314(n1,n3,n2,n3,n1,n3,n0,n2,x2,s133, 1.000)
        deallocate(s133)

        allocate(d1(n0+1:n1,n1+1:n3,n0+1:n2,n0+1:n2))
        call reorder3421(n0,n2,n0,n2,n0,n1,n1,n3,
     & n0,n1,n1,n3,n0,n2,n0,n2,s39,d1)
        allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
        call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
        allocate(s131(n1+1:n3,n0+1:n1,n0+1:n2,n0+1:n2))
        i1=k2*k2
        i2=k1*k3
        i3=k3*k1
        call egemm(i1,i2,i3,d1,d2,s131)
        deallocate(d1)
        deallocate(d2)

        call sum2413(n0,n2,n1,n3,n0,n2,n0,n1,x5,s131, 1.000)
        deallocate(s131)

        allocate(d1(n0+1:n2,n0+1:n1,n1+1:n3,n0+1:n2))
        call reorder2341(n0,n2,n0,n2,n0,n1,n1,n3,
     & n0,n2,n0,n1,n1,n3,n0,n2,s39,d1)
        allocate(b2(n0+1:n2,n2+1:n3))
        call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
        allocate(s127(n2+1:n3,n0+1:n1,n1+1:n3,n0+1:n2))
        i1=k2*k3*k1
        i2=k4
        i3=k2
        call egemm(i1,i2,i3,d1,b2,s127)
        deallocate(d1)
        deallocate(b2)
        deallocate(s39)

        call sum3124(n0,n1,n1,n3,n2,n3,n0,n2,x7,s127,-1.000)
        deallocate(s127)

        allocate(d1(n0+1:n2,n0+1:n1,n2+1:n3,n1+1:n3))
        call reorder2143(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n0,n1,n2,n3,n1,n3,intm,d1)
        allocate(b2(n0+1:n2,n2+1:n3))
        call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
        allocate(s40(n2+1:n3,n0+1:n1,n2+1:n3,n1+1:n3))
        i1=k3*k4*k1
        i2=k4
        i3=k2
        call egemm(i1,i2,i3,d1,b2,s40)
        deallocate(d1)
        deallocate(b2)

        allocate(x25(n0+1:n1,n2+1:n3,n1+1:n3,n2+1:n3))
        x25=0.0d0
        call sum4123(n0,n1,n2,n3,n1,n3,n2,n3,x25,s40,-1.000)

        call sumx1432(n0,n3,n0,n1,n2,n3,n1,n3,n2,n3,x25,intm, 1.000)

        if (lvl_q) then
          allocate(t4c(n2+1:n3,n2+1:n3,n1+1:n3,n1+1:n3,                !ilias: if no quadruples comment out the following 4 lines
     & n0+1:n2,n0+1:n2,n0+1:n1,n0+1:n1))
          rewind(tc)
          read(tc)t4c

          do i=n0+1,n1;do j=n0+1,n2-1;do k=j+1,n2                      !ilias: if no quadruples comment out the following 13 lines
            if(indocc(k,j,i).eq.1)cycle
            do a=n1+1,n3;do b=n2+1,n3-1;do c=b+1,n3
              if(indunocc(c,b,a).eq.1)cycle
              sum=0.0d0
              do e=n1+1,n3;do f=n2+1,n3;do m=n0+1,n1
                sum=sum
     &     + x25(m,f,e,c)*t4c(f,b,e,a,k,j,m,i) !cbakji(+1.000)
     &     - x25(m,f,e,b)*t4c(f,c,e,a,k,j,m,i) !bcakji(-1.000)
              enddo;enddo;enddo
              v3c(c,b,a,k,j,i)=v3c(c,b,a,k,j,i)+sum
            enddo;enddo;enddo
          enddo;enddo;enddo

          deallocate(t4c)                                              !ilias: if no quadruples comment out the following 2 lines
          deallocate(x25)
        endif

        allocate(d1(n2+1:n3,n1+1:n3,n0+1:n1,n2+1:n3))
        call reorder3421(n2,n3,n0,n1,n2,n3,n1,n3,
     & n2,n3,n1,n3,n0,n1,n2,n3,s40,d1)
        allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
        call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
        allocate(s138(n0+1:n2,n0+1:n1,n0+1:n1,n2+1:n3))
        i1=k4*k1
        i2=k1*k2
        i3=k3*k4
        call egemm(i1,i2,i3,d1,d2,s138)
        deallocate(d1)
        deallocate(d2)

        call sum3412(n0,n1,n2,n3,n0,n2,n0,n1,x1,s138,-1.000)
        deallocate(s138)

        allocate(d1(n0+1:n1,n1+1:n3,n2+1:n3,n2+1:n3))
        call reorder2431(n2,n3,n0,n1,n2,n3,n1,n3,
     & n0,n1,n1,n3,n2,n3,n2,n3,s40,d1)
        allocate(d2(n0+1:n1,n1+1:n3,n2+1:n3,n0+1:n2))
        call reorder4213(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n1,n3,n2,n3,n0,n2,t2b,d2)
        allocate(s137(n2+1:n3,n0+1:n2,n2+1:n3,n2+1:n3))
        i1=k4*k4
        i2=k2*k4
        i3=k3*k1
        call egemm(i1,i2,i3,d1,d2,s137)
        deallocate(d1)
        deallocate(d2)

        allocate(x35(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
        x35=0.0d0
        call sum3412(n2,n3,n2,n3,n2,n3,n0,n2,x35,s137, 1.000)
        deallocate(s137)

        allocate(d1(n0+1:n1,n2+1:n3,n1+1:n3,n2+1:n3))
        call reorder2341(n2,n3,n0,n1,n2,n3,n1,n3,
     & n0,n1,n2,n3,n1,n3,n2,n3,s40,d1)
        allocate(d2(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
        call reorder4123(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n2,n3,n1,n3,n0,n2,t2b,d2)
        allocate(s136(n1+1:n3,n0+1:n2,n1+1:n3,n2+1:n3))
        i1=k4*k3
        i2=k2*k3
        i3=k4*k1
        call egemm(i1,i2,i3,d1,d2,s136)
        deallocate(d1)
        deallocate(d2)

        call sum3412(n1,n3,n2,n3,n1,n3,n0,n2,x2,s136, 1.000)
        deallocate(s136)

        allocate(d1(n0+1:n1,n1+1:n3,n2+1:n3,n2+1:n3))
        call reorder2431(n2,n3,n0,n1,n2,n3,n1,n3,
     & n0,n1,n1,n3,n2,n3,n2,n3,s40,d1)
        allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
        call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
        allocate(s132(n1+1:n3,n0+1:n1,n2+1:n3,n2+1:n3))
        i1=k4*k4
        i2=k1*k3
        i3=k3*k1
        call egemm(i1,i2,i3,d1,d2,s132)
        deallocate(d1)
        deallocate(d2)
        deallocate(s40)

        call sum3412(n2,n3,n2,n3,n1,n3,n0,n1,x6,s132,-1.000)
        deallocate(s132)

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

        x21=x21+q15

        call sumx12(0,n3,n0,n1,n1,n3,x21,fockr, 1.000)

        if (lvl_q) then
          allocate(t4c(n2+1:n3,n2+1:n3,n1+1:n3,n1+1:n3,                !ilias: if no quadruples comment out the following 4 lines
     & n0+1:n2,n0+1:n2,n0+1:n1,n0+1:n1))
          rewind(tc)
          read(tc)t4c

          do i=n0+1,n1;do j=n0+1,n2-1;do k=j+1,n2                      !ilias: if no quadruples comment out the following 12 lines
            if(indocc(k,j,i).eq.1)cycle
            do a=n1+1,n3;do b=n2+1,n3-1;do c=b+1,n3
              if(indunocc(c,b,a).eq.1)cycle
              sum=0.0d0
              do e=n1+1,n3;do m=n0+1,n1
                sum=sum
     &     + x21(m,e)*t4c(c,b,e,a,k,j,m,i) !cbakji (+1.000)
              enddo;enddo
              v3c(c,b,a,k,j,i)=v3c(c,b,a,k,j,i)+sum
            enddo;enddo;enddo
          enddo;enddo;enddo

          deallocate(t4c)                                              !ilias: if no quadruples comment out the following 2 lines
          deallocate(x21)
        endif

        allocate(b1(n1+1:n3,n0+1:n1))
        call reorder21(n0,n1,n1,n3,
     & n1,n3,n0,n1,q15,b1)
        allocate(d2(n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
        call reorder2134(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n2,n3,n0,n2,n0,n1,t2b,d2)
        allocate(s139(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
        i1=k1
        i2=k1*k2*k4
        i3=k3
        call egemm(i1,i2,i3,b1,d2,s139)
        deallocate(b1)
        deallocate(d2)
        deallocate(q15)

        call sum2341(n0,n1,n2,n3,n0,n2,n0,n1,x1,s139, 1.000)
        deallocate(s139)

        allocate(d1(n2+1:n3,n0+1:n2,n0+1:n2,n2+1:n3))
        call reorder3214(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n0,n2,n2,n3,intb,d1)
        allocate(b2(n2+1:n3,n0+1:n2))
        call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
        allocate(s41(n0+1:n2,n0+1:n2,n0+1:n2,n2+1:n3))
        i1=k4*k2*k2
        i2=k2
        i3=k4
        call egemm(i1,i2,i3,d1,b2,s41)
        deallocate(d1)
        deallocate(b2)

        allocate(x29(n0+1:n2,n0+1:n2,n2+1:n3,n0+1:n2))
        x29=0.0d0
        call sum4123(n0,n2,n0,n2,n2,n3,n0,n2,x29,s41, 1.000)

        call sumx2143(n0,n3,n0,n2,n0,n2,n2,n3,n0,n2,x29,intb, 1.000)

        if (lvl_q) then
          allocate(t4d(n2+1:n3,n2+1:n3,n2+1:n3,n1+1:n3,                !ilias: if no quadruples comment out the following 4 lines
     & n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n1))
          rewind(td)
          read(td)t4d

          do i=n0+1,n1;do j=n0+1,n2-1;do k=j+1,n2                      !ilias: if no quadruples comment out the following 13 lines
            if(indocc(k,j,i).eq.1)cycle
            do a=n1+1,n3;do b=n2+1,n3-1;do c=b+1,n3
              if(indunocc(c,b,a).eq.1)cycle
              sum=0.0d0
              do e=n2+1,n3;do m=n0+1,n2;do n=n0+1,n2
                sum=sum
     &     + (x29(n,m,e,j)*t4d(e,c,b,a,n,m,k,i)      !jcbaki(+0.500)
     &     - x29(n,m,e,k)*t4d(e,c,b,a,n,m,j,i))/2.0d0!kcbaji(-0.500)
              enddo;enddo;enddo
              v3c(c,b,a,k,j,i)=v3c(c,b,a,k,j,i)+sum
            enddo;enddo;enddo
          enddo;enddo;enddo

          deallocate(t4d)                                              !ilias: if no quadruples comment out the following 2 lines
          deallocate(x29)
        endif

        allocate(d1(n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2))
        call reorder3421(n0,n2,n0,n2,n0,n2,n2,n3,
     & n0,n2,n2,n3,n0,n2,n0,n2,s41,d1)
        allocate(d2(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
        call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n2,n3,n1,n3,n0,n1,t2b,d2)
        allocate(s142(n1+1:n3,n0+1:n1,n0+1:n2,n0+1:n2))
        i1=k2*k2
        i2=k1*k3
        i3=k4*k2
        call egemm(i1,i2,i3,d1,d2,s142)
        deallocate(d1)
        deallocate(d2)

        call sum2413(n0,n2,n1,n3,n0,n2,n0,n1,x5,s142,-1.000)
        deallocate(s142)

        allocate(d1(n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2))
        call reorder2431(n0,n2,n0,n2,n0,n2,n2,n3,
     & n0,n2,n2,n3,n0,n2,n0,n2,s41,d1)
        allocate(d2(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
        call reorder3124(n2,n3,n2,n3,n0,n2,n0,n2,
     & n0,n2,n2,n3,n2,n3,n0,n2,t2c,d2)
        allocate(s141(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
        i1=k2*k2
        i2=k2*k4
        i3=k4*k2
        call egemm(i1,i2,i3,d1,d2,s141)
        deallocate(d1)
        deallocate(d2)

        call sum2314(n0,n2,n2,n3,n0,n2,n0,n2,x32,s141,-1.000)
        deallocate(s141)

        allocate(d1(n0+1:n2,n0+1:n2,n2+1:n3,n0+1:n2))
        call reorder2341(n0,n2,n0,n2,n0,n2,n2,n3,
     & n0,n2,n0,n2,n2,n3,n0,n2,s41,d1)
        allocate(d2(n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3))
        call reorder3412(n2,n3,n2,n3,n0,n2,n0,n2,
     & n0,n2,n0,n2,n2,n3,n2,n3,t2c,d2)
        allocate(s140(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
        i1=k2*k4
        i2=k4*k4
        i3=k2*k2
        call egemm(i1,i2,i3,d1,d2,s140)
        deallocate(d1)
        deallocate(d2)

        call sum2314(n2,n3,n2,n3,n2,n3,n0,n2,x4,s140, 0.500)
        deallocate(s140)

        allocate(d1(n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2))
        call reorder3421(n0,n2,n0,n2,n0,n2,n2,n3,
     & n0,n2,n2,n3,n0,n2,n0,n2,s41,d1)
        allocate(b2(n0+1:n2,n2+1:n3))
        call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
        allocate(q31(n0+1:n2,n0+1:n2))
        i1=k2*k2
        i3=k4*k2
        call egemm1(i1,i3,d1,b2,q31)
        deallocate(d1)
        deallocate(b2)

        x10=x10-q31
        deallocate(q31)

        allocate(d1(n0+1:n2,n0+1:n2,n2+1:n3,n0+1:n2))
        call reorder3241(n0,n2,n0,n2,n0,n2,n2,n3,
     & n0,n2,n0,n2,n2,n3,n0,n2,s41,d1)
        allocate(b2(n0+1:n2,n2+1:n3))
        call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
        allocate(s129(n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2))
        i1=k2*k4*k2
        i2=k4
        i3=k2
        call egemm(i1,i2,i3,d1,b2,s129)
        deallocate(d1)
        deallocate(b2)

        call sum3124(n0,n2,n2,n3,n2,n3,n0,n2,x18,s129,-1.000)

        allocate(d1(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
        call reorder2314(n2,n3,n0,n2,n2,n3,n0,n2,
     & n0,n2,n2,n3,n2,n3,n0,n2,s129,d1)
        allocate(b2(n0+1:n2,n2+1:n3))
        call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
        allocate(s153(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
        i1=k2*k4*k4
        i2=k4
        i3=k2
        call egemm(i1,i2,i3,d1,b2,s153)
        deallocate(d1)
        deallocate(b2)
        deallocate(s129)

        call sum2134(n2,n3,n2,n3,n2,n3,n0,n2,x4,s153, 1.000)
        deallocate(s153)

        allocate(d1(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
        call reorder4231(n0,n2,n0,n2,n0,n2,n2,n3,
     & n2,n3,n0,n2,n0,n2,n0,n2,s41,d1)
        allocate(b2(n2+1:n3,n0+1:n2))
        call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
        allocate(s128(n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n2))
        i1=k2*k2*k2
        i2=k2
        i3=k4
        call egemm(i1,i2,i3,d1,b2,s128)
        deallocate(d1)
        deallocate(b2)
        deallocate(s41)

        allocate(x17(n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n2))
        x17=0.0d0
        call sum3124(n0,n2,n0,n2,n0,n2,n0,n2,x17,s128, 1.000)

        allocate(d1(n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n2))
        call reorder3214(n0,n2,n0,n2,n0,n2,n0,n2,
     & n0,n2,n0,n2,n0,n2,n0,n2,s128,d1)
        allocate(b2(n0+1:n2,n2+1:n3))
        call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
        allocate(s152(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
        i1=k2*k2*k2
        i2=k4
        i3=k2
        call egemm(i1,i2,i3,d1,b2,s152)
        deallocate(d1)
        deallocate(b2)
        deallocate(s128)

        call sum2134(n0,n2,n2,n3,n0,n2,n0,n2,x3,s152,-1.000)
        deallocate(s152)

        allocate(d1(n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3))
        call reorder1243(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n0,n2,n2,n3,n2,n3,intb,d1)
        allocate(b2(n0+1:n2,n2+1:n3))
        call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
        allocate(s42(n2+1:n3,n0+1:n2,n2+1:n3,n2+1:n3))
        i1=k4*k4*k2
        i2=k4
        i3=k2
        call egemm(i1,i2,i3,d1,b2,s42)
        deallocate(d1)
        deallocate(b2)

        allocate(x30(n0+1:n2,n2+1:n3,n2+1:n3,n2+1:n3))
        x30=0.0d0
        call sum4123(n0,n2,n2,n3,n2,n3,n2,n3,x30,s42,-1.000)

        call sumx4132(n0,n3,n0,n2,n2,n3,n2,n3,n2,n3,x30,intb, 1.000)

        if (lvl_q) then
          allocate(t4d(n2+1:n3,n2+1:n3,n2+1:n3,n1+1:n3,                !ilias: if no quadruples comment out the following 4 lines
     & n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n1))
          rewind(td)
          read(td)t4d

          do i=n0+1,n1;do j=n0+1,n2-1;do k=j+1,n2                      !ilias: if no quadruples comment out the following 13 lines
            if(indocc(k,j,i).eq.1)cycle
            do a=n1+1,n3;do b=n2+1,n3-1;do c=b+1,n3
              if(indunocc(c,b,a).eq.1)cycle
              sum=0.0d0
              do e=n2+1,n3;do f=n2+1,n3;do m=n0+1,n2
                sum=sum
     &     + (x30(m,f,e,c)*t4d(f,e,b,a,m,k,j,i)      !cbakji(+0.500)
     &     - x30(m,f,e,b)*t4d(f,e,c,a,m,k,j,i))/2.0d0!bcakji(-0.500)
              enddo;enddo;enddo
              v3c(c,b,a,k,j,i)=v3c(c,b,a,k,j,i)+sum
            enddo;enddo;enddo
          enddo;enddo;enddo

          deallocate(t4d)                                              !ilias: if no quadruples comment out the following 2 lines
          deallocate(x30)
        endif

        allocate(d1(n0+1:n2,n2+1:n3,n2+1:n3,n2+1:n3))
        call reorder2431(n2,n3,n0,n2,n2,n3,n2,n3,
     & n0,n2,n2,n3,n2,n3,n2,n3,s42,d1)
        allocate(d2(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
        call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n2,n3,n1,n3,n0,n1,t2b,d2)
        allocate(s146(n1+1:n3,n0+1:n1,n2+1:n3,n2+1:n3))
        i1=k4*k4
        i2=k1*k3
        i3=k4*k2
        call egemm(i1,i2,i3,d1,d2,s146)
        deallocate(d1)
        deallocate(d2)

        call sum3412(n2,n3,n2,n3,n1,n3,n0,n1,x6,s146, 1.000)
        deallocate(s146)

        allocate(d1(n2+1:n3,n2+1:n3,n0+1:n2,n2+1:n3))
        call reorder3421(n2,n3,n0,n2,n2,n3,n2,n3,
     & n2,n3,n2,n3,n0,n2,n2,n3,s42,d1)
        allocate(d2(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
        call reorder1234(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n2,n3,n0,n2,n0,n2,t2c,d2)
        allocate(s145(n0+1:n2,n0+1:n2,n0+1:n2,n2+1:n3))
        i1=k4*k2
        i2=k2*k2
        i3=k4*k4
        call egemm(i1,i2,i3,d1,d2,s145)
        deallocate(d1)
        deallocate(d2)

        call sum3412(n0,n2,n2,n3,n0,n2,n0,n2,x3,s145,-0.500)
        deallocate(s145)

        allocate(d1(n0+1:n2,n2+1:n3,n2+1:n3,n2+1:n3))
        call reorder2341(n2,n3,n0,n2,n2,n3,n2,n3,
     & n0,n2,n2,n3,n2,n3,n2,n3,s42,d1)
        allocate(d2(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
        call reorder3124(n2,n3,n2,n3,n0,n2,n0,n2,
     & n0,n2,n2,n3,n2,n3,n0,n2,t2c,d2)
        allocate(s143(n2+1:n3,n0+1:n2,n2+1:n3,n2+1:n3))
        i1=k4*k4
        i2=k2*k4
        i3=k4*k2
        call egemm(i1,i2,i3,d1,d2,s143)
        deallocate(d1)
        deallocate(d2)

        call sum3412(n2,n3,n2,n3,n2,n3,n0,n2,x33,s143,-1.000)
        deallocate(s143)

        allocate(d1(n0+1:n2,n2+1:n3,n2+1:n3,n2+1:n3))
        call reorder2431(n2,n3,n0,n2,n2,n3,n2,n3,
     & n0,n2,n2,n3,n2,n3,n2,n3,s42,d1)
        allocate(b2(n0+1:n2,n2+1:n3))
        call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
        allocate(q32(n2+1:n3,n2+1:n3))
        i1=k4*k4
        i3=k4*k2
        call egemm1(i1,i3,d1,b2,q32)
        deallocate(d1)
        deallocate(b2)

        x11=x11+q32
        deallocate(q32)

        allocate(d1(n0+1:n2,n2+1:n3,n2+1:n3,n2+1:n3))
        call reorder2341(n2,n3,n0,n2,n2,n3,n2,n3,
     & n0,n2,n2,n3,n2,n3,n2,n3,s42,d1)
        allocate(b2(n0+1:n2,n2+1:n3))
        call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
        allocate(s130(n2+1:n3,n2+1:n3,n2+1:n3,n2+1:n3))
        i1=k4*k4*k4
        i2=k4
        i3=k2
        call egemm(i1,i2,i3,d1,b2,s130)
        deallocate(d1)
        deallocate(b2)
        deallocate(s42)

        allocate(x19(n2+1:n3,n2+1:n3,n2+1:n3,n2+1:n3))
        x19=0.0d0
        call sum3124(n2,n3,n2,n3,n2,n3,n2,n3,x19,s130, 1.000)
        deallocate(s130)

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

        x26=x26+q16

        call sumx12(0,n3,n0,n2,n2,n3,x26,fockb, 1.000)

        if (lvl_q) then
          allocate(t4d(n2+1:n3,n2+1:n3,n2+1:n3,n1+1:n3,                !ilias: if no quadruples comment out the following 4 lines
     & n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n1))
          rewind(td)
          read(td)t4d

          do i=n0+1,n1;do j=n0+1,n2-1;do k=j+1,n2                      !ilias: if no quadruples comment out the following 12 lines
            if(indocc(k,j,i).eq.1)cycle
            do a=n1+1,n3;do b=n2+1,n3-1;do c=b+1,n3
              if(indunocc(c,b,a).eq.1)cycle
              sum=0.0d0
              do e=n2+1,n3;do m=n0+1,n2
                sum=sum
     &     + x26(m,e)*t4d(e,c,b,a,m,k,j,i)   !cbakji (+1.000)
              enddo;enddo
              v3c(c,b,a,k,j,i)=v3c(c,b,a,k,j,i)+sum
            enddo;enddo;enddo
          enddo;enddo;enddo

          deallocate(t4d)                                              !ilias: if no quadruples comment out the following 2 lines
          deallocate(x26)
        endif

        allocate(b1(n2+1:n3,n0+1:n2))
        call reorder21(n0,n2,n2,n3,
     & n2,n3,n0,n2,q16,b1)
        allocate(d2(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
        call reorder1234(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n2,n3,n0,n2,n0,n2,t2c,d2)
        allocate(s147(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
        i1=k2
        i2=k2*k2*k4
        i3=k4
        call egemm(i1,i2,i3,b1,d2,s147)
        deallocate(b1)
        deallocate(d2)

        call sum2341(n0,n2,n2,n3,n0,n2,n0,n2,x31,s147, 1.000)
        deallocate(s147)

        allocate(b1(n2+1:n3,n0+1:n2))
        call reorder21(n0,n2,n2,n3,
     & n2,n3,n0,n2,q16,b1)
        allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
        call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
        allocate(s144(n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
        i1=k2
        i2=k1*k2*k3
        i3=k4
        call egemm(i1,i2,i3,b1,d2,s144)
        deallocate(b1)
        deallocate(d2)
        deallocate(q16)

        call sum2341(n0,n2,n1,n3,n0,n2,n0,n1,x5,s144, 1.000)
        deallocate(s144)

        if(t2diag4.ne.0) then         !ilias: testing the particle version of 3cc
          allocate(d1(n0+1:n1,n1+1:n3,n0+1:n2,n0+1:n2))
          call reorder1324(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n0,n2,n0,n2,intm,d1)
          allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
          call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
          allocate(s43(n1+1:n3,n0+1:n1,n0+1:n2,n0+1:n2))
          i1=k2*k2
          i2=k1*k3
          i3=k3*k1
          call egemm(i1,i2,i3,d1,d2,s43)
          deallocate(d1)
          deallocate(d2)

          factor=t2diag4
          call sum2413(n0,n2,n1,n3,n0,n2,n0,n1,x5,s43,factor)
          deallocate(s43)
        endif

        if(t2diag3.ne.0) then
          allocate(d1(n0+1:n1,n1+1:n3,n2+1:n3,n2+1:n3))                 !ilias: commented out 16 lines for 3cc
          call reorder1342(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n2,n3,n2,n3,intm,d1)
          allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
          call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
          allocate(s44(n1+1:n3,n0+1:n1,n2+1:n3,n2+1:n3))
          i1=k4*k4
          i2=k1*k3
          i3=k3*k1
          call egemm(i1,i2,i3,d1,d2,s44)
          deallocate(d1)
          deallocate(d2)

          factor=t2diag3
          call sum3412(n2,n3,n2,n3,n1,n3,n0,n1,x6,s44,factor)
          deallocate(s44)
        endif

        if(t3diag1.ne.0) then
          allocate(d1(n0+1:n1,n1+1:n3,n0+1:n1,n1+1:n3))                 !ilias: commented out 16 lines for 3cc
          if(t3diag2.eq.0)then
            call reorder1324(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n0,n1,n1,n3,intm,d1)
          else
            call reorder1324(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n0,n1,n1,n3,intr,d1)
          endif
          allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
          call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
          allocate(s45(n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3))
          i1=k3*k1
          i2=k1*k3
          i3=k3*k1
          call egemm(i1,i2,i3,d1,d2,s45)
          deallocate(d1)
          deallocate(d2)

          factor=t3diag1
          call sum3412(n0,n1,n1,n3,n1,n3,n0,n1,x12,s45,factor)
          deallocate(s45)
        endif

        if(t3diag4.ne.0) then
          allocate(d1(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
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

          factor=-0.500*t3diag4
          call sum21(n0,n1,n0,n1,x8,q17,factor)
          deallocate(q17)
        endif

        if(t3diag3.ne.0) then
          allocate(d1(n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3))                 !ilias: commented out 16 lines for 3cc
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

          factor=0.500*t3diag3
          call sum21(n1,n3,n1,n3,x9,q18,factor)
          deallocate(q18)
        endif

        if(t3diag1.ne.0) then
          allocate(d1(n0+1:n1,n1+1:n3,n0+1:n2,n2+1:n3))                 !ilias: commented out 16 lines for 3cc
          call reorder1324(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n0,n2,n2,n3,intm,d1)
          allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
          call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
          allocate(s46(n1+1:n3,n0+1:n1,n0+1:n2,n2+1:n3))
          i1=k4*k2
          i2=k1*k3
          i3=k3*k1
          call egemm(i1,i2,i3,d1,d2,s46)
          deallocate(d1)
          deallocate(d2)

          factor=t3diag1
          call sum3412(n0,n2,n2,n3,n1,n3,n0,n1,x20,s46,factor)
          deallocate(s46)
        endif

        allocate(b1(n1+1:n3,n0+1:n1))
        call reorder12(0,n3,0,n3,
     & n1,n3,n0,n1,fockr,b1)
        allocate(d2(n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
        call reorder2134(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n2,n3,n0,n2,n0,n1,t2b,d2)
        allocate(s47(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
        i1=k1
        i2=k1*k2*k4
        i3=k3
        call egemm(i1,i2,i3,b1,d2,s47)
        deallocate(b1)
        deallocate(d2)

        call sum2341(n0,n1,n2,n3,n0,n2,n0,n1,x1,s47, 1.000)
        deallocate(s47)

        if(t2diag4.ne.0) then
          allocate(d1(n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
          call reorder2413(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n0,n1,n0,n1,intr,d1)
          allocate(d2(n0+1:n1,n1+1:n3,n2+1:n3,n0+1:n2))
          call reorder4213(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n1,n3,n2,n3,n0,n2,t2b,d2)
          allocate(s48(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
          i1=k1*k1
          i2=k2*k4
          i3=k3*k1
          call egemm(i1,i2,i3,d1,d2,s48)
          deallocate(d1)
          deallocate(d2)

          factor=t2diag4
          call sum2314(n0,n1,n2,n3,n0,n2,n0,n1,x1,s48,factor)
          deallocate(s48)
        endif

        if(t2diag3.ne.0) then
          allocate(d1(n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3))                 !ilias: commented out 16 lines for 3cc
          call reorder2431(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n1,n3,n1,n3,intr,d1)
          allocate(d2(n0+1:n1,n1+1:n3,n2+1:n3,n0+1:n2))
          call reorder4213(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n1,n3,n2,n3,n0,n2,t2b,d2)
          allocate(s49(n2+1:n3,n0+1:n2,n1+1:n3,n1+1:n3))
          i1=k3*k3
          i2=k2*k4
          i3=k3*k1
          call egemm(i1,i2,i3,d1,d2,s49)
          deallocate(d1)
          deallocate(d2)

          factor=t2diag3
          call sum2413(n1,n3,n2,n3,n1,n3,n0,n2,x2,s49,factor)
          deallocate(s49)
        endif

        if(t2diag5.ne.0) then
          allocate(d1(n0+1:n2,n0+1:n1,n1+1:n3,n0+1:n2))
          call reorder2134(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n0,n1,n1,n3,n0,n2,intm,d1)
          allocate(d2(n0+1:n2,n0+1:n1,n2+1:n3,n1+1:n3))
          call reorder3412(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n0,n1,n2,n3,n1,n3,t2b,d2)
          allocate(s50(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2))
          i1=k2*k3
          i2=k3*k4
          i3=k1*k2
          call egemm(i1,i2,i3,d1,d2,s50)
          deallocate(d1)
          deallocate(d2)

          factor=t2diag5
          call sum2314(n1,n3,n2,n3,n1,n3,n0,n2,x2,s50,factor)
          deallocate(s50)
        endif

        if(t2diag4.ne.0) then
          allocate(d1(n0+1:n1,n1+1:n3,n0+1:n2,n0+1:n2))
          call reorder1324(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n0,n2,n0,n2,intm,d1)
          allocate(d2(n0+1:n1,n1+1:n3,n2+1:n3,n0+1:n2))
          call reorder4213(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n1,n3,n2,n3,n0,n2,t2b,d2)
          allocate(s51(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
          i1=k2*k2
          i2=k2*k4
          i3=k3*k1
          call egemm(i1,i2,i3,d1,d2,s51)
          deallocate(d1)
          deallocate(d2)

          factor=t2diag4
          call sum2314(n0,n2,n2,n3,n0,n2,n0,n2,x34,s51,factor)
          deallocate(s51)
        endif


        do i=n0+1,n1;do j=n0+1,n2-1;do k=j+1,n2
          if(indocc(k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-1;do c=b+1,n3
            if(indunocc(c,b,a).eq.1)cycle
            sum=0.0d0
            do n=n0+1,n2
              sum=sum
     &     + x34(n,b,k,j)*t2b(c,a,n,i)     !bkjcai    (+1.000)
     &     - x34(n,c,k,j)*t2b(b,a,n,i)     !ckjbai    (-1.000)
     &     - x34(n,b,j,k)*t2b(c,a,n,i)     !bjkcai    (-1.000)
     &     + x34(n,c,j,k)*t2b(b,a,n,i)     !cjkbai    (+1.000)
            enddo
            v3c(c,b,a,k,j,i)=v3c(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
        enddo;enddo;enddo

        deallocate(x34)

        if(t2diag4.ne.0) then
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

          factor=-t2diag4
          call sum2413(n0,n1,n2,n3,n0,n2,n0,n1,x1,s52,factor)
          deallocate(s52)
        endif

        if(t2diag3.ne.0) then
          allocate(d1(n0+1:n1,n2+1:n3,n1+1:n3,n2+1:n3))                 !ilias: commented out 16 lines for 3cc
          call reorder1432(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n2,n3,n1,n3,n2,n3,intm,d1)
          allocate(d2(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
          call reorder4123(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n2,n3,n1,n3,n0,n2,t2b,d2)
          allocate(s53(n1+1:n3,n0+1:n2,n1+1:n3,n2+1:n3))
          i1=k4*k3
          i2=k2*k3
          i3=k4*k1
          call egemm(i1,i2,i3,d1,d2,s53)
          deallocate(d1)
          deallocate(d2)

          factor=-t2diag3
          call sum3412(n1,n3,n2,n3,n1,n3,n0,n2,x2,s53,factor)
          deallocate(s53)

          allocate(d1(n0+1:n1,n1+1:n3,n2+1:n3,n2+1:n3))                 !ilias: commented out 16 lines for 3cc
          call reorder1342(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n2,n3,n2,n3,intm,d1)
          allocate(d2(n0+1:n1,n1+1:n3,n2+1:n3,n0+1:n2))
          call reorder4213(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n1,n3,n2,n3,n0,n2,t2b,d2)
          allocate(s54(n2+1:n3,n0+1:n2,n2+1:n3,n2+1:n3))
          i1=k4*k4
          i2=k2*k4
          i3=k3*k1
          call egemm(i1,i2,i3,d1,d2,s54)
          deallocate(d1)
          deallocate(d2)

          call sum3412(n2,n3,n2,n3,n2,n3,n0,n2,x35,s54,factor)
          deallocate(s54)

        endif

        do i=n0+1,n1;do j=n0+1,n2-1;do k=j+1,n2
          if(indocc(k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-1;do c=b+1,n3
            if(indunocc(c,b,a).eq.1)cycle
            sum=0.0d0
            do f=n2+1,n3
              sum=sum
     &     + x35(f,c,b,k)*t2b(f,a,j,i)     !cbkaji (+1.000)
     &     - x35(f,b,c,k)*t2b(f,a,j,i)     !bckaji (-1.000)
     &     - x35(f,c,b,j)*t2b(f,a,k,i)     !cbjaki (-1.000)
     &     + x35(f,b,c,j)*t2b(f,a,k,i)     !bcjaki (+1.000)
            enddo
            v3c(c,b,a,k,j,i)=v3c(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
        enddo;enddo;enddo

        deallocate(x35)

        if(t2diag5.ne.0) then
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

          factor=t2diag5
          call sum3412(n0,n1,n2,n3,n0,n2,n0,n1,x1,s55,factor)
          deallocate(s55)
        endif

        allocate(b1(n2+1:n3,n0+1:n2))
        call reorder12(0,n3,0,n3,
     & n2,n3,n0,n2,fockb,b1)
        allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
        call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
        allocate(s56(n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
        i1=k2
        i2=k1*k2*k3
        i3=k4
        call egemm(i1,i2,i3,b1,d2,s56)
        deallocate(b1)
        deallocate(d2)

        call sum2341(n0,n2,n1,n3,n0,n2,n0,n1,x5,s56, 1.000)
        deallocate(s56)

        allocate(b1(n2+1:n3,n0+1:n2))
        call reorder12(0,n3,0,n3,
     & n2,n3,n0,n2,fockb,b1)
        allocate(d2(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
        call reorder1234(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n2,n3,n0,n2,n0,n2,t2c,d2)
        allocate(s57(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
        i1=k2
        i2=k2*k2*k4
        i3=k4
        call egemm(i1,i2,i3,b1,d2,s57)
        deallocate(b1)
        deallocate(d2)

        call sum2341(n0,n2,n2,n3,n0,n2,n0,n2,x3,s57, 1.000)
        deallocate(s57)

        if(t2diag4.ne.0) then
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

          factor=t2diag4
          call sum2314(n0,n1,n2,n3,n0,n2,n0,n1,x1,s58,factor)
          deallocate(s58)

          allocate(d1(n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n1))
          call reorder1423(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n2,n3,n0,n2,n0,n1,intm,d1)
          allocate(d2(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
          call reorder4123(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n2,n3,n1,n3,n0,n2,t2b,d2)
          allocate(s59(n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
          i1=k1*k2
          i2=k2*k3
          i3=k4*k1
          call egemm(i1,i2,i3,d1,d2,s59)
          deallocate(d1)
          deallocate(d2)

          factor=-t2diag4
          call sum2314(n0,n2,n1,n3,n0,n2,n0,n1,x5,s59,factor)
          deallocate(s59)
        endif

        if(t2diag5.ne.0) then
          allocate(d1(n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n1))
          call reorder2143(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n0,n1,n2,n3,n0,n1,intm,d1)
          allocate(d2(n0+1:n2,n0+1:n1,n2+1:n3,n1+1:n3))
          call reorder3412(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n0,n1,n2,n3,n1,n3,t2b,d2)
          allocate(s60(n2+1:n3,n1+1:n3,n2+1:n3,n0+1:n1))
          i1=k1*k4
          i2=k3*k4
          i3=k1*k2
          call egemm(i1,i2,i3,d1,d2,s60)
          deallocate(d1)
          deallocate(d2)

          factor=t2diag5
          call sum2314(n2,n3,n2,n3,n1,n3,n0,n1,x6,s60,factor)
          deallocate(s60)
        endif

        if(t2diag3.ne.0) then
          allocate(d1(n0+1:n2,n2+1:n3,n1+1:n3,n1+1:n3))                 !ilias: commented out 16 lines for 3cc
          call reorder2431(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n1,n3,n1,n3,intm,d1)
          allocate(d2(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
          call reorder3124(n2,n3,n2,n3,n0,n2,n0,n2,
     & n0,n2,n2,n3,n2,n3,n0,n2,t2c,d2)
          allocate(s61(n2+1:n3,n0+1:n2,n1+1:n3,n1+1:n3))
          i1=k3*k3
          i2=k2*k4
          i3=k4*k2
          call egemm(i1,i2,i3,d1,d2,s61)
          deallocate(d1)
          deallocate(d2)

          factor=t2diag3
          call sum2413(n1,n3,n2,n3,n1,n3,n0,n2,x2,s61,factor)
          deallocate(s61)
        endif

        if(t2diag5.ne.0) then
          allocate(d1(n2+1:n3,n1+1:n3,n0+1:n2,n1+1:n3))
          call reorder4321(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n0,n2,n1,n3,intm,d1)
          allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
          call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
          allocate(s62(n0+1:n2,n0+1:n1,n0+1:n2,n1+1:n3))
          i1=k3*k2
          i2=k1*k2
          i3=k3*k4
          call egemm(i1,i2,i3,d1,d2,s62)
          deallocate(d1)
          deallocate(d2)

          factor=t2diag5
          call sum3412(n0,n2,n1,n3,n0,n2,n0,n1,x5,s62,factor)
          deallocate(s62)
        endif

        if(t2diag3.ne.0) then
          allocate(d1(n0+1:n2,n1+1:n3,n2+1:n3,n1+1:n3))                 !ilias: commented out 16 lines for 3cc
          call reorder2341(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n1,n3,n2,n3,n1,n3,intm,d1)
          allocate(d2(n0+1:n2,n1+1:n3,n2+1:n3,n0+1:n1))
          call reorder3214(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n1,n3,n2,n3,n0,n1,t2b,d2)
          allocate(s63(n2+1:n3,n0+1:n1,n2+1:n3,n1+1:n3))
          i1=k3*k4
          i2=k1*k4
          i3=k3*k2
          call egemm(i1,i2,i3,d1,d2,s63)
          deallocate(d1)
          deallocate(d2)

          factor=-t2diag3
          call sum2413(n2,n3,n2,n3,n1,n3,n0,n1,x6,s63,factor)
          deallocate(s63)
        endif

        if(t2diag5.ne.0) then
          allocate(d1(n0+1:n2,n0+1:n2,n2+1:n3,n0+1:n2))
          call reorder2143(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n0,n2,n2,n3,n0,n2,intb,d1)
          allocate(d2(n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3))
          call reorder3412(n2,n3,n2,n3,n0,n2,n0,n2,
     & n0,n2,n0,n2,n2,n3,n2,n3,t2c,d2)
          allocate(s64(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
          i1=k2*k4
          i2=k4*k4
          i3=k2*k2
          call egemm(i1,i2,i3,d1,d2,s64)
          deallocate(d1)
          deallocate(d2)

          factor=0.500*t2diag5
          call sum2314(n2,n3,n2,n3,n2,n3,n0,n2,x4,s64,factor)
          deallocate(s64)
        endif

        if(t2diag4.ne.0) then
          allocate(d1(n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2))
          call reorder2413(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n0,n2,n0,n2,intb,d1)
          allocate(d2(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
          call reorder3124(n2,n3,n2,n3,n0,n2,n0,n2,
     & n0,n2,n2,n3,n2,n3,n0,n2,t2c,d2)
          allocate(s65(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
          i1=k2*k2
          i2=k2*k4
          i3=k4*k2
          call egemm(i1,i2,i3,d1,d2,s65)
          deallocate(d1)
          deallocate(d2)

          factor=-t2diag4
          call sum2314(n0,n2,n2,n3,n0,n2,n0,n2,x32,s65,factor)
          deallocate(s65)

        endif

        do i=n0+1,n1;do j=n0+1,n2-1;do k=j+1,n2
          if(indocc(k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-1;do c=b+1,n3
            if(indunocc(c,b,a).eq.1)cycle
            sum=0.0d0
            do m=n0+1,n2
              sum=sum
     &     + x32(m,c,k,j)*t2b(b,a,m,i)     !ckjbai (+1.000)
     &     - x32(m,b,k,j)*t2b(c,a,m,i)     !bkjcai (-1.000)
     &     - x32(m,c,j,k)*t2b(b,a,m,i)     !cjkbai (-1.000)
     &     + x32(m,b,j,k)*t2b(c,a,m,i)     !bjkcai (+1.000)
            enddo
            v3c(c,b,a,k,j,i)=v3c(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
        enddo;enddo;enddo

        deallocate(x32)

        if(t2diag4.ne.0) then
          allocate(d1(n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2))
          call reorder1423(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n0,n2,n0,n2,intb,d1)
          allocate(d2(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
          call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n2,n3,n1,n3,n0,n1,t2b,d2)
          allocate(s66(n1+1:n3,n0+1:n1,n0+1:n2,n0+1:n2))
          i1=k2*k2
          i2=k1*k3
          i3=k4*k2
          call egemm(i1,i2,i3,d1,d2,s66)
          deallocate(d1)
          deallocate(d2)

          factor=-t2diag4
          call sum2413(n0,n2,n1,n3,n0,n2,n0,n1,x5,s66,factor)
          deallocate(s66)
        endif

        if(t2diag3.ne.0) then
          allocate(d1(n0+1:n2,n2+1:n3,n2+1:n3,n2+1:n3))                 !ilias: commented out 16 lines for 3cc
          call reorder2431(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n2,n3,n2,n3,intb,d1)
          allocate(d2(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
          call reorder3124(n2,n3,n2,n3,n0,n2,n0,n2,
     & n0,n2,n2,n3,n2,n3,n0,n2,t2c,d2)
          allocate(s67(n2+1:n3,n0+1:n2,n2+1:n3,n2+1:n3))
          i1=k4*k4
          i2=k2*k4
          i3=k4*k2
          call egemm(i1,i2,i3,d1,d2,s67)
          deallocate(d1)
          deallocate(d2)

          factor=t2diag3
          call sum3412(n2,n3,n2,n3,n2,n3,n0,n2,x33,s67,factor)
          deallocate(s67)


        endif
        do i=n0+1,n1;do j=n0+1,n2-1;do k=j+1,n2
          if(indocc(k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-1;do c=b+1,n3
            if(indunocc(c,b,a).eq.1)cycle
            sum=0.0d0
            do e=n2+1,n3
              sum=sum
     &     + x33(e,c,b,j)*t2b(e,a,k,i)     !ecbjeaki    (+1.000)
     &     - x33(e,b,c,j)*t2b(e,a,k,i)     !ebcjeaki    (-1.000)
     &     - x33(e,c,b,k)*t2b(e,a,j,i)     !ecbkeaji    (-1.000)
     &     + x33(e,b,c,k)*t2b(e,a,j,i)     !ebckeaji    (+1.000)
            enddo
            v3c(c,b,a,k,j,i)=v3c(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
        enddo;enddo;enddo

        deallocate(x33)

        if(t2diag5.ne.0) then
          allocate(d1(n2+1:n3,n2+1:n3,n0+1:n2,n2+1:n3))
          call reorder4321(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n2,n3,n0,n2,n2,n3,intb,d1)
          allocate(d2(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
          call reorder1234(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n2,n3,n0,n2,n0,n2,t2c,d2)
          allocate(s68(n0+1:n2,n0+1:n2,n0+1:n2,n2+1:n3))
          i1=k4*k2
          i2=k2*k2
          i3=k4*k4
          call egemm(i1,i2,i3,d1,d2,s68)
          deallocate(d1)
          deallocate(d2)

          factor=0.500*t2diag5
          call sum3412(n0,n2,n2,n3,n0,n2,n0,n2,x3,s68,factor)
          deallocate(s68)
        endif

        if(t2diag3.ne.0) then
          allocate(d1(n0+1:n2,n2+1:n3,n2+1:n3,n2+1:n3))                 !ilias: commented out 16 lines for 3cc
          call reorder2341(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n2,n3,n2,n3,intb,d1)
          allocate(d2(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
          call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n2,n3,n1,n3,n0,n1,t2b,d2)
          allocate(s69(n1+1:n3,n0+1:n1,n2+1:n3,n2+1:n3))
          i1=k4*k4
          i2=k1*k3
          i3=k4*k2
          call egemm(i1,i2,i3,d1,d2,s69)
          deallocate(d1)
          deallocate(d2)

          factor=-t2diag3
          call sum3412(n2,n3,n2,n3,n1,n3,n0,n1,x6,s69,factor)
          deallocate(s69)
        endif

        if(t3diag3.ne.0) then
          allocate(d1(n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3))                 !ilias: commented out 16 lines for 3cc
          call reorder2143(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n0,n1,n1,n3,n1,n3,intr,d1)
          allocate(f2(n0+1:n1,n0+1:n1,n1+1:n3,n2+1:n3,n1+1:n3,n0+1:n2))
          call reorder562134(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n0,n1,n0,n1,n1,n3,n2,n3,n1,n3,n0,n2,t3b,f2)
          allocate(s70(n2+1:n3,n1+1:n3,n0+1:n2,n1+1:n3))
          i1=k3
          i2=k2*k3*k4
          i3=k3*k1*k1
          call egemm(i1,i2,i3,d1,f2,s70)
          deallocate(d1)
          deallocate(f2)

          factor=-0.500*t3diag3
          call sum2341(n1,n3,n2,n3,n1,n3,n0,n2,x2,s70,factor)
          deallocate(s70)
        endif

        if(t3diag4.ne.0) then
          allocate(d1(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
          call reorder2431(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n1,n3,n0,n1,intr,d1)
          allocate(f2(n0+1:n1,n1+1:n3,n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
          call reorder523146(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n2,n3,n0,n2,n0,n1,t3b,f2)
          allocate(s71(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
          i1=k1
          i2=k1*k2*k4
          i3=k3*k3*k1
          call egemm(i1,i2,i3,d1,f2,s71)
          deallocate(d1)
          deallocate(f2)

          factor=0.500*t3diag4
          call sum2341(n0,n1,n2,n3,n0,n2,n0,n1,x1,s71,factor)
          deallocate(s71)
        endif

        if(t3diag1.ne.0) then
          allocate(d1(n0+1:n1,n1+1:n3,n0+1:n1,n1+1:n3))                 !ilias: commented out 16 lines for 3cc
          if(t3diag2.eq.0)then
            call reorder1324(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n0,n1,n1,n3,intm,d1)
          else
            call reorder1324(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n0,n1,n1,n3,intr,d1)
          endif
          allocate(d2(n0+1:n1,n1+1:n3,n2+1:n3,n0+1:n2))
          call reorder4213(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n1,n3,n2,n3,n0,n2,t2b,d2)
          allocate(s72(n2+1:n3,n0+1:n2,n0+1:n1,n1+1:n3))
          i1=k3*k1
          i2=k2*k4
          i3=k3*k1
          call egemm(i1,i2,i3,d1,d2,s72)
          deallocate(d1)
          deallocate(d2)

          factor=t3diag1
          call sum3412(n0,n1,n1,n3,n2,n3,n0,n2,x7,s72,factor)
          deallocate(s72)
        endif

        if(t3diag3.ne.0) then
          allocate(d1(n0+1:n2,n0+1:n1,n2+1:n3,n1+1:n3))                 !ilias: commented out 16 lines for 3cc
          call reorder2143(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n0,n1,n2,n3,n1,n3,intm,d1)
          allocate(f2(n0+1:n2,n0+1:n1,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2))
          call reorder461235(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & n0,n2,n0,n1,n2,n3,n2,n3,n1,n3,n0,n2,t3c,f2)
          allocate(s73(n2+1:n3,n1+1:n3,n0+1:n2,n1+1:n3))
          i1=k3
          i2=k2*k3*k4
          i3=k4*k1*k2
          call egemm(i1,i2,i3,d1,f2,s73)
          deallocate(d1)
          deallocate(f2)

          factor=-t3diag3
          call sum2341(n1,n3,n2,n3,n1,n3,n0,n2,x2,s73,factor)
          deallocate(s73)
        endif

        call sumx3214(n0,n3,n1,n3,n2,n3,n1,n3,n0,n2,x2,intm, 1.000)


        do i=n0+1,n1;do j=n0+1,n2-1;do k=j+1,n2
          if(indocc(k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-1;do c=b+1,n3
            if(indunocc(c,b,a).eq.1)cycle
            sum=0.0d0
            do e=n1+1,n3
              sum=sum
     &     - x2(e,c,a,j)*t2b(b,e,k,i)     !cajbki (-1.000)
     &     + x2(e,b,a,j)*t2b(c,e,k,i)     !bajcki (+1.000)
     &     + x2(e,c,a,k)*t2b(b,e,j,i)     !cakbji (+1.000)
     &     - x2(e,b,a,k)*t2b(c,e,j,i)     !bakcji (-1.000)
            enddo
            v3c(c,b,a,k,j,i)=v3c(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
        enddo;enddo;enddo

        deallocate(x2)

        if(t3diag3.ne.0) then
          allocate(d1(n0+1:n2,n0+1:n1,n1+1:n3,n2+1:n3))                 !ilias: commented out 13 lines for 3cc
          call reorder2134(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n0,n1,n1,n3,n2,n3,intm,d1)
          allocate(f2(n0+1:n2,n0+1:n1,n1+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
          call reorder463125(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & n0,n2,n0,n1,n1,n3,n2,n3,n2,n3,n0,n2,t3c,f2)
          allocate(s74(n2+1:n3,n2+1:n3,n0+1:n2,n2+1:n3))
          i1=k4
          i2=k2*k4*k4
          i3=k3*k1*k2
          call egemm(i1,i2,i3,d1,f2,s74)
          deallocate(d1)
          deallocate(f2)


          do i=n0+1,n1;do j=n0+1,n2-1;do k=j+1,n2                       !ilias: commented out 13 lines for 3cc
            if(indocc(k,j,i).eq.1)cycle
            do a=n1+1,n3;do b=n2+1,n3-1;do c=b+1,n3
              if(indunocc(c,b,a).eq.1)cycle
              sum=0.0d0
              do f=n2+1,n3
                sum=sum
     &     + s74(c,b,k,f)*t2b(f,a,j,i)     !cbkaji (+1.000)
     &     - s74(c,b,j,f)*t2b(f,a,k,i)     !cbjaki (-1.000)
              enddo
              v3c(c,b,a,k,j,i)=v3c(c,b,a,k,j,i)+t3diag3*sum
            enddo;enddo;enddo
          enddo;enddo;enddo

          deallocate(s74)                                               !ilias: commented out this line for 3cc
        endif

        if(t3diag5.ne.0) then
          allocate(d1(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
          call reorder4321(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n0,n2,n0,n1,intm,d1)
          allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
          call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
          allocate(s75(n0+1:n2,n0+1:n1,n0+1:n2,n0+1:n1))
          i1=k1*k2
          i2=k1*k2
          i3=k3*k4
          call egemm(i1,i2,i3,d1,d2,s75)
          deallocate(d1)
          deallocate(d2)

          factor=t3diag5
          call sum3412(n0,n2,n0,n1,n0,n2,n0,n1,x13,s75,factor)
          deallocate(s75)

        endif
        call sumx2143(n0,n3,n0,n2,n0,n1,n0,n2,n0,n1,x13,intm, 1.000)


        do i=n0+1,n1;do j=n0+1,n2-1;do k=j+1,n2
          if(indocc(k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-1;do c=b+1,n3
            if(indunocc(c,b,a).eq.1)cycle
            sum=0.0d0
            do m=n0+1,n1;do n=n0+1,n2
              sum=sum
     &     - x13(n,m,j,i)*t3c(c,b,a,n,k,m)   !jicbak (-1.000)
     &     + x13(n,m,k,i)*t3c(c,b,a,n,j,m)   !kicbaj (+1.000)
            enddo;enddo
            v3c(c,b,a,k,j,i)=v3c(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
        enddo;enddo;enddo

        deallocate(x13)

        if(t3diag4.ne.0) then
          allocate(d1(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
          call reorder1432(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n2,n3,n1,n3,n0,n2,intm,d1)
          allocate(f2(n0+1:n1,n2+1:n3,n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
          call reorder613245(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & n0,n1,n2,n3,n1,n3,n2,n3,n0,n2,n0,n2,t3c,f2)
          allocate(s76(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
          i1=k2
          i2=k2*k2*k4
          i3=k3*k4*k1
          call egemm(i1,i2,i3,d1,f2,s76)
          deallocate(d1)
          deallocate(f2)

          factor=t3diag4
          call sum2341(n0,n2,n2,n3,n0,n2,n0,n2,x31,s76,factor)
          deallocate(s76)

        endif

        do i=n0+1,n1;do j=n0+1,n2-1;do k=j+1,n2
          if(indocc(k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-1;do c=b+1,n3
            if(indunocc(c,b,a).eq.1)cycle
            sum=0.0d0
            do n=n0+1,n2
              sum=sum
     &     - x31(n,b,k,j)*t2b(c,a,n,i)     !bkjcai (-1.000)
     &     + x31(n,c,k,j)*t2b(b,a,n,i)     !ckjbai (+1.000)
            enddo
            v3c(c,b,a,k,j,i)=v3c(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
        enddo;enddo;enddo

        deallocate(x31)

        if(t3diag2.ne.0) then
          allocate(d1(n0+1:n2,n1+1:n3,n0+1:n1,n2+1:n3))                 !ilias: commented out 16 lines for 3cc
          call reorder2314(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n1,n3,n0,n1,n2,n3,intm,d1)
          allocate(d2(n0+1:n2,n1+1:n3,n2+1:n3,n0+1:n1))
          call reorder3214(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n1,n3,n2,n3,n0,n1,t2b,d2)
          allocate(s77(n2+1:n3,n0+1:n1,n0+1:n1,n2+1:n3))
          i1=k4*k1
          i2=k1*k4
          i3=k3*k2
          call egemm(i1,i2,i3,d1,d2,s77)
          deallocate(d1)
          deallocate(d2)

          factor=-t3diag2
          call sum3412(n0,n1,n2,n3,n2,n3,n0,n1,x14,s77,factor)
          deallocate(s77)
        endif

        call sumx1342(n0,n3,n0,n1,n2,n3,n2,n3,n0,n1,x14,intm, 1.000)


        do i=n0+1,n1;do j=n0+1,n2-1;do k=j+1,n2
          if(indocc(k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-1;do c=b+1,n3
            if(indunocc(c,b,a).eq.1)cycle
            sum=0.0d0
            do e=n2+1,n3;do m=n0+1,n1
              sum=sum
     &     - x14(m,e,c,i)*t3c(e,b,a,k,j,m)   !cibakj (-1.000)
     &     + x14(m,e,b,i)*t3c(e,c,a,k,j,m)   !bicakj (+1.000)
            enddo;enddo
            v3c(c,b,a,k,j,i)=v3c(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
        enddo;enddo;enddo

        deallocate(x14)

        if(t3diag1.ne.0) then
          allocate(d1(n0+1:n2,n2+1:n3,n0+1:n1,n1+1:n3))                 !ilias: commented out 16 lines for 3cc
          call reorder2413(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n0,n1,n1,n3,intm,d1)
          allocate(d2(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
          call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n2,n3,n1,n3,n0,n1,t2b,d2)
          allocate(s78(n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3))
          i1=k3*k1
          i2=k1*k3
          i3=k4*k2
          call egemm(i1,i2,i3,d1,d2,s78)
          deallocate(d1)
          deallocate(d2)

          factor=t3diag1
          call sum3412(n0,n1,n1,n3,n1,n3,n0,n1,x12,s78,factor)
          deallocate(s78)
        endif

        call sumx3142(n0,n3,n0,n1,n1,n3,n1,n3,n0,n1,x12,intr, 1.000)


        do i=n0+1,n1;do j=n0+1,n2-1;do k=j+1,n2
          if(indocc(k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-1;do c=b+1,n3
            if(indunocc(c,b,a).eq.1)cycle
            sum=0.0d0
            do e=n1+1,n3;do m=n0+1,n1
              sum=sum
     &     + x12(m,e,a,i)*t3c(c,b,e,k,j,m) !aicbkj (+1.000) 
            enddo;enddo
            v3c(c,b,a,k,j,i)=v3c(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
        enddo;enddo;enddo

        deallocate(x12)

        if(t3diag4.ne.0) then
          allocate(d1(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
          call reorder2431(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n1,n3,n0,n1,intm,d1)
          allocate(d2(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
          call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n2,n3,n1,n3,n0,n1,t2b,d2)
          allocate(q19(n0+1:n1,n0+1:n1))
          i1=k1
          i2=k1
          i3=k3*k4*k2
          call egemm(i1,i2,i3,d1,d2,q19)
          deallocate(d1)
          deallocate(d2)

          factor=t3diag4
          call sum21(n0,n1,n0,n1,x8,q19,factor)
          deallocate(q19)
        endif

        call sumx12(0,n3,n0,n1,n0,n1,x8,fockr, 1.000)


        do i=n0+1,n1;do j=n0+1,n2-1;do k=j+1,n2
          if(indocc(k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-1;do c=b+1,n3
            if(indunocc(c,b,a).eq.1)cycle
            sum=0.0d0
            do m=n0+1,n1
              sum=sum
     &     - x8(m,i)*t3c(c,b,a,k,j,m)     !icbakj (-1.000)
            enddo
            v3c(c,b,a,k,j,i)=v3c(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
        enddo;enddo;enddo

        deallocate(x8)

        if(t3diag4.ne.0) then
          allocate(d1(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
          call reorder2431(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n1,n3,n0,n1,intm,d1)
          allocate(f2(n0+1:n2,n2+1:n3,n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
          call reorder413256(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & n0,n2,n2,n3,n1,n3,n2,n3,n0,n2,n0,n1,t3c,f2)
          allocate(s79(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
          i1=k1
          i2=k1*k2*k4
          i3=k3*k4*k2
          call egemm(i1,i2,i3,d1,f2,s79)
          deallocate(d1)
          deallocate(f2)

          factor=t3diag4
          call sum2341(n0,n1,n2,n3,n0,n2,n0,n1,x1,s79,factor)
          deallocate(s79)
        endif

        call sumx1243(n0,n3,n0,n1,n2,n3,n0,n2,n0,n1,x1,intm, 1.000)


        do i=n0+1,n1;do j=n0+1,n2-1;do k=j+1,n2
          if(indocc(k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-1;do c=b+1,n3
            if(indunocc(c,b,a).eq.1)cycle
            sum=0.0d0
            do m=n0+1,n1
              sum=sum
     &     + x1(m,c,j,i)*t2b(b,a,k,m)     !cjibak (+1.000)
     &     - x1(m,b,j,i)*t2b(c,a,k,m)     !bjicak (-1.000)
     &     - x1(m,c,k,i)*t2b(b,a,j,m)     !ckibaj (-1.000)
     &     + x1(m,b,k,i)*t2b(c,a,j,m)     !bkicaj (+1.000)
            enddo
            v3c(c,b,a,k,j,i)=v3c(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
        enddo;enddo;enddo

        deallocate(x1)

        if(t3diag1.ne.0) then
          allocate(d1(n0+1:n1,n1+1:n3,n0+1:n2,n2+1:n3))                 !ilias: commented out 16 lines for 3cc
          call reorder1324(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n0,n2,n2,n3,intm,d1)
          allocate(d2(n0+1:n1,n1+1:n3,n2+1:n3,n0+1:n2))
          call reorder4213(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n1,n3,n2,n3,n0,n2,t2b,d2)
          allocate(s80(n2+1:n3,n0+1:n2,n0+1:n2,n2+1:n3))
          i1=k4*k2
          i2=k2*k4
          i3=k3*k1
          call egemm(i1,i2,i3,d1,d2,s80)
          deallocate(d1)
          deallocate(d2)

          factor=t3diag1
          call sum3412(n0,n2,n2,n3,n2,n3,n0,n2,x18,s80,factor)
          deallocate(s80)
        endif

        if(t3diag2.ne.0) then
          allocate(d1(n0+1:n1,n2+1:n3,n0+1:n2,n1+1:n3))                 !ilias: commented out 16 lines for 3cc
          call reorder1423(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n2,n3,n0,n2,n1,n3,intm,d1)
          allocate(d2(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
          call reorder4123(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n2,n3,n1,n3,n0,n2,t2b,d2)
          allocate(s81(n1+1:n3,n0+1:n2,n0+1:n2,n1+1:n3))
          i1=k3*k2
          i2=k2*k3
          i3=k4*k1
          call egemm(i1,i2,i3,d1,d2,s81)
          deallocate(d1)
          deallocate(d2)

          factor=-t3diag2
          call sum3412(n0,n2,n1,n3,n1,n3,n0,n2,x15,s81,factor)
          deallocate(s81)
        endif

        call sumx3124(n0,n3,n0,n2,n1,n3,n1,n3,n0,n2,x15,intm, 1.000)


        do i=n0+1,n1;do j=n0+1,n2-1;do k=j+1,n2
          if(indocc(k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-1;do c=b+1,n3
            if(indunocc(c,b,a).eq.1)cycle
            sum=0.0d0
            do e=n1+1,n3;do m=n0+1,n2
              sum=sum
     &     + x15(m,e,a,j)*t3c(c,b,e,m,k,i)   !ajcbki (+1.000)
     &     - x15(m,e,a,k)*t3c(c,b,e,m,j,i)   !akcbji (-1.000)
            enddo;enddo
            v3c(c,b,a,k,j,i)=v3c(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
        enddo;enddo;enddo

        deallocate(x15)

        if(t3diag4.ne.0) then
          allocate(d1(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
          call reorder1432(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n2,n3,n1,n3,n0,n2,intm,d1)
          allocate(d2(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
          call reorder4123(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n2,n3,n1,n3,n0,n2,t2b,d2)
          allocate(q20(n0+1:n2,n0+1:n2))
          i1=k2
          i2=k2
          i3=k3*k4*k1
          call egemm(i1,i2,i3,d1,d2,q20)
          deallocate(d1)
          deallocate(d2)

          factor=t3diag4
          call sum21(n0,n2,n0,n2,x10,q20,factor)
          deallocate(q20)
        endif

        if(t3diag5.ne.0) then
          allocate(d1(n0+1:n2,n0+1:n1,n2+1:n3,n1+1:n3))
          call reorder2143(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n0,n1,n2,n3,n1,n3,intm,d1)
          allocate(d2(n0+1:n2,n0+1:n1,n2+1:n3,n1+1:n3))
          call reorder3412(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n0,n1,n2,n3,n1,n3,t2b,d2)
          allocate(s82(n2+1:n3,n1+1:n3,n2+1:n3,n1+1:n3))
          i1=k3*k4
          i2=k3*k4
          i3=k1*k2
          call egemm(i1,i2,i3,d1,d2,s82)
          deallocate(d1)
          deallocate(d2)

          factor=t3diag5
          call sum3412(n2,n3,n1,n3,n2,n3,n1,n3,x16,s82,factor)
          deallocate(s82)
        endif

        call sumx4321(n0,n3,n2,n3,n1,n3,n2,n3,n1,n3,x16,intm, 1.000)


        do i=n0+1,n1;do j=n0+1,n2-1;do k=j+1,n2
          if(indocc(k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-1;do c=b+1,n3
            if(indunocc(c,b,a).eq.1)cycle
            sum=0.0d0
            do e=n1+1,n3;do f=n2+1,n3
              sum=sum
     &     + x16(f,e,c,a)*t3c(f,b,e,k,j,i)   !cabkji (+1.000)
     &     - x16(f,e,b,a)*t3c(f,c,e,k,j,i)   !backji (-1.000)
            enddo;enddo
            v3c(c,b,a,k,j,i)=v3c(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
        enddo;enddo;enddo

        deallocate(x16)

        if(t3diag3.ne.0) then
          allocate(d1(n0+1:n2,n0+1:n1,n1+1:n3,n2+1:n3))                 !ilias: commented out 16 lines for 3cc
          call reorder2134(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n0,n1,n1,n3,n2,n3,intm,d1)
          allocate(d2(n0+1:n2,n0+1:n1,n1+1:n3,n2+1:n3))
          call reorder3421(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n0,n1,n1,n3,n2,n3,t2b,d2)
          allocate(q21(n2+1:n3,n2+1:n3))
          i1=k4
          i2=k4
          i3=k3*k1*k2
          call egemm(i1,i2,i3,d1,d2,q21)
          deallocate(d1)
          deallocate(d2)

          factor=-t3diag3
          call sum21(n2,n3,n2,n3,x11,q21,factor)
          deallocate(q21)

          allocate(d1(n0+1:n2,n0+1:n1,n2+1:n3,n1+1:n3))                 !ilias: commented out 16 lines for 3cc
          call reorder2143(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n0,n1,n2,n3,n1,n3,intm,d1)
          allocate(d2(n0+1:n2,n0+1:n1,n2+1:n3,n1+1:n3))
          call reorder3412(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n0,n1,n2,n3,n1,n3,t2b,d2)
          allocate(q22(n1+1:n3,n1+1:n3))
          i1=k3
          i2=k3
          i3=k4*k1*k2
          call egemm(i1,i2,i3,d1,d2,q22)
          deallocate(d1)
          deallocate(d2)

          call sum21(n1,n3,n1,n3,x9,q22,factor)
          deallocate(q22)
        endif

        call sumx21(0,n3,n1,n3,n1,n3,x9,fockr, 1.000)


        do i=n0+1,n1;do j=n0+1,n2-1;do k=j+1,n2
          if(indocc(k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-1;do c=b+1,n3
            if(indunocc(c,b,a).eq.1)cycle
            sum=0.0d0
            do e=n1+1,n3
              sum=sum
     &     + x9(e,a)*t3c(c,b,e,k,j,i) !acbkji (+1.000)
            enddo
            v3c(c,b,a,k,j,i)=v3c(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
        enddo;enddo;enddo

        deallocate(x9)

        if(t3diag3.ne.0) then
          allocate(d1(n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3))                 !ilias: commented out 16 lines for 3cc
          call reorder2143(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n0,n2,n2,n3,n2,n3,intb,d1)
          allocate(f2(n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
          call reorder451236(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,
     & n0,n2,n0,n2,n2,n3,n2,n3,n2,n3,n0,n2,t3d,f2)
          allocate(s83(n2+1:n3,n2+1:n3,n0+1:n2,n2+1:n3))
          i1=k4
          i2=k2*k4*k4
          i3=k4*k2*k2
          call egemm(i1,i2,i3,d1,f2,s83)
          deallocate(d1)
          deallocate(f2)

          factor=-0.500*t3diag3
          call sum2341(n2,n3,n2,n3,n2,n3,n0,n2,x4,s83,factor)
          deallocate(s83)
        endif

        call sumx3241(n0,n3,n2,n3,n2,n3,n2,n3,n0,n2,x4,intb, 1.000)


        do i=n0+1,n1;do j=n0+1,n2-1;do k=j+1,n2
          if(indocc(k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-1;do c=b+1,n3
            if(indunocc(c,b,a).eq.1)cycle
            sum=0.0d0
            do e=n2+1,n3
              sum=sum
     &     + x4(e,c,b,j)*t2b(e,a,k,i)     !cbjaki (+1.000)
     &     - x4(e,c,b,k)*t2b(e,a,j,i)     !cbkaji (-1.000)
            enddo
            v3c(c,b,a,k,j,i)=v3c(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
        enddo;enddo;enddo

        deallocate(x4)

        if(t3diag4.ne.0) then
          allocate(d1(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
          call reorder2431(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n2,n3,n0,n2,intb,d1)
          allocate(f2(n0+1:n2,n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
          call reorder412356(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,
     & n0,n2,n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,t3d,f2)
          allocate(s84(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
          i1=k2
          i2=k2*k2*k4
          i3=k4*k4*k2
          call egemm(i1,i2,i3,d1,f2,s84)
          deallocate(d1)
          deallocate(f2)

          factor=0.500*t3diag4
          call sum2341(n0,n2,n2,n3,n0,n2,n0,n2,x3,s84,factor)
          deallocate(s84)
        endif

        call sumx2143(n0,n3,n0,n2,n2,n3,n0,n2,n0,n2,x3,intb, 1.000)


        do i=n0+1,n1;do j=n0+1,n2-1;do k=j+1,n2
          if(indocc(k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-1;do c=b+1,n3
            if(indunocc(c,b,a).eq.1)cycle
            sum=0.0d0
            do m=n0+1,n2
              sum=sum
     &     + x3(m,c,k,j)*t2b(b,a,m,i)     !ckjbai (+1.000)
     &     - x3(m,b,k,j)*t2b(c,a,m,i)     !bkjcai (-1.000)
            enddo
            v3c(c,b,a,k,j,i)=v3c(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
        enddo;enddo;enddo

        deallocate(x3)

        if(t3diag1.ne.0) then
          allocate(d1(n0+1:n2,n2+1:n3,n0+1:n2,n2+1:n3))                 !ilias: commented out 16 lines for 3cc
          if(t3diag2.eq.0)then
            call reorder1324(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n0,n2,n2,n3,intm,d1)
          else
            call reorder1324(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n0,n2,n2,n3,intb,d1)
          endif
          allocate(d2(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
          call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n2,n3,n1,n3,n0,n1,t2b,d2)
          allocate(s85(n1+1:n3,n0+1:n1,n0+1:n2,n2+1:n3))
          i1=k4*k2
          i2=k1*k3
          i3=k4*k2
          call egemm(i1,i2,i3,d1,d2,s85)
          deallocate(d1)
          deallocate(d2)

          factor=t3diag1
          call sum3412(n0,n2,n2,n3,n1,n3,n0,n1,x20,s85,factor)
          deallocate(s85)
        endif

        call sumx3142(n0,n3,n0,n2,n2,n3,n1,n3,n0,n1,x20,intm, 1.000)


        do i=n0+1,n1;do j=n0+1,n2-1;do k=j+1,n2
          if(indocc(k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-1;do c=b+1,n3
            if(indunocc(c,b,a).eq.1)cycle
            sum=0.0d0
            do e=n2+1,n3;do m=n0+1,n2
              sum=sum
     &     + x20(m,e,a,i)*t3d(e,c,b,m,k,j) !aicbkj (+1.000)
            enddo;enddo
            v3c(c,b,a,k,j,i)=v3c(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
        enddo;enddo;enddo

        deallocate(x20)

        if(t3diag3.ne.0) then
          allocate(d1(n0+1:n2,n0+1:n1,n1+1:n3,n2+1:n3))                 !ilias: commented out 13 lines for 3cc
          call reorder2134(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n0,n1,n1,n3,n2,n3,intm,d1)
          allocate(f2(n0+1:n2,n0+1:n1,n1+1:n3,n2+1:n3,n1+1:n3,n0+1:n1))
          call reorder452136(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n0,n2,n0,n1,n1,n3,n2,n3,n1,n3,n0,n1,t3b,f2)
          allocate(s86(n2+1:n3,n1+1:n3,n0+1:n1,n2+1:n3))
          i1=k4
          i2=k1*k3*k4
          i3=k3*k1*k2
          call egemm(i1,i2,i3,d1,f2,s86)
          deallocate(d1)
          deallocate(f2)


          do i=n0+1,n1;do j=n0+1,n2-1;do k=j+1,n2                       !ilias: commented out 13 lines for 3cc
            if(indocc(k,j,i).eq.1)cycle
            do a=n1+1,n3;do b=n2+1,n3-1;do c=b+1,n3
              if(indunocc(c,b,a).eq.1)cycle
              sum=0.0d0
              do f=n2+1,n3
                sum=sum
     &     + s86(b,a,i,f)*t2c(f,c,k,j)     !baickj (+1.000)
     &     - s86(c,a,i,f)*t2c(f,b,k,j)     !caibkj (-1.000)
              enddo
              v3c(c,b,a,k,j,i)=v3c(c,b,a,k,j,i)+t3diag3*sum
            enddo;enddo;enddo
          enddo;enddo;enddo

          deallocate(s86)                                               !ilias: commented out this line for 3cc
        endif


        if(t3diag4.ne.0) then
          allocate(d1(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
          call reorder1432(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n2,n3,n1,n3,n0,n2,intm,d1)
          allocate(f2(n0+1:n1,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
          call reorder512346(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n0,n1,n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,t3b,f2)
          allocate(s87(n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
          i1=k2
          i2=k1*k2*k3
          i3=k3*k4*k1
          call egemm(i1,i2,i3,d1,f2,s87)
          deallocate(d1)
          deallocate(f2)


          do i=n0+1,n1;do j=n0+1,n2-1;do k=j+1,n2
            if(indocc(k,j,i).eq.1)cycle
            do a=n1+1,n3;do b=n2+1,n3-1;do c=b+1,n3
              if(indunocc(c,b,a).eq.1)cycle
              sum=0.0d0
              do n=n0+1,n2
                sum=sum
     &     - s87(a,k,i,n)*t2c(c,b,n,j)     !akicbj (-1.000)
     &     + s87(a,j,i,n)*t2c(c,b,n,k)     !ajicbk (+1.000)
              enddo
              v3c(c,b,a,k,j,i)=v3c(c,b,a,k,j,i)+t3diag4*sum
            enddo;enddo;enddo
          enddo;enddo;enddo

          deallocate(s87)
        endif

        if(t3diag1.ne.0) then
          allocate(d1(n0+1:n2,n2+1:n3,n0+1:n1,n1+1:n3))                 !ilias: commented out 16 lines for 3cc
          call reorder2413(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n0,n1,n1,n3,intm,d1)
          allocate(d2(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
          call reorder3124(n2,n3,n2,n3,n0,n2,n0,n2,
     & n0,n2,n2,n3,n2,n3,n0,n2,t2c,d2)
          allocate(s88(n2+1:n3,n0+1:n2,n0+1:n1,n1+1:n3))
          i1=k3*k1
          i2=k2*k4
          i3=k4*k2
          call egemm(i1,i2,i3,d1,d2,s88)
          deallocate(d1)
          deallocate(d2)

          factor=t3diag1
          call sum3412(n0,n1,n1,n3,n2,n3,n0,n2,x7,s88,factor)
          deallocate(s88)
        endif

        call sumx1324(n0,n3,n0,n1,n1,n3,n2,n3,n0,n2,x7,intm, 1.000)


        do i=n0+1,n1;do j=n0+1,n2-1;do k=j+1,n2
          if(indocc(k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-1;do c=b+1,n3
            if(indunocc(c,b,a).eq.1)cycle
            sum=0.0d0
            do e=n1+1,n3;do m=n0+1,n1
              sum=sum
     &     - x7(m,e,c,j)*t3b(b,e,a,k,m,i)   !cjbaki (-1.000)
     &     + x7(m,e,b,j)*t3b(c,e,a,k,m,i)   !bjcaki (+1.000)
     &     + x7(m,e,c,k)*t3b(b,e,a,j,m,i)   !ckbaji (+1.000)
     &     - x7(m,e,b,k)*t3b(c,e,a,j,m,i)   !bkcaji (-1.000)
            enddo;enddo
            v3c(c,b,a,k,j,i)=v3c(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
        enddo;enddo;enddo

        deallocate(x7)

        if(t3diag3.ne.0) then
          allocate(d1(n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3))                 !ilias: commented out 16 lines for 3cc
          call reorder2143(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n0,n2,n2,n3,n2,n3,intb,d1)
          allocate(f2(n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n1))
          call reorder451236(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & n0,n2,n0,n2,n2,n3,n2,n3,n1,n3,n0,n1,t3c,f2)
          allocate(s89(n2+1:n3,n1+1:n3,n0+1:n1,n2+1:n3))
          i1=k4
          i2=k1*k3*k4
          i3=k4*k2*k2
          call egemm(i1,i2,i3,d1,f2,s89)
          deallocate(d1)
          deallocate(f2)

          factor=-0.500*t3diag3
          call sum2341(n2,n3,n2,n3,n1,n3,n0,n1,x6,s89,factor)
          deallocate(s89)
        endif

        call sumx3241(n0,n3,n2,n3,n2,n3,n1,n3,n0,n1,x6,intm, 1.000)


        do i=n0+1,n1;do j=n0+1,n2-1;do k=j+1,n2
          if(indocc(k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-1;do c=b+1,n3
            if(indunocc(c,b,a).eq.1)cycle
            sum=0.0d0
            do e=n2+1,n3
              sum=sum
     &     + x6(e,c,a,i)*t2c(e,b,k,j)     !caibkj (+1.000)
     &     - x6(e,b,a,i)*t2c(e,c,k,j)     !baickj (-1.000)
            enddo
            v3c(c,b,a,k,j,i)=v3c(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
        enddo;enddo;enddo

        deallocate(x6)

        if(t3diag5.ne.0) then
          allocate(d1(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
          call reorder4321(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n2,n3,n0,n2,n0,n2,intb,d1)
          allocate(d2(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
          call reorder1234(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n2,n3,n0,n2,n0,n2,t2c,d2)
          allocate(s90(n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n2))
          i1=k2*k2
          i2=k2*k2
          i3=k4*k4
          call egemm(i1,i2,i3,d1,d2,s90)
          deallocate(d1)
          deallocate(d2)

          factor=0.500*t3diag5
          call sum3412(n0,n2,n0,n2,n0,n2,n0,n2,x17,s90,factor)
          deallocate(s90)
        endif

        call sumx2143(n0,n3,n0,n2,n0,n2,n0,n2,n0,n2,x17,intb, 1.000)


        do i=n0+1,n1;do j=n0+1,n2-1;do k=j+1,n2
          if(indocc(k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-1;do c=b+1,n3
            if(indunocc(c,b,a).eq.1)cycle
            sum=0.0d0
            do m=n0+1,n2;do n=n0+1,n2
              sum=sum
     &     + (x17(n,m,k,j)*t3c(c,b,a,n,m,i))/2.0d0!kjcbai(+0.500)
            enddo;enddo
            v3c(c,b,a,k,j,i)=v3c(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
        enddo;enddo;enddo

        deallocate(x17)

        if(t3diag4.ne.0) then
          allocate(d1(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
          call reorder2431(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n2,n3,n0,n2,intb,d1)
          allocate(f2(n0+1:n2,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
          call reorder412356(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & n0,n2,n2,n3,n2,n3,n1,n3,n0,n2,n0,n1,t3c,f2)
          allocate(s91(n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
          i1=k2
          i2=k1*k2*k3
          i3=k4*k4*k2
          call egemm(i1,i2,i3,d1,f2,s91)
          deallocate(d1)
          deallocate(f2)

          factor=0.500*t3diag4
          call sum2341(n0,n2,n1,n3,n0,n2,n0,n1,x5,s91,factor)
          deallocate(s91)
        endif

        call sumx2143(n0,n3,n0,n2,n1,n3,n0,n2,n0,n1,x5,intm, 1.000)


        do i=n0+1,n1;do j=n0+1,n2-1;do k=j+1,n2
          if(indocc(k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-1;do c=b+1,n3
            if(indunocc(c,b,a).eq.1)cycle
            sum=0.0d0
            do m=n0+1,n2
              sum=sum
     &     + x5(m,a,j,i)*t2c(c,b,m,k)     !ajicbk (+1.000)
     &     - x5(m,a,k,i)*t2c(c,b,m,j)     !akicbj (-1.000)
            enddo
            v3c(c,b,a,k,j,i)=v3c(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
        enddo;enddo;enddo

        deallocate(x5)

        if(t3diag1.ne.0) then
          allocate(d1(n0+1:n2,n2+1:n3,n0+1:n2,n2+1:n3))                 !ilias: commented out 16 lines for 3cc
          if(t3diag2.eq.0)then
            call reorder1324(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n0,n2,n2,n3,intm,d1)
          else
            call reorder1324(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n0,n2,n2,n3,intb,d1)
          endif
          allocate(d2(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
          call reorder3124(n2,n3,n2,n3,n0,n2,n0,n2,
     & n0,n2,n2,n3,n2,n3,n0,n2,t2c,d2)
          allocate(s92(n2+1:n3,n0+1:n2,n0+1:n2,n2+1:n3))
          i1=k4*k2
          i2=k2*k4
          i3=k4*k2
          call egemm(i1,i2,i3,d1,d2,s92)
          deallocate(d1)
          deallocate(d2)

          factor=t3diag1
          call sum3412(n0,n2,n2,n3,n2,n3,n0,n2,x18,s92,factor)
          deallocate(s92)
        endif

        call sumx3142(n0,n3,n0,n2,n2,n3,n2,n3,n0,n2,x18,intb, 1.000)


        do i=n0+1,n1;do j=n0+1,n2-1;do k=j+1,n2
          if(indocc(k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-1;do c=b+1,n3
            if(indunocc(c,b,a).eq.1)cycle
            sum=0.0d0
            do e=n2+1,n3;do m=n0+1,n2
              sum=sum
     &     - x18(m,e,c,j)*t3c(e,b,a,m,k,i)   !cjbaki (-1.000)
     &     + x18(m,e,b,j)*t3c(e,c,a,m,k,i)   !bjcaki (+1.000)
     &     + x18(m,e,c,k)*t3c(e,b,a,m,j,i)   !ckbaji (+1.000)
     &     - x18(m,e,b,k)*t3c(e,c,a,m,j,i)   !bkcaji (-1.000)
            enddo;enddo
            v3c(c,b,a,k,j,i)=v3c(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
        enddo;enddo;enddo

        deallocate(x18)

        if(t3diag4.ne.0) then
          allocate(d1(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
          call reorder1432(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n2,n3,n0,n2,intb,d1)
          allocate(d2(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
          call reorder3124(n2,n3,n2,n3,n0,n2,n0,n2,
     & n0,n2,n2,n3,n2,n3,n0,n2,t2c,d2)
          allocate(q23(n0+1:n2,n0+1:n2))
          i1=k2
          i2=k2
          i3=k4*k4*k2
          call egemm(i1,i2,i3,d1,d2,q23)
          deallocate(d1)
          deallocate(d2)

          factor=-0.500*t3diag4
          call sum21(n0,n2,n0,n2,x10,q23,factor)
          deallocate(q23)
        endif

        call sumx12(0,n3,n0,n2,n0,n2,x10,fockb, 1.000)


        do i=n0+1,n1;do j=n0+1,n2-1;do k=j+1,n2
          if(indocc(k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-1;do c=b+1,n3
            if(indunocc(c,b,a).eq.1)cycle
            sum=0.0d0
            do m=n0+1,n2
              sum=sum
     &     + x10(m,j)*t3c(c,b,a,m,k,i)     !jcbaki (+1.000)
     &     - x10(m,k)*t3c(c,b,a,m,j,i)     !kcbaji (-1.000)
            enddo
            v3c(c,b,a,k,j,i)=v3c(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
        enddo;enddo;enddo

        deallocate(x10)

        if(t3diag5.ne.0) then
          allocate(d1(n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3))
          call reorder2143(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n0,n2,n2,n3,n2,n3,intb,d1)
          allocate(d2(n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3))
          call reorder3412(n2,n3,n2,n3,n0,n2,n0,n2,
     & n0,n2,n0,n2,n2,n3,n2,n3,t2c,d2)
          allocate(s93(n2+1:n3,n2+1:n3,n2+1:n3,n2+1:n3))
          i1=k4*k4
          i2=k4*k4
          i3=k2*k2
          call egemm(i1,i2,i3,d1,d2,s93)
          deallocate(d1)
          deallocate(d2)

          factor=0.500*t3diag5
          call sum3412(n2,n3,n2,n3,n2,n3,n2,n3,x19,s93,factor)
          deallocate(s93)
        endif

        call sumx4321(n0,n3,n2,n3,n2,n3,n2,n3,n2,n3,x19,intb, 1.000)


        do i=n0+1,n1;do j=n0+1,n2-1;do k=j+1,n2
          if(indocc(k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-1;do c=b+1,n3
            if(indunocc(c,b,a).eq.1)cycle
            sum=0.0d0
            do e=n2+1,n3;do f=n2+1,n3
              sum=sum
     &     + (x19(f,e,c,b)*t3c(f,e,a,k,j,i))/2.0d0!cbakji (+0.500)
            enddo;enddo
            v3c(c,b,a,k,j,i)=v3c(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
        enddo;enddo;enddo

        deallocate(x19)

        if(t3diag3.ne.0) then
          allocate(d1(n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3))                 !ilias: commented out 16 lines for 3cc
          call reorder2134(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n0,n2,n2,n3,n2,n3,intb,d1)
          allocate(d2(n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3))
          call reorder3412(n2,n3,n2,n3,n0,n2,n0,n2,
     & n0,n2,n0,n2,n2,n3,n2,n3,t2c,d2)
          allocate(q24(n2+1:n3,n2+1:n3))
          i1=k4
          i2=k4
          i3=k4*k2*k2
          call egemm(i1,i2,i3,d1,d2,q24)
          deallocate(d1)
          deallocate(d2)

          factor=0.500*t3diag3
          call sum21(n2,n3,n2,n3,x11,q24,factor)
          deallocate(q24)
        endif

        call sumx21(0,n3,n2,n3,n2,n3,x11,fockb, 1.000)


        do i=n0+1,n1;do j=n0+1,n2-1;do k=j+1,n2
          if(indocc(k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-1;do c=b+1,n3
            if(indunocc(c,b,a).eq.1)cycle
            sum=0.0d0
            do e=n2+1,n3
              sum=sum
     &     + x11(e,c)*t3c(e,b,a,k,j,i)     !cbakji (+1.000)
     &     - x11(e,b)*t3c(e,c,a,k,j,i)     !bcakji (-1.000)
            enddo
            v3c(c,b,a,k,j,i)=v3c(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
        enddo;enddo;enddo

        deallocate(x11)

        do i=n0+1,n1
          do j=n0+1,n2-1
            do k=j+1,n2
              do a=n1+1,n3
                do b=n2+1,n3-1
                  do c=b+1,n3
                    coeleft=fockb(c,c)
     &          +fockb(b,b)
     &          +fockr(a,a)
     &          -fockb(k,k)
     &          -fockb(j,j)
     &          -fockr(i,i)
     &          +shift
                    t3c(c,b,a,k,j,i)=t3c(c,b,a,k,j,i)-v3c(c,b,a,k,j,i)/coeleft
                    t3c(c,b,a,j,k,i)=-t3c(c,b,a,k,j,i)
                    t3c(b,c,a,k,j,i)=-t3c(c,b,a,k,j,i)
                    t3c(b,c,a,j,k,i)= t3c(c,b,a,k,j,i)
                  enddo
                enddo
              enddo
            enddo
          enddo
        enddo

        end
