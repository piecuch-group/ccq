       subroutine t1A11_update(N0,N1,N2,N3,HT1A,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t1A,t1B,t2A,t2B,t2C,
     & t3A,t3B1,t3B2,t3B3,t3B4,t3C1,t3C2,t3C3,t3C4,t3D)
C
       integer a,b,c,e,f,g,h,i,j,k,m,n,o,p
       real*8 CoeLeft,shift,PP
       real*8 FAHH(N0+1:N1,N0+1:N1)
       real*8 FAHP(N1+1:N3,N0+1:N1)
       real*8 FAPP(N1+1:N3,N1+1:N3)
       real*8 FBHH(N0+1:N2,N0+1:N2)
       real*8 FBHP(N2+1:N3,N0+1:N2)
       real*8 FBPP(N2+1:N3,N2+1:N3)
       real*8 VAHHHH(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1)
       real*8 VAHHHP(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1)
       real*8 VAHHPP(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1)
       real*8 VAHPHP(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1)
       real*8 VAHPPP(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1)
       real*8 VBHHHH(N0+1:N2,N0+1:N1,N0+1:N2,N0+1:N1)
       real*8 VBHHHP(N2+1:N3,N0+1:N1,N0+1:N2,N0+1:N1)
       real*8 VBHHPH(N0+1:N2,N1+1:N3,N0+1:N2,N0+1:N1)
       real*8 VBHHPP(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1)
       real*8 VBHPHP(N2+1:N3,N0+1:N1,N2+1:N3,N0+1:N1)
       real*8 VBHPPH(N0+1:N2,N1+1:N3,N2+1:N3,N0+1:N1)
       real*8 VBPHPH(N0+1:N2,N1+1:N3,N0+1:N2,N1+1:N3)
       real*8 VBHPPP(N2+1:N3,N1+1:N3,N2+1:N3,N0+1:N1)
       real*8 VBPHPP(N2+1:N3,N1+1:N3,N0+1:N2,N1+1:N3)
       real*8 VCHHHH(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2)
       real*8 VCHHHP(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2)
       real*8 VCHHPP(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2)
       real*8 VCHPHP(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2)
       real*8 VCHPPP(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2)
       real*8 VAAPPP(N1+1:N3,N1+1:N3,N1+1:N3,N1+1:M2)
       real*8 VBAPPP(N2+1:N3,N1+1:N3,N2+1:N3,N1+1:M2)
       real*8 VBPAPP(N2+1:N3,N1+1:N3,N2+1:M2,N1+1:N3)
       real*8 VCAPPP(N2+1:N3,N2+1:N3,N2+1:N3,N2+1:M2)
       real*8 t1A(N1+1:N3,N0+1:N1)
       real*8 t1B(N2+1:N3,N0+1:N2)
       real*8 t2A(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1)
       real*8 t2B(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1)
       real*8 t2C(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2)
       real*8 t3A(N1+1:N3,N1+1:N3,N1+1:M2,N0+1:N1,N0+1:N1,M1+1:N1)
       real*8 t3B1(N2+1:N3,N1+1:N3,N1+1:M2,N0+1:N2,N0+1:N1,M1+1:N1)
       real*8 t3B2(N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N2,N0+1:M1,N0+1:M1)
       real*8 t3B3(N2+1:M2,M2+1:N3,M2+1:N3,N0+1:N2,N0+1:N1,M1+1:N1)
       real*8 t3B4(N2+1:N3,N1+1:N3,N1+1:M2,M1+1:N2,N0+1:M1,N0+1:M1)
       real*8 t3C1(N2+1:N3,N2+1:M2,N1+1:N3,N0+1:N2,M1+1:N2,N0+1:N1)
       real*8 t3C2(M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1)
       real*8 t3C3(M2+1:N3,M2+1:N3,N1+1:M2,N0+1:N2,M1+1:N2,N0+1:N1)
       real*8 t3C4(N2+1:N3,N2+1:M2,N1+1:N3,N0+1:M1,N0+1:M1,M1+1:N1)
       real*8 t3D(N2+1:N3,N2+1:N3,N2+1:M2,N0+1:N2,N0+1:N2,M1+1:N2)
       real*8 HT1A(N1+1:N3,N0+1:N1)
C
       real*8,allocatable::V1A(:,:)
       real*8,allocatable::B1(:,:)
       real*8,allocatable::B2(:,:)
       real*8,allocatable::C1(:,:,:)
       real*8,allocatable::C2(:,:,:)
       real*8,allocatable::D1(:,:,:,:)
       real*8,allocatable::D2(:,:,:,:)
       real*8,allocatable::F1(:,:,:,:,:,:)
       real*8,allocatable::F2(:,:,:,:,:,:)
       real*8,allocatable::Z1(:,:)
       real*8,allocatable::Z2(:,:)
       real*8,allocatable::Z3(:,:)
       real*8,allocatable::Z4(:,:)
       real*8,allocatable::Z5(:,:)
       real*8,allocatable::Z6(:,:)
       real*8,allocatable::Z7(:,:)
       real*8,allocatable::Z8(:,:)
       real*8,allocatable::Z9(:,:)
       real*8,allocatable::Z10(:,:)
       real*8,allocatable::Z11(:,:)
       real*8,allocatable::Z12(:,:)
       real*8,allocatable::Z13(:,:)
       real*8,allocatable::Z14(:,:)
       real*8,allocatable::Z15(:,:)
       real*8,allocatable::Z16(:,:)
       real*8,allocatable::Z17(:,:)
       real*8,allocatable::Z18(:,:)
       real*8,allocatable::Z19(:,:)
       real*8,allocatable::Z20(:,:)
       real*8,allocatable::Z21(:,:)
       real*8,allocatable::Z22(:,:)
       real*8,allocatable::Z23(:,:)
       real*8,allocatable::Z24(:,:)
       real*8,allocatable::Z25(:,:)
       real*8,allocatable::Z26(:,:)
       real*8,allocatable::Z27(:,:)
       real*8,allocatable::Z28(:,:)
       real*8,allocatable::Z29(:,:)
       real*8,allocatable::Z30(:,:)
       real*8,allocatable::Z31(:,:)
       real*8,allocatable::Z32(:,:)
       real*8,allocatable::Z33(:,:)
       real*8,allocatable::Z34(:,:)
C
       allocate(V1A(N1+1:M2,M1+1:N1))
       V1A=0.0d0
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N1,M2-N1/),'3412',VAHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,N0-N0,M2-N1,M2-N1,N1-N1,M1-M1/),'451236',t3A,F2)
       allocate(Z1(N1+1:M2,M1+1:N1))
       I2=K7*K9
       I3=K6*K6*K5*K5
       call EGEMM2(I2,I3,D1,F2,Z1)
       deallocate(D1)
       deallocate(F2)
C
       V1A=V1A+0.250*Z1
       deallocate(Z1)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N1,M2-N1/),'3412',VAHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,M1-N0,M2-N1,M2-N1,N1-N1,M1-M1/),'451236',t3A,F2)
       allocate(Z2(N1+1:M2,M1+1:N1))
       I2=K7*K9
       I3=K6*K6*K7*K5
       call EGEMM2(I2,I3,D1,F2,Z2)
       deallocate(D1)
       deallocate(F2)
C
       V1A=V1A+0.500*Z2
       deallocate(Z2)
C
       allocate(D1(M1+1:N1,M1+1:N1,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,M2-N1,M2-N1/),'3412',VAHHPP,D1)
       allocate(F2(M1+1:N1,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-N0,M1-N0,M2-N1,M2-N1,N1-N1,M1-M1/),'451236',t3A,F2)
       allocate(Z3(N1+1:M2,M1+1:N1))
       I2=K7*K9
       I3=K6*K6*K7*K7
       call EGEMM2(I2,I3,D1,F2,Z3)
       deallocate(D1)
       deallocate(F2)
C
       V1A=V1A+0.250*Z3
       deallocate(Z3)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N1,N1-N1/),'3412',VAHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,N0-N0,M2-N1,N1-N1,N1-N1,M1-M1/),'451236',t3A,F2)
       allocate(Z4(N1+1:M2,M1+1:N1))
       I2=K7*K9
       I3=K9*K6*K5*K5
       call EGEMM2(I2,I3,D1,F2,Z4)
       deallocate(D1)
       deallocate(F2)
C
       V1A=V1A+0.500*Z4
       deallocate(Z4)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N1,N1-N1/),'3412',VAHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,M1-N0,M2-N1,N1-N1,N1-N1,M1-M1/),'451236',t3A,F2)
       allocate(Z5(N1+1:M2,M1+1:N1))
       I2=K7*K9
       I3=K9*K6*K7*K5
       call EGEMM2(I2,I3,D1,F2,Z5)
       deallocate(D1)
       deallocate(F2)
C
       V1A=V1A+Z5
       deallocate(Z5)
C
       allocate(D1(M1+1:N1,M1+1:N1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,M2-N1,N1-N1/),'3412',VAHHPP,D1)
       allocate(F2(M1+1:N1,M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-N0,M1-N0,M2-N1,N1-N1,N1-N1,M1-M1/),'451236',t3A,F2)
       allocate(Z6(N1+1:M2,M1+1:N1))
       I2=K7*K9
       I3=K9*K6*K7*K7
       call EGEMM2(I2,I3,D1,F2,Z6)
       deallocate(D1)
       deallocate(F2)
C
       V1A=V1A+0.500*Z6
       deallocate(Z6)
C
       allocate(D1(N0+1:M1,N0+1:M1,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N1-N1/),'3412',VAHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N1+1:M2,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,N0-N0,N1-N1,N1-N1,N1-N1,M1-M1/),'451236',t3A,F2)
       allocate(Z7(N1+1:M2,M1+1:N1))
       I2=K7*K9
       I3=K9*K9*K5*K5
       call EGEMM2(I2,I3,D1,F2,Z7)
       deallocate(D1)
       deallocate(F2)
C
       V1A=V1A+0.250*Z7
       deallocate(Z7)
C
       allocate(D1(N0+1:M1,M1+1:N1,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N1-N1,N1-N1/),'3412',VAHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,M1-N0,N1-N1,N1-N1,N1-N1,M1-M1/),'451236',t3A,F2)
       allocate(Z8(N1+1:M2,M1+1:N1))
       I2=K7*K9
       I3=K9*K9*K7*K5
       call EGEMM2(I2,I3,D1,F2,Z8)
       deallocate(D1)
       deallocate(F2)
C
       V1A=V1A+0.500*Z8
       deallocate(Z8)
C
       allocate(D1(M1+1:N1,M1+1:N1,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,N1-N1,N1-N1/),'3412',VAHHPP,D1)
       allocate(F2(M1+1:N1,M1+1:N1,N1+1:M2,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-N0,M1-N0,N1-N1,N1-N1,N1-N1,M1-M1/),'451236',t3A,F2)
       allocate(Z9(N1+1:M2,M1+1:N1))
       I2=K7*K9
       I3=K9*K9*K7*K7
       call EGEMM2(I2,I3,D1,F2,Z9)
       deallocate(D1)
       deallocate(F2)
C
       V1A=V1A+0.250*Z9
       deallocate(Z9)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,M2-N1/),'3412',VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,N0-N0,M2-N2,M2-N1,N1-N1,M1-M1/),'451236',t3B1,F2)
       allocate(Z10(N1+1:M2,M1+1:N1))
       I2=K7*K9
       I3=K6*K6*K5*K5
       call EGEMM2(I2,I3,D1,F2,Z10)
       deallocate(D1)
       deallocate(F2)
C
       V1A=V1A+Z10
       deallocate(Z10)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N0-N0,M2-N2,M2-N1/),'3412',VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,N0-N0,M2-N2,M2-N1,N1-N1,M1-M1/),'451236',t3B1,F2)
       allocate(Z11(N1+1:M2,M1+1:N1))
       I2=K7*K9
       I3=K6*K6*K5*K8
       call EGEMM2(I2,I3,D1,F2,Z11)
       deallocate(D1)
       deallocate(F2)
C
       V1A=V1A+Z11
       deallocate(Z11)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N2,M2-N1/),'3412',VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M1-N0,M2-N2,M2-N1,N1-N1,M1-M1/),'451236',t3B1,F2)
       allocate(Z12(N1+1:M2,M1+1:N1))
       I2=K7*K9
       I3=K6*K6*K7*K5
       call EGEMM2(I2,I3,D1,F2,Z12)
       deallocate(D1)
       deallocate(F2)
C
       V1A=V1A+Z12
       deallocate(Z12)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,M2-N2,M2-N1/),'3412',VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M1-N0,M2-N2,M2-N1,N1-N1,M1-M1/),'451236',t3B1,F2)
       allocate(Z13(N1+1:M2,M1+1:N1))
       I2=K7*K9
       I3=K6*K6*K7*K8
       call EGEMM2(I2,I3,D1,F2,Z13)
       deallocate(D1)
       deallocate(F2)
C
       V1A=V1A+Z13
       deallocate(Z13)
C
       allocate(D1(N0+1:M1,N0+1:M1,N2+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,M2-N1/),'3412',VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,N0-N0,N2-N2,M2-N1,N1-N1,M1-M1/),'451236',t3B1,F2)
       allocate(Z14(N1+1:M2,M1+1:N1))
       I2=K7*K9
       I3=K6*K0*K5*K5
       call EGEMM2(I2,I3,D1,F2,Z14)
       deallocate(D1)
       deallocate(F2)
C
       V1A=V1A+Z14
       deallocate(Z14)
C
       allocate(D1(M1+1:N2,N0+1:M1,N2+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N0-N0,N2-N2,M2-N1/),'3412',VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,N0-N0,N2-N2,M2-N1,N1-N1,M1-M1/),'451236',t3B1,F2)
       allocate(Z15(N1+1:M2,M1+1:N1))
       I2=K7*K9
       I3=K6*K0*K5*K8
       call EGEMM2(I2,I3,D1,F2,Z15)
       deallocate(D1)
       deallocate(F2)
C
       V1A=V1A+Z15
       deallocate(Z15)
C
       allocate(D1(N0+1:M1,M1+1:N1,N2+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N2-N2,M2-N1/),'3412',VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M1-N0,N2-N2,M2-N1,N1-N1,M1-M1/),'451236',t3B1,F2)
       allocate(Z16(N1+1:M2,M1+1:N1))
       I2=K7*K9
       I3=K6*K0*K7*K5
       call EGEMM2(I2,I3,D1,F2,Z16)
       deallocate(D1)
       deallocate(F2)
C
       V1A=V1A+Z16
       deallocate(Z16)
C
       allocate(D1(M1+1:N2,M1+1:N1,N2+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,N2-N2,M2-N1/),'3412',VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M1-N0,N2-N2,M2-N1,N1-N1,M1-M1/),'451236',t3B1,F2)
       allocate(Z17(N1+1:M2,M1+1:N1))
       I2=K7*K9
       I3=K6*K0*K7*K8
       call EGEMM2(I2,I3,D1,F2,Z17)
       deallocate(D1)
       deallocate(F2)
C
       V1A=V1A+Z17
       deallocate(Z17)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,N0-N0,M2-N2,N1-N1,N1-N1,M1-M1/),'451236',t3B1,F2)
       allocate(Z18(N1+1:M2,M1+1:N1))
       I2=K7*K9
       I3=K9*K6*K5*K5
       call EGEMM2(I2,I3,D1,F2,Z18)
       deallocate(D1)
       deallocate(F2)
C
       V1A=V1A+Z18
       deallocate(Z18)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N0-N0,M2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,N0-N0,M2-N2,N1-N1,N1-N1,M1-M1/),'451236',t3B1,F2)
       allocate(Z19(N1+1:M2,M1+1:N1))
       I2=K7*K9
       I3=K9*K6*K5*K8
       call EGEMM2(I2,I3,D1,F2,Z19)
       deallocate(D1)
       deallocate(F2)
C
       V1A=V1A+Z19
       deallocate(Z19)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M1-N0,M2-N2,N1-N1,N1-N1,M1-M1/),'451236',t3B1,F2)
       allocate(Z20(N1+1:M2,M1+1:N1))
       I2=K7*K9
       I3=K9*K6*K7*K5
       call EGEMM2(I2,I3,D1,F2,Z20)
       deallocate(D1)
       deallocate(F2)
C
       V1A=V1A+Z20
       deallocate(Z20)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,M2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M1-N0,M2-N2,N1-N1,N1-N1,M1-M1/),'451236',t3B1,F2)
       allocate(Z21(N1+1:M2,M1+1:N1))
       I2=K7*K9
       I3=K9*K6*K7*K8
       call EGEMM2(I2,I3,D1,F2,Z21)
       deallocate(D1)
       deallocate(F2)
C
       V1A=V1A+Z21
       deallocate(Z21)
C
       allocate(D1(N0+1:M1,N0+1:M1,N2+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N2+1:M2,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,N0-N0,N2-N2,N1-N1,N1-N1,M1-M1/),'451236',t3B1,F2)
       allocate(Z22(N1+1:M2,M1+1:N1))
       I2=K7*K9
       I3=K9*K0*K5*K5
       call EGEMM2(I2,I3,D1,F2,Z22)
       deallocate(D1)
       deallocate(F2)
C
       V1A=V1A+Z22
       deallocate(Z22)
C
       allocate(D1(M1+1:N2,N0+1:M1,N2+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N0-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N2+1:M2,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,N0-N0,N2-N2,N1-N1,N1-N1,M1-M1/),'451236',t3B1,F2)
       allocate(Z23(N1+1:M2,M1+1:N1))
       I2=K7*K9
       I3=K9*K0*K5*K8
       call EGEMM2(I2,I3,D1,F2,Z23)
       deallocate(D1)
       deallocate(F2)
C
       V1A=V1A+Z23
       deallocate(Z23)
C
       allocate(D1(N0+1:M1,M1+1:N1,N2+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N2+1:M2,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M1-N0,N2-N2,N1-N1,N1-N1,M1-M1/),'451236',t3B1,F2)
       allocate(Z24(N1+1:M2,M1+1:N1))
       I2=K7*K9
       I3=K9*K0*K7*K5
       call EGEMM2(I2,I3,D1,F2,Z24)
       deallocate(D1)
       deallocate(F2)
C
       V1A=V1A+Z24
       deallocate(Z24)
C
       allocate(D1(M1+1:N2,M1+1:N1,N2+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N2+1:M2,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M1-N0,N2-N2,N1-N1,N1-N1,M1-M1/),'451236',t3B1,F2)
       allocate(Z25(N1+1:M2,M1+1:N1))
       I2=K7*K9
       I3=K9*K0*K7*K8
       call EGEMM2(I2,I3,D1,F2,Z25)
       deallocate(D1)
       deallocate(F2)
C
       V1A=V1A+Z25
       deallocate(Z25)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,M2-N2/),'3412',VCHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3C2),size(t3C2),shape(F2),size(F2),
     & (/N0-N0,N0-N0,M2-M2,M2-M2,N1-N1,M1-M1/),'451236',t3C2,F2)
       allocate(Z26(N1+1:M2,M1+1:N1))
       I2=K7*K9
       I3=K6*K6*K5*K5
       call EGEMM2(I2,I3,D1,F2,Z26)
       deallocate(D1)
       deallocate(F2)
C
       V1A=V1A+0.250*Z26
       deallocate(Z26)
C
       allocate(D1(N0+1:M1,M1+1:N2,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N2,M2-N2/),'3412',VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3C3),size(t3C3),shape(F2),size(F2),
     & (/N0-N0,M1-M1,M2-M2,M2-M2,N1-N1,M1-N0/),'451236',t3C3,F2)
       allocate(Z27(N1+1:M2,M1+1:N1))
       I2=K7*K9
       I3=K6*K6*K8*K5
       call EGEMM2(I2,I3,D1,F2,Z27)
       deallocate(D1)
       deallocate(F2)
C
       V1A=V1A+0.500*Z27
       deallocate(Z27)
C
       allocate(D1(M1+1:N2,M1+1:N2,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,M2-N2,M2-N2/),'3412',VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3C3),size(t3C3),shape(F2),size(F2),
     & (/M1-N0,M1-M1,M2-M2,M2-M2,N1-N1,M1-N0/),'451236',t3C3,F2)
       allocate(Z28(N1+1:M2,M1+1:N1))
       I2=K7*K9
       I3=K6*K6*K8*K8
       call EGEMM2(I2,I3,D1,F2,Z28)
       deallocate(D1)
       deallocate(F2)
C
       V1A=V1A+0.250*Z28
       deallocate(Z28)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,N2-N2/),'3412',VCHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/N0-N0,N0-N0,M2-N2,N2-N2,N1-N1,M1-M1/),'451236',t3C4,F2)
       allocate(Z29(N1+1:M2,M1+1:N1))
       I2=K7*K9
       I3=K0*K6*K5*K5
       call EGEMM2(I2,I3,D1,F2,Z29)
       deallocate(D1)
       deallocate(F2)
C
       V1A=V1A+0.500*Z29
       deallocate(Z29)
C
       allocate(D1(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N2,N2-N2/),'3412',VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,M1-M1,M2-N2,N2-N2,N1-N1,M1-N0/),'451236',t3C1,F2)
       allocate(Z30(N1+1:M2,M1+1:N1))
       I2=K7*K9
       I3=K0*K6*K8*K5
       call EGEMM2(I2,I3,D1,F2,Z30)
       deallocate(D1)
       deallocate(F2)
C
       V1A=V1A+Z30
       deallocate(Z30)
C
       allocate(D1(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,M2-N2,N2-N2/),'3412',VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,M1-M1,M2-N2,N2-N2,N1-N1,M1-N0/),'451236',t3C1,F2)
       allocate(Z31(N1+1:M2,M1+1:N1))
       I2=K7*K9
       I3=K0*K6*K8*K8
       call EGEMM2(I2,I3,D1,F2,Z31)
       deallocate(D1)
       deallocate(F2)
C
       V1A=V1A+0.500*Z31
       deallocate(Z31)
C
       allocate(D1(N0+1:M1,N0+1:M1,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N2-N2/),'3412',VCHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/N0-N0,N0-N0,N2-N2,N2-N2,N1-N1,M1-M1/),'451236',t3C4,F2)
       allocate(Z32(N1+1:M2,M1+1:N1))
       I2=K7*K9
       I3=K0*K0*K5*K5
       call EGEMM2(I2,I3,D1,F2,Z32)
       deallocate(D1)
       deallocate(F2)
C
       V1A=V1A+0.250*Z32
       deallocate(Z32)
C
       allocate(D1(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N2-N2,N2-N2/),'3412',VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,M1-M1,N2-N2,N2-N2,N1-N1,M1-N0/),'451236',t3C1,F2)
       allocate(Z33(N1+1:M2,M1+1:N1))
       I2=K7*K9
       I3=K0*K0*K8*K5
       call EGEMM2(I2,I3,D1,F2,Z33)
       deallocate(D1)
       deallocate(F2)
C
       V1A=V1A+0.500*Z33
       deallocate(Z33)
C
       allocate(D1(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,N2-N2,N2-N2/),'3412',VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,M1-M1,N2-N2,N2-N2,N1-N1,M1-N0/),'451236',t3C1,F2)
       allocate(Z34(N1+1:M2,M1+1:N1))
       I2=K7*K9
       I3=K0*K0*K8*K8
       call EGEMM2(I2,I3,D1,F2,Z34)
       deallocate(D1)
       deallocate(F2)
C
       V1A=V1A+0.250*Z34
       deallocate(Z34)
C
       call sumx_sorted1(N1,N3,N0,N1,
     & N1,M2,M1,N1,HT1A,V1A,1.0)
       deallocate(V1A)
C
       end
