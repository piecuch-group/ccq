       subroutine L1A_update(N0,N1,N2,N3,V1A,
     & K1,K2,K3,K4,
     & FockR,FockB,IntR,IntB,IntM,
     & H1A,H1B,H2A,H2B,H2C,
     & t1A,t1B,t2A,t2B,t2C,
     & l1A,l1B,l2A,l2B,l2C)
C
       integer a,b,c,e,f,g,h,i,j,k,m,n,o,p
       real*8 FockR(N3,N3)
       real*8 FockB(N3,N3)
       real*8 IntR(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 IntB(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 IntM(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 H1A(N0+1:N3,N0+1:N3)
       real*8 H1B(N0+1:N3,N0+1:N3)
       real*8 H2A(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 H2B(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 H2C(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 t1A(N1+1:N3,N0+1:N1)
       real*8 t1B(N2+1:N3,N0+1:N2)
       real*8 t2A(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1)
       real*8 t2B(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1)
       real*8 t2C(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2)
       real*8 l1A(N1+1:N3,N0+1:N1)
       real*8 l1B(N2+1:N3,N0+1:N2)
       real*8 l2A(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1)
       real*8 l2B(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1)
       real*8 l2C(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2)
C
       real*8 V1A(N1+1:N3,N0+1:N1)
C
       real*8,allocatable::B1(:,:)
       real*8,allocatable::B2(:,:)
       real*8,allocatable::C1(:,:,:)
       real*8,allocatable::C2(:,:,:)
       real*8,allocatable::D1(:,:,:,:)
       real*8,allocatable::D2(:,:,:,:)
C
       real*8,allocatable::Q7(:,:)
       real*8,allocatable::Q9(:,:)
       real*8,allocatable::Q11(:,:)
       real*8,allocatable::Q12(:,:)
       real*8,allocatable::Q14(:,:)
       real*8,allocatable::Q15(:,:)
       real*8,allocatable::Q19(:,:)
       real*8,allocatable::Q23(:,:)
       real*8,allocatable::Q21(:,:)
       real*8,allocatable::Q25(:,:)
       real*8,allocatable::Q27(:,:)
       real*8,allocatable::Q31(:,:)
       real*8,allocatable::Q29(:,:)
       real*8,allocatable::Q33(:,:)
       real*8,allocatable::Q35(:,:)
       real*8,allocatable::Q37(:,:)
       real*8,allocatable::Q39(:,:)
       real*8,allocatable::Q40(:,:)
       real*8,allocatable::Q42(:,:)
       real*8,allocatable::Q43(:,:)
       real*8,allocatable::U1(:,:)
       real*8,allocatable::U2(:,:)
       real*8,allocatable::U3(:,:)
       real*8,allocatable::U4(:,:)
       real*8,allocatable::U5(:,:)
       real*8,allocatable::U6(:,:)
       real*8,allocatable::U8(:,:)
       real*8,allocatable::U10(:,:)
       real*8,allocatable::U13(:,:)
       real*8,allocatable::U16(:,:)
       real*8,allocatable::U17(:,:)
       real*8,allocatable::U18(:,:)
       real*8,allocatable::U20(:,:)
       real*8,allocatable::U24(:,:)
       real*8,allocatable::U22(:,:)
       real*8,allocatable::U26(:,:)
       real*8,allocatable::U28(:,:)
       real*8,allocatable::U32(:,:)
       real*8,allocatable::U30(:,:)
       real*8,allocatable::U34(:,:)
       real*8,allocatable::U36(:,:)
       real*8,allocatable::U38(:,:)
       real*8,allocatable::U41(:,:)
       real*8,allocatable::U44(:,:)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder_stripe(4,shape(l2A),size(l2A),'3124',l2A,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder_stripe(4,shape(t2A),size(t2A),'3124',t2A,D2)
       allocate(Q7(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3*K3*K1
       call EGEMM(I1,I2,I3,D1,D2,Q7)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       call reorder_shift(4,shape(IntR),size(IntR),shape(D1),size(D1),
     & (/N0-n0,N0-n0,N1-n0,N0-n0/),'1324',IntR,D1)
       allocate(B2(N0+1:N1,N0+1:N1))
       call reorder_stripe(2,shape(Q7),size(Q7),'21',Q7,B2)
       allocate(U8(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K1*K1
       call EGEMM1(I1,I3,D1,B2,U8)
       deallocate(D1)
       deallocate(B2)
C
       V1A=V1A-1.0d0/2*U8
       deallocate(U8)
       deallocate(Q7)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call reorder_stripe(4,shape(l2A),size(l2A),'3412',l2A,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call reorder_stripe(4,shape(t2A),size(t2A),'3412',t2A,D2)
       allocate(Q9(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K3*K1*K1
       call EGEMM(I1,I2,I3,D1,D2,Q9)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder_shift(4,shape(IntR),size(IntR),shape(D1),size(D1),
     & (/N1-n0,N1-n0,N1-n0,N0-n0/),'1324',IntR,D1)
       allocate(U10(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K3*K3
       call EGEMM1(I1,I3,D1,Q9,U10)
       deallocate(D1)
C
       V1A=V1A+1.0d0/2*U10
       deallocate(U10)
       deallocate(Q9)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder_stripe(4,shape(l2A),size(l2A),'3124',l2A,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder_stripe(4,shape(t2A),size(t2A),'3124',t2A,D2)
       allocate(Q11(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3*K3*K1
       call EGEMM(I1,I2,I3,D1,D2,Q11)
       deallocate(D1)
       deallocate(D2)
C
       allocate(B1(N0+1:N1,N0+1:N1))
       call reorder_stripe(2,shape(Q11),size(Q11),'21',Q11,B1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_stripe(2,shape(t1A),size(t1A),'21',t1A,B2)
       allocate(Q12(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K1
       call EGEMM(I1,I2,I3,B1,B2,Q12)
       deallocate(B1)
       deallocate(B2)
       deallocate(Q11)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder_shift(4,shape(IntR),size(IntR),shape(D1),size(D1),
     & (/N0-n0,N1-n0,N1-n0,N0-n0/),'3124',IntR,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_stripe(2,shape(Q12),size(Q12),'21',Q12,B2)
       allocate(U13(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,U13)
       deallocate(D1)
       deallocate(B2)
C
       V1A=V1A-1.0d0/2*U13
       deallocate(U13)
       deallocate(Q12)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call reorder_stripe(4,shape(l2A),size(l2A),'3412',l2A,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call reorder_stripe(4,shape(t2A),size(t2A),'3412',t2A,D2)
       allocate(Q14(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K3*K1*K1
       call EGEMM(I1,I2,I3,D1,D2,Q14)
       deallocate(D1)
       deallocate(D2)
C
       allocate(B1(N1+1:N3,N1+1:N3))
       call reorder_stripe(2,shape(Q14),size(Q14),'21',Q14,B1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder_stripe(2,shape(t1A),size(t1A),'12',t1A,B2)
       allocate(Q15(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K3
       call EGEMM(I1,I2,I3,B1,B2,Q15)
       deallocate(B1)
       deallocate(B2)
       deallocate(Q14)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder_shift(4,shape(IntR),size(IntR),shape(D1),size(D1),
     & (/N0-n0,N1-n0,N1-n0,N0-n0/),'3124',IntR,D1)
       allocate(U16(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K3*K1
       call EGEMM1(I1,I3,D1,Q15,U16)
       deallocate(D1)
C
       V1A=V1A-1.0d0/2*U16
       deallocate(U16)
       deallocate(Q15)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder_stripe(4,shape(l2B),size(l2B),'3124',l2B,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder_stripe(4,shape(t2B),size(t2B),'3124',t2B,D2)
       allocate(Q19(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3*K4*K2
       call EGEMM(I1,I2,I3,D1,D2,Q19)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       call reorder_shift(4,shape(IntR),size(IntR),shape(D1),size(D1),
     & (/N0-n0,N0-n0,N1-n0,N0-n0/),'1324',IntR,D1)
       allocate(B2(N0+1:N1,N0+1:N1))
       call reorder_stripe(2,shape(Q19),size(Q19),'21',Q19,B2)
       allocate(U20(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K1*K1
       call EGEMM1(I1,I3,D1,B2,U20)
       deallocate(D1)
       deallocate(B2)
C
       V1A=V1A-U20
       deallocate(U20)
C
       allocate(B1(N0+1:N1,N0+1:N1))
       call reorder_stripe(2,shape(Q19),size(Q19),'21',Q19,B1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_stripe(2,shape(t1A),size(t1A),'21',t1A,B2)
       allocate(Q23(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K1
       call EGEMM(I1,I2,I3,B1,B2,Q23)
       deallocate(B1)
       deallocate(B2)
       deallocate(Q19)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder_shift(4,shape(IntR),size(IntR),shape(D1),size(D1),
     & (/N0-n0,N1-n0,N1-n0,N0-n0/),'3124',IntR,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_stripe(2,shape(Q23),size(Q23),'21',Q23,B2)
       allocate(U24(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,U24)
       deallocate(D1)
       deallocate(B2)
C
       V1A=V1A-U24
       deallocate(U24)
       deallocate(Q23)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call reorder_stripe(4,shape(l2B),size(l2B),'3412',l2B,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call reorder_stripe(4,shape(t2B),size(t2B),'3412',t2B,D2)
       allocate(Q21(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K4*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q21)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder_shift(4,shape(IntR),size(IntR),shape(D1),size(D1),
     & (/N1-n0,N1-n0,N1-n0,N0-n0/),'1324',IntR,D1)
       allocate(U22(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K3*K3
       call EGEMM1(I1,I3,D1,Q21,U22)
       deallocate(D1)
C
       V1A=V1A+U22
       deallocate(U22)
C
       allocate(B1(N1+1:N3,N1+1:N3))
       call reorder_stripe(2,shape(Q21),size(Q21),'21',Q21,B1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder_stripe(2,shape(t1A),size(t1A),'12',t1A,B2)
       allocate(Q25(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K3
       call EGEMM(I1,I2,I3,B1,B2,Q25)
       deallocate(B1)
       deallocate(B2)
       deallocate(Q21)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder_shift(4,shape(IntR),size(IntR),shape(D1),size(D1),
     & (/N0-n0,N1-n0,N1-n0,N0-n0/),'3124',IntR,D1)
       allocate(U26(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K3*K1
       call EGEMM1(I1,I3,D1,Q25,U26)
       deallocate(D1)
C
       V1A=V1A-U26
       deallocate(U26)
       deallocate(Q25)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call reorder_stripe(4,shape(l2B),size(l2B),'4123',l2B,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call reorder_stripe(4,shape(t2B),size(t2B),'4123',t2B,D2)
       allocate(Q27(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K3*K4*K1
       call EGEMM(I1,I2,I3,D1,D2,Q27)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N1+1:N3,N0+1:N1))
       call reorder_shift(4,shape(IntM),size(IntM),shape(D1),size(D1),
     & (/N0-n0,N0-n0,N1-n0,N0-n0/),'1324',IntM,D1)
       allocate(B2(N0+1:N2,N0+1:N2))
       call reorder_stripe(2,shape(Q27),size(Q27),'21',Q27,B2)
       allocate(U28(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K2*K2
       call EGEMM1(I1,I3,D1,B2,U28)
       deallocate(D1)
       deallocate(B2)
C
       V1A=V1A-U28
       deallocate(U28)
C
       allocate(B1(N0+1:N2,N0+1:N2))
       call reorder_stripe(2,shape(Q27),size(Q27),'21',Q27,B1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_stripe(2,shape(t1B),size(t1B),'21',t1B,B2)
       allocate(Q31(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K2
       call EGEMM(I1,I2,I3,B1,B2,Q31)
       deallocate(B1)
       deallocate(B2)
       deallocate(Q27)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder_shift(4,shape(IntM),size(IntM),shape(D1),size(D1),
     & (/N0-n0,N2-n0,N1-n0,N0-n0/),'3124',IntM,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_stripe(2,shape(Q31),size(Q31),'21',Q31,B2)
       allocate(U32(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,U32)
       deallocate(D1)
       deallocate(B2)
C
       V1A=V1A-U32
       deallocate(U32)
       deallocate(Q31)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call reorder_stripe(4,shape(l2B),size(l2B),'3421',l2B,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call reorder_stripe(4,shape(t2B),size(t2B),'3421',t2B,D2)
       allocate(Q29(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K3*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q29)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder_shift(4,shape(IntM),size(IntM),shape(D1),size(D1),
     & (/N2-n0,N2-n0,N1-n0,N0-n0/),'3124',IntM,D1)
       allocate(B2(N2+1:N3,N2+1:N3))
       call reorder_stripe(2,shape(Q29),size(Q29),'21',Q29,B2)
       allocate(U30(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K4*K4
       call EGEMM1(I1,I3,D1,B2,U30)
       deallocate(D1)
       deallocate(B2)
C
       V1A=V1A+U30
       deallocate(U30)
C
       allocate(B1(N2+1:N3,N2+1:N3))
       call reorder_stripe(2,shape(Q29),size(Q29),'21',Q29,B1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder_stripe(2,shape(t1B),size(t1B),'12',t1B,B2)
       allocate(Q33(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,B1,B2,Q33)
       deallocate(B1)
       deallocate(B2)
       deallocate(Q29)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder_shift(4,shape(IntM),size(IntM),shape(D1),size(D1),
     & (/N0-n0,N2-n0,N1-n0,N0-n0/),'3124',IntM,D1)
       allocate(U34(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K4*K2
       call EGEMM1(I1,I3,D1,Q33,U34)
       deallocate(D1)
C
       V1A=V1A-U34
       deallocate(U34)
       deallocate(Q33)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder_stripe(4,shape(l2C),size(l2C),'3124',l2C,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder_stripe(4,shape(t2C),size(t2C),'3124',t2C,D2)
       allocate(Q35(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K4*K4*K2
       call EGEMM(I1,I2,I3,D1,D2,Q35)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N1+1:N3,N0+1:N1))
       call reorder_shift(4,shape(IntM),size(IntM),shape(D1),size(D1),
     & (/N0-n0,N0-n0,N1-n0,N0-n0/),'1324',IntM,D1)
       allocate(B2(N0+1:N2,N0+1:N2))
       call reorder_stripe(2,shape(Q35),size(Q35),'21',Q35,B2)
       allocate(U36(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K2*K2
       call EGEMM1(I1,I3,D1,B2,U36)
       deallocate(D1)
       deallocate(B2)
C
       V1A=V1A-1.0d0/2*U36
       deallocate(U36)
       deallocate(Q35)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call reorder_stripe(4,shape(l2C),size(l2C),'3412',l2C,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call reorder_stripe(4,shape(t2C),size(t2C),'3412',t2C,D2)
       allocate(Q37(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K4*K2*K2
       call EGEMM(I1,I2,I3,D1,D2,Q37)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder_shift(4,shape(IntM),size(IntM),shape(D1),size(D1),
     & (/N2-n0,N2-n0,N1-n0,N0-n0/),'1324',IntM,D1)
       allocate(U38(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K4*K4
       call EGEMM1(I1,I3,D1,Q37,U38)
       deallocate(D1)
C
       V1A=V1A+1.0d0/2*U38
       deallocate(U38)
       deallocate(Q37)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder_stripe(4,shape(l2C),size(l2C),'3124',l2C,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder_stripe(4,shape(t2C),size(t2C),'3124',t2C,D2)
       allocate(Q39(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K4*K4*K2
       call EGEMM(I1,I2,I3,D1,D2,Q39)
       deallocate(D1)
       deallocate(D2)
C
       allocate(B1(N0+1:N2,N0+1:N2))
       call reorder_stripe(2,shape(Q39),size(Q39),'21',Q39,B1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_stripe(2,shape(t1B),size(t1B),'21',t1B,B2)
       allocate(Q40(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K2
       call EGEMM(I1,I2,I3,B1,B2,Q40)
       deallocate(B1)
       deallocate(B2)
       deallocate(Q39)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder_shift(4,shape(IntM),size(IntM),shape(D1),size(D1),
     & (/N0-n0,N2-n0,N1-n0,N0-n0/),'3124',IntM,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_stripe(2,shape(Q40),size(Q40),'21',Q40,B2)
       allocate(U41(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,U41)
       deallocate(D1)
       deallocate(B2)
C
       V1A=V1A-1.0d0/2*U41
       deallocate(U41)
       deallocate(Q40)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call reorder_stripe(4,shape(l2C),size(l2C),'3412',l2C,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call reorder_stripe(4,shape(t2C),size(t2C),'3412',t2C,D2)
       allocate(Q42(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K4*K2*K2
       call EGEMM(I1,I2,I3,D1,D2,Q42)
       deallocate(D1)
       deallocate(D2)
C
       allocate(B1(N2+1:N3,N2+1:N3))
       call reorder_stripe(2,shape(Q42),size(Q42),'21',Q42,B1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder_stripe(2,shape(t1B),size(t1B),'12',t1B,B2)
       allocate(Q43(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,B1,B2,Q43)
       deallocate(B1)
       deallocate(B2)
       deallocate(Q42)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder_shift(4,shape(IntM),size(IntM),shape(D1),size(D1),
     & (/N0-n0,N2-n0,N1-n0,N0-n0/),'3124',IntM,D1)
       allocate(U44(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K4*K2
       call EGEMM1(I1,I3,D1,Q43,U44)
       deallocate(D1)
C
       V1A=V1A-1.0d0/2*U44
       deallocate(U44)
       deallocate(Q43)
C
       allocate(B1(N0+1:N1,N0+1:N1))
       call reorder_shift(2,shape(H1A),size(H1A),shape(B1),size(B1),
     & (/N0-n0,N0-n0/),'12',H1A,B1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_stripe(2,shape(l1A),size(l1A),'21',l1A,B2)
       allocate(U1(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K1
       call EGEMM(I1,I2,I3,B1,B2,U1)
       deallocate(B1)
       deallocate(B2)
C
       V1A=V1A-U1
       deallocate(U1)
C
       allocate(B1(N1+1:N3,N1+1:N3))
       call reorder_shift(2,shape(H1A),size(H1A),shape(B1),size(B1),
     & (/N1-n0,N1-n0/),'21',H1A,B1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder_stripe(2,shape(l1A),size(l1A),'12',l1A,B2)
       allocate(U2(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K3
       call EGEMM(I1,I2,I3,B1,B2,U2)
       deallocate(B1)
       deallocate(B2)
C
       call sum_stripe(2,shape(V1A),size(V1A),'21',1.000,
     & V1A,U2)
       deallocate(U2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder_shift(4,shape(H2A),size(H2A),shape(D1),size(D1),
     & (/N0-n0,N1-n0,N1-n0,N0-n0/),'1324',H2A,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_stripe(2,shape(l1A),size(l1A),'21',l1A,B2)
       allocate(U3(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,U3)
       deallocate(D1)
       deallocate(B2)
C
       V1A=V1A+U3
       deallocate(U3)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder_shift(4,shape(H2B),size(H2B),shape(D1),size(D1),
     & (/N0-n0,N2-n0,N1-n0,N0-n0/),'1324',H2B,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_stripe(2,shape(l1B),size(l1B),'21',l1B,B2)
       allocate(U4(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,U4)
       deallocate(D1)
       deallocate(B2)
C
       V1A=V1A+U4
       deallocate(U4)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       call reorder_shift(4,shape(H2A),size(H2A),shape(D1),size(D1),
     & (/N0-n0,N0-n0,N1-n0,N0-n0/),'1234',H2A,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call reorder_stripe(4,shape(l2A),size(l2A),'3412',l2A,D2)
       allocate(U5(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K3*K1*K1
       call EGEMM(I1,I2,I3,D1,D2,U5)
       deallocate(D1)
       deallocate(D2)
C
       V1A=V1A-1.0d0/2*U5
       deallocate(U5)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call reorder_shift(4,shape(H2A),size(H2A),shape(D1),size(D1),
     & (/N0-n0,N1-n0,N1-n0,N1-n0/),'1342',H2A,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder_stripe(4,shape(l2A),size(l2A),'3124',l2A,D2)
       allocate(U6(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K3*K3*K1
       call EGEMM(I1,I2,I3,D1,D2,U6)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(V1A),size(V1A),'21',0.500,
     & V1A,U6)
       deallocate(U6)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       call reorder_shift(4,shape(H2B),size(H2B),shape(D1),size(D1),
     & (/N0-n0,N0-n0,N2-n0,N0-n0/),'1234',H2B,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call reorder_stripe(4,shape(l2B),size(l2B),'3412',l2B,D2)
       allocate(U17(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K4*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,U17)
       deallocate(D1)
       deallocate(D2)
C
       V1A=V1A-U17
       deallocate(U17)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:N3))
       call reorder_shift(4,shape(H2B),size(H2B),shape(D1),size(D1),
     & (/N0-n0,N2-n0,N1-n0,N1-n0/),'1342',H2B,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder_stripe(4,shape(l2B),size(l2B),'3124',l2B,D2)
       allocate(U18(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K3*K4*K2
       call EGEMM(I1,I2,I3,D1,D2,U18)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(V1A),size(V1A),'21',1.000,
     & V1A,U18)
       deallocate(U18)
C
       end
