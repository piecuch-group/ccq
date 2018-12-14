       subroutine reorder12534678(
     & M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,M7,N7,M8,N8,
     & K1,L1,K2,L2,K5,L5,K3,L3,K4,L4,K6,L6,K7,L7,K8,L8,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,
     & M5+1:N5,M6+1:N6,M7+1:N7,M8+1:N8)
       real*8 B(K1+1:L1,K2+1:L2,K5+1:L5,K3+1:L3,
     & K4+1:L4,K6+1:L6,K7+1:L7,K8+1:L8)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
       do I7=K7+1,L7
       do I8=K8+1,L8
        B(I1,I2,I5,I3,I4,I6,I7,I8)=A(I1,I2,I3,I4,I5,I6,I7,I8)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder12634578(
     & M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,M7,N7,M8,N8,
     & K1,L1,K2,L2,K6,L6,K3,L3,K4,L4,K5,L5,K7,L7,K8,L8,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,
     & M5+1:N5,M6+1:N6,M7+1:N7,M8+1:N8)
       real*8 B(K1+1:L1,K2+1:L2,K6+1:L6,K3+1:L3,
     & K4+1:L4,K5+1:L5,K7+1:L7,K8+1:L8)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
       do I7=K7+1,L7
       do I8=K8+1,L8
        B(I1,I2,I6,I3,I4,I5,I7,I8)=A(I1,I2,I3,I4,I5,I6,I7,I8)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder13524678(
     & M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,M7,N7,M8,N8,
     & K1,L1,K3,L3,K5,L5,K2,L2,K4,L4,K6,L6,K7,L7,K8,L8,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,
     & M5+1:N5,M6+1:N6,M7+1:N7,M8+1:N8)
       real*8 B(K1+1:L1,K3+1:L3,K5+1:L5,K2+1:L2,
     & K4+1:L4,K6+1:L6,K7+1:L7,K8+1:L8)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
       do I7=K7+1,L7
       do I8=K8+1,L8
        B(I1,I3,I5,I2,I4,I6,I7,I8)=A(I1,I2,I3,I4,I5,I6,I7,I8)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder13724568(
     & M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,M7,N7,M8,N8,
     & K1,L1,K3,L3,K7,L7,K2,L2,K4,L4,K5,L5,K6,L6,K8,L8,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,
     & M5+1:N5,M6+1:N6,M7+1:N7,M8+1:N8)
       real*8 B(K1+1:L1,K3+1:L3,K7+1:L7,K2+1:L2,
     & K4+1:L4,K5+1:L5,K6+1:L6,K8+1:L8)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
       do I7=K7+1,L7
       do I8=K8+1,L8
        B(I1,I3,I7,I2,I4,I5,I6,I8)=A(I1,I2,I3,I4,I5,I6,I7,I8)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder14523678(
     & M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,M7,N7,M8,N8,
     & K1,L1,K4,L4,K5,L5,K2,L2,K3,L3,K6,L6,K7,L7,K8,L8,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,
     & M5+1:N5,M6+1:N6,M7+1:N7,M8+1:N8)
       real*8 B(K1+1:L1,K4+1:L4,K5+1:L5,K2+1:L2,
     & K3+1:L3,K6+1:L6,K7+1:L7,K8+1:L8)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
       do I7=K7+1,L7
       do I8=K8+1,L8
        B(I1,I4,I5,I2,I3,I6,I7,I8)=A(I1,I2,I3,I4,I5,I6,I7,I8)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder14823567(
     & M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,M7,N7,M8,N8,
     & K1,L1,K4,L4,K8,L8,K2,L2,K3,L3,K5,L5,K6,L6,K7,L7,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,
     & M5+1:N5,M6+1:N6,M7+1:N7,M8+1:N8)
       real*8 B(K1+1:L1,K4+1:L4,K8+1:L8,K2+1:L2,
     & K3+1:L3,K5+1:L5,K6+1:L6,K7+1:L7)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
       do I7=K7+1,L7
       do I8=K8+1,L8
        B(I1,I4,I8,I2,I3,I5,I6,I7)=A(I1,I2,I3,I4,I5,I6,I7,I8)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder23614578(
     & M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,M7,N7,M8,N8,
     & K2,L2,K3,L3,K6,L6,K1,L1,K4,L4,K5,L5,K7,L7,K8,L8,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,
     & M5+1:N5,M6+1:N6,M7+1:N7,M8+1:N8)
       real*8 B(K2+1:L2,K3+1:L3,K6+1:L6,K1+1:L1,
     & K4+1:L4,K5+1:L5,K7+1:L7,K8+1:L8)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
       do I7=K7+1,L7
       do I8=K8+1,L8
        B(I2,I3,I6,I1,I4,I5,I7,I8)=A(I1,I2,I3,I4,I5,I6,I7,I8)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder34712568(
     & M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,M7,N7,M8,N8,
     & K3,L3,K4,L4,K7,L7,K1,L1,K2,L2,K5,L5,K6,L6,K8,L8,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,
     & M5+1:N5,M6+1:N6,M7+1:N7,M8+1:N8)
       real*8 B(K3+1:L3,K4+1:L4,K7+1:L7,K1+1:L1,
     & K2+1:L2,K5+1:L5,K6+1:L6,K8+1:L8)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
       do I7=K7+1,L7
       do I8=K8+1,L8
        B(I3,I4,I7,I1,I2,I5,I6,I8)=A(I1,I2,I3,I4,I5,I6,I7,I8)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder56123478(
     & M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,M7,N7,M8,N8,
     & K5,L5,K6,L6,K1,L1,K2,L2,K3,L3,K4,L4,K7,L7,K8,L8,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,
     & M5+1:N5,M6+1:N6,M7+1:N7,M8+1:N8)
       real*8 B(K5+1:L5,K6+1:L6,K1+1:L1,K2+1:L2,
     & K3+1:L3,K4+1:L4,K7+1:L7,K8+1:L8)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
       do I7=K7+1,L7
       do I8=K8+1,L8
        B(I5,I6,I1,I2,I3,I4,I7,I8)=A(I1,I2,I3,I4,I5,I6,I7,I8)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder57132468(
     & M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,M7,N7,M8,N8,
     & K5,L5,K7,L7,K1,L1,K3,L3,K2,L2,K4,L4,K6,L6,K8,L8,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,
     & M5+1:N5,M6+1:N6,M7+1:N7,M8+1:N8)
       real*8 B(K5+1:L5,K7+1:L7,K1+1:L1,K3+1:L3,
     & K2+1:L2,K4+1:L4,K6+1:L6,K8+1:L8)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
       do I7=K7+1,L7
       do I8=K8+1,L8
        B(I5,I7,I1,I3,I2,I4,I6,I8)=A(I1,I2,I3,I4,I5,I6,I7,I8)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder58142367(
     & M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,M7,N7,M8,N8,
     & K5,L5,K8,L8,K1,L1,K4,L4,K2,L2,K3,L3,K6,L6,K7,L7,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,
     & M5+1:N5,M6+1:N6,M7+1:N7,M8+1:N8)
       real*8 B(K5+1:L5,K8+1:L8,K1+1:L1,K4+1:L4,
     & K2+1:L2,K3+1:L3,K6+1:L6,K7+1:L7)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
       do I7=K7+1,L7
       do I8=K8+1,L8
        B(I5,I8,I1,I4,I2,I3,I6,I7)=A(I1,I2,I3,I4,I5,I6,I7,I8)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder67231458(
     & M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,M7,N7,M8,N8,
     & K6,L6,K7,L7,K2,L2,K3,L3,K1,L1,K4,L4,K5,L5,K8,L8,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,
     & M5+1:N5,M6+1:N6,M7+1:N7,M8+1:N8)
       real*8 B(K6+1:L6,K7+1:L7,K2+1:L2,K3+1:L3,
     & K1+1:L1,K4+1:L4,K5+1:L5,K8+1:L8)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
       do I7=K7+1,L7
       do I8=K8+1,L8
        B(I6,I7,I2,I3,I1,I4,I5,I8)=A(I1,I2,I3,I4,I5,I6,I7,I8)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder78341256(
     & M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,M7,N7,M8,N8,
     & K7,L7,K8,L8,K3,L3,K4,L4,K1,L1,K2,L2,K5,L5,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,
     & M5+1:N5,M6+1:N6,M7+1:N7,M8+1:N8)
       real*8 B(K7+1:L7,K8+1:L8,K3+1:L3,K4+1:L4,
     & K1+1:L1,K2+1:L2,K5+1:L5,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
       do I7=K7+1,L7
       do I8=K8+1,L8
        B(I7,I8,I3,I4,I1,I2,I5,I6)=A(I1,I2,I3,I4,I5,I6,I7,I8)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder123456(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder124356(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K4,L4,K3,L3,K5,L5,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K4+1:L4,K3+1:L3,K5+1:L5,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I2,I4,I3,I5,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder125346(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K5,L5,K3,L3,K4,L4,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K5+1:L5,K3+1:L3,K4+1:L4,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I2,I5,I3,I4,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder132456(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K2,L2,K4,L4,K5,L5,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K2+1:L2,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I3,I2,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder134256(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K4,L4,K2,L2,K5,L5,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K4+1:L4,K2+1:L2,K5+1:L5,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I3,I4,I2,I5,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder136245(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K6,L6,K2,L2,K4,L4,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K6+1:L6,K2+1:L2,K4+1:L4,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I3,I6,I2,I4,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder142356(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K4,L4,K2,L2,K3,L3,K5,L5,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K4+1:L4,K2+1:L2,K3+1:L3,K5+1:L5,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I4,I2,I3,I5,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder152346(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K2,L2,K3,L3,K4,L4,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K2+1:L2,K3+1:L3,K4+1:L4,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I5,I2,I3,I4,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder154236(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K4,L4,K2,L2,K3,L3,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K4+1:L4,K2+1:L2,K3+1:L3,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I5,I4,I2,I3,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder162345(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K2,L2,K3,L3,K4,L4,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I6,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder164235(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K4,L4,K2,L2,K3,L3,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K4+1:L4,K2+1:L2,K3+1:L3,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I6,I4,I2,I3,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder231456(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K1,L1,K4,L4,K5,L5,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K1+1:L1,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I3,I1,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder235146(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K5,L5,K1,L1,K4,L4,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K5+1:L5,K1+1:L1,K4+1:L4,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I3,I5,I1,I4,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder241356(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K4,L4,K1,L1,K3,L3,K5,L5,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K4+1:L4,K1+1:L1,K3+1:L3,K5+1:L5,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I4,I1,I3,I5,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder251346(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K1,L1,K3,L3,K4,L4,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K1+1:L1,K3+1:L3,K4+1:L4,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I5,I1,I3,I4,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder254136(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K4,L4,K1,L1,K3,L3,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K4+1:L4,K1+1:L1,K3+1:L3,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I5,I4,I1,I3,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder261345(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K1,L1,K3,L3,K4,L4,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K1+1:L1,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I6,I1,I3,I4,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder263145(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K3,L3,K1,L1,K4,L4,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K3+1:L3,K1+1:L1,K4+1:L4,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I6,I3,I1,I4,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder263415(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K3,L3,K4,L4,K1,L1,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K3+1:L3,K4+1:L4,K1+1:L1,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I6,I3,I4,I1,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder263451(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K3,L3,K4,L4,K5,L5,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K3+1:L3,K4+1:L4,K5+1:L5,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I6,I3,I4,I5,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder265134(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K5,L5,K1,L1,K3,L3,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K5+1:L5,K1+1:L1,K3+1:L3,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I6,I5,I1,I3,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder341256(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K4,L4,K1,L1,K2,L2,K5,L5,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K4+1:L4,K1+1:L1,K2+1:L2,K5+1:L5,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I4,I1,I2,I5,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder361245(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K1,L1,K2,L2,K4,L4,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K1+1:L1,K2+1:L2,K4+1:L4,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I6,I1,I2,I4,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder364125(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K4,L4,K1,L1,K2,L2,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K4+1:L4,K1+1:L1,K2+1:L2,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I6,I4,I1,I2,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder412356(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K2,L2,K3,L3,K5,L5,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K2+1:L2,K3+1:L3,K5+1:L5,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I1,I2,I3,I5,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder413256(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K3,L3,K2,L2,K5,L5,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K3+1:L3,K2+1:L2,K5+1:L5,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I1,I3,I2,I5,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder451236(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K1,L1,K2,L2,K3,L3,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K1+1:L1,K2+1:L2,K3+1:L3,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I5,I1,I2,I3,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder452136(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K2,L2,K1,L1,K3,L3,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K2+1:L2,K1+1:L1,K3+1:L3,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I5,I2,I1,I3,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder461235(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K1,L1,K2,L2,K3,L3,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K1+1:L1,K2+1:L2,K3+1:L3,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I6,I1,I2,I3,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder461325(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K1,L1,K3,L3,K2,L2,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K1+1:L1,K3+1:L3,K2+1:L2,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I6,I1,I3,I2,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder463125(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K3,L3,K1,L1,K2,L2,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K3+1:L3,K1+1:L1,K2+1:L2,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I6,I3,I1,I2,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder465123(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K5,L5,K1,L1,K2,L2,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K5+1:L5,K1+1:L1,K2+1:L2,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I6,I5,I1,I2,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder512346(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K1,L1,K2,L2,K3,L3,K4,L4,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I1,I2,I3,I4,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder521346(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K1,L1,K3,L3,K4,L4,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K1+1:L1,K3+1:L3,K4+1:L4,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I2,I1,I3,I4,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder523146(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K3,L3,K1,L1,K4,L4,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K3+1:L3,K1+1:L1,K4+1:L4,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I2,I3,I1,I4,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder541236(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K4,L4,K1,L1,K2,L2,K3,L3,K6,L6,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K4+1:L4,K1+1:L1,K2+1:L2,K3+1:L3,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I4,I1,I2,I3,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder561234(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K1,L1,K2,L2,K3,L3,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I6,I1,I2,I3,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder562134(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K2,L2,K1,L1,K3,L3,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K2+1:L2,K1+1:L1,K3+1:L3,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I6,I2,I1,I3,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder562314(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K2,L2,K3,L3,K1,L1,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K2+1:L2,K3+1:L3,K1+1:L1,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I6,I2,I3,I1,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder564123(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K4,L4,K1,L1,K2,L2,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K4+1:L4,K1+1:L1,K2+1:L2,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I6,I4,I1,I2,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder612345(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder613245(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K3,L3,K2,L2,K4,L4,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K3+1:L3,K2+1:L2,K4+1:L4,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I1,I3,I2,I4,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder621345(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K1,L1,K3,L3,K4,L4,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K1+1:L1,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I2,I1,I3,I4,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder623145(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K3,L3,K1,L1,K4,L4,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K3+1:L3,K1+1:L1,K4+1:L4,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I2,I3,I1,I4,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder623415(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K3,L3,K4,L4,K1,L1,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K3+1:L3,K4+1:L4,K1+1:L1,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I2,I3,I4,I1,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder623451(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K3,L3,K4,L4,K5,L5,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I2,I3,I4,I5,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder631245(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K3,L3,K1,L1,K2,L2,K4,L4,K5,L5,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K3+1:L3,K1+1:L1,K2+1:L2,K4+1:L4,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I3,I1,I2,I4,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder651234(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K1,L1,K2,L2,K3,L3,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I5,I1,I2,I3,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder654123(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K4,L4,K1,L1,K2,L2,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K4+1:L4,K1+1:L1,K2+1:L2,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I5,I4,I1,I2,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder12(M1,N1,M2,N2,
     & K1,L1,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2)
       real*8 B(K1+1:L1,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
        B(I1,I2)=A(I1,I2)
       enddo
       enddo
C
       end

       subroutine reorder21(M1,N1,M2,N2,
     & K2,L2,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2)
       real*8 B(K2+1:L2,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
        B(I2,I1)=A(I1,I2)
       enddo
       enddo
C
       end

       subroutine reorder1234(M1,N1,M2,N2,M3,N3,M4,N4,
     & K1,L1,K2,L2,K3,L3,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        B(I1,I2,I3,I4)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder1243(M1,N1,M2,N2,M3,N3,M4,N4,
     & K1,L1,K2,L2,K4,L4,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K1+1:L1,K2+1:L2,K4+1:L4,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        B(I1,I2,I4,I3)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder1324(M1,N1,M2,N2,M3,N3,M4,N4,
     & K1,L1,K3,L3,K2,L2,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K1+1:L1,K3+1:L3,K2+1:L2,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        B(I1,I3,I2,I4)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder1342(M1,N1,M2,N2,M3,N3,M4,N4,
     & K1,L1,K3,L3,K4,L4,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K1+1:L1,K3+1:L3,K4+1:L4,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        B(I1,I3,I4,I2)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder1423(M1,N1,M2,N2,M3,N3,M4,N4,
     & K1,L1,K4,L4,K2,L2,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K1+1:L1,K4+1:L4,K2+1:L2,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        B(I1,I4,I2,I3)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder1432(M1,N1,M2,N2,M3,N3,M4,N4,
     & K1,L1,K4,L4,K3,L3,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K1+1:L1,K4+1:L4,K3+1:L3,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        B(I1,I4,I3,I2)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder2134(M1,N1,M2,N2,M3,N3,M4,N4,
     & K2,L2,K1,L1,K3,L3,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K2+1:L2,K1+1:L1,K3+1:L3,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        B(I2,I1,I3,I4)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder2143(M1,N1,M2,N2,M3,N3,M4,N4,
     & K2,L2,K1,L1,K4,L4,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K2+1:L2,K1+1:L1,K4+1:L4,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        B(I2,I1,I4,I3)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder2314(M1,N1,M2,N2,M3,N3,M4,N4,
     & K2,L2,K3,L3,K1,L1,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K2+1:L2,K3+1:L3,K1+1:L1,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        B(I2,I3,I1,I4)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder2341(M1,N1,M2,N2,M3,N3,M4,N4,
     & K2,L2,K3,L3,K4,L4,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K2+1:L2,K3+1:L3,K4+1:L4,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        B(I2,I3,I4,I1)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder2413(M1,N1,M2,N2,M3,N3,M4,N4,
     & K2,L2,K4,L4,K1,L1,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K2+1:L2,K4+1:L4,K1+1:L1,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        B(I2,I4,I1,I3)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder2431(M1,N1,M2,N2,M3,N3,M4,N4,
     & K2,L2,K4,L4,K3,L3,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K2+1:L2,K4+1:L4,K3+1:L3,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        B(I2,I4,I3,I1)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder3124(M1,N1,M2,N2,M3,N3,M4,N4,
     & K3,L3,K1,L1,K2,L2,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K3+1:L3,K1+1:L1,K2+1:L2,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        B(I3,I1,I2,I4)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder3142(M1,N1,M2,N2,M3,N3,M4,N4,
     & K3,L3,K1,L1,K4,L4,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K3+1:L3,K1+1:L1,K4+1:L4,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        B(I3,I1,I4,I2)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder3214(M1,N1,M2,N2,M3,N3,M4,N4,
     & K3,L3,K2,L2,K1,L1,K4,L4,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K3+1:L3,K2+1:L2,K1+1:L1,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        B(I3,I2,I1,I4)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder3241(M1,N1,M2,N2,M3,N3,M4,N4,
     & K3,L3,K2,L2,K4,L4,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K3+1:L3,K2+1:L2,K4+1:L4,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        B(I3,I2,I4,I1)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder3412(M1,N1,M2,N2,M3,N3,M4,N4,
     & K3,L3,K4,L4,K1,L1,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K3+1:L3,K4+1:L4,K1+1:L1,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        B(I3,I4,I1,I2)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder3421(M1,N1,M2,N2,M3,N3,M4,N4,
     & K3,L3,K4,L4,K2,L2,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K3+1:L3,K4+1:L4,K2+1:L2,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        B(I3,I4,I2,I1)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder4123(M1,N1,M2,N2,M3,N3,M4,N4,
     & K4,L4,K1,L1,K2,L2,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K4+1:L4,K1+1:L1,K2+1:L2,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        B(I4,I1,I2,I3)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder4132(M1,N1,M2,N2,M3,N3,M4,N4,
     & K4,L4,K1,L1,K3,L3,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K4+1:L4,K1+1:L1,K3+1:L3,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        B(I4,I1,I3,I2)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder4213(M1,N1,M2,N2,M3,N3,M4,N4,
     & K4,L4,K2,L2,K1,L1,K3,L3,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K4+1:L4,K2+1:L2,K1+1:L1,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        B(I4,I2,I1,I3)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder4231(M1,N1,M2,N2,M3,N3,M4,N4,
     & K4,L4,K2,L2,K3,L3,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K4+1:L4,K2+1:L2,K3+1:L3,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        B(I4,I2,I3,I1)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder4312(M1,N1,M2,N2,M3,N3,M4,N4,
     & K4,L4,K3,L3,K1,L1,K2,L2,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K4+1:L4,K3+1:L3,K1+1:L1,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        B(I4,I3,I1,I2)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
C
       end

       subroutine reorder4321(M1,N1,M2,N2,M3,N3,M4,N4,
     & K4,L4,K3,L3,K2,L2,K1,L1,A,B)
C
       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K4+1:L4,K3+1:L3,K2+1:L2,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        B(I4,I3,I2,I1)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
C
       end

