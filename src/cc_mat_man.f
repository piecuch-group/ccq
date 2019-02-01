      subroutine reorder412563(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs4,Be4,Bs1,Be1,Bs2,Be2,Bs5,Be5,Bs6,Be6,Bs3,Be3,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs4+1:Be4,Bs1+1:Be1,Bs2+1:Be2,Bs5+1:Be5,
     & Bs6+1:Be6,Bs3+1:Be3)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i4,i1,i2,i5,i6,i3)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder14235678(As1,Ae1,As2,Ae2,As3,Ae3,As4,
     & Ae4,As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,Bs1,Be1,Bs4,Be4,Bs2,Be2,Bs3,
     & Be3,Bs5,Be5,Bs6,Be6,Bs7,Be7,Bs8,Be8,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6,
     & Bs7,Be7,Bs8,Be8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(Bs1+1:Be1,Bs4+1:Be4,Bs2+1:Be2,Bs3+1:Be3,
     & Bs5+1:Be5,Bs6+1:Be6,Bs7+1:Be7,Bs8+1:Be8)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      do i7=Bs7+1,Be7
      do i8=Bs8+1,Be8
      B(i1,i4,i2,i3,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder12534678(As1,Ae1,As2,Ae2,As3,Ae3,As4,
     & Ae4,As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,Bs1,Be1,Bs2,Be2,Bs5,Be5,Bs3,
     & Be3,Bs4,Be4,Bs6,Be6,Bs7,Be7,Bs8,Be8,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6,
     & Bs7,Be7,Bs8,Be8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(Bs1+1:Be1,Bs2+1:Be2,Bs5+1:Be5,Bs3+1:Be3,
     & Bs4+1:Be4,Bs6+1:Be6,Bs7+1:Be7,Bs8+1:Be8)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      do i7=Bs7+1,Be7
      do i8=Bs8+1,Be8
      B(i1,i2,i5,i3,i4,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder462135(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs4,Be4,Bs6,Be6,Bs2,Be2,Bs1,Be1,Bs3,Be3,Bs5,Be5,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs4+1:Be4,Bs6+1:Be6,Bs2+1:Be2,Bs1+1:Be1,
     & Bs3+1:Be3,Bs5+1:Be5)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i4,i6,i2,i1,i3,i5)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder3241(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & Bs3,Be3,Bs2,Be2,Bs4,Be4,Bs1,Be1,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4
      integer i1,i2,i3,i4

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real(kind=8) B(Bs3+1:Be3,Bs2+1:Be2,Bs4+1:Be4,Bs1+1:Be1)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      B(i3,i2,i4,i1)=A(i1,i2,i3,i4)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder12634578(As1,Ae1,As2,Ae2,As3,Ae3,As4,
     & Ae4,As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,Bs1,Be1,Bs2,Be2,Bs6,Be6,Bs3,
     & Be3,Bs4,Be4,Bs5,Be5,Bs7,Be7,Bs8,Be8,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6,
     & Bs7,Be7,Bs8,Be8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(Bs1+1:Be1,Bs2+1:Be2,Bs6+1:Be6,Bs3+1:Be3,
     & Bs4+1:Be4,Bs5+1:Be5,Bs7+1:Be7,Bs8+1:Be8)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      do i7=Bs7+1,Be7
      do i8=Bs8+1,Be8
      B(i1,i2,i6,i3,i4,i5,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder1234(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4
      integer i1,i2,i3,i4

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real(kind=8) B(Bs1+1:Be1,Bs2+1:Be2,Bs3+1:Be3,Bs4+1:Be4)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      B(i1,i2,i3,i4)=A(i1,i2,i3,i4)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder213456(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs2,Be2,Bs1,Be1,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs2+1:Be2,Bs1+1:Be1,Bs3+1:Be3,Bs4+1:Be4,
     & Bs5+1:Be5,Bs6+1:Be6)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i2,i1,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder431256(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs4,Be4,Bs3,Be3,Bs1,Be1,Bs2,Be2,Bs5,Be5,Bs6,Be6,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs4+1:Be4,Bs3+1:Be3,Bs1+1:Be1,Bs2+1:Be2,
     & Bs5+1:Be5,Bs6+1:Be6)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i4,i3,i1,i2,i5,i6)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder21(As1,Ae1,As2,Ae2,Bs2,Be2,Bs1,Be1,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2
      integer Bs1,Be1,Bs2,Be2
      integer i1,i2

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2)
      real(kind=8) B(Bs2+1:Be2,Bs1+1:Be1)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      B(i2,i1)=A(i1,i2)
      enddo
      enddo

      end


      subroutine reorder645123(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs6,Be6,Bs4,Be4,Bs5,Be5,Bs1,Be1,Bs2,Be2,Bs3,Be3,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs6+1:Be6,Bs4+1:Be4,Bs5+1:Be5,Bs1+1:Be1,
     & Bs2+1:Be2,Bs3+1:Be3)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i6,i4,i5,i1,i2,i3)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder641235(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs6,Be6,Bs4,Be4,Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs5,Be5,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs6+1:Be6,Bs4+1:Be4,Bs1+1:Be1,Bs2+1:Be2,
     & Bs3+1:Be3,Bs5+1:Be5)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i6,i4,i1,i2,i3,i5)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder125346(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs1,Be1,Bs2,Be2,Bs5,Be5,Bs3,Be3,Bs4,Be4,Bs6,Be6,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs1+1:Be1,Bs2+1:Be2,Bs5+1:Be5,Bs3+1:Be3,
     & Bs4+1:Be4,Bs6+1:Be6)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i1,i2,i5,i3,i4,i6)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder71234568(As1,Ae1,As2,Ae2,As3,Ae3,As4,
     & Ae4,As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,Bs7,Be7,Bs1,Be1,Bs2,Be2,Bs3,
     & Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6,Bs8,Be8,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6,
     & Bs7,Be7,Bs8,Be8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(Bs7+1:Be7,Bs1+1:Be1,Bs2+1:Be2,Bs3+1:Be3,
     & Bs4+1:Be4,Bs5+1:Be5,Bs6+1:Be6,Bs8+1:Be8)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      do i7=Bs7+1,Be7
      do i8=Bs8+1,Be8
      B(i7,i1,i2,i3,i4,i5,i6,i8)=A(i1,i2,i3,i4,i5,i6,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder523146(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs5,Be5,Bs2,Be2,Bs3,Be3,Bs1,Be1,Bs4,Be4,Bs6,Be6,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs5+1:Be5,Bs2+1:Be2,Bs3+1:Be3,Bs1+1:Be1,
     & Bs4+1:Be4,Bs6+1:Be6)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i5,i2,i3,i1,i4,i6)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder58123467(As1,Ae1,As2,Ae2,As3,Ae3,As4,
     & Ae4,As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,Bs5,Be5,Bs8,Be8,Bs1,Be1,Bs2,
     & Be2,Bs3,Be3,Bs4,Be4,Bs6,Be6,Bs7,Be7,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6,
     & Bs7,Be7,Bs8,Be8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(Bs5+1:Be5,Bs8+1:Be8,Bs1+1:Be1,Bs2+1:Be2,
     & Bs3+1:Be3,Bs4+1:Be4,Bs6+1:Be6,Bs7+1:Be7)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      do i7=Bs7+1,Be7
      do i8=Bs8+1,Be8
      B(i5,i8,i1,i2,i3,i4,i6,i7)=A(i1,i2,i3,i4,i5,i6,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder451236(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs4,Be4,Bs5,Be5,Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs6,Be6,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs4+1:Be4,Bs5+1:Be5,Bs1+1:Be1,Bs2+1:Be2,
     & Bs3+1:Be3,Bs6+1:Be6)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i4,i5,i1,i2,i3,i6)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder1324(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & Bs1,Be1,Bs3,Be3,Bs2,Be2,Bs4,Be4,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4
      integer i1,i2,i3,i4

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real(kind=8) B(Bs1+1:Be1,Bs3+1:Be3,Bs2+1:Be2,Bs4+1:Be4)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      B(i1,i3,i2,i4)=A(i1,i2,i3,i4)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder13724568(As1,Ae1,As2,Ae2,As3,Ae3,As4,
     & Ae4,As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,Bs1,Be1,Bs3,Be3,Bs7,Be7,Bs2,
     & Be2,Bs4,Be4,Bs5,Be5,Bs6,Be6,Bs8,Be8,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6,
     & Bs7,Be7,Bs8,Be8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(Bs1+1:Be1,Bs3+1:Be3,Bs7+1:Be7,Bs2+1:Be2,
     & Bs4+1:Be4,Bs5+1:Be5,Bs6+1:Be6,Bs8+1:Be8)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      do i7=Bs7+1,Be7
      do i8=Bs8+1,Be8
      B(i1,i3,i7,i2,i4,i5,i6,i8)=A(i1,i2,i3,i4,i5,i6,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder2134(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & Bs2,Be2,Bs1,Be1,Bs3,Be3,Bs4,Be4,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4
      integer i1,i2,i3,i4

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real(kind=8) B(Bs2+1:Be2,Bs1+1:Be1,Bs3+1:Be3,Bs4+1:Be4)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      B(i2,i1,i3,i4)=A(i1,i2,i3,i4)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder142356(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs1,Be1,Bs4,Be4,Bs2,Be2,Bs3,Be3,Bs5,Be5,Bs6,Be6,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs1+1:Be1,Bs4+1:Be4,Bs2+1:Be2,Bs3+1:Be3,
     & Bs5+1:Be5,Bs6+1:Be6)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i1,i4,i2,i3,i5,i6)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder452316(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs4,Be4,Bs5,Be5,Bs2,Be2,Bs3,Be3,Bs1,Be1,Bs6,Be6,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs4+1:Be4,Bs5+1:Be5,Bs2+1:Be2,Bs3+1:Be3,
     & Bs1+1:Be1,Bs6+1:Be6)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i4,i5,i2,i3,i1,i6)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder1432(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & Bs1,Be1,Bs4,Be4,Bs3,Be3,Bs2,Be2,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4
      integer i1,i2,i3,i4

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real(kind=8) B(Bs1+1:Be1,Bs4+1:Be4,Bs3+1:Be3,Bs2+1:Be2)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      B(i1,i4,i3,i2)=A(i1,i2,i3,i4)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder623451(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs6,Be6,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs1,Be1,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs6+1:Be6,Bs2+1:Be2,Bs3+1:Be3,Bs4+1:Be4,
     & Bs5+1:Be5,Bs1+1:Be1)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i6,i2,i3,i4,i5,i1)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder546123(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs5,Be5,Bs4,Be4,Bs6,Be6,Bs1,Be1,Bs2,Be2,Bs3,Be3,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs5+1:Be5,Bs4+1:Be4,Bs6+1:Be6,Bs1+1:Be1,
     & Bs2+1:Be2,Bs3+1:Be3)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i5,i4,i6,i1,i2,i3)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder4132(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & Bs4,Be4,Bs1,Be1,Bs3,Be3,Bs2,Be2,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4
      integer i1,i2,i3,i4

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real(kind=8) B(Bs4+1:Be4,Bs1+1:Be1,Bs3+1:Be3,Bs2+1:Be2)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      B(i4,i1,i3,i2)=A(i1,i2,i3,i4)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder465123(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs4,Be4,Bs6,Be6,Bs5,Be5,Bs1,Be1,Bs2,Be2,Bs3,Be3,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs4+1:Be4,Bs6+1:Be6,Bs5+1:Be5,Bs1+1:Be1,
     & Bs2+1:Be2,Bs3+1:Be3)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i4,i6,i5,i1,i2,i3)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder51234678(As1,Ae1,As2,Ae2,As3,Ae3,As4,
     & Ae4,As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,Bs5,Be5,Bs1,Be1,Bs2,Be2,Bs3,
     & Be3,Bs4,Be4,Bs6,Be6,Bs7,Be7,Bs8,Be8,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6,
     & Bs7,Be7,Bs8,Be8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(Bs5+1:Be5,Bs1+1:Be1,Bs2+1:Be2,Bs3+1:Be3,
     & Bs4+1:Be4,Bs6+1:Be6,Bs7+1:Be7,Bs8+1:Be8)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      do i7=Bs7+1,Be7
      do i8=Bs8+1,Be8
      B(i5,i1,i2,i3,i4,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder2143(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & Bs2,Be2,Bs1,Be1,Bs4,Be4,Bs3,Be3,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4
      integer i1,i2,i3,i4

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real(kind=8) B(Bs2+1:Be2,Bs1+1:Be1,Bs4+1:Be4,Bs3+1:Be3)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      B(i2,i1,i4,i3)=A(i1,i2,i3,i4)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder561234(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs5,Be5,Bs6,Be6,Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs5+1:Be5,Bs6+1:Be6,Bs1+1:Be1,Bs2+1:Be2,
     & Bs3+1:Be3,Bs4+1:Be4)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i5,i6,i1,i2,i3,i4)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder58142367(As1,Ae1,As2,Ae2,As3,Ae3,As4,
     & Ae4,As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,Bs5,Be5,Bs8,Be8,Bs1,Be1,Bs4,
     & Be4,Bs2,Be2,Bs3,Be3,Bs6,Be6,Bs7,Be7,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6,
     & Bs7,Be7,Bs8,Be8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(Bs5+1:Be5,Bs8+1:Be8,Bs1+1:Be1,Bs4+1:Be4,
     & Bs2+1:Be2,Bs3+1:Be3,Bs6+1:Be6,Bs7+1:Be7)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      do i7=Bs7+1,Be7
      do i8=Bs8+1,Be8
      B(i5,i8,i1,i4,i2,i3,i6,i7)=A(i1,i2,i3,i4,i5,i6,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder154236(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs1,Be1,Bs5,Be5,Bs4,Be4,Bs2,Be2,Bs3,Be3,Bs6,Be6,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs1+1:Be1,Bs5+1:Be5,Bs4+1:Be4,Bs2+1:Be2,
     & Bs3+1:Be3,Bs6+1:Be6)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i1,i5,i4,i2,i3,i6)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder14523678(As1,Ae1,As2,Ae2,As3,Ae3,As4,
     & Ae4,As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,Bs1,Be1,Bs4,Be4,Bs5,Be5,Bs2,
     & Be2,Bs3,Be3,Bs6,Be6,Bs7,Be7,Bs8,Be8,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6,
     & Bs7,Be7,Bs8,Be8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(Bs1+1:Be1,Bs4+1:Be4,Bs5+1:Be5,Bs2+1:Be2,
     & Bs3+1:Be3,Bs6+1:Be6,Bs7+1:Be7,Bs8+1:Be8)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      do i7=Bs7+1,Be7
      do i8=Bs8+1,Be8
      B(i1,i4,i5,i2,i3,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder512346(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs5,Be5,Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs6,Be6,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs5+1:Be5,Bs1+1:Be1,Bs2+1:Be2,Bs3+1:Be3,
     & Bs4+1:Be4,Bs6+1:Be6)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i5,i1,i2,i3,i4,i6)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder415623(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs4,Be4,Bs1,Be1,Bs5,Be5,Bs6,Be6,Bs2,Be2,Bs3,Be3,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs4+1:Be4,Bs1+1:Be1,Bs5+1:Be5,Bs6+1:Be6,
     & Bs2+1:Be2,Bs3+1:Be3)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i4,i1,i5,i6,i2,i3)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder71523468(As1,Ae1,As2,Ae2,As3,Ae3,As4,
     & Ae4,As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,Bs7,Be7,Bs1,Be1,Bs5,Be5,Bs2,
     & Be2,Bs3,Be3,Bs4,Be4,Bs6,Be6,Bs8,Be8,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6,
     & Bs7,Be7,Bs8,Be8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(Bs7+1:Be7,Bs1+1:Be1,Bs5+1:Be5,Bs2+1:Be2,
     & Bs3+1:Be3,Bs4+1:Be4,Bs6+1:Be6,Bs8+1:Be8)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      do i7=Bs7+1,Be7
      do i8=Bs8+1,Be8
      B(i7,i1,i5,i2,i3,i4,i6,i8)=A(i1,i2,i3,i4,i5,i6,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder132456(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs1,Be1,Bs3,Be3,Bs2,Be2,Bs4,Be4,Bs5,Be5,Bs6,Be6,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs1+1:Be1,Bs3+1:Be3,Bs2+1:Be2,Bs4+1:Be4,
     & Bs5+1:Be5,Bs6+1:Be6)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i1,i3,i2,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder54123678(As1,Ae1,As2,Ae2,As3,Ae3,As4,
     & Ae4,As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,Bs5,Be5,Bs4,Be4,Bs1,Be1,Bs2,
     & Be2,Bs3,Be3,Bs6,Be6,Bs7,Be7,Bs8,Be8,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6,
     & Bs7,Be7,Bs8,Be8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(Bs5+1:Be5,Bs4+1:Be4,Bs1+1:Be1,Bs2+1:Be2,
     & Bs3+1:Be3,Bs6+1:Be6,Bs7+1:Be7,Bs8+1:Be8)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      do i7=Bs7+1,Be7
      do i8=Bs8+1,Be8
      B(i5,i4,i1,i2,i3,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder613245(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs6,Be6,Bs1,Be1,Bs3,Be3,Bs2,Be2,Bs4,Be4,Bs5,Be5,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs6+1:Be6,Bs1+1:Be1,Bs3+1:Be3,Bs2+1:Be2,
     & Bs4+1:Be4,Bs5+1:Be5)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i6,i1,i3,i2,i4,i5)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder53124678(As1,Ae1,As2,Ae2,As3,Ae3,As4,
     & Ae4,As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,Bs5,Be5,Bs3,Be3,Bs1,Be1,Bs2,
     & Be2,Bs4,Be4,Bs6,Be6,Bs7,Be7,Bs8,Be8,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6,
     & Bs7,Be7,Bs8,Be8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(Bs5+1:Be5,Bs3+1:Be3,Bs1+1:Be1,Bs2+1:Be2,
     & Bs4+1:Be4,Bs6+1:Be6,Bs7+1:Be7,Bs8+1:Be8)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      do i7=Bs7+1,Be7
      do i8=Bs8+1,Be8
      B(i5,i3,i1,i2,i4,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder162345(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs1,Be1,Bs6,Be6,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs1+1:Be1,Bs6+1:Be6,Bs2+1:Be2,Bs3+1:Be3,
     & Bs4+1:Be4,Bs5+1:Be5)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i1,i6,i2,i3,i4,i5)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder361245(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs3,Be3,Bs6,Be6,Bs1,Be1,Bs2,Be2,Bs4,Be4,Bs5,Be5,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs3+1:Be3,Bs6+1:Be6,Bs1+1:Be1,Bs2+1:Be2,
     & Bs4+1:Be4,Bs5+1:Be5)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i3,i6,i1,i2,i4,i5)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder516234(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs5,Be5,Bs1,Be1,Bs6,Be6,Bs2,Be2,Bs3,Be3,Bs4,Be4,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs5+1:Be5,Bs1+1:Be1,Bs6+1:Be6,Bs2+1:Be2,
     & Bs3+1:Be3,Bs4+1:Be4)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i5,i1,i6,i2,i3,i4)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder241356(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs2,Be2,Bs4,Be4,Bs1,Be1,Bs3,Be3,Bs5,Be5,Bs6,Be6,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs2+1:Be2,Bs4+1:Be4,Bs1+1:Be1,Bs3+1:Be3,
     & Bs5+1:Be5,Bs6+1:Be6)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i2,i4,i1,i3,i5,i6)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder463125(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs4,Be4,Bs6,Be6,Bs3,Be3,Bs1,Be1,Bs2,Be2,Bs5,Be5,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs4+1:Be4,Bs6+1:Be6,Bs3+1:Be3,Bs1+1:Be1,
     & Bs2+1:Be2,Bs5+1:Be5)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i4,i6,i3,i1,i2,i5)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder251346(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs2,Be2,Bs5,Be5,Bs1,Be1,Bs3,Be3,Bs4,Be4,Bs6,Be6,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs2+1:Be2,Bs5+1:Be5,Bs1+1:Be1,Bs3+1:Be3,
     & Bs4+1:Be4,Bs6+1:Be6)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i2,i5,i1,i3,i4,i6)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder62134578(As1,Ae1,As2,Ae2,As3,Ae3,As4,
     & Ae4,As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,Bs6,Be6,Bs2,Be2,Bs1,Be1,Bs3,
     & Be3,Bs4,Be4,Bs5,Be5,Bs7,Be7,Bs8,Be8,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6,
     & Bs7,Be7,Bs8,Be8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(Bs6+1:Be6,Bs2+1:Be2,Bs1+1:Be1,Bs3+1:Be3,
     & Bs4+1:Be4,Bs5+1:Be5,Bs7+1:Be7,Bs8+1:Be8)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      do i7=Bs7+1,Be7
      do i8=Bs8+1,Be8
      B(i6,i2,i1,i3,i4,i5,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder14823567(As1,Ae1,As2,Ae2,As3,Ae3,As4,
     & Ae4,As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,Bs1,Be1,Bs4,Be4,Bs8,Be8,Bs2,
     & Be2,Bs3,Be3,Bs5,Be5,Bs6,Be6,Bs7,Be7,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6,
     & Bs7,Be7,Bs8,Be8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(Bs1+1:Be1,Bs4+1:Be4,Bs8+1:Be8,Bs2+1:Be2,
     & Bs3+1:Be3,Bs5+1:Be5,Bs6+1:Be6,Bs7+1:Be7)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      do i7=Bs7+1,Be7
      do i8=Bs8+1,Be8
      B(i1,i4,i8,i2,i3,i5,i6,i7)=A(i1,i2,i3,i4,i5,i6,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder3412(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & Bs3,Be3,Bs4,Be4,Bs1,Be1,Bs2,Be2,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4
      integer i1,i2,i3,i4

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real(kind=8) B(Bs3+1:Be3,Bs4+1:Be4,Bs1+1:Be1,Bs2+1:Be2)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      B(i3,i4,i1,i2)=A(i1,i2,i3,i4)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder1423(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & Bs1,Be1,Bs4,Be4,Bs2,Be2,Bs3,Be3,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4
      integer i1,i2,i3,i4

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real(kind=8) B(Bs1+1:Be1,Bs4+1:Be4,Bs2+1:Be2,Bs3+1:Be3)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      B(i1,i4,i2,i3)=A(i1,i2,i3,i4)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder51324678(As1,Ae1,As2,Ae2,As3,Ae3,As4,
     & Ae4,As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,Bs5,Be5,Bs1,Be1,Bs3,Be3,Bs2,
     & Be2,Bs4,Be4,Bs6,Be6,Bs7,Be7,Bs8,Be8,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6,
     & Bs7,Be7,Bs8,Be8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(Bs5+1:Be5,Bs1+1:Be1,Bs3+1:Be3,Bs2+1:Be2,
     & Bs4+1:Be4,Bs6+1:Be6,Bs7+1:Be7,Bs8+1:Be8)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      do i7=Bs7+1,Be7
      do i8=Bs8+1,Be8
      B(i5,i1,i3,i2,i4,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder415236(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs4,Be4,Bs1,Be1,Bs5,Be5,Bs2,Be2,Bs3,Be3,Bs6,Be6,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs4+1:Be4,Bs1+1:Be1,Bs5+1:Be5,Bs2+1:Be2,
     & Bs3+1:Be3,Bs6+1:Be6)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i4,i1,i5,i2,i3,i6)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder462315(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs4,Be4,Bs6,Be6,Bs2,Be2,Bs3,Be3,Bs1,Be1,Bs5,Be5,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs4+1:Be4,Bs6+1:Be6,Bs2+1:Be2,Bs3+1:Be3,
     & Bs1+1:Be1,Bs5+1:Be5)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i4,i6,i2,i3,i1,i5)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder562134(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs5,Be5,Bs6,Be6,Bs2,Be2,Bs1,Be1,Bs3,Be3,Bs4,Be4,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs5+1:Be5,Bs6+1:Be6,Bs2+1:Be2,Bs1+1:Be1,
     & Bs3+1:Be3,Bs4+1:Be4)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i5,i6,i2,i1,i3,i4)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder651234(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs6,Be6,Bs5,Be5,Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs6+1:Be6,Bs5+1:Be5,Bs1+1:Be1,Bs2+1:Be2,
     & Bs3+1:Be3,Bs4+1:Be4)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i6,i5,i1,i2,i3,i4)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder81523467(As1,Ae1,As2,Ae2,As3,Ae3,As4,
     & Ae4,As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,Bs8,Be8,Bs1,Be1,Bs5,Be5,Bs2,
     & Be2,Bs3,Be3,Bs4,Be4,Bs6,Be6,Bs7,Be7,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6,
     & Bs7,Be7,Bs8,Be8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(Bs8+1:Be8,Bs1+1:Be1,Bs5+1:Be5,Bs2+1:Be2,
     & Bs3+1:Be3,Bs4+1:Be4,Bs6+1:Be6,Bs7+1:Be7)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      do i7=Bs7+1,Be7
      do i8=Bs8+1,Be8
      B(i8,i1,i5,i2,i3,i4,i6,i7)=A(i1,i2,i3,i4,i5,i6,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder456123(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs4,Be4,Bs5,Be5,Bs6,Be6,Bs1,Be1,Bs2,Be2,Bs3,Be3,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs4+1:Be4,Bs5+1:Be5,Bs6+1:Be6,Bs1+1:Be1,
     & Bs2+1:Be2,Bs3+1:Be3)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i4,i5,i6,i1,i2,i3)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder623415(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs6,Be6,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs1,Be1,Bs5,Be5,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs6+1:Be6,Bs2+1:Be2,Bs3+1:Be3,Bs4+1:Be4,
     & Bs1+1:Be1,Bs5+1:Be5)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i6,i2,i3,i4,i1,i5)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder461235(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs4,Be4,Bs6,Be6,Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs5,Be5,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs4+1:Be4,Bs6+1:Be6,Bs1+1:Be1,Bs2+1:Be2,
     & Bs3+1:Be3,Bs5+1:Be5)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i4,i6,i1,i2,i3,i5)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder78123456(As1,Ae1,As2,Ae2,As3,Ae3,As4,
     & Ae4,As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,Bs7,Be7,Bs8,Be8,Bs1,Be1,Bs2,
     & Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6,
     & Bs7,Be7,Bs8,Be8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(Bs7+1:Be7,Bs8+1:Be8,Bs1+1:Be1,Bs2+1:Be2,
     & Bs3+1:Be3,Bs4+1:Be4,Bs5+1:Be5,Bs6+1:Be6)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      do i7=Bs7+1,Be7
      do i8=Bs8+1,Be8
      B(i7,i8,i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder123456(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs1+1:Be1,Bs2+1:Be2,Bs3+1:Be3,Bs4+1:Be4,
     & Bs5+1:Be5,Bs6+1:Be6)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder56213478(As1,Ae1,As2,Ae2,As3,Ae3,As4,
     & Ae4,As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,Bs5,Be5,Bs6,Be6,Bs2,Be2,Bs1,
     & Be1,Bs3,Be3,Bs4,Be4,Bs7,Be7,Bs8,Be8,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6,
     & Bs7,Be7,Bs8,Be8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(Bs5+1:Be5,Bs6+1:Be6,Bs2+1:Be2,Bs1+1:Be1,
     & Bs3+1:Be3,Bs4+1:Be4,Bs7+1:Be7,Bs8+1:Be8)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      do i7=Bs7+1,Be7
      do i8=Bs8+1,Be8
      B(i5,i6,i2,i1,i3,i4,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder2431(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & Bs2,Be2,Bs4,Be4,Bs3,Be3,Bs1,Be1,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4
      integer i1,i2,i3,i4

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real(kind=8) B(Bs2+1:Be2,Bs4+1:Be4,Bs3+1:Be3,Bs1+1:Be1)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      B(i2,i4,i3,i1)=A(i1,i2,i3,i4)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder1243(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & Bs1,Be1,Bs2,Be2,Bs4,Be4,Bs3,Be3,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4
      integer i1,i2,i3,i4

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real(kind=8) B(Bs1+1:Be1,Bs2+1:Be2,Bs4+1:Be4,Bs3+1:Be3)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      B(i1,i2,i4,i3)=A(i1,i2,i3,i4)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder615234(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs6,Be6,Bs1,Be1,Bs5,Be5,Bs2,Be2,Bs3,Be3,Bs4,Be4,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs6+1:Be6,Bs1+1:Be1,Bs5+1:Be5,Bs2+1:Be2,
     & Bs3+1:Be3,Bs4+1:Be4)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i6,i1,i5,i2,i3,i4)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder57123468(As1,Ae1,As2,Ae2,As3,Ae3,As4,
     & Ae4,As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,Bs5,Be5,Bs7,Be7,Bs1,Be1,Bs2,
     & Be2,Bs3,Be3,Bs4,Be4,Bs6,Be6,Bs8,Be8,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6,
     & Bs7,Be7,Bs8,Be8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(Bs5+1:Be5,Bs7+1:Be7,Bs1+1:Be1,Bs2+1:Be2,
     & Bs3+1:Be3,Bs4+1:Be4,Bs6+1:Be6,Bs8+1:Be8)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      do i7=Bs7+1,Be7
      do i8=Bs8+1,Be8
      B(i5,i7,i1,i2,i3,i4,i6,i8)=A(i1,i2,i3,i4,i5,i6,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder41235678(As1,Ae1,As2,Ae2,As3,Ae3,As4,
     & Ae4,As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,Bs4,Be4,Bs1,Be1,Bs2,Be2,Bs3,
     & Be3,Bs5,Be5,Bs6,Be6,Bs7,Be7,Bs8,Be8,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6,
     & Bs7,Be7,Bs8,Be8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(Bs4+1:Be4,Bs1+1:Be1,Bs2+1:Be2,Bs3+1:Be3,
     & Bs5+1:Be5,Bs6+1:Be6,Bs7+1:Be7,Bs8+1:Be8)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      do i7=Bs7+1,Be7
      do i8=Bs8+1,Be8
      B(i4,i1,i2,i3,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder3124(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & Bs3,Be3,Bs1,Be1,Bs2,Be2,Bs4,Be4,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4
      integer i1,i2,i3,i4

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real(kind=8) B(Bs3+1:Be3,Bs1+1:Be1,Bs2+1:Be2,Bs4+1:Be4)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      B(i3,i1,i2,i4)=A(i1,i2,i3,i4)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder413256(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs4,Be4,Bs1,Be1,Bs3,Be3,Bs2,Be2,Bs5,Be5,Bs6,Be6,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs4+1:Be4,Bs1+1:Be1,Bs3+1:Be3,Bs2+1:Be2,
     & Bs5+1:Be5,Bs6+1:Be6)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i4,i1,i3,i2,i5,i6)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder4231(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & Bs4,Be4,Bs2,Be2,Bs3,Be3,Bs1,Be1,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4
      integer i1,i2,i3,i4

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real(kind=8) B(Bs4+1:Be4,Bs2+1:Be2,Bs3+1:Be3,Bs1+1:Be1)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      B(i4,i2,i3,i1)=A(i1,i2,i3,i4)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder31245678(As1,Ae1,As2,Ae2,As3,Ae3,As4,
     & Ae4,As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,Bs3,Be3,Bs1,Be1,Bs2,Be2,Bs4,
     & Be4,Bs5,Be5,Bs6,Be6,Bs7,Be7,Bs8,Be8,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6,
     & Bs7,Be7,Bs8,Be8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(Bs3+1:Be3,Bs1+1:Be1,Bs2+1:Be2,Bs4+1:Be4,
     & Bs5+1:Be5,Bs6+1:Be6,Bs7+1:Be7,Bs8+1:Be8)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      do i7=Bs7+1,Be7
      do i8=Bs8+1,Be8
      B(i3,i1,i2,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder78341256(As1,Ae1,As2,Ae2,As3,Ae3,As4,
     & Ae4,As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,Bs7,Be7,Bs8,Be8,Bs3,Be3,Bs4,
     & Be4,Bs1,Be1,Bs2,Be2,Bs5,Be5,Bs6,Be6,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6,
     & Bs7,Be7,Bs8,Be8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(Bs7+1:Be7,Bs8+1:Be8,Bs3+1:Be3,Bs4+1:Be4,
     & Bs1+1:Be1,Bs2+1:Be2,Bs5+1:Be5,Bs6+1:Be6)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      do i7=Bs7+1,Be7
      do i8=Bs8+1,Be8
      B(i7,i8,i3,i4,i1,i2,i5,i6)=A(i1,i2,i3,i4,i5,i6,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder21345678(As1,Ae1,As2,Ae2,As3,Ae3,As4,
     & Ae4,As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,Bs2,Be2,Bs1,Be1,Bs3,Be3,Bs4,
     & Be4,Bs5,Be5,Bs6,Be6,Bs7,Be7,Bs8,Be8,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6,
     & Bs7,Be7,Bs8,Be8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(Bs2+1:Be2,Bs1+1:Be1,Bs3+1:Be3,Bs4+1:Be4,
     & Bs5+1:Be5,Bs6+1:Be6,Bs7+1:Be7,Bs8+1:Be8)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      do i7=Bs7+1,Be7
      do i8=Bs8+1,Be8
      B(i2,i1,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder73512468(As1,Ae1,As2,Ae2,As3,Ae3,As4,
     & Ae4,As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,Bs7,Be7,Bs3,Be3,Bs5,Be5,Bs1,
     & Be1,Bs2,Be2,Bs4,Be4,Bs6,Be6,Bs8,Be8,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6,
     & Bs7,Be7,Bs8,Be8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(Bs7+1:Be7,Bs3+1:Be3,Bs5+1:Be5,Bs1+1:Be1,
     & Bs2+1:Be2,Bs4+1:Be4,Bs6+1:Be6,Bs8+1:Be8)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      do i7=Bs7+1,Be7
      do i8=Bs8+1,Be8
      B(i7,i3,i5,i1,i2,i4,i6,i8)=A(i1,i2,i3,i4,i5,i6,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder164235(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs1,Be1,Bs6,Be6,Bs4,Be4,Bs2,Be2,Bs3,Be3,Bs5,Be5,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs1+1:Be1,Bs6+1:Be6,Bs4+1:Be4,Bs2+1:Be2,
     & Bs3+1:Be3,Bs5+1:Be5)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i1,i6,i4,i2,i3,i5)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder34125678(As1,Ae1,As2,Ae2,As3,Ae3,As4,
     & Ae4,As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,Bs3,Be3,Bs4,Be4,Bs1,Be1,Bs2,
     & Be2,Bs5,Be5,Bs6,Be6,Bs7,Be7,Bs8,Be8,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6,
     & Bs7,Be7,Bs8,Be8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(Bs3+1:Be3,Bs4+1:Be4,Bs1+1:Be1,Bs2+1:Be2,
     & Bs5+1:Be5,Bs6+1:Be6,Bs7+1:Be7,Bs8+1:Be8)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      do i7=Bs7+1,Be7
      do i8=Bs8+1,Be8
      B(i3,i4,i1,i2,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder412356(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs4,Be4,Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs5,Be5,Bs6,Be6,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs4+1:Be4,Bs1+1:Be1,Bs2+1:Be2,Bs3+1:Be3,
     & Bs5+1:Be5,Bs6+1:Be6)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i4,i1,i2,i3,i5,i6)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder34712568(As1,Ae1,As2,Ae2,As3,Ae3,As4,
     & Ae4,As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,Bs3,Be3,Bs4,Be4,Bs7,Be7,Bs1,
     & Be1,Bs2,Be2,Bs5,Be5,Bs6,Be6,Bs8,Be8,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6,
     & Bs7,Be7,Bs8,Be8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(Bs3+1:Be3,Bs4+1:Be4,Bs7+1:Be7,Bs1+1:Be1,
     & Bs2+1:Be2,Bs5+1:Be5,Bs6+1:Be6,Bs8+1:Be8)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      do i7=Bs7+1,Be7
      do i8=Bs8+1,Be8
      B(i3,i4,i7,i1,i2,i5,i6,i8)=A(i1,i2,i3,i4,i5,i6,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder3421(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & Bs3,Be3,Bs4,Be4,Bs2,Be2,Bs1,Be1,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4
      integer i1,i2,i3,i4

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real(kind=8) B(Bs3+1:Be3,Bs4+1:Be4,Bs2+1:Be2,Bs1+1:Be1)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      B(i3,i4,i2,i1)=A(i1,i2,i3,i4)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder61523478(As1,Ae1,As2,Ae2,As3,Ae3,As4,
     & Ae4,As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,Bs6,Be6,Bs1,Be1,Bs5,Be5,Bs2,
     & Be2,Bs3,Be3,Bs4,Be4,Bs7,Be7,Bs8,Be8,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6,
     & Bs7,Be7,Bs8,Be8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(Bs6+1:Be6,Bs1+1:Be1,Bs5+1:Be5,Bs2+1:Be2,
     & Bs3+1:Be3,Bs4+1:Be4,Bs7+1:Be7,Bs8+1:Be8)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      do i7=Bs7+1,Be7
      do i8=Bs8+1,Be8
      B(i6,i1,i5,i2,i3,i4,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder134256(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs1,Be1,Bs3,Be3,Bs4,Be4,Bs2,Be2,Bs5,Be5,Bs6,Be6,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs1+1:Be1,Bs3+1:Be3,Bs4+1:Be4,Bs2+1:Be2,
     & Bs5+1:Be5,Bs6+1:Be6)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i1,i3,i4,i2,i5,i6)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder84512367(As1,Ae1,As2,Ae2,As3,Ae3,As4,
     & Ae4,As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,Bs8,Be8,Bs4,Be4,Bs5,Be5,Bs1,
     & Be1,Bs2,Be2,Bs3,Be3,Bs6,Be6,Bs7,Be7,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6,
     & Bs7,Be7,Bs8,Be8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(Bs8+1:Be8,Bs4+1:Be4,Bs5+1:Be5,Bs1+1:Be1,
     & Bs2+1:Be2,Bs3+1:Be3,Bs6+1:Be6,Bs7+1:Be7)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      do i7=Bs7+1,Be7
      do i8=Bs8+1,Be8
      B(i8,i4,i5,i1,i2,i3,i6,i7)=A(i1,i2,i3,i4,i5,i6,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder81423567(As1,Ae1,As2,Ae2,As3,Ae3,As4,
     & Ae4,As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,Bs8,Be8,Bs1,Be1,Bs4,Be4,Bs2,
     & Be2,Bs3,Be3,Bs5,Be5,Bs6,Be6,Bs7,Be7,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6,
     & Bs7,Be7,Bs8,Be8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(Bs8+1:Be8,Bs1+1:Be1,Bs4+1:Be4,Bs2+1:Be2,
     & Bs3+1:Be3,Bs5+1:Be5,Bs6+1:Be6,Bs7+1:Be7)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      do i7=Bs7+1,Be7
      do i8=Bs8+1,Be8
      B(i8,i1,i4,i2,i3,i5,i6,i7)=A(i1,i2,i3,i4,i5,i6,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder261345(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs2,Be2,Bs6,Be6,Bs1,Be1,Bs3,Be3,Bs4,Be4,Bs5,Be5,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs2+1:Be2,Bs6+1:Be6,Bs1+1:Be1,Bs3+1:Be3,
     & Bs4+1:Be4,Bs5+1:Be5)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i2,i6,i1,i3,i4,i5)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder136245(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs1,Be1,Bs3,Be3,Bs6,Be6,Bs2,Be2,Bs4,Be4,Bs5,Be5,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs1+1:Be1,Bs3+1:Be3,Bs6+1:Be6,Bs2+1:Be2,
     & Bs4+1:Be4,Bs5+1:Be5)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i1,i3,i6,i2,i4,i5)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder81234567(As1,Ae1,As2,Ae2,As3,Ae3,As4,
     & Ae4,As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,Bs8,Be8,Bs1,Be1,Bs2,Be2,Bs3,
     & Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6,Bs7,Be7,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6,
     & Bs7,Be7,Bs8,Be8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(Bs8+1:Be8,Bs1+1:Be1,Bs2+1:Be2,Bs3+1:Be3,
     & Bs4+1:Be4,Bs5+1:Be5,Bs6+1:Be6,Bs7+1:Be7)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      do i7=Bs7+1,Be7
      do i8=Bs8+1,Be8
      B(i8,i1,i2,i3,i4,i5,i6,i7)=A(i1,i2,i3,i4,i5,i6,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder513246(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs5,Be5,Bs1,Be1,Bs3,Be3,Bs2,Be2,Bs4,Be4,Bs6,Be6,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs5+1:Be5,Bs1+1:Be1,Bs3+1:Be3,Bs2+1:Be2,
     & Bs4+1:Be4,Bs6+1:Be6)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i5,i1,i3,i2,i4,i6)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder2314(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & Bs2,Be2,Bs3,Be3,Bs1,Be1,Bs4,Be4,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4
      integer i1,i2,i3,i4

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real(kind=8) B(Bs2+1:Be2,Bs3+1:Be3,Bs1+1:Be1,Bs4+1:Be4)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      B(i2,i3,i1,i4)=A(i1,i2,i3,i4)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder67231458(As1,Ae1,As2,Ae2,As3,Ae3,As4,
     & Ae4,As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,Bs6,Be6,Bs7,Be7,Bs2,Be2,Bs3,
     & Be3,Bs1,Be1,Bs4,Be4,Bs5,Be5,Bs8,Be8,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6,
     & Bs7,Be7,Bs8,Be8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(Bs6+1:Be6,Bs7+1:Be7,Bs2+1:Be2,Bs3+1:Be3,
     & Bs1+1:Be1,Bs4+1:Be4,Bs5+1:Be5,Bs8+1:Be8)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      do i7=Bs7+1,Be7
      do i8=Bs8+1,Be8
      B(i6,i7,i2,i3,i1,i4,i5,i8)=A(i1,i2,i3,i4,i5,i6,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder57132468(As1,Ae1,As2,Ae2,As3,Ae3,As4,
     & Ae4,As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,Bs5,Be5,Bs7,Be7,Bs1,Be1,Bs3,
     & Be3,Bs2,Be2,Bs4,Be4,Bs6,Be6,Bs8,Be8,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6,
     & Bs7,Be7,Bs8,Be8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(Bs5+1:Be5,Bs7+1:Be7,Bs1+1:Be1,Bs3+1:Be3,
     & Bs2+1:Be2,Bs4+1:Be4,Bs6+1:Be6,Bs8+1:Be8)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      do i7=Bs7+1,Be7
      do i8=Bs8+1,Be8
      B(i5,i7,i1,i3,i2,i4,i6,i8)=A(i1,i2,i3,i4,i5,i6,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder451326(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs4,Be4,Bs5,Be5,Bs1,Be1,Bs3,Be3,Bs2,Be2,Bs6,Be6,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs4+1:Be4,Bs5+1:Be5,Bs1+1:Be1,Bs3+1:Be3,
     & Bs2+1:Be2,Bs6+1:Be6)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i4,i5,i1,i3,i2,i6)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder231456(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs2,Be2,Bs3,Be3,Bs1,Be1,Bs4,Be4,Bs5,Be5,Bs6,Be6,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs2+1:Be2,Bs3+1:Be3,Bs1+1:Be1,Bs4+1:Be4,
     & Bs5+1:Be5,Bs6+1:Be6)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i2,i3,i1,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder561324(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs5,Be5,Bs6,Be6,Bs1,Be1,Bs3,Be3,Bs2,Be2,Bs4,Be4,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs5+1:Be5,Bs6+1:Be6,Bs1+1:Be1,Bs3+1:Be3,
     & Bs2+1:Be2,Bs4+1:Be4)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i5,i6,i1,i3,i2,i4)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder461325(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs4,Be4,Bs6,Be6,Bs1,Be1,Bs3,Be3,Bs2,Be2,Bs5,Be5,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs4+1:Be4,Bs6+1:Be6,Bs1+1:Be1,Bs3+1:Be3,
     & Bs2+1:Be2,Bs5+1:Be5)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i4,i6,i1,i3,i2,i5)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder4312(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & Bs4,Be4,Bs3,Be3,Bs1,Be1,Bs2,Be2,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4
      integer i1,i2,i3,i4

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real(kind=8) B(Bs4+1:Be4,Bs3+1:Be3,Bs1+1:Be1,Bs2+1:Be2)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      B(i4,i3,i1,i2)=A(i1,i2,i3,i4)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder4123(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & Bs4,Be4,Bs1,Be1,Bs2,Be2,Bs3,Be3,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4
      integer i1,i2,i3,i4

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real(kind=8) B(Bs4+1:Be4,Bs1+1:Be1,Bs2+1:Be2,Bs3+1:Be3)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      B(i4,i1,i2,i3)=A(i1,i2,i3,i4)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder13524678(As1,Ae1,As2,Ae2,As3,Ae3,As4,
     & Ae4,As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,Bs1,Be1,Bs3,Be3,Bs5,Be5,Bs2,
     & Be2,Bs4,Be4,Bs6,Be6,Bs7,Be7,Bs8,Be8,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6,
     & Bs7,Be7,Bs8,Be8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(Bs1+1:Be1,Bs3+1:Be3,Bs5+1:Be5,Bs2+1:Be2,
     & Bs4+1:Be4,Bs6+1:Be6,Bs7+1:Be7,Bs8+1:Be8)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      do i7=Bs7+1,Be7
      do i8=Bs8+1,Be8
      B(i1,i3,i5,i2,i4,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder1342(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & Bs1,Be1,Bs3,Be3,Bs4,Be4,Bs2,Be2,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4
      integer i1,i2,i3,i4

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real(kind=8) B(Bs1+1:Be1,Bs3+1:Be3,Bs4+1:Be4,Bs2+1:Be2)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      B(i1,i3,i4,i2)=A(i1,i2,i3,i4)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder452136(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs4,Be4,Bs5,Be5,Bs2,Be2,Bs1,Be1,Bs3,Be3,Bs6,Be6,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs4+1:Be4,Bs5+1:Be5,Bs2+1:Be2,Bs1+1:Be1,
     & Bs3+1:Be3,Bs6+1:Be6)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i4,i5,i2,i1,i3,i6)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder341526(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs3,Be3,Bs4,Be4,Bs1,Be1,Bs5,Be5,Bs2,Be2,Bs6,Be6,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs3+1:Be3,Bs4+1:Be4,Bs1+1:Be1,Bs5+1:Be5,
     & Bs2+1:Be2,Bs6+1:Be6)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i3,i4,i1,i5,i2,i6)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder13245678(As1,Ae1,As2,Ae2,As3,Ae3,As4,
     & Ae4,As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,Bs1,Be1,Bs3,Be3,Bs2,Be2,Bs4,
     & Be4,Bs5,Be5,Bs6,Be6,Bs7,Be7,Bs8,Be8,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6,
     & Bs7,Be7,Bs8,Be8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(Bs1+1:Be1,Bs3+1:Be3,Bs2+1:Be2,Bs4+1:Be4,
     & Bs5+1:Be5,Bs6+1:Be6,Bs7+1:Be7,Bs8+1:Be8)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      do i7=Bs7+1,Be7
      do i8=Bs8+1,Be8
      B(i1,i3,i2,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder521346(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs5,Be5,Bs2,Be2,Bs1,Be1,Bs3,Be3,Bs4,Be4,Bs6,Be6,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs5+1:Be5,Bs2+1:Be2,Bs1+1:Be1,Bs3+1:Be3,
     & Bs4+1:Be4,Bs6+1:Be6)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i5,i2,i1,i3,i4,i6)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder263451(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs2,Be2,Bs6,Be6,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs1,Be1,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs2+1:Be2,Bs6+1:Be6,Bs3+1:Be3,Bs4+1:Be4,
     & Bs5+1:Be5,Bs1+1:Be1)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i2,i6,i3,i4,i5,i1)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder453126(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs4,Be4,Bs5,Be5,Bs3,Be3,Bs1,Be1,Bs2,Be2,Bs6,Be6,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs4+1:Be4,Bs5+1:Be5,Bs3+1:Be3,Bs1+1:Be1,
     & Bs2+1:Be2,Bs6+1:Be6)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i4,i5,i3,i1,i2,i6)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder265134(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs2,Be2,Bs6,Be6,Bs5,Be5,Bs1,Be1,Bs3,Be3,Bs4,Be4,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs2+1:Be2,Bs6+1:Be6,Bs5+1:Be5,Bs1+1:Be1,
     & Bs3+1:Be3,Bs4+1:Be4)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i2,i6,i5,i1,i3,i4)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder654123(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs6,Be6,Bs5,Be5,Bs4,Be4,Bs1,Be1,Bs2,Be2,Bs3,Be3,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs6+1:Be6,Bs5+1:Be5,Bs4+1:Be4,Bs1+1:Be1,
     & Bs2+1:Be2,Bs3+1:Be3)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i6,i5,i4,i1,i2,i3)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder631245(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs6,Be6,Bs3,Be3,Bs1,Be1,Bs2,Be2,Bs4,Be4,Bs5,Be5,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs6+1:Be6,Bs3+1:Be3,Bs1+1:Be1,Bs2+1:Be2,
     & Bs4+1:Be4,Bs5+1:Be5)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i6,i3,i1,i2,i4,i5)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder4321(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & Bs4,Be4,Bs3,Be3,Bs2,Be2,Bs1,Be1,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4
      integer i1,i2,i3,i4

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real(kind=8) B(Bs4+1:Be4,Bs3+1:Be3,Bs2+1:Be2,Bs1+1:Be1)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      B(i4,i3,i2,i1)=A(i1,i2,i3,i4)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder124356(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs1,Be1,Bs2,Be2,Bs4,Be4,Bs3,Be3,Bs5,Be5,Bs6,Be6,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs1+1:Be1,Bs2+1:Be2,Bs4+1:Be4,Bs3+1:Be3,
     & Bs5+1:Be5,Bs6+1:Be6)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i1,i2,i4,i3,i5,i6)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder234561(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6,Bs1,Be1,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs2+1:Be2,Bs3+1:Be3,Bs4+1:Be4,Bs5+1:Be5,
     & Bs6+1:Be6,Bs1+1:Be1)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i2,i3,i4,i5,i6,i1)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder23614578(As1,Ae1,As2,Ae2,As3,Ae3,As4,
     & Ae4,As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,Bs2,Be2,Bs3,Be3,Bs6,Be6,Bs1,
     & Be1,Bs4,Be4,Bs5,Be5,Bs7,Be7,Bs8,Be8,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6,
     & Bs7,Be7,Bs8,Be8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(Bs2+1:Be2,Bs3+1:Be3,Bs6+1:Be6,Bs1+1:Be1,
     & Bs4+1:Be4,Bs5+1:Be5,Bs7+1:Be7,Bs8+1:Be8)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      do i7=Bs7+1,Be7
      do i8=Bs8+1,Be8
      B(i2,i3,i6,i1,i4,i5,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder421356(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs4,Be4,Bs2,Be2,Bs1,Be1,Bs3,Be3,Bs5,Be5,Bs6,Be6,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs4+1:Be4,Bs2+1:Be2,Bs1+1:Be1,Bs3+1:Be3,
     & Bs5+1:Be5,Bs6+1:Be6)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i4,i2,i1,i3,i5,i6)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder531246(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs5,Be5,Bs3,Be3,Bs1,Be1,Bs2,Be2,Bs4,Be4,Bs6,Be6,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs5+1:Be5,Bs3+1:Be3,Bs1+1:Be1,Bs2+1:Be2,
     & Bs4+1:Be4,Bs6+1:Be6)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i5,i3,i1,i2,i4,i6)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder61234578(As1,Ae1,As2,Ae2,As3,Ae3,As4,
     & Ae4,As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,Bs6,Be6,Bs1,Be1,Bs2,Be2,Bs3,
     & Be3,Bs4,Be4,Bs5,Be5,Bs7,Be7,Bs8,Be8,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6,
     & Bs7,Be7,Bs8,Be8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(Bs6+1:Be6,Bs1+1:Be1,Bs2+1:Be2,Bs3+1:Be3,
     & Bs4+1:Be4,Bs5+1:Be5,Bs7+1:Be7,Bs8+1:Be8)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      do i7=Bs7+1,Be7
      do i8=Bs8+1,Be8
      B(i6,i1,i2,i3,i4,i5,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder2341(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs1,Be1,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4
      integer i1,i2,i3,i4

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real(kind=8) B(Bs2+1:Be2,Bs3+1:Be3,Bs4+1:Be4,Bs1+1:Be1)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      B(i2,i3,i4,i1)=A(i1,i2,i3,i4)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder364125(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs3,Be3,Bs6,Be6,Bs4,Be4,Bs1,Be1,Bs2,Be2,Bs5,Be5,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs3+1:Be3,Bs6+1:Be6,Bs4+1:Be4,Bs1+1:Be1,
     & Bs2+1:Be2,Bs5+1:Be5)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i3,i6,i4,i1,i2,i5)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder235146(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs2,Be2,Bs3,Be3,Bs5,Be5,Bs1,Be1,Bs4,Be4,Bs6,Be6,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs2+1:Be2,Bs3+1:Be3,Bs5+1:Be5,Bs1+1:Be1,
     & Bs4+1:Be4,Bs6+1:Be6)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i2,i3,i5,i1,i4,i6)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder263415(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs2,Be2,Bs6,Be6,Bs3,Be3,Bs4,Be4,Bs1,Be1,Bs5,Be5,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs2+1:Be2,Bs6+1:Be6,Bs3+1:Be3,Bs4+1:Be4,
     & Bs1+1:Be1,Bs5+1:Be5)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i2,i6,i3,i4,i1,i5)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder541236(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs5,Be5,Bs4,Be4,Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs6,Be6,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs5+1:Be5,Bs4+1:Be4,Bs1+1:Be1,Bs2+1:Be2,
     & Bs3+1:Be3,Bs6+1:Be6)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i5,i4,i1,i2,i3,i6)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder312456(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs3,Be3,Bs1,Be1,Bs2,Be2,Bs4,Be4,Bs5,Be5,Bs6,Be6,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs3+1:Be3,Bs1+1:Be1,Bs2+1:Be2,Bs4+1:Be4,
     & Bs5+1:Be5,Bs6+1:Be6)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i3,i1,i2,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder58412367(As1,Ae1,As2,Ae2,As3,Ae3,As4,
     & Ae4,As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,Bs5,Be5,Bs8,Be8,Bs4,Be4,Bs1,
     & Be1,Bs2,Be2,Bs3,Be3,Bs6,Be6,Bs7,Be7,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6,
     & Bs7,Be7,Bs8,Be8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(Bs5+1:Be5,Bs8+1:Be8,Bs4+1:Be4,Bs1+1:Be1,
     & Bs2+1:Be2,Bs3+1:Be3,Bs6+1:Be6,Bs7+1:Be7)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      do i7=Bs7+1,Be7
      do i8=Bs8+1,Be8
      B(i5,i8,i4,i1,i2,i3,i6,i7)=A(i1,i2,i3,i4,i5,i6,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder12345678(As1,Ae1,As2,Ae2,As3,Ae3,As4,
     & Ae4,As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,
     & Be4,Bs5,Be5,Bs6,Be6,Bs7,Be7,Bs8,Be8,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6,
     & Bs7,Be7,Bs8,Be8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(Bs1+1:Be1,Bs2+1:Be2,Bs3+1:Be3,Bs4+1:Be4,
     & Bs5+1:Be5,Bs6+1:Be6,Bs7+1:Be7,Bs8+1:Be8)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      do i7=Bs7+1,Be7
      do i8=Bs8+1,Be8
      B(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder52134678(As1,Ae1,As2,Ae2,As3,Ae3,As4,
     & Ae4,As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,Bs5,Be5,Bs2,Be2,Bs1,Be1,Bs3,
     & Be3,Bs4,Be4,Bs6,Be6,Bs7,Be7,Bs8,Be8,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6,
     & Bs7,Be7,Bs8,Be8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(Bs5+1:Be5,Bs2+1:Be2,Bs1+1:Be1,Bs3+1:Be3,
     & Bs4+1:Be4,Bs6+1:Be6,Bs7+1:Be7,Bs8+1:Be8)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      do i7=Bs7+1,Be7
      do i8=Bs8+1,Be8
      B(i5,i2,i1,i3,i4,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder67213458(As1,Ae1,As2,Ae2,As3,Ae3,As4,
     & Ae4,As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,Bs6,Be6,Bs7,Be7,Bs2,Be2,Bs1,
     & Be1,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs8,Be8,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6,
     & Bs7,Be7,Bs8,Be8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(Bs6+1:Be6,Bs7+1:Be7,Bs2+1:Be2,Bs1+1:Be1,
     & Bs3+1:Be3,Bs4+1:Be4,Bs5+1:Be5,Bs8+1:Be8)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      do i7=Bs7+1,Be7
      do i8=Bs8+1,Be8
      B(i6,i7,i2,i1,i3,i4,i5,i8)=A(i1,i2,i3,i4,i5,i6,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder254136(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs2,Be2,Bs5,Be5,Bs4,Be4,Bs1,Be1,Bs3,Be3,Bs6,Be6,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs2+1:Be2,Bs5+1:Be5,Bs4+1:Be4,Bs1+1:Be1,
     & Bs3+1:Be3,Bs6+1:Be6)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i2,i5,i4,i1,i3,i6)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder56123478(As1,Ae1,As2,Ae2,As3,Ae3,As4,
     & Ae4,As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,Bs5,Be5,Bs6,Be6,Bs1,Be1,Bs2,
     & Be2,Bs3,Be3,Bs4,Be4,Bs7,Be7,Bs8,Be8,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6,
     & Bs7,Be7,Bs8,Be8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(Bs5+1:Be5,Bs6+1:Be6,Bs1+1:Be1,Bs2+1:Be2,
     & Bs3+1:Be3,Bs4+1:Be4,Bs7+1:Be7,Bs8+1:Be8)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      do i7=Bs7+1,Be7
      do i8=Bs8+1,Be8
      B(i5,i6,i1,i2,i3,i4,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder623145(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs6,Be6,Bs2,Be2,Bs3,Be3,Bs1,Be1,Bs4,Be4,Bs5,Be5,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs6+1:Be6,Bs2+1:Be2,Bs3+1:Be3,Bs1+1:Be1,
     & Bs4+1:Be4,Bs5+1:Be5)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i6,i2,i3,i1,i4,i5)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder4213(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & Bs4,Be4,Bs2,Be2,Bs1,Be1,Bs3,Be3,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4
      integer i1,i2,i3,i4

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real(kind=8) B(Bs4+1:Be4,Bs2+1:Be2,Bs1+1:Be1,Bs3+1:Be3)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      B(i4,i2,i1,i3)=A(i1,i2,i3,i4)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder621345(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs6,Be6,Bs2,Be2,Bs1,Be1,Bs3,Be3,Bs4,Be4,Bs5,Be5,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs6+1:Be6,Bs2+1:Be2,Bs1+1:Be1,Bs3+1:Be3,
     & Bs4+1:Be4,Bs5+1:Be5)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i6,i2,i1,i3,i4,i5)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder564123(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs5,Be5,Bs6,Be6,Bs4,Be4,Bs1,Be1,Bs2,Be2,Bs3,Be3,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs5+1:Be5,Bs6+1:Be6,Bs4+1:Be4,Bs1+1:Be1,
     & Bs2+1:Be2,Bs3+1:Be3)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i5,i6,i4,i1,i2,i3)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder562314(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs5,Be5,Bs6,Be6,Bs2,Be2,Bs3,Be3,Bs1,Be1,Bs4,Be4,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs5+1:Be5,Bs6+1:Be6,Bs2+1:Be2,Bs3+1:Be3,
     & Bs1+1:Be1,Bs4+1:Be4)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i5,i6,i2,i3,i1,i4)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder62314578(As1,Ae1,As2,Ae2,As3,Ae3,As4,
     & Ae4,As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,Bs6,Be6,Bs2,Be2,Bs3,Be3,Bs1,
     & Be1,Bs4,Be4,Bs5,Be5,Bs7,Be7,Bs8,Be8,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6,
     & Bs7,Be7,Bs8,Be8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(Bs6+1:Be6,Bs2+1:Be2,Bs3+1:Be3,Bs1+1:Be1,
     & Bs4+1:Be4,Bs5+1:Be5,Bs7+1:Be7,Bs8+1:Be8)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      do i7=Bs7+1,Be7
      do i8=Bs8+1,Be8
      B(i6,i2,i3,i1,i4,i5,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder152346(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs1,Be1,Bs5,Be5,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs6,Be6,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs1+1:Be1,Bs5+1:Be5,Bs2+1:Be2,Bs3+1:Be3,
     & Bs4+1:Be4,Bs6+1:Be6)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i1,i5,i2,i3,i4,i6)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder423156(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs4,Be4,Bs2,Be2,Bs3,Be3,Bs1,Be1,Bs5,Be5,Bs6,Be6,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs4+1:Be4,Bs2+1:Be2,Bs3+1:Be3,Bs1+1:Be1,
     & Bs5+1:Be5,Bs6+1:Be6)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i4,i2,i3,i1,i5,i6)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder3214(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & Bs3,Be3,Bs2,Be2,Bs1,Be1,Bs4,Be4,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4
      integer i1,i2,i3,i4

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real(kind=8) B(Bs3+1:Be3,Bs2+1:Be2,Bs1+1:Be1,Bs4+1:Be4)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      B(i3,i2,i1,i4)=A(i1,i2,i3,i4)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder3142(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & Bs3,Be3,Bs1,Be1,Bs4,Be4,Bs2,Be2,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4
      integer i1,i2,i3,i4

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real(kind=8) B(Bs3+1:Be3,Bs1+1:Be1,Bs4+1:Be4,Bs2+1:Be2)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      B(i3,i1,i4,i2)=A(i1,i2,i3,i4)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder341256(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs3,Be3,Bs4,Be4,Bs1,Be1,Bs2,Be2,Bs5,Be5,Bs6,Be6,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs3+1:Be3,Bs4+1:Be4,Bs1+1:Be1,Bs2+1:Be2,
     & Bs5+1:Be5,Bs6+1:Be6)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i3,i4,i1,i2,i5,i6)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder314256(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs3,Be3,Bs1,Be1,Bs4,Be4,Bs2,Be2,Bs5,Be5,Bs6,Be6,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs3+1:Be3,Bs1+1:Be1,Bs4+1:Be4,Bs2+1:Be2,
     & Bs5+1:Be5,Bs6+1:Be6)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i3,i1,i4,i2,i5,i6)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder72613458(As1,Ae1,As2,Ae2,As3,Ae3,As4,
     & Ae4,As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,Bs7,Be7,Bs2,Be2,Bs6,Be6,Bs1,
     & Be1,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs8,Be8,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6,
     & Bs7,Be7,Bs8,Be8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(Bs7+1:Be7,Bs2+1:Be2,Bs6+1:Be6,Bs1+1:Be1,
     & Bs3+1:Be3,Bs4+1:Be4,Bs5+1:Be5,Bs8+1:Be8)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      do i7=Bs7+1,Be7
      do i8=Bs8+1,Be8
      B(i7,i2,i6,i1,i3,i4,i5,i8)=A(i1,i2,i3,i4,i5,i6,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder263145(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs2,Be2,Bs6,Be6,Bs3,Be3,Bs1,Be1,Bs4,Be4,Bs5,Be5,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs2+1:Be2,Bs6+1:Be6,Bs3+1:Be3,Bs1+1:Be1,
     & Bs4+1:Be4,Bs5+1:Be5)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i2,i6,i3,i1,i4,i5)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder563124(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs5,Be5,Bs6,Be6,Bs3,Be3,Bs1,Be1,Bs2,Be2,Bs4,Be4,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs5+1:Be5,Bs6+1:Be6,Bs3+1:Be3,Bs1+1:Be1,
     & Bs2+1:Be2,Bs4+1:Be4)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i5,i6,i3,i1,i2,i4)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder62513478(As1,Ae1,As2,Ae2,As3,Ae3,As4,
     & Ae4,As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,Bs6,Be6,Bs2,Be2,Bs5,Be5,Bs1,
     & Be1,Bs3,Be3,Bs4,Be4,Bs7,Be7,Bs8,Be8,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6,
     & Bs7,Be7,Bs8,Be8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(Bs6+1:Be6,Bs2+1:Be2,Bs5+1:Be5,Bs1+1:Be1,
     & Bs3+1:Be3,Bs4+1:Be4,Bs7+1:Be7,Bs8+1:Be8)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      do i7=Bs7+1,Be7
      do i8=Bs8+1,Be8
      B(i6,i2,i5,i1,i3,i4,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder12(As1,Ae1,As2,Ae2,Bs1,Be1,Bs2,Be2,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2
      integer Bs1,Be1,Bs2,Be2
      integer i1,i2

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2)
      real(kind=8) B(Bs1+1:Be1,Bs2+1:Be2)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      B(i1,i2)=A(i1,i2)
      enddo
      enddo

      end


      subroutine reorder612345(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs6,Be6,Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,
     & A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs6+1:Be6,Bs1+1:Be1,Bs2+1:Be2,Bs3+1:Be3,
     & Bs4+1:Be4,Bs5+1:Be5)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      B(i6,i1,i2,i3,i4,i5)=A(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder73124568(As1,Ae1,As2,Ae2,As3,Ae3,As4,
     & Ae4,As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,Bs7,Be7,Bs3,Be3,Bs1,Be1,Bs2,
     & Be2,Bs4,Be4,Bs5,Be5,Bs6,Be6,Bs8,Be8,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6,
     & Bs7,Be7,Bs8,Be8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(Bs7+1:Be7,Bs3+1:Be3,Bs1+1:Be1,Bs2+1:Be2,
     & Bs4+1:Be4,Bs5+1:Be5,Bs6+1:Be6,Bs8+1:Be8)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      do i7=Bs7+1,Be7
      do i8=Bs8+1,Be8
      B(i7,i3,i1,i2,i4,i5,i6,i8)=A(i1,i2,i3,i4,i5,i6,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder23145678(As1,Ae1,As2,Ae2,As3,Ae3,As4,
     & Ae4,As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,Bs2,Be2,Bs3,Be3,Bs1,Be1,Bs4,
     & Be4,Bs5,Be5,Bs6,Be6,Bs7,Be7,Bs8,Be8,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6,
     & Bs7,Be7,Bs8,Be8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(Bs2+1:Be2,Bs3+1:Be3,Bs1+1:Be1,Bs4+1:Be4,
     & Bs5+1:Be5,Bs6+1:Be6,Bs7+1:Be7,Bs8+1:Be8)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      do i7=Bs7+1,Be7
      do i8=Bs8+1,Be8
      B(i2,i3,i1,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder83712456(As1,Ae1,As2,Ae2,As3,Ae3,As4,
     & Ae4,As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,Bs8,Be8,Bs3,Be3,Bs7,Be7,Bs1,
     & Be1,Bs2,Be2,Bs4,Be4,Bs5,Be5,Bs6,Be6,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6,
     & Bs7,Be7,Bs8,Be8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(Bs8+1:Be8,Bs3+1:Be3,Bs7+1:Be7,Bs1+1:Be1,
     & Bs2+1:Be2,Bs4+1:Be4,Bs5+1:Be5,Bs6+1:Be6)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      do i7=Bs7+1,Be7
      do i8=Bs8+1,Be8
      B(i8,i3,i7,i1,i2,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder67123458(As1,Ae1,As2,Ae2,As3,Ae3,As4,
     & Ae4,As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,Bs6,Be6,Bs7,Be7,Bs1,Be1,Bs2,
     & Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs8,Be8,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6,
     & Bs7,Be7,Bs8,Be8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(Bs6+1:Be6,Bs7+1:Be7,Bs1+1:Be1,Bs2+1:Be2,
     & Bs3+1:Be3,Bs4+1:Be4,Bs5+1:Be5,Bs8+1:Be8)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      do i7=Bs7+1,Be7
      do i8=Bs8+1,Be8
      B(i6,i7,i1,i2,i3,i4,i5,i8)=A(i1,i2,i3,i4,i5,i6,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder2413(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & Bs2,Be2,Bs4,Be4,Bs1,Be1,Bs3,Be3,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4
      integer i1,i2,i3,i4

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real(kind=8) B(Bs2+1:Be2,Bs4+1:Be4,Bs1+1:Be1,Bs3+1:Be3)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      B(i2,i4,i1,i3)=A(i1,i2,i3,i4)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine reorder84123567(As1,Ae1,As2,Ae2,As3,Ae3,As4,
     & Ae4,As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,Bs8,Be8,Bs4,Be4,Bs1,Be1,Bs2,
     & Be2,Bs3,Be3,Bs5,Be5,Bs6,Be6,Bs7,Be7,A, B)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6,
     & Bs7,Be7,Bs8,Be8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(Bs8+1:Be8,Bs4+1:Be4,Bs1+1:Be1,Bs2+1:Be2,
     & Bs3+1:Be3,Bs5+1:Be5,Bs6+1:Be6,Bs7+1:Be7)

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      do i7=Bs7+1,Be7
      do i8=Bs8+1,Be8
      B(i8,i4,i1,i2,i3,i5,i6,i7)=A(i1,i2,i3,i4,i5,i6,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum14723685(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As4+1:Ae4,As7+1:Ae7,As2+1:Ae2,
     & As3+1:Ae3,As6+1:Ae6,As8+1:Ae8,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i4,i7,i2,i3,i6,i8,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12483657(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As4+1:Ae4,As8+1:Ae8,
     & As3+1:Ae3,As6+1:Ae6,As5+1:Ae5,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i4,i8,i3,i6,i5,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum14723568(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As4+1:Ae4,As7+1:Ae7,As2+1:Ae2,
     & As3+1:Ae3,As5+1:Ae5,As6+1:Ae6,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i4,i7,i2,i3,i5,i6,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23841675(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As8+1:Ae8,As4+1:Ae4,
     & As1+1:Ae1,As6+1:Ae6,As7+1:Ae7,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i8,i4,i1,i6,i7,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12354768(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As5+1:Ae5,
     & As4+1:Ae4,As7+1:Ae7,As6+1:Ae6,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i3,i5,i4,i7,i6,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12534678(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As5+1:Ae5,As3+1:Ae3,
     & As4+1:Ae4,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i5,i3,i4,i6,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12456387(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As4+1:Ae4,As5+1:Ae5,
     & As6+1:Ae6,As3+1:Ae3,As8+1:Ae8,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i4,i5,i6,i3,i8,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum17842356(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As7+1:Ae7,As8+1:Ae8,As4+1:Ae4,
     & As2+1:Ae2,As3+1:Ae3,As5+1:Ae5,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i7,i8,i4,i2,i3,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum16734258(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As6+1:Ae6,As7+1:Ae7,As3+1:Ae3,
     & As4+1:Ae4,As2+1:Ae2,As5+1:Ae5,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i6,i7,i3,i4,i2,i5,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12368475(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As6+1:Ae6,
     & As8+1:Ae8,As4+1:Ae4,As7+1:Ae7,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i3,i6,i8,i4,i7,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum34682157(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As6+1:Ae6,As8+1:Ae8,
     & As2+1:Ae2,As1+1:Ae1,As5+1:Ae5,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i4,i6,i8,i2,i1,i5,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12784356(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As7+1:Ae7,As8+1:Ae8,
     & As4+1:Ae4,As3+1:Ae3,As5+1:Ae5,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i7,i8,i4,i3,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12463587(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As4+1:Ae4,As6+1:Ae6,
     & As3+1:Ae3,As5+1:Ae5,As8+1:Ae8,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i4,i6,i3,i5,i8,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum213456(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As2+1:Ae2,As1+1:Ae1,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i2,i1,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23614758(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As6+1:Ae6,As1+1:Ae1,
     & As4+1:Ae4,As7+1:Ae7,As5+1:Ae5,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i6,i1,i4,i7,i5,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum14673258(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As4+1:Ae4,As6+1:Ae6,As7+1:Ae7,
     & As3+1:Ae3,As2+1:Ae2,As5+1:Ae5,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i4,i6,i7,i3,i2,i5,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23814756(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As8+1:Ae8,As1+1:Ae1,
     & As4+1:Ae4,As7+1:Ae7,As5+1:Ae5,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i8,i1,i4,i7,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum15724368(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As5+1:Ae5,As7+1:Ae7,As2+1:Ae2,
     & As4+1:Ae4,As3+1:Ae3,As6+1:Ae6,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i5,i7,i2,i4,i3,i6,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23568417(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As5+1:Ae5,As6+1:Ae6,
     & As8+1:Ae8,As4+1:Ae4,As1+1:Ae1,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i5,i6,i8,i4,i1,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23641578(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As6+1:Ae6,As4+1:Ae4,
     & As1+1:Ae1,As5+1:Ae5,As7+1:Ae7,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i6,i4,i1,i5,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23456178(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,As5+1:Ae5,
     & As6+1:Ae6,As1+1:Ae1,As7+1:Ae7,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i4,i5,i6,i1,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum24813567(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As4+1:Ae4,As8+1:Ae8,As1+1:Ae1,
     & As3+1:Ae3,As5+1:Ae5,As6+1:Ae6,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i4,i8,i1,i3,i5,i6,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12356478(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As5+1:Ae5,
     & As6+1:Ae6,As4+1:Ae4,As7+1:Ae7,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i3,i5,i6,i4,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum243561(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As2+1:Ae2,As4+1:Ae4,As3+1:Ae3,As5+1:Ae5,
     & As6+1:Ae6,As1+1:Ae1)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i2,i4,i3,i5,i6,i1)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum126354(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As6+1:Ae6,As3+1:Ae3,
     & As5+1:Ae5,As4+1:Ae4)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i1,i2,i6,i3,i5,i4)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum34812567(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As8+1:Ae8,As1+1:Ae1,
     & As2+1:Ae2,As5+1:Ae5,As6+1:Ae6,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i4,i8,i1,i2,i5,i6,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23481576(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,As8+1:Ae8,
     & As1+1:Ae1,As5+1:Ae5,As7+1:Ae7,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i4,i8,i1,i5,i7,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum45813267(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As4+1:Ae4,As5+1:Ae5,As8+1:Ae8,As1+1:Ae1,
     & As3+1:Ae3,As2+1:Ae2,As6+1:Ae6,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i4,i5,i8,i1,i3,i2,i6,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum234615(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,As6+1:Ae6,
     & As1+1:Ae1,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i2,i3,i4,i6,i1,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12684357(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As6+1:Ae6,As8+1:Ae8,
     & As4+1:Ae4,As3+1:Ae3,As5+1:Ae5,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i6,i8,i4,i3,i5,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum26813457(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As6+1:Ae6,As8+1:Ae8,As1+1:Ae1,
     & As3+1:Ae3,As4+1:Ae4,As5+1:Ae5,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i6,i8,i1,i3,i4,i5,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12367458(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As6+1:Ae6,
     & As7+1:Ae7,As4+1:Ae4,As5+1:Ae5,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i3,i6,i7,i4,i5,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum34821576(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As8+1:Ae8,As2+1:Ae2,
     & As1+1:Ae1,As5+1:Ae5,As7+1:Ae7,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i4,i8,i2,i1,i5,i7,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum34812657(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As8+1:Ae8,As1+1:Ae1,
     & As2+1:Ae2,As6+1:Ae6,As5+1:Ae5,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i4,i8,i1,i2,i6,i5,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum2134(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer i1,i2,i3,i4

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real(kind=8) B(As2+1:Ae2,As1+1:Ae1,As3+1:Ae3,As4+1:Ae4)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      A(i1,i2,i3,i4)=A(i1,i2,i3,i4)+C*B(i2,i1,i3,i4)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12478356(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As4+1:Ae4,As7+1:Ae7,
     & As8+1:Ae8,As3+1:Ae3,As5+1:Ae5,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i4,i7,i8,i3,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum14678325(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As4+1:Ae4,As6+1:Ae6,As7+1:Ae7,
     & As8+1:Ae8,As3+1:Ae3,As2+1:Ae2,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i4,i6,i7,i8,i3,i2,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum623451(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As6+1:Ae6,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As1+1:Ae1)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i6,i2,i3,i4,i5,i1)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum24578316(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As4+1:Ae4,As5+1:Ae5,As7+1:Ae7,
     & As8+1:Ae8,As3+1:Ae3,As1+1:Ae1,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i4,i5,i7,i8,i3,i1,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum25713468(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As5+1:Ae5,As7+1:Ae7,As1+1:Ae1,
     & As3+1:Ae3,As4+1:Ae4,As6+1:Ae6,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i5,i7,i1,i3,i4,i6,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13542768(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As5+1:Ae5,As4+1:Ae4,
     & As2+1:Ae2,As7+1:Ae7,As6+1:Ae6,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i5,i4,i2,i7,i6,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12453768(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As4+1:Ae4,As5+1:Ae5,
     & As3+1:Ae3,As7+1:Ae7,As6+1:Ae6,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i4,i5,i3,i7,i6,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13824576(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As8+1:Ae8,As2+1:Ae2,
     & As4+1:Ae4,As5+1:Ae5,As7+1:Ae7,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i8,i2,i4,i5,i7,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13524876(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As5+1:Ae5,As2+1:Ae2,
     & As4+1:Ae4,As8+1:Ae8,As7+1:Ae7,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i5,i2,i4,i8,i7,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum14532687(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As4+1:Ae4,As5+1:Ae5,As3+1:Ae3,
     & As2+1:Ae2,As6+1:Ae6,As8+1:Ae8,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i4,i5,i3,i2,i6,i8,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13684257(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As6+1:Ae6,As8+1:Ae8,
     & As4+1:Ae4,As2+1:Ae2,As5+1:Ae5,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i6,i8,i4,i2,i5,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23741586(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As7+1:Ae7,As4+1:Ae4,
     & As1+1:Ae1,As5+1:Ae5,As8+1:Ae8,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i7,i4,i1,i5,i8,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum14632578(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As4+1:Ae4,As6+1:Ae6,As3+1:Ae3,
     & As2+1:Ae2,As5+1:Ae5,As7+1:Ae7,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i4,i6,i3,i2,i5,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum14823576(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As4+1:Ae4,As8+1:Ae8,As2+1:Ae2,
     & As3+1:Ae3,As5+1:Ae5,As7+1:Ae7,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i4,i8,i2,i3,i5,i7,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum356214(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As3+1:Ae3,As5+1:Ae5,As6+1:Ae6,As2+1:Ae2,
     & As1+1:Ae1,As4+1:Ae4)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i3,i5,i6,i2,i1,i4)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23541768(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As5+1:Ae5,As4+1:Ae4,
     & As1+1:Ae1,As7+1:Ae7,As6+1:Ae6,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i5,i4,i1,i7,i6,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum124563(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As4+1:Ae4,As5+1:Ae5,
     & As6+1:Ae6,As3+1:Ae3)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i1,i2,i4,i5,i6,i3)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum14523687(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As4+1:Ae4,As5+1:Ae5,As2+1:Ae2,
     & As3+1:Ae3,As6+1:Ae6,As8+1:Ae8,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i4,i5,i2,i3,i6,i8,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum346215(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As6+1:Ae6,As2+1:Ae2,
     & As1+1:Ae1,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i3,i4,i6,i2,i1,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12463578(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As4+1:Ae4,As6+1:Ae6,
     & As3+1:Ae3,As5+1:Ae5,As7+1:Ae7,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i4,i6,i3,i5,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13456287(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As4+1:Ae4,As5+1:Ae5,
     & As6+1:Ae6,As2+1:Ae2,As8+1:Ae8,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i4,i5,i6,i2,i8,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum14567238(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As4+1:Ae4,As5+1:Ae5,As6+1:Ae6,
     & As7+1:Ae7,As2+1:Ae2,As3+1:Ae3,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i4,i5,i6,i7,i2,i3,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum134625(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As4+1:Ae4,As6+1:Ae6,
     & As2+1:Ae2,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i1,i3,i4,i6,i2,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum15734268(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As5+1:Ae5,As7+1:Ae7,As3+1:Ae3,
     & As4+1:Ae4,As2+1:Ae2,As6+1:Ae6,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i5,i7,i3,i4,i2,i6,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13624587(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As6+1:Ae6,As2+1:Ae2,
     & As4+1:Ae4,As5+1:Ae5,As8+1:Ae8,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i6,i2,i4,i5,i8,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23451678(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,As5+1:Ae5,
     & As1+1:Ae1,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i4,i5,i1,i6,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23567148(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As5+1:Ae5,As6+1:Ae6,
     & As7+1:Ae7,As1+1:Ae1,As4+1:Ae4,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i5,i6,i7,i1,i4,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12467385(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As4+1:Ae4,As6+1:Ae6,
     & As7+1:Ae7,As3+1:Ae3,As8+1:Ae8,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i4,i6,i7,i3,i8,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23456718(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,As5+1:Ae5,
     & As6+1:Ae6,As7+1:Ae7,As1+1:Ae1,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i4,i5,i6,i7,i1,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum132456(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As2+1:Ae2,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i1,i3,i2,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum17823456(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As7+1:Ae7,As8+1:Ae8,As2+1:Ae2,
     & As3+1:Ae3,As4+1:Ae4,As5+1:Ae5,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i7,i8,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum15834267(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As5+1:Ae5,As8+1:Ae8,As3+1:Ae3,
     & As4+1:Ae4,As2+1:Ae2,As6+1:Ae6,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i5,i8,i3,i4,i2,i6,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23471568(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,As7+1:Ae7,
     & As1+1:Ae1,As5+1:Ae5,As6+1:Ae6,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i4,i7,i1,i5,i6,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum27814356(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As7+1:Ae7,As8+1:Ae8,As1+1:Ae1,
     & As4+1:Ae4,As3+1:Ae3,As5+1:Ae5,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i7,i8,i1,i4,i3,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23614857(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As6+1:Ae6,As1+1:Ae1,
     & As4+1:Ae4,As8+1:Ae8,As5+1:Ae5,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i6,i1,i4,i8,i5,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum34571268(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As5+1:Ae5,As7+1:Ae7,
     & As1+1:Ae1,As2+1:Ae2,As6+1:Ae6,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i4,i5,i7,i1,i2,i6,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum24583167(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As4+1:Ae4,As5+1:Ae5,As8+1:Ae8,
     & As3+1:Ae3,As1+1:Ae1,As6+1:Ae6,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i4,i5,i8,i3,i1,i6,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12467835(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As4+1:Ae4,As6+1:Ae6,
     & As7+1:Ae7,As8+1:Ae8,As3+1:Ae3,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i4,i6,i7,i8,i3,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum251346(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As2+1:Ae2,As5+1:Ae5,As1+1:Ae1,As3+1:Ae3,
     & As4+1:Ae4,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i2,i5,i1,i3,i4,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum35841267(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As5+1:Ae5,As8+1:Ae8,As4+1:Ae4,
     & As1+1:Ae1,As2+1:Ae2,As6+1:Ae6,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i5,i8,i4,i1,i2,i6,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23581467(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As5+1:Ae5,As8+1:Ae8,
     & As1+1:Ae1,As4+1:Ae4,As6+1:Ae6,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i5,i8,i1,i4,i6,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12354786(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As5+1:Ae5,
     & As4+1:Ae4,As7+1:Ae7,As8+1:Ae8,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i3,i5,i4,i7,i8,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum523461(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As5+1:Ae5,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As6+1:Ae6,As1+1:Ae1)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i5,i2,i3,i4,i6,i1)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum35641278(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As5+1:Ae5,As6+1:Ae6,As4+1:Ae4,
     & As1+1:Ae1,As2+1:Ae2,As7+1:Ae7,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i5,i6,i4,i1,i2,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum14567832(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As4+1:Ae4,As5+1:Ae5,As6+1:Ae6,
     & As7+1:Ae7,As8+1:Ae8,As3+1:Ae3,As2+1:Ae2)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i4,i5,i6,i7,i8,i3,i2)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23471586(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,As7+1:Ae7,
     & As1+1:Ae1,As5+1:Ae5,As8+1:Ae8,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i4,i7,i1,i5,i8,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum14723586(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As4+1:Ae4,As7+1:Ae7,As2+1:Ae2,
     & As3+1:Ae3,As5+1:Ae5,As8+1:Ae8,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i4,i7,i2,i3,i5,i8,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13562478(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As5+1:Ae5,As6+1:Ae6,
     & As2+1:Ae2,As4+1:Ae4,As7+1:Ae7,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i5,i6,i2,i4,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23614785(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As6+1:Ae6,As1+1:Ae1,
     & As4+1:Ae4,As7+1:Ae7,As8+1:Ae8,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i6,i1,i4,i7,i8,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23781456(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As7+1:Ae7,As8+1:Ae8,
     & As1+1:Ae1,As4+1:Ae4,As5+1:Ae5,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i7,i8,i1,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23467158(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,As6+1:Ae6,
     & As7+1:Ae7,As1+1:Ae1,As5+1:Ae5,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i4,i6,i7,i1,i5,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12345867(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As8+1:Ae8,As6+1:Ae6,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i3,i4,i5,i8,i6,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum345621(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As5+1:Ae5,As6+1:Ae6,
     & As2+1:Ae2,As1+1:Ae1)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i3,i4,i5,i6,i2,i1)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12567348(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As5+1:Ae5,As6+1:Ae6,
     & As7+1:Ae7,As3+1:Ae3,As4+1:Ae4,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i5,i6,i7,i3,i4,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13458267(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As4+1:Ae4,As5+1:Ae5,
     & As8+1:Ae8,As2+1:Ae2,As6+1:Ae6,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i4,i5,i8,i2,i6,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum24578136(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As4+1:Ae4,As5+1:Ae5,As7+1:Ae7,
     & As8+1:Ae8,As1+1:Ae1,As3+1:Ae3,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i4,i5,i7,i8,i1,i3,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13742586(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As7+1:Ae7,As4+1:Ae4,
     & As2+1:Ae2,As5+1:Ae5,As8+1:Ae8,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i7,i4,i2,i5,i8,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13456728(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As4+1:Ae4,As5+1:Ae5,
     & As6+1:Ae6,As7+1:Ae7,As2+1:Ae2,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i4,i5,i6,i7,i2,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum123654(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As6+1:Ae6,
     & As5+1:Ae5,As4+1:Ae4)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i1,i2,i3,i6,i5,i4)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13672458(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As6+1:Ae6,As7+1:Ae7,
     & As2+1:Ae2,As4+1:Ae4,As5+1:Ae5,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i6,i7,i2,i4,i5,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum36841257(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As6+1:Ae6,As8+1:Ae8,As4+1:Ae4,
     & As1+1:Ae1,As2+1:Ae2,As5+1:Ae5,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i6,i8,i4,i1,i2,i5,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12356847(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As5+1:Ae5,
     & As6+1:Ae6,As8+1:Ae8,As4+1:Ae4,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i3,i5,i6,i8,i4,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum24678135(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As4+1:Ae4,As6+1:Ae6,As7+1:Ae7,
     & As8+1:Ae8,As1+1:Ae1,As3+1:Ae3,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i4,i6,i7,i8,i1,i3,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13724865(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As7+1:Ae7,As2+1:Ae2,
     & As4+1:Ae4,As8+1:Ae8,As6+1:Ae6,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i7,i2,i4,i8,i6,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23478156(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,As7+1:Ae7,
     & As8+1:Ae8,As1+1:Ae1,As5+1:Ae5,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i4,i7,i8,i1,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum24613785(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As4+1:Ae4,As6+1:Ae6,As1+1:Ae1,
     & As3+1:Ae3,As7+1:Ae7,As8+1:Ae8,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i4,i6,i1,i3,i7,i8,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum25731468(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As5+1:Ae5,As7+1:Ae7,As3+1:Ae3,
     & As1+1:Ae1,As4+1:Ae4,As6+1:Ae6,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i5,i7,i3,i1,i4,i6,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12457836(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As4+1:Ae4,As5+1:Ae5,
     & As7+1:Ae7,As8+1:Ae8,As3+1:Ae3,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i4,i5,i7,i8,i3,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12473658(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As4+1:Ae4,As7+1:Ae7,
     & As3+1:Ae3,As6+1:Ae6,As5+1:Ae5,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i4,i7,i3,i6,i5,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum236154(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As6+1:Ae6,As1+1:Ae1,
     & As5+1:Ae5,As4+1:Ae4)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i2,i3,i6,i1,i5,i4)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13784256(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As7+1:Ae7,As8+1:Ae8,
     & As4+1:Ae4,As2+1:Ae2,As5+1:Ae5,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i7,i8,i4,i2,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum34712856(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As7+1:Ae7,As1+1:Ae1,
     & As2+1:Ae2,As8+1:Ae8,As5+1:Ae5,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i4,i7,i1,i2,i8,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum134256(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As4+1:Ae4,As2+1:Ae2,
     & As5+1:Ae5,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i1,i3,i4,i2,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum34572168(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As5+1:Ae5,As7+1:Ae7,
     & As2+1:Ae2,As1+1:Ae1,As6+1:Ae6,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i4,i5,i7,i2,i1,i6,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12543678(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As5+1:Ae5,As4+1:Ae4,
     & As3+1:Ae3,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i5,i4,i3,i6,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum34561278(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As5+1:Ae5,As6+1:Ae6,
     & As1+1:Ae1,As2+1:Ae2,As7+1:Ae7,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i4,i5,i6,i1,i2,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12563478(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As5+1:Ae5,As6+1:Ae6,
     & As3+1:Ae3,As4+1:Ae4,As7+1:Ae7,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i5,i6,i3,i4,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum231456(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As1+1:Ae1,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i2,i3,i1,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13524678(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As5+1:Ae5,As2+1:Ae2,
     & As4+1:Ae4,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i5,i2,i4,i6,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum14683257(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As4+1:Ae4,As6+1:Ae6,As8+1:Ae8,
     & As3+1:Ae3,As2+1:Ae2,As5+1:Ae5,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i4,i6,i8,i3,i2,i5,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum14732568(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As4+1:Ae4,As7+1:Ae7,As3+1:Ae3,
     & As2+1:Ae2,As5+1:Ae5,As6+1:Ae6,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i4,i7,i3,i2,i5,i6,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum135624(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As5+1:Ae5,As6+1:Ae6,
     & As2+1:Ae2,As4+1:Ae4)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i1,i3,i5,i6,i2,i4)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23514768(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As5+1:Ae5,As1+1:Ae1,
     & As4+1:Ae4,As7+1:Ae7,As6+1:Ae6,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i5,i1,i4,i7,i6,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12364587(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As6+1:Ae6,
     & As4+1:Ae4,As5+1:Ae5,As8+1:Ae8,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i3,i6,i4,i5,i8,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum14632758(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As4+1:Ae4,As6+1:Ae6,As3+1:Ae3,
     & As2+1:Ae2,As7+1:Ae7,As5+1:Ae5,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i4,i6,i3,i2,i7,i5,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum124356(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As4+1:Ae4,As3+1:Ae3,
     & As5+1:Ae5,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i1,i2,i4,i3,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23578146(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As5+1:Ae5,As7+1:Ae7,
     & As8+1:Ae8,As1+1:Ae1,As4+1:Ae4,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i5,i7,i8,i1,i4,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23784156(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As7+1:Ae7,As8+1:Ae8,
     & As4+1:Ae4,As1+1:Ae1,As5+1:Ae5,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i7,i8,i4,i1,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum24613857(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As4+1:Ae4,As6+1:Ae6,As1+1:Ae1,
     & As3+1:Ae3,As8+1:Ae8,As5+1:Ae5,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i4,i6,i1,i3,i8,i5,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23561478(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As5+1:Ae5,As6+1:Ae6,
     & As1+1:Ae1,As4+1:Ae4,As7+1:Ae7,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i5,i6,i1,i4,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13674258(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As6+1:Ae6,As7+1:Ae7,
     & As4+1:Ae4,As2+1:Ae2,As5+1:Ae5,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i6,i7,i4,i2,i5,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum14678235(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As4+1:Ae4,As6+1:Ae6,As7+1:Ae7,
     & As8+1:Ae8,As2+1:Ae2,As3+1:Ae3,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i4,i6,i7,i8,i2,i3,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum24567138(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As4+1:Ae4,As5+1:Ae5,As6+1:Ae6,
     & As7+1:Ae7,As1+1:Ae1,As3+1:Ae3,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i4,i5,i6,i7,i1,i3,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12357486(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As5+1:Ae5,
     & As7+1:Ae7,As4+1:Ae4,As8+1:Ae8,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i3,i5,i7,i4,i8,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum14672358(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As4+1:Ae4,As6+1:Ae6,As7+1:Ae7,
     & As2+1:Ae2,As3+1:Ae3,As5+1:Ae5,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i4,i6,i7,i2,i3,i5,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum34821657(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As8+1:Ae8,As2+1:Ae2,
     & As1+1:Ae1,As6+1:Ae6,As5+1:Ae5,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i4,i8,i2,i1,i6,i5,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum34821756(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As8+1:Ae8,As2+1:Ae2,
     & As1+1:Ae1,As7+1:Ae7,As5+1:Ae5,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i4,i8,i2,i1,i7,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum24783156(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As4+1:Ae4,As7+1:Ae7,As8+1:Ae8,
     & As3+1:Ae3,As1+1:Ae1,As5+1:Ae5,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i4,i7,i8,i3,i1,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum34512867(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As5+1:Ae5,As1+1:Ae1,
     & As2+1:Ae2,As8+1:Ae8,As6+1:Ae6,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i4,i5,i1,i2,i8,i6,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12473685(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As4+1:Ae4,As7+1:Ae7,
     & As3+1:Ae3,As6+1:Ae6,As8+1:Ae8,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i4,i7,i3,i6,i8,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum34521687(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As5+1:Ae5,As2+1:Ae2,
     & As1+1:Ae1,As6+1:Ae6,As8+1:Ae8,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i4,i5,i2,i1,i6,i8,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum24631578(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As4+1:Ae4,As6+1:Ae6,As3+1:Ae3,
     & As1+1:Ae1,As5+1:Ae5,As7+1:Ae7,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i4,i6,i3,i1,i5,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13582467(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As5+1:Ae5,As8+1:Ae8,
     & As2+1:Ae2,As4+1:Ae4,As6+1:Ae6,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i5,i8,i2,i4,i6,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum34521768(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As5+1:Ae5,As2+1:Ae2,
     & As1+1:Ae1,As7+1:Ae7,As6+1:Ae6,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i4,i5,i2,i1,i7,i6,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13542687(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As5+1:Ae5,As4+1:Ae4,
     & As2+1:Ae2,As6+1:Ae6,As8+1:Ae8,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i5,i4,i2,i6,i8,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23456781(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,As5+1:Ae5,
     & As6+1:Ae6,As7+1:Ae7,As8+1:Ae8,As1+1:Ae1)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i4,i5,i6,i7,i8,i1)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum14732658(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As4+1:Ae4,As7+1:Ae7,As3+1:Ae3,
     & As2+1:Ae2,As6+1:Ae6,As5+1:Ae5,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i4,i7,i3,i2,i6,i5,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12534768(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As5+1:Ae5,As3+1:Ae3,
     & As4+1:Ae4,As7+1:Ae7,As6+1:Ae6,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i5,i3,i4,i7,i6,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum345612(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As5+1:Ae5,As6+1:Ae6,
     & As1+1:Ae1,As2+1:Ae2)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i3,i4,i5,i6,i1,i2)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12843567(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As8+1:Ae8,As4+1:Ae4,
     & As3+1:Ae3,As5+1:Ae5,As6+1:Ae6,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i8,i4,i3,i5,i6,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23461587(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,As6+1:Ae6,
     & As1+1:Ae1,As5+1:Ae5,As8+1:Ae8,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i4,i6,i1,i5,i8,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum24631758(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As4+1:Ae4,As6+1:Ae6,As3+1:Ae3,
     & As1+1:Ae1,As7+1:Ae7,As5+1:Ae5,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i4,i6,i3,i1,i7,i5,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum24631785(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As4+1:Ae4,As6+1:Ae6,As3+1:Ae3,
     & As1+1:Ae1,As7+1:Ae7,As8+1:Ae8,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i4,i6,i3,i1,i7,i8,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum15642378(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As5+1:Ae5,As6+1:Ae6,As4+1:Ae4,
     & As2+1:Ae2,As3+1:Ae3,As7+1:Ae7,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i5,i6,i4,i2,i3,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13824675(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As8+1:Ae8,As2+1:Ae2,
     & As4+1:Ae4,As6+1:Ae6,As7+1:Ae7,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i8,i2,i4,i6,i7,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12357468(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As5+1:Ae5,
     & As7+1:Ae7,As4+1:Ae4,As6+1:Ae6,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i3,i5,i7,i4,i6,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum36741258(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As6+1:Ae6,As7+1:Ae7,As4+1:Ae4,
     & As1+1:Ae1,As2+1:Ae2,As5+1:Ae5,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i6,i7,i4,i1,i2,i5,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23567418(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As5+1:Ae5,As6+1:Ae6,
     & As7+1:Ae7,As4+1:Ae4,As1+1:Ae1,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i5,i6,i7,i4,i1,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum24831576(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As4+1:Ae4,As8+1:Ae8,As3+1:Ae3,
     & As1+1:Ae1,As5+1:Ae5,As7+1:Ae7,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i4,i8,i3,i1,i5,i7,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23614587(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As6+1:Ae6,As1+1:Ae1,
     & As4+1:Ae4,As5+1:Ae5,As8+1:Ae8,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i6,i1,i4,i5,i8,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum46721358(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As4+1:Ae4,As6+1:Ae6,As7+1:Ae7,As2+1:Ae2,
     & As1+1:Ae1,As3+1:Ae3,As5+1:Ae5,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i4,i6,i7,i2,i1,i3,i5,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum16732458(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As6+1:Ae6,As7+1:Ae7,As3+1:Ae3,
     & As2+1:Ae2,As4+1:Ae4,As5+1:Ae5,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i6,i7,i3,i2,i4,i5,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum25831467(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As5+1:Ae5,As8+1:Ae8,As3+1:Ae3,
     & As1+1:Ae1,As4+1:Ae4,As6+1:Ae6,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i5,i8,i3,i1,i4,i6,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12374568(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As7+1:Ae7,
     & As4+1:Ae4,As5+1:Ae5,As6+1:Ae6,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i3,i7,i4,i5,i6,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12345786(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As7+1:Ae7,As8+1:Ae8,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i3,i4,i5,i7,i8,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23814567(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As8+1:Ae8,As1+1:Ae1,
     & As4+1:Ae4,As5+1:Ae5,As6+1:Ae6,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i8,i1,i4,i5,i6,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum14623785(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As4+1:Ae4,As6+1:Ae6,As2+1:Ae2,
     & As3+1:Ae3,As7+1:Ae7,As8+1:Ae8,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i4,i6,i2,i3,i7,i8,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13462578(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As4+1:Ae4,As6+1:Ae6,
     & As2+1:Ae2,As5+1:Ae5,As7+1:Ae7,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i4,i6,i2,i5,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12843657(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As8+1:Ae8,As4+1:Ae4,
     & As3+1:Ae3,As6+1:Ae6,As5+1:Ae5,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i8,i4,i3,i6,i5,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum24678315(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As4+1:Ae4,As6+1:Ae6,As7+1:Ae7,
     & As8+1:Ae8,As3+1:Ae3,As1+1:Ae1,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i4,i6,i7,i8,i3,i1,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23684157(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As6+1:Ae6,As8+1:Ae8,
     & As4+1:Ae4,As1+1:Ae1,As5+1:Ae5,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i6,i8,i4,i1,i5,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum125634(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As5+1:Ae5,As6+1:Ae6,
     & As3+1:Ae3,As4+1:Ae4)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i1,i2,i5,i6,i3,i4)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum24831675(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As4+1:Ae4,As8+1:Ae8,As3+1:Ae3,
     & As1+1:Ae1,As6+1:Ae6,As7+1:Ae7,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i4,i8,i3,i1,i6,i7,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13624758(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As6+1:Ae6,As2+1:Ae2,
     & As4+1:Ae4,As7+1:Ae7,As5+1:Ae5,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i6,i2,i4,i7,i5,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13482675(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As4+1:Ae4,As8+1:Ae8,
     & As2+1:Ae2,As6+1:Ae6,As7+1:Ae7,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i4,i8,i2,i6,i7,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum14562378(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As4+1:Ae4,As5+1:Ae5,As6+1:Ae6,
     & As2+1:Ae2,As3+1:Ae3,As7+1:Ae7,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i4,i5,i6,i2,i3,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12743568(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As7+1:Ae7,As4+1:Ae4,
     & As3+1:Ae3,As5+1:Ae5,As6+1:Ae6,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i7,i4,i3,i5,i6,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum14832576(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As4+1:Ae4,As8+1:Ae8,As3+1:Ae3,
     & As2+1:Ae2,As5+1:Ae5,As7+1:Ae7,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i4,i8,i3,i2,i5,i7,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13452786(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As4+1:Ae4,As5+1:Ae5,
     & As2+1:Ae2,As7+1:Ae7,As8+1:Ae8,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i4,i5,i2,i7,i8,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum24531768(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As4+1:Ae4,As5+1:Ae5,As3+1:Ae3,
     & As1+1:Ae1,As7+1:Ae7,As6+1:Ae6,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i4,i5,i3,i1,i7,i6,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum34521678(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As5+1:Ae5,As2+1:Ae2,
     & As1+1:Ae1,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i4,i5,i2,i1,i6,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum2413(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer i1,i2,i3,i4

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real(kind=8) B(As2+1:Ae2,As4+1:Ae4,As1+1:Ae1,As3+1:Ae3)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      A(i1,i2,i3,i4)=A(i1,i2,i3,i4)+C*B(i2,i4,i1,i3)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum34721586(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As7+1:Ae7,As2+1:Ae2,
     & As1+1:Ae1,As5+1:Ae5,As8+1:Ae8,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i4,i7,i2,i1,i5,i8,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12584367(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As5+1:Ae5,As8+1:Ae8,
     & As4+1:Ae4,As3+1:Ae3,As6+1:Ae6,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i5,i8,i4,i3,i6,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12468375(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As4+1:Ae4,As6+1:Ae6,
     & As8+1:Ae8,As3+1:Ae3,As7+1:Ae7,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i4,i6,i8,i3,i7,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13584267(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As5+1:Ae5,As8+1:Ae8,
     & As4+1:Ae4,As2+1:Ae2,As6+1:Ae6,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i5,i8,i4,i2,i6,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum24513786(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As4+1:Ae4,As5+1:Ae5,As1+1:Ae1,
     & As3+1:Ae3,As7+1:Ae7,As8+1:Ae8,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i4,i5,i1,i3,i7,i8,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12843756(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As8+1:Ae8,As4+1:Ae4,
     & As3+1:Ae3,As7+1:Ae7,As5+1:Ae5,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i8,i4,i3,i7,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13567248(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As5+1:Ae5,As6+1:Ae6,
     & As7+1:Ae7,As2+1:Ae2,As4+1:Ae4,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i5,i6,i7,i2,i4,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum24713568(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As4+1:Ae4,As7+1:Ae7,As1+1:Ae1,
     & As3+1:Ae3,As5+1:Ae5,As6+1:Ae6,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i4,i7,i1,i3,i5,i6,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23568147(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As5+1:Ae5,As6+1:Ae6,
     & As8+1:Ae8,As1+1:Ae1,As4+1:Ae4,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i5,i6,i8,i1,i4,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum34567812(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As5+1:Ae5,As6+1:Ae6,
     & As7+1:Ae7,As8+1:Ae8,As1+1:Ae1,As2+1:Ae2)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i4,i5,i6,i7,i8,i1,i2)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12456738(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As4+1:Ae4,As5+1:Ae5,
     & As6+1:Ae6,As7+1:Ae7,As3+1:Ae3,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i4,i5,i6,i7,i3,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum34671258(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As6+1:Ae6,As7+1:Ae7,
     & As1+1:Ae1,As2+1:Ae2,As5+1:Ae5,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i4,i6,i7,i1,i2,i5,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23671458(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As6+1:Ae6,As7+1:Ae7,
     & As1+1:Ae1,As4+1:Ae4,As5+1:Ae5,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i6,i7,i1,i4,i5,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum135246(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As5+1:Ae5,As2+1:Ae2,
     & As4+1:Ae4,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i1,i3,i5,i2,i4,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23714586(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As7+1:Ae7,As1+1:Ae1,
     & As4+1:Ae4,As5+1:Ae5,As8+1:Ae8,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i7,i1,i4,i5,i8,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23567841(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As5+1:Ae5,As6+1:Ae6,
     & As7+1:Ae7,As8+1:Ae8,As4+1:Ae4,As1+1:Ae1)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i5,i6,i7,i8,i4,i1)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum16823457(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As6+1:Ae6,As8+1:Ae8,As2+1:Ae2,
     & As3+1:Ae3,As4+1:Ae4,As5+1:Ae5,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i6,i8,i2,i3,i4,i5,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12643758(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As6+1:Ae6,As4+1:Ae4,
     & As3+1:Ae3,As7+1:Ae7,As5+1:Ae5,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i6,i4,i3,i7,i5,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12743856(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As7+1:Ae7,As4+1:Ae4,
     & As3+1:Ae3,As8+1:Ae8,As5+1:Ae5,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i7,i4,i3,i8,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12634578(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As6+1:Ae6,As3+1:Ae3,
     & As4+1:Ae4,As5+1:Ae5,As7+1:Ae7,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i6,i3,i4,i5,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum352461(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As3+1:Ae3,As5+1:Ae5,As2+1:Ae2,As4+1:Ae4,
     & As6+1:Ae6,As1+1:Ae1)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i3,i5,i2,i4,i6,i1)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12456378(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As4+1:Ae4,As5+1:Ae5,
     & As6+1:Ae6,As3+1:Ae3,As7+1:Ae7,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i4,i5,i6,i3,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum14567328(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As4+1:Ae4,As5+1:Ae5,As6+1:Ae6,
     & As7+1:Ae7,As3+1:Ae3,As2+1:Ae2,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i4,i5,i6,i7,i3,i2,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12643857(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As6+1:Ae6,As4+1:Ae4,
     & As3+1:Ae3,As8+1:Ae8,As5+1:Ae5,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i6,i4,i3,i8,i5,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum135264(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As5+1:Ae5,As2+1:Ae2,
     & As6+1:Ae6,As4+1:Ae4)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i1,i3,i5,i2,i6,i4)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13462587(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As4+1:Ae4,As6+1:Ae6,
     & As2+1:Ae2,As5+1:Ae5,As8+1:Ae8,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i4,i6,i2,i5,i8,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum14723856(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As4+1:Ae4,As7+1:Ae7,As2+1:Ae2,
     & As3+1:Ae3,As8+1:Ae8,As5+1:Ae5,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i4,i7,i2,i3,i8,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13542867(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As5+1:Ae5,As4+1:Ae4,
     & As2+1:Ae2,As8+1:Ae8,As6+1:Ae6,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i5,i4,i2,i8,i6,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12543687(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As5+1:Ae5,As4+1:Ae4,
     & As3+1:Ae3,As6+1:Ae6,As8+1:Ae8,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i5,i4,i3,i6,i8,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum324561(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As3+1:Ae3,As2+1:Ae2,As4+1:Ae4,As5+1:Ae5,
     & As6+1:Ae6,As1+1:Ae1)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i3,i2,i4,i5,i6,i1)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12578436(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As5+1:Ae5,As7+1:Ae7,
     & As8+1:Ae8,As4+1:Ae4,As3+1:Ae3,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i5,i7,i8,i4,i3,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum15742368(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As5+1:Ae5,As7+1:Ae7,As4+1:Ae4,
     & As2+1:Ae2,As3+1:Ae3,As6+1:Ae6,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i5,i7,i4,i2,i3,i6,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum45612378(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As4+1:Ae4,As5+1:Ae5,As6+1:Ae6,As1+1:Ae1,
     & As2+1:Ae2,As3+1:Ae3,As7+1:Ae7,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i4,i5,i6,i1,i2,i3,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum34712685(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As7+1:Ae7,As1+1:Ae1,
     & As2+1:Ae2,As6+1:Ae6,As8+1:Ae8,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i4,i7,i1,i2,i6,i8,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12573468(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As5+1:Ae5,As7+1:Ae7,
     & As3+1:Ae3,As4+1:Ae4,As6+1:Ae6,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i5,i7,i3,i4,i6,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13524687(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As5+1:Ae5,As2+1:Ae2,
     & As4+1:Ae4,As6+1:Ae6,As8+1:Ae8,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i5,i2,i4,i6,i8,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum1324(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer i1,i2,i3,i4

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As2+1:Ae2,As4+1:Ae4)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      A(i1,i2,i3,i4)=A(i1,i2,i3,i4)+C*B(i1,i3,i2,i4)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum14783256(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As4+1:Ae4,As7+1:Ae7,As8+1:Ae8,
     & As3+1:Ae3,As2+1:Ae2,As5+1:Ae5,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i4,i7,i8,i3,i2,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23578416(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As5+1:Ae5,As7+1:Ae7,
     & As8+1:Ae8,As4+1:Ae4,As1+1:Ae1,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i5,i7,i8,i4,i1,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum34612587(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As6+1:Ae6,As1+1:Ae1,
     & As2+1:Ae2,As5+1:Ae5,As8+1:Ae8,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i4,i6,i1,i2,i5,i8,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum24531786(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As4+1:Ae4,As5+1:Ae5,As3+1:Ae3,
     & As1+1:Ae1,As7+1:Ae7,As8+1:Ae8,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i4,i5,i3,i1,i7,i8,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum26741358(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As6+1:Ae6,As7+1:Ae7,As4+1:Ae4,
     & As1+1:Ae1,As3+1:Ae3,As5+1:Ae5,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i6,i7,i4,i1,i3,i5,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13742658(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As7+1:Ae7,As4+1:Ae4,
     & As2+1:Ae2,As6+1:Ae6,As5+1:Ae5,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i7,i4,i2,i6,i5,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12346758(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As6+1:Ae6,As7+1:Ae7,As5+1:Ae5,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i3,i4,i6,i7,i5,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum34621758(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As6+1:Ae6,As2+1:Ae2,
     & As1+1:Ae1,As7+1:Ae7,As5+1:Ae5,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i4,i6,i2,i1,i7,i5,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum14823657(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As4+1:Ae4,As8+1:Ae8,As2+1:Ae2,
     & As3+1:Ae3,As6+1:Ae6,As5+1:Ae5,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i4,i8,i2,i3,i6,i5,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum17824356(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As7+1:Ae7,As8+1:Ae8,As2+1:Ae2,
     & As4+1:Ae4,As3+1:Ae3,As5+1:Ae5,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i7,i8,i2,i4,i3,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum24567813(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As4+1:Ae4,As5+1:Ae5,As6+1:Ae6,
     & As7+1:Ae7,As8+1:Ae8,As1+1:Ae1,As3+1:Ae3)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i4,i5,i6,i7,i8,i1,i3)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12384675(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As8+1:Ae8,
     & As4+1:Ae4,As6+1:Ae6,As7+1:Ae7,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i3,i8,i4,i6,i7,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum24613578(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As4+1:Ae4,As6+1:Ae6,As1+1:Ae1,
     & As3+1:Ae3,As5+1:Ae5,As7+1:Ae7,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i4,i6,i1,i3,i5,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum14523867(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As4+1:Ae4,As5+1:Ae5,As2+1:Ae2,
     & As3+1:Ae3,As8+1:Ae8,As6+1:Ae6,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i4,i5,i2,i3,i8,i6,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum24531867(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As4+1:Ae4,As5+1:Ae5,As3+1:Ae3,
     & As1+1:Ae1,As8+1:Ae8,As6+1:Ae6,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i4,i5,i3,i1,i8,i6,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12456783(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As4+1:Ae4,As5+1:Ae5,
     & As6+1:Ae6,As7+1:Ae7,As8+1:Ae8,As3+1:Ae3)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i4,i5,i6,i7,i8,i3)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12567843(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As5+1:Ae5,As6+1:Ae6,
     & As7+1:Ae7,As8+1:Ae8,As4+1:Ae4,As3+1:Ae3)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i5,i6,i7,i8,i4,i3)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13542678(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As5+1:Ae5,As4+1:Ae4,
     & As2+1:Ae2,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i5,i4,i2,i6,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23514867(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As5+1:Ae5,As1+1:Ae1,
     & As4+1:Ae4,As8+1:Ae8,As6+1:Ae6,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i5,i1,i4,i8,i6,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12783456(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As7+1:Ae7,As8+1:Ae8,
     & As3+1:Ae3,As4+1:Ae4,As5+1:Ae5,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i7,i8,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23457816(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,As5+1:Ae5,
     & As7+1:Ae7,As8+1:Ae8,As1+1:Ae1,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i4,i5,i7,i8,i1,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum145623(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As1+1:Ae1,As4+1:Ae4,As5+1:Ae5,As6+1:Ae6,
     & As2+1:Ae2,As3+1:Ae3)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i1,i4,i5,i6,i2,i3)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum34568217(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As5+1:Ae5,As6+1:Ae6,
     & As8+1:Ae8,As2+1:Ae2,As1+1:Ae1,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i4,i5,i6,i8,i2,i1,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum14623857(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As4+1:Ae4,As6+1:Ae6,As2+1:Ae2,
     & As3+1:Ae3,As8+1:Ae8,As5+1:Ae5,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i4,i6,i2,i3,i8,i5,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum24513687(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As4+1:Ae4,As5+1:Ae5,As1+1:Ae1,
     & As3+1:Ae3,As6+1:Ae6,As8+1:Ae8,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i4,i5,i1,i3,i6,i8,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12843576(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As8+1:Ae8,As4+1:Ae4,
     & As3+1:Ae3,As5+1:Ae5,As7+1:Ae7,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i8,i4,i3,i5,i7,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum14732685(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As4+1:Ae4,As7+1:Ae7,As3+1:Ae3,
     & As2+1:Ae2,As6+1:Ae6,As8+1:Ae8,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i4,i7,i3,i2,i6,i8,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum36812457(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As6+1:Ae6,As8+1:Ae8,As1+1:Ae1,
     & As2+1:Ae2,As4+1:Ae4,As5+1:Ae5,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i6,i8,i1,i2,i4,i5,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum346251(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As6+1:Ae6,As2+1:Ae2,
     & As5+1:Ae5,As1+1:Ae1)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i3,i4,i6,i2,i5,i1)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12378465(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As7+1:Ae7,
     & As8+1:Ae8,As4+1:Ae4,As6+1:Ae6,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i3,i7,i8,i4,i6,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum14567823(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As4+1:Ae4,As5+1:Ae5,As6+1:Ae6,
     & As7+1:Ae7,As8+1:Ae8,As2+1:Ae2,As3+1:Ae3)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i4,i5,i6,i7,i8,i2,i3)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum24813756(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As4+1:Ae4,As8+1:Ae8,As1+1:Ae1,
     & As3+1:Ae3,As7+1:Ae7,As5+1:Ae5,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i4,i8,i1,i3,i7,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23461758(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,As6+1:Ae6,
     & As1+1:Ae1,As7+1:Ae7,As5+1:Ae5,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i4,i6,i1,i7,i5,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum34567128(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As5+1:Ae5,As6+1:Ae6,
     & As7+1:Ae7,As1+1:Ae1,As2+1:Ae2,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i4,i5,i6,i7,i1,i2,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum14823567(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As4+1:Ae4,As8+1:Ae8,As2+1:Ae2,
     & As3+1:Ae3,As5+1:Ae5,As6+1:Ae6,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i4,i8,i2,i3,i5,i6,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum136254(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As6+1:Ae6,As2+1:Ae2,
     & As5+1:Ae5,As4+1:Ae4)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i1,i3,i6,i2,i5,i4)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum25614378(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As5+1:Ae5,As6+1:Ae6,As1+1:Ae1,
     & As4+1:Ae4,As3+1:Ae3,As7+1:Ae7,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i5,i6,i1,i4,i3,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum256314(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As2+1:Ae2,As5+1:Ae5,As6+1:Ae6,As3+1:Ae3,
     & As1+1:Ae1,As4+1:Ae4)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i2,i5,i6,i3,i1,i4)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum3412(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer i1,i2,i3,i4

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As1+1:Ae1,As2+1:Ae2)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      A(i1,i2,i3,i4)=A(i1,i2,i3,i4)+C*B(i3,i4,i1,i2)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13452687(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As4+1:Ae4,As5+1:Ae5,
     & As2+1:Ae2,As6+1:Ae6,As8+1:Ae8,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i4,i5,i2,i6,i8,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23471685(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,As7+1:Ae7,
     & As1+1:Ae1,As6+1:Ae6,As8+1:Ae8,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i4,i7,i1,i6,i8,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum34512786(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As5+1:Ae5,As1+1:Ae1,
     & As2+1:Ae2,As7+1:Ae7,As8+1:Ae8,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i4,i5,i1,i2,i7,i8,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12358476(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As5+1:Ae5,
     & As8+1:Ae8,As4+1:Ae4,As7+1:Ae7,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i3,i5,i8,i4,i7,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23456817(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,As5+1:Ae5,
     & As6+1:Ae6,As8+1:Ae8,As1+1:Ae1,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i4,i5,i6,i8,i1,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum25631478(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As5+1:Ae5,As6+1:Ae6,As3+1:Ae3,
     & As1+1:Ae1,As4+1:Ae4,As7+1:Ae7,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i5,i6,i3,i1,i4,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum14832675(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As4+1:Ae4,As8+1:Ae8,As3+1:Ae3,
     & As2+1:Ae2,As6+1:Ae6,As7+1:Ae7,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i4,i8,i3,i2,i6,i7,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12463758(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As4+1:Ae4,As6+1:Ae6,
     & As3+1:Ae3,As7+1:Ae7,As5+1:Ae5,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i4,i6,i3,i7,i5,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum34812675(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As8+1:Ae8,As1+1:Ae1,
     & As2+1:Ae2,As6+1:Ae6,As7+1:Ae7,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i4,i8,i1,i2,i6,i7,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum35712468(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As5+1:Ae5,As7+1:Ae7,As1+1:Ae1,
     & As2+1:Ae2,As4+1:Ae4,As6+1:Ae6,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i5,i7,i1,i2,i4,i6,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum24513678(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As4+1:Ae4,As5+1:Ae5,As1+1:Ae1,
     & As3+1:Ae3,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i4,i5,i1,i3,i6,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23714865(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As7+1:Ae7,As1+1:Ae1,
     & As4+1:Ae4,As8+1:Ae8,As6+1:Ae6,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i7,i1,i4,i8,i6,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum36721458(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As6+1:Ae6,As7+1:Ae7,As2+1:Ae2,
     & As1+1:Ae1,As4+1:Ae4,As5+1:Ae5,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i6,i7,i2,i1,i4,i5,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum24573168(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As4+1:Ae4,As5+1:Ae5,As7+1:Ae7,
     & As3+1:Ae3,As1+1:Ae1,As6+1:Ae6,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i4,i5,i7,i3,i1,i6,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12453687(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As4+1:Ae4,As5+1:Ae5,
     & As3+1:Ae3,As6+1:Ae6,As8+1:Ae8,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i4,i5,i3,i6,i8,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum256134(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As2+1:Ae2,As5+1:Ae5,As6+1:Ae6,As1+1:Ae1,
     & As3+1:Ae3,As4+1:Ae4)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i2,i5,i6,i1,i3,i4)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13724856(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As7+1:Ae7,As2+1:Ae2,
     & As4+1:Ae4,As8+1:Ae8,As5+1:Ae5,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i7,i2,i4,i8,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum16842357(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As6+1:Ae6,As8+1:Ae8,As4+1:Ae4,
     & As2+1:Ae2,As3+1:Ae3,As5+1:Ae5,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i6,i8,i4,i2,i3,i5,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12734865(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As7+1:Ae7,As3+1:Ae3,
     & As4+1:Ae4,As8+1:Ae8,As6+1:Ae6,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i7,i3,i4,i8,i6,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum45721368(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As4+1:Ae4,As5+1:Ae5,As7+1:Ae7,As2+1:Ae2,
     & As1+1:Ae1,As3+1:Ae3,As6+1:Ae6,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i4,i5,i7,i2,i1,i3,i6,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12634857(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As6+1:Ae6,As3+1:Ae3,
     & As4+1:Ae4,As8+1:Ae8,As5+1:Ae5,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i6,i3,i4,i8,i5,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum236145(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As6+1:Ae6,As1+1:Ae1,
     & As4+1:Ae4,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i2,i3,i6,i1,i4,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum4231(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer i1,i2,i3,i4

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real(kind=8) B(As4+1:Ae4,As2+1:Ae2,As3+1:Ae3,As1+1:Ae1)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      A(i1,i2,i3,i4)=A(i1,i2,i3,i4)+C*B(i4,i2,i3,i1)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13842675(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As8+1:Ae8,As4+1:Ae4,
     & As2+1:Ae2,As6+1:Ae6,As7+1:Ae7,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i8,i4,i2,i6,i7,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12678345(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As6+1:Ae6,As7+1:Ae7,
     & As8+1:Ae8,As3+1:Ae3,As4+1:Ae4,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i6,i7,i8,i3,i4,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13457286(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As4+1:Ae4,As5+1:Ae5,
     & As7+1:Ae7,As2+1:Ae2,As8+1:Ae8,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i4,i5,i7,i2,i8,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12643785(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As6+1:Ae6,As4+1:Ae4,
     & As3+1:Ae3,As7+1:Ae7,As8+1:Ae8,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i6,i4,i3,i7,i8,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum24567318(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As4+1:Ae4,As5+1:Ae5,As6+1:Ae6,
     & As7+1:Ae7,As3+1:Ae3,As1+1:Ae1,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i4,i5,i6,i7,i3,i1,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum25741368(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As5+1:Ae5,As7+1:Ae7,As4+1:Ae4,
     & As1+1:Ae1,As3+1:Ae3,As6+1:Ae6,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i5,i7,i4,i1,i3,i6,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum14632587(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As4+1:Ae4,As6+1:Ae6,As3+1:Ae3,
     & As2+1:Ae2,As5+1:Ae5,As8+1:Ae8,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i4,i6,i3,i2,i5,i8,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum24713685(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As4+1:Ae4,As7+1:Ae7,As1+1:Ae1,
     & As3+1:Ae3,As6+1:Ae6,As8+1:Ae8,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i4,i7,i1,i3,i6,i8,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum16723458(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As6+1:Ae6,As7+1:Ae7,As2+1:Ae2,
     & As3+1:Ae3,As4+1:Ae4,As5+1:Ae5,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i6,i7,i2,i3,i4,i5,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13458276(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As4+1:Ae4,As5+1:Ae5,
     & As8+1:Ae8,As2+1:Ae2,As7+1:Ae7,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i4,i5,i8,i2,i7,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum14578326(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As4+1:Ae4,As5+1:Ae5,As7+1:Ae7,
     & As8+1:Ae8,As3+1:Ae3,As2+1:Ae2,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i4,i5,i7,i8,i3,i2,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23458167(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,As5+1:Ae5,
     & As8+1:Ae8,As1+1:Ae1,As6+1:Ae6,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i4,i5,i8,i1,i6,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12354687(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As5+1:Ae5,
     & As4+1:Ae4,As6+1:Ae6,As8+1:Ae8,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i3,i5,i4,i6,i8,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum412356(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As4+1:Ae4,As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,
     & As5+1:Ae5,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i4,i1,i2,i3,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13472658(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As4+1:Ae4,As7+1:Ae7,
     & As2+1:Ae2,As6+1:Ae6,As5+1:Ae5,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i4,i7,i2,i6,i5,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum245631(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As2+1:Ae2,As4+1:Ae4,As5+1:Ae5,As6+1:Ae6,
     & As3+1:Ae3,As1+1:Ae1)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i2,i4,i5,i6,i3,i1)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum3421(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer i1,i2,i3,i4

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As2+1:Ae2,As1+1:Ae1)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      A(i1,i2,i3,i4)=A(i1,i2,i3,i4)+C*B(i3,i4,i2,i1)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum24531687(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As4+1:Ae4,As5+1:Ae5,As3+1:Ae3,
     & As1+1:Ae1,As6+1:Ae6,As8+1:Ae8,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i4,i5,i3,i1,i6,i8,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum246135(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As2+1:Ae2,As4+1:Ae4,As6+1:Ae6,As1+1:Ae1,
     & As3+1:Ae3,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i2,i4,i6,i1,i3,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13824756(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As8+1:Ae8,As2+1:Ae2,
     & As4+1:Ae4,As7+1:Ae7,As5+1:Ae5,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i8,i2,i4,i7,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum145632(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As1+1:Ae1,As4+1:Ae4,As5+1:Ae5,As6+1:Ae6,
     & As3+1:Ae3,As2+1:Ae2)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i1,i4,i5,i6,i3,i2)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum35814267(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As5+1:Ae5,As8+1:Ae8,As1+1:Ae1,
     & As4+1:Ae4,As2+1:Ae2,As6+1:Ae6,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i5,i8,i1,i4,i2,i6,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13642578(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As6+1:Ae6,As4+1:Ae4,
     & As2+1:Ae2,As5+1:Ae5,As7+1:Ae7,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i6,i4,i2,i5,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12347856(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As7+1:Ae7,As8+1:Ae8,As5+1:Ae5,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i3,i4,i7,i8,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum17843256(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As7+1:Ae7,As8+1:Ae8,As4+1:Ae4,
     & As3+1:Ae3,As2+1:Ae2,As5+1:Ae5,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i7,i8,i4,i3,i2,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum4123(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer i1,i2,i3,i4

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real(kind=8) B(As4+1:Ae4,As1+1:Ae1,As2+1:Ae2,As3+1:Ae3)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      A(i1,i2,i3,i4)=A(i1,i2,i3,i4)+C*B(i4,i1,i2,i3)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23571468(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As5+1:Ae5,As7+1:Ae7,
     & As1+1:Ae1,As4+1:Ae4,As6+1:Ae6,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i5,i7,i1,i4,i6,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum25641378(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As5+1:Ae5,As6+1:Ae6,As4+1:Ae4,
     & As1+1:Ae1,As3+1:Ae3,As7+1:Ae7,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i5,i6,i4,i1,i3,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum15832467(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As5+1:Ae5,As8+1:Ae8,As3+1:Ae3,
     & As2+1:Ae2,As4+1:Ae4,As6+1:Ae6,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i5,i8,i3,i2,i4,i6,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13742856(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As7+1:Ae7,As4+1:Ae4,
     & As2+1:Ae2,As8+1:Ae8,As5+1:Ae5,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i7,i4,i2,i8,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum235164(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As5+1:Ae5,As1+1:Ae1,
     & As6+1:Ae6,As4+1:Ae4)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i2,i3,i5,i1,i6,i4)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12834675(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As8+1:Ae8,As3+1:Ae3,
     & As4+1:Ae4,As6+1:Ae6,As7+1:Ae7,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i8,i3,i4,i6,i7,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12468357(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As4+1:Ae4,As6+1:Ae6,
     & As8+1:Ae8,As3+1:Ae3,As5+1:Ae5,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i4,i6,i8,i3,i5,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12345687(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As8+1:Ae8,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i3,i4,i5,i6,i8,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum35612478(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As5+1:Ae5,As6+1:Ae6,As1+1:Ae1,
     & As2+1:Ae2,As4+1:Ae4,As7+1:Ae7,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i5,i6,i1,i2,i4,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum14832657(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As4+1:Ae4,As8+1:Ae8,As3+1:Ae3,
     & As2+1:Ae2,As6+1:Ae6,As5+1:Ae5,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i4,i8,i3,i2,i6,i5,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum46812357(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As4+1:Ae4,As6+1:Ae6,As8+1:Ae8,As1+1:Ae1,
     & As2+1:Ae2,As3+1:Ae3,As5+1:Ae5,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i4,i6,i8,i1,i2,i3,i5,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13624578(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As6+1:Ae6,As2+1:Ae2,
     & As4+1:Ae4,As5+1:Ae5,As7+1:Ae7,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i6,i2,i4,i5,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12456837(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As4+1:Ae4,As5+1:Ae5,
     & As6+1:Ae6,As8+1:Ae8,As3+1:Ae3,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i4,i5,i6,i8,i3,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23714568(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As7+1:Ae7,As1+1:Ae1,
     & As4+1:Ae4,As5+1:Ae5,As6+1:Ae6,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i7,i1,i4,i5,i6,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum24568137(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As4+1:Ae4,As5+1:Ae5,As6+1:Ae6,
     & As8+1:Ae8,As1+1:Ae1,As3+1:Ae3,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i4,i5,i6,i8,i1,i3,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13568427(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As5+1:Ae5,As6+1:Ae6,
     & As8+1:Ae8,As4+1:Ae4,As2+1:Ae2,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i5,i6,i8,i4,i2,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12453786(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As4+1:Ae4,As5+1:Ae5,
     & As3+1:Ae3,As7+1:Ae7,As8+1:Ae8,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i4,i5,i3,i7,i8,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum34782156(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As7+1:Ae7,As8+1:Ae8,
     & As2+1:Ae2,As1+1:Ae1,As5+1:Ae5,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i4,i7,i8,i2,i1,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum2341(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer i1,i2,i3,i4

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,As1+1:Ae1)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      A(i1,i2,i3,i4)=A(i1,i2,i3,i4)+C*B(i2,i3,i4,i1)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum24563178(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As4+1:Ae4,As5+1:Ae5,As6+1:Ae6,
     & As3+1:Ae3,As1+1:Ae1,As7+1:Ae7,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i4,i5,i6,i3,i1,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23741568(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As7+1:Ae7,As4+1:Ae4,
     & As1+1:Ae1,As5+1:Ae5,As6+1:Ae6,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i7,i4,i1,i5,i6,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum24613587(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As4+1:Ae4,As6+1:Ae6,As1+1:Ae1,
     & As3+1:Ae3,As5+1:Ae5,As8+1:Ae8,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i4,i6,i1,i3,i5,i8,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum16824357(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As6+1:Ae6,As8+1:Ae8,As2+1:Ae2,
     & As4+1:Ae4,As3+1:Ae3,As5+1:Ae5,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i6,i8,i2,i4,i3,i5,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum235146(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As5+1:Ae5,As1+1:Ae1,
     & As4+1:Ae4,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i2,i3,i5,i1,i4,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum24568317(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As4+1:Ae4,As5+1:Ae5,As6+1:Ae6,
     & As8+1:Ae8,As3+1:Ae3,As1+1:Ae1,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i4,i5,i6,i8,i3,i1,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12534786(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As5+1:Ae5,As3+1:Ae3,
     & As4+1:Ae4,As7+1:Ae7,As8+1:Ae8,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i5,i3,i4,i7,i8,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum34821567(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As8+1:Ae8,As2+1:Ae2,
     & As1+1:Ae1,As5+1:Ae5,As6+1:Ae6,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i4,i8,i2,i1,i5,i6,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12673458(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As6+1:Ae6,As7+1:Ae7,
     & As3+1:Ae3,As4+1:Ae4,As5+1:Ae5,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i6,i7,i3,i4,i5,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum24531678(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As4+1:Ae4,As5+1:Ae5,As3+1:Ae3,
     & As1+1:Ae1,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i4,i5,i3,i1,i6,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23541786(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As5+1:Ae5,As4+1:Ae4,
     & As1+1:Ae1,As7+1:Ae7,As8+1:Ae8,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i5,i4,i1,i7,i8,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23456187(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,As5+1:Ae5,
     & As6+1:Ae6,As1+1:Ae1,As8+1:Ae8,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i4,i5,i6,i1,i8,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12457368(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As4+1:Ae4,As5+1:Ae5,
     & As7+1:Ae7,As3+1:Ae3,As6+1:Ae6,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i4,i5,i7,i3,i6,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum25714368(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As5+1:Ae5,As7+1:Ae7,As1+1:Ae1,
     & As4+1:Ae4,As3+1:Ae3,As6+1:Ae6,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i5,i7,i1,i4,i3,i6,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum351246(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As3+1:Ae3,As5+1:Ae5,As1+1:Ae1,As2+1:Ae2,
     & As4+1:Ae4,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i3,i5,i1,i2,i4,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum16743258(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As6+1:Ae6,As7+1:Ae7,As4+1:Ae4,
     & As3+1:Ae3,As2+1:Ae2,As5+1:Ae5,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i6,i7,i4,i3,i2,i5,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum14832756(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As4+1:Ae4,As8+1:Ae8,As3+1:Ae3,
     & As2+1:Ae2,As7+1:Ae7,As5+1:Ae5,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i4,i8,i3,i2,i7,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13642758(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As6+1:Ae6,As4+1:Ae4,
     & As2+1:Ae2,As7+1:Ae7,As5+1:Ae5,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i6,i4,i2,i7,i5,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum24631587(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As4+1:Ae4,As6+1:Ae6,As3+1:Ae3,
     & As1+1:Ae1,As5+1:Ae5,As8+1:Ae8,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i4,i6,i3,i1,i5,i8,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum17834256(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As7+1:Ae7,As8+1:Ae8,As3+1:Ae3,
     & As4+1:Ae4,As2+1:Ae2,As5+1:Ae5,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i7,i8,i3,i4,i2,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13624875(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As6+1:Ae6,As2+1:Ae2,
     & As4+1:Ae4,As8+1:Ae8,As7+1:Ae7,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i6,i2,i4,i8,i7,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23841567(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As8+1:Ae8,As4+1:Ae4,
     & As1+1:Ae1,As5+1:Ae5,As6+1:Ae6,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i8,i4,i1,i5,i6,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum24731685(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As4+1:Ae4,As7+1:Ae7,As3+1:Ae3,
     & As1+1:Ae1,As6+1:Ae6,As8+1:Ae8,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i4,i7,i3,i1,i6,i8,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12347865(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As7+1:Ae7,As8+1:Ae8,As6+1:Ae6,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i3,i4,i7,i8,i6,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum34721568(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As7+1:Ae7,As2+1:Ae2,
     & As1+1:Ae1,As5+1:Ae5,As6+1:Ae6,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i4,i7,i2,i1,i5,i6,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13742685(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As7+1:Ae7,As4+1:Ae4,
     & As2+1:Ae2,As6+1:Ae6,As8+1:Ae8,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i7,i4,i2,i6,i8,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum341256(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As1+1:Ae1,As2+1:Ae2,
     & As5+1:Ae5,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i3,i4,i1,i2,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum26831457(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As6+1:Ae6,As8+1:Ae8,As3+1:Ae3,
     & As1+1:Ae1,As4+1:Ae4,As5+1:Ae5,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i6,i8,i3,i1,i4,i5,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum124536(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As4+1:Ae4,As5+1:Ae5,
     & As3+1:Ae3,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i1,i2,i4,i5,i3,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum26731458(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As6+1:Ae6,As7+1:Ae7,As3+1:Ae3,
     & As1+1:Ae1,As4+1:Ae4,As5+1:Ae5,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i6,i7,i3,i1,i4,i5,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum45712368(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As4+1:Ae4,As5+1:Ae5,As7+1:Ae7,As1+1:Ae1,
     & As2+1:Ae2,As3+1:Ae3,As6+1:Ae6,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i4,i5,i7,i1,i2,i3,i6,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13482657(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As4+1:Ae4,As8+1:Ae8,
     & As2+1:Ae2,As6+1:Ae6,As5+1:Ae5,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i4,i8,i2,i6,i5,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12356487(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As5+1:Ae5,
     & As6+1:Ae6,As4+1:Ae4,As8+1:Ae8,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i3,i5,i6,i4,i8,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12578346(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As5+1:Ae5,As7+1:Ae7,
     & As8+1:Ae8,As3+1:Ae3,As4+1:Ae4,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i5,i7,i8,i3,i4,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum612345(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As6+1:Ae6,As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,
     & As4+1:Ae4,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i6,i1,i2,i3,i4,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13724658(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As7+1:Ae7,As2+1:Ae2,
     & As4+1:Ae4,As6+1:Ae6,As5+1:Ae5,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i7,i2,i4,i6,i5,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12358467(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As5+1:Ae5,
     & As8+1:Ae8,As4+1:Ae4,As6+1:Ae6,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i3,i5,i8,i4,i6,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13567842(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As5+1:Ae5,As6+1:Ae6,
     & As7+1:Ae7,As8+1:Ae8,As4+1:Ae4,As2+1:Ae2)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i5,i6,i7,i8,i4,i2)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13468257(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As4+1:Ae4,As6+1:Ae6,
     & As8+1:Ae8,As2+1:Ae2,As5+1:Ae5,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i4,i6,i8,i2,i5,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum14523876(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As4+1:Ae4,As5+1:Ae5,As2+1:Ae2,
     & As3+1:Ae3,As8+1:Ae8,As7+1:Ae7,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i4,i5,i2,i3,i8,i7,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23641758(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As6+1:Ae6,As4+1:Ae4,
     & As1+1:Ae1,As7+1:Ae7,As5+1:Ae5,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i6,i4,i1,i7,i5,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum345261(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As5+1:Ae5,As2+1:Ae2,
     & As6+1:Ae6,As1+1:Ae1)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i3,i4,i5,i2,i6,i1)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum156324(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As1+1:Ae1,As5+1:Ae5,As6+1:Ae6,As3+1:Ae3,
     & As2+1:Ae2,As4+1:Ae4)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i1,i5,i6,i3,i2,i4)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum16843257(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As6+1:Ae6,As8+1:Ae8,As4+1:Ae4,
     & As3+1:Ae3,As2+1:Ae2,As5+1:Ae5,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i6,i8,i4,i3,i2,i5,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23681457(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As6+1:Ae6,As8+1:Ae8,
     & As1+1:Ae1,As4+1:Ae4,As5+1:Ae5,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i6,i8,i1,i4,i5,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12734685(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As7+1:Ae7,As3+1:Ae3,
     & As4+1:Ae4,As6+1:Ae6,As8+1:Ae8,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i7,i3,i4,i6,i8,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum25841367(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As5+1:Ae5,As8+1:Ae8,As4+1:Ae4,
     & As1+1:Ae1,As3+1:Ae3,As6+1:Ae6,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i5,i8,i4,i1,i3,i6,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23714856(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As7+1:Ae7,As1+1:Ae1,
     & As4+1:Ae4,As8+1:Ae8,As5+1:Ae5,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i7,i1,i4,i8,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum14832567(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As4+1:Ae4,As8+1:Ae8,As3+1:Ae3,
     & As2+1:Ae2,As5+1:Ae5,As6+1:Ae6,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i4,i8,i3,i2,i5,i6,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum15634278(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As5+1:Ae5,As6+1:Ae6,As3+1:Ae3,
     & As4+1:Ae4,As2+1:Ae2,As7+1:Ae7,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i5,i6,i3,i4,i2,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13467825(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As4+1:Ae4,As6+1:Ae6,
     & As7+1:Ae7,As8+1:Ae8,As2+1:Ae2,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i4,i6,i7,i8,i2,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum24731658(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As4+1:Ae4,As7+1:Ae7,As3+1:Ae3,
     & As1+1:Ae1,As6+1:Ae6,As5+1:Ae5,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i4,i7,i3,i1,i6,i5,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum16742358(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As6+1:Ae6,As7+1:Ae7,As4+1:Ae4,
     & As2+1:Ae2,As3+1:Ae3,As5+1:Ae5,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i6,i7,i4,i2,i3,i5,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum27813456(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As7+1:Ae7,As8+1:Ae8,As1+1:Ae1,
     & As3+1:Ae3,As4+1:Ae4,As5+1:Ae5,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i7,i8,i1,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12564378(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As5+1:Ae5,As6+1:Ae6,
     & As4+1:Ae4,As3+1:Ae3,As7+1:Ae7,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i5,i6,i4,i3,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12467358(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As4+1:Ae4,As6+1:Ae6,
     & As7+1:Ae7,As3+1:Ae3,As5+1:Ae5,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i4,i6,i7,i3,i5,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13452768(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As4+1:Ae4,As5+1:Ae5,
     & As2+1:Ae2,As7+1:Ae7,As6+1:Ae6,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i4,i5,i2,i7,i6,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum21(As1,Ae1,As2,Ae2,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2
      integer i1,i2

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2)
      real(kind=8) B(As2+1:Ae2,As1+1:Ae1)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      A(i1,i2)=A(i1,i2)+C*B(i2,i1)
      enddo
      enddo

      end


      subroutine sum24581367(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As4+1:Ae4,As5+1:Ae5,As8+1:Ae8,
     & As1+1:Ae1,As3+1:Ae3,As6+1:Ae6,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i4,i5,i8,i1,i3,i6,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum15723468(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As5+1:Ae5,As7+1:Ae7,As2+1:Ae2,
     & As3+1:Ae3,As4+1:Ae4,As6+1:Ae6,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i5,i7,i2,i3,i4,i6,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13457268(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As4+1:Ae4,As5+1:Ae5,
     & As7+1:Ae7,As2+1:Ae2,As6+1:Ae6,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i4,i5,i7,i2,i6,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13468275(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As4+1:Ae4,As6+1:Ae6,
     & As8+1:Ae8,As2+1:Ae2,As7+1:Ae7,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i4,i6,i8,i2,i7,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum34612758(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As6+1:Ae6,As1+1:Ae1,
     & As2+1:Ae2,As7+1:Ae7,As5+1:Ae5,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i4,i6,i1,i2,i7,i5,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12534687(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As5+1:Ae5,As3+1:Ae3,
     & As4+1:Ae4,As6+1:Ae6,As8+1:Ae8,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i5,i3,i4,i6,i8,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum14723658(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As4+1:Ae4,As7+1:Ae7,As2+1:Ae2,
     & As3+1:Ae3,As6+1:Ae6,As5+1:Ae5,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i4,i7,i2,i3,i6,i5,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum123564(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As5+1:Ae5,
     & As6+1:Ae6,As4+1:Ae4)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i1,i2,i3,i5,i6,i4)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum14782356(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As4+1:Ae4,As7+1:Ae7,As8+1:Ae8,
     & As2+1:Ae2,As3+1:Ae3,As5+1:Ae5,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i4,i7,i8,i2,i3,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum34582167(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As5+1:Ae5,As8+1:Ae8,
     & As2+1:Ae2,As1+1:Ae1,As6+1:Ae6,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i4,i5,i8,i2,i1,i6,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12734568(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As7+1:Ae7,As3+1:Ae3,
     & As4+1:Ae4,As5+1:Ae5,As6+1:Ae6,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i7,i3,i4,i5,i6,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13724568(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As7+1:Ae7,As2+1:Ae2,
     & As4+1:Ae4,As5+1:Ae5,As6+1:Ae6,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i7,i2,i4,i5,i6,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13482576(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As4+1:Ae4,As8+1:Ae8,
     & As2+1:Ae2,As5+1:Ae5,As7+1:Ae7,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i4,i8,i2,i5,i7,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum345216(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As5+1:Ae5,As2+1:Ae2,
     & As1+1:Ae1,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i3,i4,i5,i2,i1,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum34612785(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As6+1:Ae6,As1+1:Ae1,
     & As2+1:Ae2,As7+1:Ae7,As8+1:Ae8,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i4,i6,i1,i2,i7,i8,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13542786(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As5+1:Ae5,As4+1:Ae4,
     & As2+1:Ae2,As7+1:Ae7,As8+1:Ae8,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i5,i4,i2,i7,i8,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum17832456(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As7+1:Ae7,As8+1:Ae8,As3+1:Ae3,
     & As2+1:Ae2,As4+1:Ae4,As5+1:Ae5,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i7,i8,i3,i2,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum4132(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer i1,i2,i3,i4

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real(kind=8) B(As4+1:Ae4,As1+1:Ae1,As3+1:Ae3,As2+1:Ae2)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      A(i1,i2,i3,i4)=A(i1,i2,i3,i4)+C*B(i4,i1,i3,i2)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12634785(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As6+1:Ae6,As3+1:Ae3,
     & As4+1:Ae4,As7+1:Ae7,As8+1:Ae8,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i6,i3,i4,i7,i8,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23814657(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As8+1:Ae8,As1+1:Ae1,
     & As4+1:Ae4,As6+1:Ae6,As5+1:Ae5,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i8,i1,i4,i6,i5,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum24681357(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As4+1:Ae4,As6+1:Ae6,As8+1:Ae8,
     & As1+1:Ae1,As3+1:Ae3,As5+1:Ae5,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i4,i6,i8,i1,i3,i5,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23514786(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As5+1:Ae5,As1+1:Ae1,
     & As4+1:Ae4,As7+1:Ae7,As8+1:Ae8,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i5,i1,i4,i7,i8,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum24613758(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As4+1:Ae4,As6+1:Ae6,As1+1:Ae1,
     & As3+1:Ae3,As7+1:Ae7,As5+1:Ae5,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i4,i6,i1,i3,i7,i5,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum24831657(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As4+1:Ae4,As8+1:Ae8,As3+1:Ae3,
     & As1+1:Ae1,As6+1:Ae6,As5+1:Ae5,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i4,i8,i3,i1,i6,i5,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12345768(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As7+1:Ae7,As6+1:Ae6,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i3,i4,i5,i7,i6,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12743658(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As7+1:Ae7,As4+1:Ae4,
     & As3+1:Ae3,As6+1:Ae6,As5+1:Ae5,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i7,i4,i3,i6,i5,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum14523678(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As4+1:Ae4,As5+1:Ae5,As2+1:Ae2,
     & As3+1:Ae3,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i4,i5,i2,i3,i6,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12583467(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As5+1:Ae5,As8+1:Ae8,
     & As3+1:Ae3,As4+1:Ae4,As6+1:Ae6,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i5,i8,i3,i4,i6,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13462785(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As4+1:Ae4,As6+1:Ae6,
     & As2+1:Ae2,As7+1:Ae7,As8+1:Ae8,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i4,i6,i2,i7,i8,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum24813675(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As4+1:Ae4,As8+1:Ae8,As1+1:Ae1,
     & As3+1:Ae3,As6+1:Ae6,As7+1:Ae7,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i4,i8,i1,i3,i6,i7,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23678145(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As6+1:Ae6,As7+1:Ae7,
     & As8+1:Ae8,As1+1:Ae1,As4+1:Ae4,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i6,i7,i8,i1,i4,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13724586(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As7+1:Ae7,As2+1:Ae2,
     & As4+1:Ae4,As5+1:Ae5,As8+1:Ae8,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i7,i2,i4,i5,i8,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12534876(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As5+1:Ae5,As3+1:Ae3,
     & As4+1:Ae4,As8+1:Ae8,As7+1:Ae7,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i5,i3,i4,i8,i7,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum36712458(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As6+1:Ae6,As7+1:Ae7,As1+1:Ae1,
     & As2+1:Ae2,As4+1:Ae4,As5+1:Ae5,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i6,i7,i1,i2,i4,i5,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12458376(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As4+1:Ae4,As5+1:Ae5,
     & As8+1:Ae8,As3+1:Ae3,As7+1:Ae7,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i4,i5,i8,i3,i7,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23641587(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As6+1:Ae6,As4+1:Ae4,
     & As1+1:Ae1,As5+1:Ae5,As8+1:Ae8,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i6,i4,i1,i5,i8,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum14582367(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As4+1:Ae4,As5+1:Ae5,As8+1:Ae8,
     & As2+1:Ae2,As3+1:Ae3,As6+1:Ae6,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i4,i5,i8,i2,i3,i6,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13724685(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As7+1:Ae7,As2+1:Ae2,
     & As4+1:Ae4,As6+1:Ae6,As8+1:Ae8,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i7,i2,i4,i6,i8,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum14532867(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As4+1:Ae4,As5+1:Ae5,As3+1:Ae3,
     & As2+1:Ae2,As8+1:Ae8,As6+1:Ae6,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i4,i5,i3,i2,i8,i6,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum14732856(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As4+1:Ae4,As7+1:Ae7,As3+1:Ae3,
     & As2+1:Ae2,As8+1:Ae8,As5+1:Ae5,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i4,i7,i3,i2,i8,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12543768(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As5+1:Ae5,As4+1:Ae4,
     & As3+1:Ae3,As7+1:Ae7,As6+1:Ae6,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i5,i4,i3,i7,i6,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23814765(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As8+1:Ae8,As1+1:Ae1,
     & As4+1:Ae4,As7+1:Ae7,As6+1:Ae6,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i8,i1,i4,i7,i6,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12346857(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As6+1:Ae6,As8+1:Ae8,As5+1:Ae5,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i3,i4,i6,i8,i5,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12374658(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As7+1:Ae7,
     & As4+1:Ae4,As6+1:Ae6,As5+1:Ae5,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i3,i7,i4,i6,i5,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23614875(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As6+1:Ae6,As1+1:Ae1,
     & As4+1:Ae4,As8+1:Ae8,As7+1:Ae7,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i6,i1,i4,i8,i7,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum26713458(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As6+1:Ae6,As7+1:Ae7,As1+1:Ae1,
     & As3+1:Ae3,As4+1:Ae4,As5+1:Ae5,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i6,i7,i1,i3,i4,i5,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12567834(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As5+1:Ae5,As6+1:Ae6,
     & As7+1:Ae7,As8+1:Ae8,As3+1:Ae3,As4+1:Ae4)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i5,i6,i7,i8,i3,i4)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12457386(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As4+1:Ae4,As5+1:Ae5,
     & As7+1:Ae7,As3+1:Ae3,As8+1:Ae8,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i4,i5,i7,i3,i8,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23674158(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As6+1:Ae6,As7+1:Ae7,
     & As4+1:Ae4,As1+1:Ae1,As5+1:Ae5,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i6,i7,i4,i1,i5,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12356784(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As5+1:Ae5,
     & As6+1:Ae6,As7+1:Ae7,As8+1:Ae8,As4+1:Ae4)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i3,i5,i6,i7,i8,i4)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum241356(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As2+1:Ae2,As4+1:Ae4,As1+1:Ae1,As3+1:Ae3,
     & As5+1:Ae5,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i2,i4,i1,i3,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12354678(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As5+1:Ae5,
     & As4+1:Ae4,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i3,i5,i4,i6,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum456231(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As4+1:Ae4,As5+1:Ae5,As6+1:Ae6,As2+1:Ae2,
     & As3+1:Ae3,As1+1:Ae1)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i4,i5,i6,i2,i3,i1)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum1423(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer i1,i2,i3,i4

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real(kind=8) B(As1+1:Ae1,As4+1:Ae4,As2+1:Ae2,As3+1:Ae3)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      A(i1,i2,i3,i4)=A(i1,i2,i3,i4)+C*B(i1,i4,i2,i3)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12834567(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As8+1:Ae8,As3+1:Ae3,
     & As4+1:Ae4,As5+1:Ae5,As6+1:Ae6,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i8,i3,i4,i5,i6,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum34568127(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As5+1:Ae5,As6+1:Ae6,
     & As8+1:Ae8,As1+1:Ae1,As2+1:Ae2,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i4,i5,i6,i8,i1,i2,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum14532768(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As4+1:Ae4,As5+1:Ae5,As3+1:Ae3,
     & As2+1:Ae2,As7+1:Ae7,As6+1:Ae6,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i4,i5,i3,i2,i7,i6,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum134526(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As4+1:Ae4,As5+1:Ae5,
     & As2+1:Ae2,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i1,i3,i4,i5,i2,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12678435(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As6+1:Ae6,As7+1:Ae7,
     & As8+1:Ae8,As4+1:Ae4,As3+1:Ae3,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i6,i7,i8,i4,i3,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum24831567(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As4+1:Ae4,As8+1:Ae8,As3+1:Ae3,
     & As1+1:Ae1,As5+1:Ae5,As6+1:Ae6,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i4,i8,i3,i1,i5,i6,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum15643278(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As5+1:Ae5,As6+1:Ae6,As4+1:Ae4,
     & As3+1:Ae3,As2+1:Ae2,As7+1:Ae7,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i5,i6,i4,i3,i2,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum34821675(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As8+1:Ae8,As2+1:Ae2,
     & As1+1:Ae1,As6+1:Ae6,As7+1:Ae7,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i4,i8,i2,i1,i6,i7,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12368457(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As6+1:Ae6,
     & As8+1:Ae8,As4+1:Ae4,As5+1:Ae5,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i3,i6,i8,i4,i5,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum156234(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As1+1:Ae1,As5+1:Ae5,As6+1:Ae6,As2+1:Ae2,
     & As3+1:Ae3,As4+1:Ae4)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i1,i5,i6,i2,i3,i4)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12534867(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As5+1:Ae5,As3+1:Ae3,
     & As4+1:Ae4,As8+1:Ae8,As6+1:Ae6,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i5,i3,i4,i8,i6,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum34681257(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As6+1:Ae6,As8+1:Ae8,
     & As1+1:Ae1,As2+1:Ae2,As5+1:Ae5,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i4,i6,i8,i1,i2,i5,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum14532786(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As4+1:Ae4,As5+1:Ae5,As3+1:Ae3,
     & As2+1:Ae2,As7+1:Ae7,As8+1:Ae8,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i4,i5,i3,i2,i7,i8,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum46821357(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As4+1:Ae4,As6+1:Ae6,As8+1:Ae8,As2+1:Ae2,
     & As1+1:Ae1,As3+1:Ae3,As5+1:Ae5,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i4,i6,i8,i2,i1,i3,i5,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23451786(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,As5+1:Ae5,
     & As1+1:Ae1,As7+1:Ae7,As8+1:Ae8,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i4,i5,i1,i7,i8,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum15732468(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As5+1:Ae5,As7+1:Ae7,As3+1:Ae3,
     & As2+1:Ae2,As4+1:Ae4,As6+1:Ae6,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i5,i7,i3,i2,i4,i6,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum37812456(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As7+1:Ae7,As8+1:Ae8,As1+1:Ae1,
     & As2+1:Ae2,As4+1:Ae4,As5+1:Ae5,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i7,i8,i1,i2,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum14578236(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As4+1:Ae4,As5+1:Ae5,As7+1:Ae7,
     & As8+1:Ae8,As2+1:Ae2,As3+1:Ae3,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i4,i5,i7,i8,i2,i3,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12364758(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As6+1:Ae6,
     & As4+1:Ae4,As7+1:Ae7,As5+1:Ae5,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i3,i6,i4,i7,i5,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum2431(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer i1,i2,i3,i4

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real(kind=8) B(As2+1:Ae2,As4+1:Ae4,As3+1:Ae3,As1+1:Ae1)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      A(i1,i2,i3,i4)=A(i1,i2,i3,i4)+C*B(i2,i4,i3,i1)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12384567(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As8+1:Ae8,
     & As4+1:Ae4,As5+1:Ae5,As6+1:Ae6,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i3,i8,i4,i5,i6,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12374685(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As7+1:Ae7,
     & As4+1:Ae4,As6+1:Ae6,As8+1:Ae8,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i3,i7,i4,i6,i8,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12834765(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As8+1:Ae8,As3+1:Ae3,
     & As4+1:Ae4,As7+1:Ae7,As6+1:Ae6,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i8,i3,i4,i7,i6,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum24731568(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As4+1:Ae4,As7+1:Ae7,As3+1:Ae3,
     & As1+1:Ae1,As5+1:Ae5,As6+1:Ae6,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i4,i7,i3,i1,i5,i6,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23714658(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As7+1:Ae7,As1+1:Ae1,
     & As4+1:Ae4,As6+1:Ae6,As5+1:Ae5,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i7,i1,i4,i6,i5,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum34621587(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As6+1:Ae6,As2+1:Ae2,
     & As1+1:Ae1,As5+1:Ae5,As8+1:Ae8,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i4,i6,i2,i1,i5,i8,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum14563278(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As4+1:Ae4,As5+1:Ae5,As6+1:Ae6,
     & As3+1:Ae3,As2+1:Ae2,As7+1:Ae7,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i4,i5,i6,i3,i2,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum27831456(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As7+1:Ae7,As8+1:Ae8,As3+1:Ae3,
     & As1+1:Ae1,As4+1:Ae4,As5+1:Ae5,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i7,i8,i3,i1,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12634587(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As6+1:Ae6,As3+1:Ae3,
     & As4+1:Ae4,As5+1:Ae5,As8+1:Ae8,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i6,i3,i4,i5,i8,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum24813657(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As4+1:Ae4,As8+1:Ae8,As1+1:Ae1,
     & As3+1:Ae3,As6+1:Ae6,As5+1:Ae5,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i4,i8,i1,i3,i6,i5,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12568437(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As5+1:Ae5,As6+1:Ae6,
     & As8+1:Ae8,As4+1:Ae4,As3+1:Ae3,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i5,i6,i8,i4,i3,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum34578216(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As5+1:Ae5,As7+1:Ae7,
     & As8+1:Ae8,As2+1:Ae2,As1+1:Ae1,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i4,i5,i7,i8,i2,i1,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13642785(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As6+1:Ae6,As4+1:Ae4,
     & As2+1:Ae2,As7+1:Ae7,As8+1:Ae8,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i6,i4,i2,i7,i8,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum356241(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As3+1:Ae3,As5+1:Ae5,As6+1:Ae6,As2+1:Ae2,
     & As4+1:Ae4,As1+1:Ae1)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i3,i5,i6,i2,i4,i1)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23461578(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,As6+1:Ae6,
     & As1+1:Ae1,As5+1:Ae5,As7+1:Ae7,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i4,i6,i1,i5,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23541867(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As5+1:Ae5,As4+1:Ae4,
     & As1+1:Ae1,As8+1:Ae8,As6+1:Ae6,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i5,i4,i1,i8,i6,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum134562(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As4+1:Ae4,As5+1:Ae5,
     & As6+1:Ae6,As2+1:Ae2)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i1,i3,i4,i5,i6,i2)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum45613278(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As4+1:Ae4,As5+1:Ae5,As6+1:Ae6,As1+1:Ae1,
     & As3+1:Ae3,As2+1:Ae2,As7+1:Ae7,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i4,i5,i6,i1,i3,i2,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum45812367(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As4+1:Ae4,As5+1:Ae5,As8+1:Ae8,As1+1:Ae1,
     & As2+1:Ae2,As3+1:Ae3,As6+1:Ae6,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i4,i5,i8,i1,i2,i3,i6,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23457186(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,As5+1:Ae5,
     & As7+1:Ae7,As1+1:Ae1,As8+1:Ae8,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i4,i5,i7,i1,i8,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13524867(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As5+1:Ae5,As2+1:Ae2,
     & As4+1:Ae4,As8+1:Ae8,As6+1:Ae6,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i5,i2,i4,i8,i6,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum24671358(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As4+1:Ae4,As6+1:Ae6,As7+1:Ae7,
     & As1+1:Ae1,As3+1:Ae3,As5+1:Ae5,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i4,i6,i7,i1,i3,i5,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12543867(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As5+1:Ae5,As4+1:Ae4,
     & As3+1:Ae3,As8+1:Ae8,As6+1:Ae6,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i5,i4,i3,i8,i6,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum234165(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,As1+1:Ae1,
     & As6+1:Ae6,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i2,i3,i4,i1,i6,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum34521786(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As5+1:Ae5,As2+1:Ae2,
     & As1+1:Ae1,As7+1:Ae7,As8+1:Ae8,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i4,i5,i2,i1,i7,i8,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13467258(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As4+1:Ae4,As6+1:Ae6,
     & As7+1:Ae7,As2+1:Ae2,As5+1:Ae5,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i4,i6,i7,i2,i5,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum14568327(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As4+1:Ae4,As5+1:Ae5,As6+1:Ae6,
     & As8+1:Ae8,As3+1:Ae3,As2+1:Ae2,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i4,i5,i6,i8,i3,i2,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum34567218(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As5+1:Ae5,As6+1:Ae6,
     & As7+1:Ae7,As2+1:Ae2,As1+1:Ae1,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i4,i5,i6,i7,i2,i1,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum34712568(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As7+1:Ae7,As1+1:Ae1,
     & As2+1:Ae2,As5+1:Ae5,As6+1:Ae6,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i4,i7,i1,i2,i5,i6,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12734658(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As7+1:Ae7,As3+1:Ae3,
     & As4+1:Ae4,As6+1:Ae6,As5+1:Ae5,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i7,i3,i4,i6,i5,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum356124(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As3+1:Ae3,As5+1:Ae5,As6+1:Ae6,As1+1:Ae1,
     & As2+1:Ae2,As4+1:Ae4)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i3,i5,i6,i1,i2,i4)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum26814357(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As6+1:Ae6,As8+1:Ae8,As1+1:Ae1,
     & As4+1:Ae4,As3+1:Ae3,As5+1:Ae5,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i6,i8,i1,i4,i3,i5,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum136245(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As6+1:Ae6,As2+1:Ae2,
     & As4+1:Ae4,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i1,i3,i6,i2,i4,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum2314(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer i1,i2,i3,i4

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As1+1:Ae1,As4+1:Ae4)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      A(i1,i2,i3,i4)=A(i1,i2,i3,i4)+C*B(i2,i3,i1,i4)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12683457(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As6+1:Ae6,As8+1:Ae8,
     & As3+1:Ae3,As4+1:Ae4,As5+1:Ae5,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i6,i8,i3,i4,i5,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12634758(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As6+1:Ae6,As3+1:Ae3,
     & As4+1:Ae4,As7+1:Ae7,As5+1:Ae5,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i6,i3,i4,i7,i5,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum235614(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As5+1:Ae5,As6+1:Ae6,
     & As1+1:Ae1,As4+1:Ae4)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i2,i3,i5,i6,i1,i4)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23641857(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As6+1:Ae6,As4+1:Ae4,
     & As1+1:Ae1,As8+1:Ae8,As5+1:Ae5,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i6,i4,i1,i8,i5,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13568247(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As5+1:Ae5,As6+1:Ae6,
     & As8+1:Ae8,As2+1:Ae2,As4+1:Ae4,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i5,i6,i8,i2,i4,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum14823675(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As4+1:Ae4,As8+1:Ae8,As2+1:Ae2,
     & As3+1:Ae3,As6+1:Ae6,As7+1:Ae7,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i4,i8,i2,i3,i6,i7,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12543786(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As5+1:Ae5,As4+1:Ae4,
     & As3+1:Ae3,As7+1:Ae7,As8+1:Ae8,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i5,i4,i3,i7,i8,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum34512687(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As5+1:Ae5,As1+1:Ae1,
     & As2+1:Ae2,As6+1:Ae6,As8+1:Ae8,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i4,i5,i1,i2,i6,i8,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum245613(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As2+1:Ae2,As4+1:Ae4,As5+1:Ae5,As6+1:Ae6,
     & As1+1:Ae1,As3+1:Ae3)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i2,i4,i5,i6,i1,i3)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum345126(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As5+1:Ae5,As1+1:Ae1,
     & As2+1:Ae2,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i3,i4,i5,i1,i2,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13462758(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As4+1:Ae4,As6+1:Ae6,
     & As2+1:Ae2,As7+1:Ae7,As5+1:Ae5,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i4,i6,i2,i7,i5,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum145236(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As1+1:Ae1,As4+1:Ae4,As5+1:Ae5,As2+1:Ae2,
     & As3+1:Ae3,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i1,i4,i5,i2,i3,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12643587(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As6+1:Ae6,As4+1:Ae4,
     & As3+1:Ae3,As5+1:Ae5,As8+1:Ae8,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i6,i4,i3,i5,i8,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13456278(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As4+1:Ae4,As5+1:Ae5,
     & As6+1:Ae6,As2+1:Ae2,As7+1:Ae7,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i4,i5,i6,i2,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum235641(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As5+1:Ae5,As6+1:Ae6,
     & As4+1:Ae4,As1+1:Ae1)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i2,i3,i5,i6,i4,i1)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23457168(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,As5+1:Ae5,
     & As7+1:Ae7,As1+1:Ae1,As6+1:Ae6,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i4,i5,i7,i1,i6,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum34578126(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As5+1:Ae5,As7+1:Ae7,
     & As8+1:Ae8,As1+1:Ae1,As2+1:Ae2,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i4,i5,i7,i8,i1,i2,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum123546(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As5+1:Ae5,
     & As4+1:Ae4,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i1,i2,i3,i5,i4,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum14823756(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As4+1:Ae4,As8+1:Ae8,As2+1:Ae2,
     & As3+1:Ae3,As7+1:Ae7,As5+1:Ae5,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i4,i8,i2,i3,i7,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12634875(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As6+1:Ae6,As3+1:Ae3,
     & As4+1:Ae4,As8+1:Ae8,As7+1:Ae7,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i6,i3,i4,i8,i7,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12834756(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As8+1:Ae8,As3+1:Ae3,
     & As4+1:Ae4,As7+1:Ae7,As5+1:Ae5,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i8,i3,i4,i7,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13824657(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As8+1:Ae8,As2+1:Ae2,
     & As4+1:Ae4,As6+1:Ae6,As5+1:Ae5,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i8,i2,i4,i6,i5,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12483576(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As4+1:Ae4,As8+1:Ae8,
     & As3+1:Ae3,As5+1:Ae5,As7+1:Ae7,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i4,i8,i3,i5,i7,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12364578(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As6+1:Ae6,
     & As4+1:Ae4,As5+1:Ae5,As7+1:Ae7,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i3,i6,i4,i5,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23814576(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As8+1:Ae8,As1+1:Ae1,
     & As4+1:Ae4,As5+1:Ae5,As7+1:Ae7,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i8,i1,i4,i5,i7,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum14523786(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As4+1:Ae4,As5+1:Ae5,As2+1:Ae2,
     & As3+1:Ae3,As7+1:Ae7,As8+1:Ae8,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i4,i5,i2,i3,i7,i8,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12743685(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As7+1:Ae7,As4+1:Ae4,
     & As3+1:Ae3,As6+1:Ae6,As8+1:Ae8,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i7,i4,i3,i6,i8,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12674358(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As6+1:Ae6,As7+1:Ae7,
     & As4+1:Ae4,As3+1:Ae3,As5+1:Ae5,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i6,i7,i4,i3,i5,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum35621478(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As5+1:Ae5,As6+1:Ae6,As2+1:Ae2,
     & As1+1:Ae1,As4+1:Ae4,As7+1:Ae7,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i5,i6,i2,i1,i4,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum15623478(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As5+1:Ae5,As6+1:Ae6,As2+1:Ae2,
     & As3+1:Ae3,As4+1:Ae4,As7+1:Ae7,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i5,i6,i2,i3,i4,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum245136(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As2+1:Ae2,As4+1:Ae4,As5+1:Ae5,As1+1:Ae1,
     & As3+1:Ae3,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i2,i4,i5,i1,i3,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23841756(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As8+1:Ae8,As4+1:Ae4,
     & As1+1:Ae1,As7+1:Ae7,As5+1:Ae5,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i8,i4,i1,i7,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum24673158(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As4+1:Ae4,As6+1:Ae6,As7+1:Ae7,
     & As3+1:Ae3,As1+1:Ae1,As5+1:Ae5,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i4,i6,i7,i3,i1,i5,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum15632478(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As5+1:Ae5,As6+1:Ae6,As3+1:Ae3,
     & As2+1:Ae2,As4+1:Ae4,As7+1:Ae7,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i5,i6,i3,i2,i4,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13842756(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As8+1:Ae8,As4+1:Ae4,
     & As2+1:Ae2,As7+1:Ae7,As5+1:Ae5,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i8,i4,i2,i7,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum24781356(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As4+1:Ae4,As7+1:Ae7,As8+1:Ae8,
     & As1+1:Ae1,As3+1:Ae3,As5+1:Ae5,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i4,i7,i8,i1,i3,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum35721468(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As5+1:Ae5,As7+1:Ae7,As2+1:Ae2,
     & As1+1:Ae1,As4+1:Ae4,As6+1:Ae6,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i5,i7,i2,i1,i4,i6,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum146325(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As1+1:Ae1,As4+1:Ae4,As6+1:Ae6,As3+1:Ae3,
     & As2+1:Ae2,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i1,i4,i6,i3,i2,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum37821456(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As7+1:Ae7,As8+1:Ae8,As2+1:Ae2,
     & As1+1:Ae1,As4+1:Ae4,As5+1:Ae5,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i7,i8,i2,i1,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12458367(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As4+1:Ae4,As5+1:Ae5,
     & As8+1:Ae8,As3+1:Ae3,As6+1:Ae6,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i4,i5,i8,i3,i6,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum234651(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,As6+1:Ae6,
     & As5+1:Ae5,As1+1:Ae1)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i2,i3,i4,i6,i5,i1)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum24713658(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As4+1:Ae4,As7+1:Ae7,As1+1:Ae1,
     & As3+1:Ae3,As6+1:Ae6,As5+1:Ae5,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i4,i7,i1,i3,i6,i5,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum342561(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As2+1:Ae2,As5+1:Ae5,
     & As6+1:Ae6,As1+1:Ae1)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i3,i4,i2,i5,i6,i1)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum34812576(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As8+1:Ae8,As1+1:Ae1,
     & As2+1:Ae2,As5+1:Ae5,As7+1:Ae7,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i4,i8,i1,i2,i5,i7,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23714685(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As7+1:Ae7,As1+1:Ae1,
     & As4+1:Ae4,As6+1:Ae6,As8+1:Ae8,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i7,i1,i4,i6,i8,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum24813576(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As4+1:Ae4,As8+1:Ae8,As1+1:Ae1,
     & As3+1:Ae3,As5+1:Ae5,As7+1:Ae7,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i4,i8,i1,i3,i5,i7,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum14682357(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As4+1:Ae4,As6+1:Ae6,As8+1:Ae8,
     & As2+1:Ae2,As3+1:Ae3,As5+1:Ae5,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i4,i6,i8,i2,i3,i5,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum15743268(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As5+1:Ae5,As7+1:Ae7,As4+1:Ae4,
     & As3+1:Ae3,As2+1:Ae2,As6+1:Ae6,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i5,i7,i4,i3,i2,i6,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum34721856(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As7+1:Ae7,As2+1:Ae2,
     & As1+1:Ae1,As8+1:Ae8,As5+1:Ae5,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i4,i7,i2,i1,i8,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12478365(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As4+1:Ae4,As7+1:Ae7,
     & As8+1:Ae8,As3+1:Ae3,As6+1:Ae6,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i4,i7,i8,i3,i6,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum34612578(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As6+1:Ae6,As1+1:Ae1,
     & As2+1:Ae2,As5+1:Ae5,As7+1:Ae7,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i4,i6,i1,i2,i5,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum24567831(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As4+1:Ae4,As5+1:Ae5,As6+1:Ae6,
     & As7+1:Ae7,As8+1:Ae8,As3+1:Ae3,As1+1:Ae1)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i4,i5,i6,i7,i8,i3,i1)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum14532678(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As4+1:Ae4,As5+1:Ae5,As3+1:Ae3,
     & As2+1:Ae2,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i4,i5,i3,i2,i6,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum34712586(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As7+1:Ae7,As1+1:Ae1,
     & As2+1:Ae2,As5+1:Ae5,As8+1:Ae8,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i4,i7,i1,i2,i5,i8,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12574368(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As5+1:Ae5,As7+1:Ae7,
     & As4+1:Ae4,As3+1:Ae3,As6+1:Ae6,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i5,i7,i4,i3,i6,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12367485(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As6+1:Ae6,
     & As7+1:Ae7,As4+1:Ae4,As8+1:Ae8,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i3,i6,i7,i4,i8,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23741856(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As7+1:Ae7,As4+1:Ae4,
     & As1+1:Ae1,As8+1:Ae8,As5+1:Ae5,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i7,i4,i1,i8,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23471658(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,As7+1:Ae7,
     & As1+1:Ae1,As6+1:Ae6,As5+1:Ae5,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i4,i7,i1,i6,i5,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum25814367(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As5+1:Ae5,As8+1:Ae8,As1+1:Ae1,
     & As4+1:Ae4,As3+1:Ae3,As6+1:Ae6,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i5,i8,i1,i4,i3,i6,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum14568237(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As4+1:Ae4,As5+1:Ae5,As6+1:Ae6,
     & As8+1:Ae8,As2+1:Ae2,As3+1:Ae3,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i4,i5,i6,i8,i2,i3,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum3214(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer i1,i2,i3,i4

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real(kind=8) B(As3+1:Ae3,As2+1:Ae2,As1+1:Ae1,As4+1:Ae4)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      A(i1,i2,i3,i4)=A(i1,i2,i3,i4)+C*B(i3,i2,i1,i4)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13456782(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As4+1:Ae4,As5+1:Ae5,
     & As6+1:Ae6,As7+1:Ae7,As8+1:Ae8,As2+1:Ae2)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i4,i5,i6,i7,i8,i2)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12356748(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As5+1:Ae5,
     & As6+1:Ae6,As7+1:Ae7,As4+1:Ae4,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i3,i5,i6,i7,i4,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23814675(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As8+1:Ae8,As1+1:Ae1,
     & As4+1:Ae4,As6+1:Ae6,As7+1:Ae7,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i8,i1,i4,i6,i7,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum34678215(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As6+1:Ae6,As7+1:Ae7,
     & As8+1:Ae8,As2+1:Ae2,As1+1:Ae1,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i4,i6,i7,i8,i2,i1,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13567428(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As5+1:Ae5,As6+1:Ae6,
     & As7+1:Ae7,As4+1:Ae4,As2+1:Ae2,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i5,i6,i7,i4,i2,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12473586(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As4+1:Ae4,As7+1:Ae7,
     & As3+1:Ae3,As5+1:Ae5,As8+1:Ae8,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i4,i7,i3,i5,i8,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum25813467(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As5+1:Ae5,As8+1:Ae8,As1+1:Ae1,
     & As3+1:Ae3,As4+1:Ae4,As6+1:Ae6,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i5,i8,i1,i3,i4,i6,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13742568(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As7+1:Ae7,As4+1:Ae4,
     & As2+1:Ae2,As5+1:Ae5,As6+1:Ae6,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i7,i4,i2,i5,i6,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12345876(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As8+1:Ae8,As7+1:Ae7,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i3,i4,i5,i8,i7,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12384576(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As8+1:Ae8,
     & As4+1:Ae4,As5+1:Ae5,As7+1:Ae7,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i3,i8,i4,i5,i7,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23467185(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,As6+1:Ae6,
     & As7+1:Ae7,As1+1:Ae1,As8+1:Ae8,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i4,i6,i7,i1,i8,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13578246(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As5+1:Ae5,As7+1:Ae7,
     & As8+1:Ae8,As2+1:Ae2,As4+1:Ae4,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i5,i7,i8,i2,i4,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum34512678(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As5+1:Ae5,As1+1:Ae1,
     & As2+1:Ae2,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i4,i5,i1,i2,i6,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum34621578(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As6+1:Ae6,As2+1:Ae2,
     & As1+1:Ae1,As5+1:Ae5,As7+1:Ae7,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i4,i6,i2,i1,i5,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum34521867(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As5+1:Ae5,As2+1:Ae2,
     & As1+1:Ae1,As8+1:Ae8,As6+1:Ae6,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i4,i5,i2,i1,i8,i6,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13452678(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As4+1:Ae4,As5+1:Ae5,
     & As2+1:Ae2,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i4,i5,i2,i6,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13482567(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As4+1:Ae4,As8+1:Ae8,
     & As2+1:Ae2,As5+1:Ae5,As6+1:Ae6,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i4,i8,i2,i5,i6,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum35812467(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As5+1:Ae5,As8+1:Ae8,As1+1:Ae1,
     & As2+1:Ae2,As4+1:Ae4,As6+1:Ae6,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i5,i8,i1,i2,i4,i6,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13567824(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As5+1:Ae5,As6+1:Ae6,
     & As7+1:Ae7,As8+1:Ae8,As2+1:Ae2,As4+1:Ae4)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i5,i6,i7,i8,i2,i4)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13472568(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As4+1:Ae4,As7+1:Ae7,
     & As2+1:Ae2,As5+1:Ae5,As6+1:Ae6,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i4,i7,i2,i5,i6,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23841657(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As8+1:Ae8,As4+1:Ae4,
     & As1+1:Ae1,As6+1:Ae6,As5+1:Ae5,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i8,i4,i1,i6,i5,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum24713586(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As4+1:Ae4,As7+1:Ae7,As1+1:Ae1,
     & As3+1:Ae3,As5+1:Ae5,As8+1:Ae8,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i4,i7,i1,i3,i5,i8,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum34562178(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As5+1:Ae5,As6+1:Ae6,
     & As2+1:Ae2,As1+1:Ae1,As7+1:Ae7,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i4,i5,i6,i2,i1,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13624857(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As6+1:Ae6,As2+1:Ae2,
     & As4+1:Ae4,As8+1:Ae8,As5+1:Ae5,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i6,i2,i4,i8,i5,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12357846(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As5+1:Ae5,
     & As7+1:Ae7,As8+1:Ae8,As4+1:Ae4,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i3,i5,i7,i8,i4,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13478256(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As4+1:Ae4,As7+1:Ae7,
     & As8+1:Ae8,As2+1:Ae2,As5+1:Ae5,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i4,i7,i8,i2,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum423561(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As4+1:Ae4,As2+1:Ae2,As3+1:Ae3,As5+1:Ae5,
     & As6+1:Ae6,As1+1:Ae1)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i4,i2,i3,i5,i6,i1)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum3241(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer i1,i2,i3,i4

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real(kind=8) B(As3+1:Ae3,As2+1:Ae2,As4+1:Ae4,As1+1:Ae1)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      A(i1,i2,i3,i4)=A(i1,i2,i3,i4)+C*B(i3,i2,i4,i1)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum34612857(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As6+1:Ae6,As1+1:Ae1,
     & As2+1:Ae2,As8+1:Ae8,As5+1:Ae5,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i4,i6,i1,i2,i8,i5,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum34812756(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As8+1:Ae8,As1+1:Ae1,
     & As2+1:Ae2,As7+1:Ae7,As5+1:Ae5,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i4,i8,i1,i2,i7,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum46712358(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As4+1:Ae4,As6+1:Ae6,As7+1:Ae7,As1+1:Ae1,
     & As2+1:Ae2,As3+1:Ae3,As5+1:Ae5,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i4,i6,i7,i1,i2,i3,i5,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13578426(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As5+1:Ae5,As7+1:Ae7,
     & As8+1:Ae8,As4+1:Ae4,As2+1:Ae2,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i5,i7,i8,i4,i2,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23574168(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As5+1:Ae5,As7+1:Ae7,
     & As4+1:Ae4,As1+1:Ae1,As6+1:Ae6,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i5,i7,i4,i1,i6,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum24683157(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As4+1:Ae4,As6+1:Ae6,As8+1:Ae8,
     & As3+1:Ae3,As1+1:Ae1,As5+1:Ae5,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i4,i6,i8,i3,i1,i5,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum125346(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As5+1:Ae5,As3+1:Ae3,
     & As4+1:Ae4,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i1,i2,i5,i3,i4,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13478265(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As4+1:Ae4,As7+1:Ae7,
     & As8+1:Ae8,As2+1:Ae2,As6+1:Ae6,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i4,i7,i8,i2,i6,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13624785(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As6+1:Ae6,As2+1:Ae2,
     & As4+1:Ae4,As7+1:Ae7,As8+1:Ae8,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i6,i2,i4,i7,i8,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12568347(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As5+1:Ae5,As6+1:Ae6,
     & As8+1:Ae8,As3+1:Ae3,As4+1:Ae4,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i5,i6,i8,i3,i4,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12643578(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As6+1:Ae6,As4+1:Ae4,
     & As3+1:Ae3,As5+1:Ae5,As7+1:Ae7,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i6,i4,i3,i5,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13678245(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As6+1:Ae6,As7+1:Ae7,
     & As8+1:Ae8,As2+1:Ae2,As4+1:Ae4,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i6,i7,i8,i2,i4,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23451768(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,As5+1:Ae5,
     & As1+1:Ae1,As7+1:Ae7,As6+1:Ae6,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i4,i5,i1,i7,i6,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum346125(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As6+1:Ae6,As1+1:Ae1,
     & As2+1:Ae2,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i3,i4,i6,i1,i2,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13642857(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As6+1:Ae6,As4+1:Ae4,
     & As2+1:Ae2,As8+1:Ae8,As5+1:Ae5,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i6,i4,i2,i8,i5,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum14572368(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As4+1:Ae4,As5+1:Ae5,As7+1:Ae7,
     & As2+1:Ae2,As3+1:Ae3,As6+1:Ae6,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i4,i5,i7,i2,i3,i6,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum15824367(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As5+1:Ae5,As8+1:Ae8,As2+1:Ae2,
     & As4+1:Ae4,As3+1:Ae3,As6+1:Ae6,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i5,i8,i2,i4,i3,i6,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12483567(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As4+1:Ae4,As8+1:Ae8,
     & As3+1:Ae3,As5+1:Ae5,As6+1:Ae6,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i4,i8,i3,i5,i6,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum134265(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As4+1:Ae4,As2+1:Ae2,
     & As6+1:Ae6,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i1,i3,i4,i2,i6,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12346785(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As6+1:Ae6,As7+1:Ae7,As8+1:Ae8,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i3,i4,i6,i7,i8,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23467815(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,As6+1:Ae6,
     & As7+1:Ae7,As8+1:Ae8,As1+1:Ae1,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i4,i6,i7,i8,i1,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum1432(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer i1,i2,i3,i4

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real(kind=8) B(As1+1:Ae1,As4+1:Ae4,As3+1:Ae3,As2+1:Ae2)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      A(i1,i2,i3,i4)=A(i1,i2,i3,i4)+C*B(i1,i4,i3,i2)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13678425(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As6+1:Ae6,As7+1:Ae7,
     & As8+1:Ae8,As4+1:Ae4,As2+1:Ae2,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i6,i7,i8,i4,i2,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12346875(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As6+1:Ae6,As8+1:Ae8,As7+1:Ae7,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i3,i4,i6,i8,i7,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12453678(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As4+1:Ae4,As5+1:Ae5,
     & As3+1:Ae3,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i4,i5,i3,i6,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23461785(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,As6+1:Ae6,
     & As1+1:Ae1,As7+1:Ae7,As8+1:Ae8,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i4,i6,i1,i7,i8,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum26841357(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As6+1:Ae6,As8+1:Ae8,As4+1:Ae4,
     & As1+1:Ae1,As3+1:Ae3,As5+1:Ae5,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i6,i8,i4,i1,i3,i5,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12374586(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As7+1:Ae7,
     & As4+1:Ae4,As5+1:Ae5,As8+1:Ae8,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i3,i7,i4,i5,i8,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum2143(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer i1,i2,i3,i4

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real(kind=8) B(As2+1:Ae2,As1+1:Ae1,As4+1:Ae4,As3+1:Ae3)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      A(i1,i2,i3,i4)=A(i1,i2,i3,i4)+C*B(i2,i1,i4,i3)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum27841356(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As7+1:Ae7,As8+1:Ae8,As4+1:Ae4,
     & As1+1:Ae1,As3+1:Ae3,As5+1:Ae5,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i7,i8,i4,i1,i3,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum24731586(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As4+1:Ae4,As7+1:Ae7,As3+1:Ae3,
     & As1+1:Ae1,As5+1:Ae5,As8+1:Ae8,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i4,i7,i3,i1,i5,i8,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum512346(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As5+1:Ae5,As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,
     & As4+1:Ae4,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i5,i1,i2,i3,i4,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum34621785(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As6+1:Ae6,As2+1:Ae2,
     & As1+1:Ae1,As7+1:Ae7,As8+1:Ae8,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i4,i6,i2,i1,i7,i8,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23451687(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,As5+1:Ae5,
     & As1+1:Ae1,As6+1:Ae6,As8+1:Ae8,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i4,i5,i1,i6,i8,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum234156(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,As1+1:Ae1,
     & As5+1:Ae5,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i2,i3,i4,i1,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23481675(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,As8+1:Ae8,
     & As1+1:Ae1,As6+1:Ae6,As7+1:Ae7,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i4,i8,i1,i6,i7,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12743586(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As7+1:Ae7,As4+1:Ae4,
     & As3+1:Ae3,As5+1:Ae5,As8+1:Ae8,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i7,i4,i3,i5,i8,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum15823467(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As5+1:Ae5,As8+1:Ae8,As2+1:Ae2,
     & As3+1:Ae3,As4+1:Ae4,As6+1:Ae6,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i5,i8,i2,i3,i4,i6,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum35714268(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As5+1:Ae5,As7+1:Ae7,As1+1:Ae1,
     & As4+1:Ae4,As2+1:Ae2,As6+1:Ae6,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i5,i7,i1,i4,i2,i6,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12364785(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As6+1:Ae6,
     & As4+1:Ae4,As7+1:Ae7,As8+1:Ae8,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i3,i6,i4,i7,i8,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23481657(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,As8+1:Ae8,
     & As1+1:Ae1,As6+1:Ae6,As5+1:Ae5,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i4,i8,i1,i6,i5,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13574268(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As5+1:Ae5,As7+1:Ae7,
     & As4+1:Ae4,As2+1:Ae2,As6+1:Ae6,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i5,i7,i4,i2,i6,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13842576(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As8+1:Ae8,As4+1:Ae4,
     & As2+1:Ae2,As5+1:Ae5,As7+1:Ae7,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i8,i4,i2,i5,i7,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum24831756(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As4+1:Ae4,As8+1:Ae8,As3+1:Ae3,
     & As1+1:Ae1,As7+1:Ae7,As5+1:Ae5,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i4,i8,i3,i1,i7,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13472685(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As4+1:Ae4,As7+1:Ae7,
     & As2+1:Ae2,As6+1:Ae6,As8+1:Ae8,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i4,i7,i2,i6,i8,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum123465(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As6+1:Ae6,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i1,i2,i3,i4,i6,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum26714358(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As6+1:Ae6,As7+1:Ae7,As1+1:Ae1,
     & As4+1:Ae4,As3+1:Ae3,As5+1:Ae5,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i6,i7,i1,i4,i3,i5,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum24571368(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As4+1:Ae4,As5+1:Ae5,As7+1:Ae7,
     & As1+1:Ae1,As3+1:Ae3,As6+1:Ae6,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i4,i5,i7,i1,i3,i6,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13524786(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As5+1:Ae5,As2+1:Ae2,
     & As4+1:Ae4,As7+1:Ae7,As8+1:Ae8,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i5,i2,i4,i7,i8,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum24631857(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As4+1:Ae4,As6+1:Ae6,As3+1:Ae3,
     & As1+1:Ae1,As8+1:Ae8,As5+1:Ae5,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i4,i6,i3,i1,i8,i5,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum34567821(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As5+1:Ae5,As6+1:Ae6,
     & As7+1:Ae7,As8+1:Ae8,As2+1:Ae2,As1+1:Ae1)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i4,i5,i6,i7,i8,i2,i1)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum14732586(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As4+1:Ae4,As7+1:Ae7,As3+1:Ae3,
     & As2+1:Ae2,As5+1:Ae5,As8+1:Ae8,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i4,i7,i3,i2,i5,i8,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum15842367(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As5+1:Ae5,As8+1:Ae8,As4+1:Ae4,
     & As2+1:Ae2,As3+1:Ae3,As6+1:Ae6,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i5,i8,i4,i2,i3,i6,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum14573268(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As4+1:Ae4,As5+1:Ae5,As7+1:Ae7,
     & As3+1:Ae3,As2+1:Ae2,As6+1:Ae6,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i4,i5,i7,i3,i2,i6,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum34581267(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As5+1:Ae5,As8+1:Ae8,
     & As1+1:Ae1,As2+1:Ae2,As6+1:Ae6,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i4,i5,i8,i1,i2,i6,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum24731856(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As4+1:Ae4,As7+1:Ae7,As3+1:Ae3,
     & As1+1:Ae1,As8+1:Ae8,As5+1:Ae5,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i4,i7,i3,i1,i8,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23481567(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,As8+1:Ae8,
     & As1+1:Ae1,As5+1:Ae5,As6+1:Ae6,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i4,i8,i1,i5,i6,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum34781256(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As7+1:Ae7,As8+1:Ae8,
     & As1+1:Ae1,As2+1:Ae2,As5+1:Ae5,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i4,i7,i8,i1,i2,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum16832457(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As6+1:Ae6,As8+1:Ae8,As3+1:Ae3,
     & As2+1:Ae2,As4+1:Ae4,As5+1:Ae5,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i6,i8,i3,i2,i4,i5,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum124365(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As4+1:Ae4,As3+1:Ae3,
     & As6+1:Ae6,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i1,i2,i4,i3,i6,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum25613478(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As5+1:Ae5,As6+1:Ae6,As1+1:Ae1,
     & As3+1:Ae3,As4+1:Ae4,As7+1:Ae7,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i5,i6,i1,i3,i4,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum34512768(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As5+1:Ae5,As1+1:Ae1,
     & As2+1:Ae2,As7+1:Ae7,As6+1:Ae6,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i4,i5,i1,i2,i7,i6,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23514876(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As5+1:Ae5,As1+1:Ae1,
     & As4+1:Ae4,As8+1:Ae8,As7+1:Ae7,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i5,i1,i4,i8,i7,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum456123(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As4+1:Ae4,As5+1:Ae5,As6+1:Ae6,As1+1:Ae1,
     & As2+1:Ae2,As3+1:Ae3)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i4,i5,i6,i1,i2,i3)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum45821367(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As4+1:Ae4,As5+1:Ae5,As8+1:Ae8,As2+1:Ae2,
     & As1+1:Ae1,As3+1:Ae3,As6+1:Ae6,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i4,i5,i8,i2,i1,i3,i6,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23478165(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,As7+1:Ae7,
     & As8+1:Ae8,As1+1:Ae1,As6+1:Ae6,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i4,i7,i8,i1,i6,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum34672158(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As6+1:Ae6,As7+1:Ae7,
     & As2+1:Ae2,As1+1:Ae1,As5+1:Ae5,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i4,i6,i7,i2,i1,i5,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23584167(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As5+1:Ae5,As8+1:Ae8,
     & As4+1:Ae4,As1+1:Ae1,As6+1:Ae6,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i5,i8,i4,i1,i6,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum24513867(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As4+1:Ae4,As5+1:Ae5,As1+1:Ae1,
     & As3+1:Ae3,As8+1:Ae8,As6+1:Ae6,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i4,i5,i1,i3,i8,i6,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum1243(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer i1,i2,i3,i4

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As4+1:Ae4,As3+1:Ae3)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      A(i1,i2,i3,i4)=A(i1,i2,i3,i4)+C*B(i1,i2,i4,i3)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23567814(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As5+1:Ae5,As6+1:Ae6,
     & As7+1:Ae7,As8+1:Ae8,As1+1:Ae1,As4+1:Ae4)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i5,i6,i7,i8,i1,i4)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13564278(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As5+1:Ae5,As6+1:Ae6,
     & As4+1:Ae4,As2+1:Ae2,As7+1:Ae7,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i5,i6,i4,i2,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum3124(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer i1,i2,i3,i4

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real(kind=8) B(As3+1:Ae3,As1+1:Ae1,As2+1:Ae2,As4+1:Ae4)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      A(i1,i2,i3,i4)=A(i1,i2,i3,i4)+C*B(i3,i1,i2,i4)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum245316(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As2+1:Ae2,As4+1:Ae4,As5+1:Ae5,As3+1:Ae3,
     & As1+1:Ae1,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i2,i4,i5,i3,i1,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23514678(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As5+1:Ae5,As1+1:Ae1,
     & As4+1:Ae4,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i5,i1,i4,i6,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum15624378(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As5+1:Ae5,As6+1:Ae6,As2+1:Ae2,
     & As4+1:Ae4,As3+1:Ae3,As7+1:Ae7,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i5,i6,i2,i4,i3,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum34621857(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As6+1:Ae6,As2+1:Ae2,
     & As1+1:Ae1,As8+1:Ae8,As5+1:Ae5,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i4,i6,i2,i1,i8,i5,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12483675(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As4+1:Ae4,As8+1:Ae8,
     & As3+1:Ae3,As6+1:Ae6,As7+1:Ae7,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i4,i8,i3,i6,i7,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum123645(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As6+1:Ae6,
     & As4+1:Ae4,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i1,i2,i3,i6,i4,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13824765(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As8+1:Ae8,As2+1:Ae2,
     & As4+1:Ae4,As7+1:Ae7,As6+1:Ae6,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i8,i2,i4,i7,i6,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13682457(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As6+1:Ae6,As8+1:Ae8,
     & As2+1:Ae2,As4+1:Ae4,As5+1:Ae5,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i6,i8,i2,i4,i5,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23514687(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As5+1:Ae5,As1+1:Ae1,
     & As4+1:Ae4,As6+1:Ae6,As8+1:Ae8,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i5,i1,i4,i6,i8,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12473568(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As4+1:Ae4,As7+1:Ae7,
     & As3+1:Ae3,As5+1:Ae5,As6+1:Ae6,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i4,i7,i3,i5,i6,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum34721685(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As7+1:Ae7,As2+1:Ae2,
     & As1+1:Ae1,As6+1:Ae6,As8+1:Ae8,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i4,i7,i2,i1,i6,i8,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12843675(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As8+1:Ae8,As4+1:Ae4,
     & As3+1:Ae3,As6+1:Ae6,As7+1:Ae7,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i8,i4,i3,i6,i7,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum14623578(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As4+1:Ae4,As6+1:Ae6,As2+1:Ae2,
     & As3+1:Ae3,As5+1:Ae5,As7+1:Ae7,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i4,i6,i2,i3,i5,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum24513768(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As4+1:Ae4,As5+1:Ae5,As1+1:Ae1,
     & As3+1:Ae3,As7+1:Ae7,As6+1:Ae6,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i4,i5,i1,i3,i7,i6,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12834576(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As8+1:Ae8,As3+1:Ae3,
     & As4+1:Ae4,As5+1:Ae5,As7+1:Ae7,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i8,i3,i4,i5,i7,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum145326(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As1+1:Ae1,As4+1:Ae4,As5+1:Ae5,As3+1:Ae3,
     & As2+1:Ae2,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i1,i4,i5,i3,i2,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum45713268(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As4+1:Ae4,As5+1:Ae5,As7+1:Ae7,As1+1:Ae1,
     & As3+1:Ae3,As2+1:Ae2,As6+1:Ae6,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i4,i5,i7,i1,i3,i2,i6,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum34678125(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As6+1:Ae6,As7+1:Ae7,
     & As8+1:Ae8,As1+1:Ae1,As2+1:Ae2,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i4,i6,i7,i8,i1,i2,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum35821467(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As5+1:Ae5,As8+1:Ae8,As2+1:Ae2,
     & As1+1:Ae1,As4+1:Ae4,As6+1:Ae6,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i5,i8,i2,i1,i4,i6,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum1342(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer i1,i2,i3,i4

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As4+1:Ae4,As2+1:Ae2)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      A(i1,i2,i3,i4)=A(i1,i2,i3,i4)+C*B(i1,i3,i4,i2)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum24713856(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As4+1:Ae4,As7+1:Ae7,As1+1:Ae1,
     & As3+1:Ae3,As8+1:Ae8,As5+1:Ae5,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i4,i7,i1,i3,i8,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13467285(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As4+1:Ae4,As6+1:Ae6,
     & As7+1:Ae7,As2+1:Ae2,As8+1:Ae8,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i4,i6,i7,i2,i8,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13824567(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As8+1:Ae8,As2+1:Ae2,
     & As4+1:Ae4,As5+1:Ae5,As6+1:Ae6,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i8,i2,i4,i5,i6,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23541687(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As5+1:Ae5,As4+1:Ae4,
     & As1+1:Ae1,As6+1:Ae6,As8+1:Ae8,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i5,i4,i1,i6,i8,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum14623587(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As4+1:Ae4,As6+1:Ae6,As2+1:Ae2,
     & As3+1:Ae3,As5+1:Ae5,As8+1:Ae8,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i4,i6,i2,i3,i5,i8,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum15843267(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As5+1:Ae5,As8+1:Ae8,As4+1:Ae4,
     & As3+1:Ae3,As2+1:Ae2,As6+1:Ae6,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i5,i8,i4,i3,i2,i6,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum16724358(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As6+1:Ae6,As7+1:Ae7,As2+1:Ae2,
     & As4+1:Ae4,As3+1:Ae3,As5+1:Ae5,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i6,i7,i2,i4,i3,i5,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum35741268(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As5+1:Ae5,As7+1:Ae7,As4+1:Ae4,
     & As1+1:Ae1,As2+1:Ae2,As6+1:Ae6,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i5,i7,i4,i1,i2,i6,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum146235(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As1+1:Ae1,As4+1:Ae4,As6+1:Ae6,As2+1:Ae2,
     & As3+1:Ae3,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i1,i4,i6,i2,i3,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23468157(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,As6+1:Ae6,
     & As8+1:Ae8,As1+1:Ae1,As5+1:Ae5,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i4,i6,i8,i1,i5,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum234561(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,As5+1:Ae5,
     & As6+1:Ae6,As1+1:Ae1)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i2,i3,i4,i5,i6,i1)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23614578(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As6+1:Ae6,As1+1:Ae1,
     & As4+1:Ae4,As5+1:Ae5,As7+1:Ae7,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i6,i1,i4,i5,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum234516(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,As5+1:Ae5,
     & As1+1:Ae1,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i2,i3,i4,i5,i1,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12834657(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As8+1:Ae8,As3+1:Ae3,
     & As4+1:Ae4,As6+1:Ae6,As5+1:Ae5,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i8,i3,i4,i6,i5,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13842657(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As8+1:Ae8,As4+1:Ae4,
     & As2+1:Ae2,As6+1:Ae6,As5+1:Ae5,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i8,i4,i2,i6,i5,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23678415(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As6+1:Ae6,As7+1:Ae7,
     & As8+1:Ae8,As4+1:Ae4,As1+1:Ae1,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i6,i7,i8,i4,i1,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12367845(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As6+1:Ae6,
     & As7+1:Ae7,As8+1:Ae8,As4+1:Ae4,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i3,i6,i7,i8,i4,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23458176(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,As5+1:Ae5,
     & As8+1:Ae8,As1+1:Ae1,As7+1:Ae7,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i4,i5,i8,i1,i7,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum24561378(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As4+1:Ae4,As5+1:Ae5,As6+1:Ae6,
     & As1+1:Ae1,As3+1:Ae3,As7+1:Ae7,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i4,i5,i6,i1,i3,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13572468(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As5+1:Ae5,As7+1:Ae7,
     & As2+1:Ae2,As4+1:Ae4,As6+1:Ae6,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i5,i7,i2,i4,i6,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum125364(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As5+1:Ae5,As3+1:Ae3,
     & As6+1:Ae6,As4+1:Ae4)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i1,i2,i5,i3,i6,i4)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13842567(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As8+1:Ae8,As4+1:Ae4,
     & As2+1:Ae2,As5+1:Ae5,As6+1:Ae6,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i8,i4,i2,i5,i6,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum312456(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As3+1:Ae3,As1+1:Ae1,As2+1:Ae2,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i3,i1,i2,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13782456(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As7+1:Ae7,As8+1:Ae8,
     & As2+1:Ae2,As4+1:Ae4,As5+1:Ae5,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i7,i8,i2,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum246315(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As2+1:Ae2,As4+1:Ae4,As6+1:Ae6,As3+1:Ae3,
     & As1+1:Ae1,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i2,i4,i6,i3,i1,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13642587(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As6+1:Ae6,As4+1:Ae4,
     & As2+1:Ae2,As5+1:Ae5,As8+1:Ae8,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i6,i4,i2,i5,i8,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum4213(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer i1,i2,i3,i4

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real(kind=8) B(As4+1:Ae4,As2+1:Ae2,As1+1:Ae1,As3+1:Ae3)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      A(i1,i2,i3,i4)=A(i1,i2,i3,i4)+C*B(i4,i2,i1,i3)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13457826(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As4+1:Ae4,As5+1:Ae5,
     & As7+1:Ae7,As8+1:Ae8,As2+1:Ae2,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i4,i5,i7,i8,i2,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12463785(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As4+1:Ae4,As6+1:Ae6,
     & As3+1:Ae3,As7+1:Ae7,As8+1:Ae8,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i4,i6,i3,i7,i8,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum34712658(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As7+1:Ae7,As1+1:Ae1,
     & As2+1:Ae2,As6+1:Ae6,As5+1:Ae5,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i4,i7,i1,i2,i6,i5,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23741685(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As7+1:Ae7,As4+1:Ae4,
     & As1+1:Ae1,As6+1:Ae6,As8+1:Ae8,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i7,i4,i1,i6,i8,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum14632857(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As4+1:Ae4,As6+1:Ae6,As3+1:Ae3,
     & As2+1:Ae2,As8+1:Ae8,As5+1:Ae5,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i4,i6,i3,i2,i8,i5,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum35614278(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As5+1:Ae5,As6+1:Ae6,As1+1:Ae1,
     & As4+1:Ae4,As2+1:Ae2,As7+1:Ae7,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i5,i6,i1,i4,i2,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23641785(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As6+1:Ae6,As4+1:Ae4,
     & As1+1:Ae1,As7+1:Ae7,As8+1:Ae8,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i6,i4,i1,i7,i8,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23564178(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As5+1:Ae5,As6+1:Ae6,
     & As4+1:Ae4,As1+1:Ae1,As7+1:Ae7,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i5,i6,i4,i1,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum14583267(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As4+1:Ae4,As5+1:Ae5,As8+1:Ae8,
     & As3+1:Ae3,As2+1:Ae2,As6+1:Ae6,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i4,i5,i8,i3,i2,i6,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12734586(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As7+1:Ae7,As3+1:Ae3,
     & As4+1:Ae4,As5+1:Ae5,As8+1:Ae8,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i7,i3,i4,i5,i8,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum14632785(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As4+1:Ae4,As6+1:Ae6,As3+1:Ae3,
     & As2+1:Ae2,As7+1:Ae7,As8+1:Ae8,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i4,i6,i3,i2,i7,i8,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23468175(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,As6+1:Ae6,
     & As8+1:Ae8,As1+1:Ae1,As7+1:Ae7,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i4,i6,i8,i1,i7,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12567438(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As5+1:Ae5,As6+1:Ae6,
     & As7+1:Ae7,As4+1:Ae4,As3+1:Ae3,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i5,i6,i7,i4,i3,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum14623758(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As4+1:Ae4,As6+1:Ae6,As2+1:Ae2,
     & As3+1:Ae3,As7+1:Ae7,As5+1:Ae5,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i4,i6,i2,i3,i7,i5,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum34721658(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As4+1:Ae4,As7+1:Ae7,As2+1:Ae2,
     & As1+1:Ae1,As6+1:Ae6,As5+1:Ae5,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i4,i7,i2,i1,i6,i5,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12384657(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As8+1:Ae8,
     & As4+1:Ae4,As6+1:Ae6,As5+1:Ae5,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i3,i8,i4,i6,i5,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13472586(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As4+1:Ae4,As7+1:Ae7,
     & As2+1:Ae2,As5+1:Ae5,As8+1:Ae8,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i4,i7,i2,i5,i8,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum124635(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As4+1:Ae4,As6+1:Ae6,
     & As3+1:Ae3,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i1,i2,i4,i6,i3,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23841576(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As8+1:Ae8,As4+1:Ae4,
     & As1+1:Ae1,As5+1:Ae5,As7+1:Ae7,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i8,i4,i1,i5,i7,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12378456(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As7+1:Ae7,
     & As8+1:Ae8,As4+1:Ae4,As5+1:Ae5,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i3,i7,i8,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23741658(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As7+1:Ae7,As4+1:Ae4,
     & As1+1:Ae1,As6+1:Ae6,As5+1:Ae5,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i7,i4,i1,i6,i5,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum12734856(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As7+1:Ae7,As3+1:Ae3,
     & As4+1:Ae4,As8+1:Ae8,As5+1:Ae5,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i2,i7,i3,i4,i8,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum23541678(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As2+1:Ae2,As3+1:Ae3,As5+1:Ae5,As4+1:Ae4,
     & As1+1:Ae1,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i2,i3,i5,i4,i1,i6,i7,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum36821457(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As6+1:Ae6,As8+1:Ae8,As2+1:Ae2,
     & As1+1:Ae1,As4+1:Ae4,As5+1:Ae5,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i6,i8,i2,i1,i4,i5,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13456827(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As4+1:Ae4,As5+1:Ae5,
     & As6+1:Ae6,As8+1:Ae8,As2+1:Ae2,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i4,i5,i6,i8,i2,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum37841256(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As3+1:Ae3,As7+1:Ae7,As8+1:Ae8,As4+1:Ae4,
     & As1+1:Ae1,As2+1:Ae2,As5+1:Ae5,As6+1:Ae6)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i3,i7,i8,i4,i1,i2,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum13524768(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As3+1:Ae3,As5+1:Ae5,As2+1:Ae2,
     & As4+1:Ae4,As7+1:Ae7,As6+1:Ae6,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i3,i5,i2,i4,i7,i6,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum16834257(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As6+1:Ae6,As8+1:Ae8,As3+1:Ae3,
     & As4+1:Ae4,As2+1:Ae2,As5+1:Ae5,As7+1:Ae7)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i6,i8,i3,i4,i2,i5,i7)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum126345(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,
     & Ae5,As6,Ae6,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As6+1:Ae6,As3+1:Ae3,
     & As4+1:Ae4,As5+1:Ae5)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i1,i2,i6,i3,i4,i5)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sum14523768(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,As7,Ae7,As8,Ae8,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6,
     & As7,Ae7,As8,Ae8
      integer i1,i2,i3,i4,i5,i6,i7,i8

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6,As7+1:Ae7,As8+1:Ae8)
      real(kind=8) B(As1+1:Ae1,As4+1:Ae4,As5+1:Ae5,As2+1:Ae2,
     & As3+1:Ae3,As7+1:Ae7,As6+1:Ae6,As8+1:Ae8)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      do i5=As5+1,Ae5
      do i6=As6+1,Ae6
      do i7=As7+1,Ae7
      do i8=As8+1,Ae8
      A(i1,i2,i3,i4,i5,i6,i7,i8)=A(i1,i2,i3,i4,i5,i6,i7,
     & i8)+C*B(i1,i4,i5,i2,i3,i7,i6,i8)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sumx2341(Bs,Be,As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & A, B, C)

      implicit none

      integer Bs,Be
      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer i1,i2,i3,i4

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real(kind=8) B(Bs+1:Be,Bs+1:Be,Bs+1:Be,Bs+1:Be)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      A(i1,i2,i3,i4)=A(i1,i2,i3,i4)+C*B(i2,i3,i4,i1)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sumx3241(Bs,Be,As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & A, B, C)

      implicit none

      integer Bs,Be
      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer i1,i2,i3,i4

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real(kind=8) B(Bs+1:Be,Bs+1:Be,Bs+1:Be,Bs+1:Be)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      A(i1,i2,i3,i4)=A(i1,i2,i3,i4)+C*B(i3,i2,i4,i1)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sumx4213(Bs,Be,As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & A, B, C)

      implicit none

      integer Bs,Be
      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer i1,i2,i3,i4

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real(kind=8) B(Bs+1:Be,Bs+1:Be,Bs+1:Be,Bs+1:Be)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      A(i1,i2,i3,i4)=A(i1,i2,i3,i4)+C*B(i4,i2,i1,i3)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sumx21(Bs,Be,As1,Ae1,As2,Ae2,A, B, C)

      implicit none

      integer Bs,Be
      integer As1,Ae1,As2,Ae2
      integer i1,i2

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2)
      real(kind=8) B(Bs+1:Be,Bs+1:Be)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      A(i1,i2)=A(i1,i2)+C*B(i2,i1)
      enddo
      enddo

      end


      subroutine sumx3421(Bs,Be,As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & A, B, C)

      implicit none

      integer Bs,Be
      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer i1,i2,i3,i4

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real(kind=8) B(Bs+1:Be,Bs+1:Be,Bs+1:Be,Bs+1:Be)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      A(i1,i2,i3,i4)=A(i1,i2,i3,i4)+C*B(i3,i4,i2,i1)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sumx3412(Bs,Be,As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & A, B, C)

      implicit none

      integer Bs,Be
      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer i1,i2,i3,i4

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real(kind=8) B(Bs+1:Be,Bs+1:Be,Bs+1:Be,Bs+1:Be)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      A(i1,i2,i3,i4)=A(i1,i2,i3,i4)+C*B(i3,i4,i1,i2)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sumx1423(Bs,Be,As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & A, B, C)

      implicit none

      integer Bs,Be
      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer i1,i2,i3,i4

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real(kind=8) B(Bs+1:Be,Bs+1:Be,Bs+1:Be,Bs+1:Be)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      A(i1,i2,i3,i4)=A(i1,i2,i3,i4)+C*B(i1,i4,i2,i3)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sumx2314(Bs,Be,As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & A, B, C)

      implicit none

      integer Bs,Be
      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer i1,i2,i3,i4

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real(kind=8) B(Bs+1:Be,Bs+1:Be,Bs+1:Be,Bs+1:Be)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      A(i1,i2,i3,i4)=A(i1,i2,i3,i4)+C*B(i2,i3,i1,i4)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sumx4312(Bs,Be,As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & A, B, C)

      implicit none

      integer Bs,Be
      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer i1,i2,i3,i4

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real(kind=8) B(Bs+1:Be,Bs+1:Be,Bs+1:Be,Bs+1:Be)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      A(i1,i2,i3,i4)=A(i1,i2,i3,i4)+C*B(i4,i3,i1,i2)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sumx4123(Bs,Be,As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & A, B, C)

      implicit none

      integer Bs,Be
      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer i1,i2,i3,i4

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real(kind=8) B(Bs+1:Be,Bs+1:Be,Bs+1:Be,Bs+1:Be)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      A(i1,i2,i3,i4)=A(i1,i2,i3,i4)+C*B(i4,i1,i2,i3)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sumx3142(Bs,Be,As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & A, B, C)

      implicit none

      integer Bs,Be
      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer i1,i2,i3,i4

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real(kind=8) B(Bs+1:Be,Bs+1:Be,Bs+1:Be,Bs+1:Be)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      A(i1,i2,i3,i4)=A(i1,i2,i3,i4)+C*B(i3,i1,i4,i2)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sumx1342(Bs,Be,As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & A, B, C)

      implicit none

      integer Bs,Be
      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer i1,i2,i3,i4

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real(kind=8) B(Bs+1:Be,Bs+1:Be,Bs+1:Be,Bs+1:Be)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      A(i1,i2,i3,i4)=A(i1,i2,i3,i4)+C*B(i1,i3,i4,i2)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sumx3214(Bs,Be,As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & A, B, C)

      implicit none

      integer Bs,Be
      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer i1,i2,i3,i4

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real(kind=8) B(Bs+1:Be,Bs+1:Be,Bs+1:Be,Bs+1:Be)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      A(i1,i2,i3,i4)=A(i1,i2,i3,i4)+C*B(i3,i2,i1,i4)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sumx1324(Bs,Be,As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & A, B, C)

      implicit none

      integer Bs,Be
      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer i1,i2,i3,i4

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real(kind=8) B(Bs+1:Be,Bs+1:Be,Bs+1:Be,Bs+1:Be)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      A(i1,i2,i3,i4)=A(i1,i2,i3,i4)+C*B(i1,i3,i2,i4)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sumx2134(Bs,Be,As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & A, B, C)

      implicit none

      integer Bs,Be
      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer i1,i2,i3,i4

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real(kind=8) B(Bs+1:Be,Bs+1:Be,Bs+1:Be,Bs+1:Be)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      A(i1,i2,i3,i4)=A(i1,i2,i3,i4)+C*B(i2,i1,i3,i4)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sumx1432(Bs,Be,As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & A, B, C)

      implicit none

      integer Bs,Be
      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer i1,i2,i3,i4

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real(kind=8) B(Bs+1:Be,Bs+1:Be,Bs+1:Be,Bs+1:Be)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      A(i1,i2,i3,i4)=A(i1,i2,i3,i4)+C*B(i1,i4,i3,i2)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sumx12(Bs,Be,As1,Ae1,As2,Ae2,A, B, C)

      implicit none

      integer Bs,Be
      integer As1,Ae1,As2,Ae2
      integer i1,i2

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2)
      real(kind=8) B(Bs+1:Be,Bs+1:Be)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      A(i1,i2)=A(i1,i2)+C*B(i1,i2)
      enddo
      enddo

      end


      subroutine sumx4132(Bs,Be,As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & A, B, C)

      implicit none

      integer Bs,Be
      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer i1,i2,i3,i4

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real(kind=8) B(Bs+1:Be,Bs+1:Be,Bs+1:Be,Bs+1:Be)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      A(i1,i2,i3,i4)=A(i1,i2,i3,i4)+C*B(i4,i1,i3,i2)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sumx2431(Bs,Be,As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & A, B, C)

      implicit none

      integer Bs,Be
      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer i1,i2,i3,i4

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real(kind=8) B(Bs+1:Be,Bs+1:Be,Bs+1:Be,Bs+1:Be)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      A(i1,i2,i3,i4)=A(i1,i2,i3,i4)+C*B(i2,i4,i3,i1)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sumx4321(Bs,Be,As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & A, B, C)

      implicit none

      integer Bs,Be
      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer i1,i2,i3,i4

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real(kind=8) B(Bs+1:Be,Bs+1:Be,Bs+1:Be,Bs+1:Be)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      A(i1,i2,i3,i4)=A(i1,i2,i3,i4)+C*B(i4,i3,i2,i1)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sumx1243(Bs,Be,As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & A, B, C)

      implicit none

      integer Bs,Be
      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer i1,i2,i3,i4

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real(kind=8) B(Bs+1:Be,Bs+1:Be,Bs+1:Be,Bs+1:Be)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      A(i1,i2,i3,i4)=A(i1,i2,i3,i4)+C*B(i1,i2,i4,i3)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sumx2143(Bs,Be,As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & A, B, C)

      implicit none

      integer Bs,Be
      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer i1,i2,i3,i4

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real(kind=8) B(Bs+1:Be,Bs+1:Be,Bs+1:Be,Bs+1:Be)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      A(i1,i2,i3,i4)=A(i1,i2,i3,i4)+C*B(i2,i1,i4,i3)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sumx2413(Bs,Be,As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & A, B, C)

      implicit none

      integer Bs,Be
      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer i1,i2,i3,i4

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real(kind=8) B(Bs+1:Be,Bs+1:Be,Bs+1:Be,Bs+1:Be)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      A(i1,i2,i3,i4)=A(i1,i2,i3,i4)+C*B(i2,i4,i1,i3)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sumx3124(Bs,Be,As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & A, B, C)

      implicit none

      integer Bs,Be
      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer i1,i2,i3,i4

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real(kind=8) B(Bs+1:Be,Bs+1:Be,Bs+1:Be,Bs+1:Be)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      A(i1,i2,i3,i4)=A(i1,i2,i3,i4)+C*B(i3,i1,i2,i4)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sumx4231(Bs,Be,As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & A, B, C)

      implicit none

      integer Bs,Be
      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer i1,i2,i3,i4

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real(kind=8) B(Bs+1:Be,Bs+1:Be,Bs+1:Be,Bs+1:Be)
      real C

      do i1=As1+1,Ae1
      do i2=As2+1,Ae2
      do i3=As3+1,Ae3
      do i4=As4+1,Ae4
      A(i1,i2,i3,i4)=A(i1,i2,i3,i4)+C*B(i4,i2,i3,i1)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sumx_sorted3(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & As5,Ae5,As6,Ae6,Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6,
     & A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,As5,Ae5,As6,Ae6
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,Bs5,Be5,Bs6,Be6
      integer i1,i2,i3,i4,i5,i6

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4,
     & As5+1:Ae5,As6+1:Ae6)
      real(kind=8) B(Bs1+1:Be1,Bs2+1:Be2,Bs3+1:Be3,Bs4+1:Be4,
     & Bs5+1:Be5,Bs6+1:Be6)
      real C

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      do i5=Bs5+1,Be5
      do i6=Bs6+1,Be6
      A(i1,i2,i3,i4,i5,i6)=A(i1,i2,i3,i4,i5,i6)+C*B(i1,i2,i3,i4,i5,i6)
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sumx_sorted4312(As1,Ae1,As2,Ae2,As3,Ae3,As4,
     & Ae4,Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4
      integer i1,i2,i3,i4

      real(kind=8) A(Bs1+1:Be1,Bs2+1:Be2,Bs3+1:Be3,Bs4+1:Be4)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real C

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      A(i1,i2,i3,i4)=A(i1,i2,i3,i4)+C*B(i4,i3,i1,i2)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sumx_sorted2341(As1,Ae1,As2,Ae2,As3,Ae3,As4,
     & Ae4,Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4
      integer i1,i2,i3,i4

      real(kind=8) A(Bs1+1:Be1,Bs2+1:Be2,Bs3+1:Be3,Bs4+1:Be4)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real C

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      A(i1,i2,i3,i4)=A(i1,i2,i3,i4)+C*B(i2,i3,i4,i1)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sumx_sorted2431(As1,Ae1,As2,Ae2,As3,Ae3,As4,
     & Ae4,Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4
      integer i1,i2,i3,i4

      real(kind=8) A(Bs1+1:Be1,Bs2+1:Be2,Bs3+1:Be3,Bs4+1:Be4)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real C

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      A(i1,i2,i3,i4)=A(i1,i2,i3,i4)+C*B(i2,i4,i3,i1)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sumx_sorted4213(As1,Ae1,As2,Ae2,As3,Ae3,As4,
     & Ae4,Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4
      integer i1,i2,i3,i4

      real(kind=8) A(Bs1+1:Be1,Bs2+1:Be2,Bs3+1:Be3,Bs4+1:Be4)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real C

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      A(i1,i2,i3,i4)=A(i1,i2,i3,i4)+C*B(i4,i2,i1,i3)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sumx_sorted21(As1,Ae1,As2,Ae2,Bs1,Be1,Bs2,Be2,
     & A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2
      integer Bs1,Be1,Bs2,Be2
      integer i1,i2

      real(kind=8) A(Bs1+1:Be1,Bs2+1:Be2)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2)
      real C

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      A(i1,i2)=A(i1,i2)+C*B(i2,i1)
      enddo
      enddo

      end


      subroutine sumx_sorted1324(As1,Ae1,As2,Ae2,As3,Ae3,As4,
     & Ae4,Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4
      integer i1,i2,i3,i4

      real(kind=8) A(Bs1+1:Be1,Bs2+1:Be2,Bs3+1:Be3,Bs4+1:Be4)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real C

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      A(i1,i2,i3,i4)=A(i1,i2,i3,i4)+C*B(i1,i3,i2,i4)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sumx_sorted1(As1,Ae1,As2,Ae2,Bs1,Be1,Bs2,Be2,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2
      integer Bs1,Be1,Bs2,Be2
      integer i1,i2

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2)
      real(kind=8) B(Bs1+1:Be1,Bs2+1:Be2)
      real C

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      A(i1,i2)=A(i1,i2)+C*B(i1,i2)
      enddo
      enddo

      end


      subroutine sumx_sorted3412(As1,Ae1,As2,Ae2,As3,Ae3,As4,
     & Ae4,Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4
      integer i1,i2,i3,i4

      real(kind=8) A(Bs1+1:Be1,Bs2+1:Be2,Bs3+1:Be3,Bs4+1:Be4)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real C

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      A(i1,i2,i3,i4)=A(i1,i2,i3,i4)+C*B(i3,i4,i1,i2)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sumx_sorted2(As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4,
     & Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4
      integer i1,i2,i3,i4

      real(kind=8) A(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real(kind=8) B(Bs1+1:Be1,Bs2+1:Be2,Bs3+1:Be3,Bs4+1:Be4)
      real C

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      A(i1,i2,i3,i4)=A(i1,i2,i3,i4)+C*B(i1,i2,i3,i4)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sumx_sorted2314(As1,Ae1,As2,Ae2,As3,Ae3,As4,
     & Ae4,Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4
      integer i1,i2,i3,i4

      real(kind=8) A(Bs1+1:Be1,Bs2+1:Be2,Bs3+1:Be3,Bs4+1:Be4)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real C

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      A(i1,i2,i3,i4)=A(i1,i2,i3,i4)+C*B(i2,i3,i1,i4)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sumx_sorted2134(As1,Ae1,As2,Ae2,As3,Ae3,As4,
     & Ae4,Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4
      integer i1,i2,i3,i4

      real(kind=8) A(Bs1+1:Be1,Bs2+1:Be2,Bs3+1:Be3,Bs4+1:Be4)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real C

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      A(i1,i2,i3,i4)=A(i1,i2,i3,i4)+C*B(i2,i1,i3,i4)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sumx_sorted12(As1,Ae1,As2,Ae2,Bs1,Be1,Bs2,Be2,
     & A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2
      integer Bs1,Be1,Bs2,Be2
      integer i1,i2

      real(kind=8) A(Bs1+1:Be1,Bs2+1:Be2)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2)
      real C

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      A(i1,i2)=A(i1,i2)+C*B(i1,i2)
      enddo
      enddo

      end


      subroutine sumx_sorted4231(As1,Ae1,As2,Ae2,As3,Ae3,As4,
     & Ae4,Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4
      integer i1,i2,i3,i4

      real(kind=8) A(Bs1+1:Be1,Bs2+1:Be2,Bs3+1:Be3,Bs4+1:Be4)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real C

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      A(i1,i2,i3,i4)=A(i1,i2,i3,i4)+C*B(i4,i2,i3,i1)
      enddo
      enddo
      enddo
      enddo

      end


      subroutine sumx_sorted1234(As1,Ae1,As2,Ae2,As3,Ae3,As4,
     & Ae4,Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4,A, B, C)

      implicit none

      integer As1,Ae1,As2,Ae2,As3,Ae3,As4,Ae4
      integer Bs1,Be1,Bs2,Be2,Bs3,Be3,Bs4,Be4
      integer i1,i2,i3,i4

      real(kind=8) A(Bs1+1:Be1,Bs2+1:Be2,Bs3+1:Be3,Bs4+1:Be4)
      real(kind=8) B(As1+1:Ae1,As2+1:Ae2,As3+1:Ae3,As4+1:Ae4)
      real C

      do i1=Bs1+1,Be1
      do i2=Bs2+1,Be2
      do i3=Bs3+1,Be3
      do i4=Bs4+1,Be4
      A(i1,i2,i3,i4)=A(i1,i2,i3,i4)+C*B(i1,i2,i3,i4)
      enddo
      enddo
      enddo
      enddo

      end


