PROGRAM nm48
!---Lanczos Reduction of Symmetrical Matrix to Tridiagonal Form---
 USE precision
 IMPLICIT NONE
 INTEGER::i,j,n
 REAL(iwp)::zero=0.0_iwp
 REAL(iwp),ALLOCATABLE::a(:,:),alpha(:),beta(:),v(:),y0(:),y1(:),z(:)
 OPEN(10,FILE='nm95.dat')
 OPEN(11,FILE='nm95.res')
 READ(10,*)n
 ALLOCATE(a(n,n),alpha(n),beta(0:n-1),v(n),y0(n),y1(n),z(n))
 DO i=1,n
   READ(10,*)a(i,i:n)
   a(i:n,i)=a(i,i:n)
 END DO
 READ(10,*)y1
 WRITE(11,'(A)')                                                          &
   "---Lanczos Reduction of Symmetrical Matrix to Tridiagonal Form---"
 WRITE(11,'(/,A)')"Coefficient Matrix"
 DO i=1,n
   WRITE(11,'(6E12.4)')a(i,:)
 END DO
 WRITE(11,'(/,A)')"Guessed Starting vector"
 WRITE(11,'(6E12.4)')y1
 y0=zero
 beta(0)=zero
 DO j=1,n
   v=MATMUL(a,y1)
   alpha(j)=DOT_PRODUCT(y1,v)
   IF(j==n)EXIT
   z=v-alpha(j)*y1-beta(j-1)*y0
   y0=y1
   beta(j)=SQRT(DOT_PRODUCT(z,z))
   y1=z/beta(j)
 END DO
 WRITE(11,'(/A)')"Transformed Main Diagonal"
 WRITE(11,'(6E12.4)')alpha
 WRITE(11,'(/A)')"Transformed Off-Diagonal"
 WRITE(11,'(6E12.4)')beta(1:)
END PROGRAM nm48
