PROGRAM nm52
!---Interpolation by Forward Differences---
 USE precision
 IMPLICIT NONE
 INTEGER::factorial,i,j,np
 REAL(iwp)::h,one=1.0_iwp,term,xi,yi
 REAL(iwp),ALLOCATABLE::c(:),diff(:,:),x(:)
 OPEN(10,FILE='nm95.dat')
 OPEN(11,FILE='nm95.res')
 READ(10,*)np
 ALLOCATE(c(0:np-1),diff(0:np-1,0:np-1),x(0:np-1))
 READ(10,*)(x(i),diff(i,0),i=0,np-1),xi
 WRITE(11,'(A/)')"---Interpolation by Forward Differences---"
 WRITE(11,'(A/A)')"       Data Points","      x           y"
 WRITE(11,'(2E12.4)')(x(i),diff(i,0),i=0,np-1)
 h=x(1)-x(0)
 DO i=1,np-1
   DO j=0,np-1-i
     diff(j,i)=diff(j+1,i-1)-diff(j,i-1)
   END DO
 END DO
 c(0)=diff(0,0)
 yi=c(0)
 term=one
 factorial=one
 DO i=1,np-1
   factorial=factorial*i
   c(i)=diff(0,i)/(factorial*h**i)
   term=term*(xi-x(i-1))
   yi=yi+term*c(i)
 END DO
 WRITE(11,'(/A/A)')" Polynomial Coefficients","      C"
 WRITE(11,'(E12.4)')c
 WRITE(11,'(/A/A)')"   Interpolated Point","      x           y"
 WRITE(11,'(2E12.4)')xi,yi
END PROGRAM nm52
