PROGRAM nm51
!---Interpolation by Lagrangian Polynomials---
 USE precision
 IMPLICIT NONE
 INTEGER::i,j,np
 REAL(iwp)::one=1.0_iwp,term,xi,yi,zero=0.0_iwp
 REAL(iwp),ALLOCATABLE::x(:),y(:)
 OPEN(10,FILE='nm95.dat')
 OPEN(11,FILE='nm95.res')
 READ(10,*)np
 ALLOCATE(x(np),y(np))
 READ(10,*)(x(i),y(i),i=1,np),xi
 WRITE(11,'(A,/)')"---Interpolation by Lagrangian Polynomials---"
 WRITE(11,'(A/A)')"       Data Points","      x           y"
 WRITE(11,'(2E12.4)')(x(i),y(i),i=1,np)
 yi=zero
 DO i=1,np
   term=one
   DO j=1,np
     IF(j/=i)term=term*(xi-x(j))/(x(i)-x(j))
   END DO
 yi=yi+term*y(i)
 END DO
 WRITE(11,'(/A/A)')"   Interpolated Point","      x           y"
 WRITE (11,'(2E12.4)')xi,yi
END PROGRAM nm51
