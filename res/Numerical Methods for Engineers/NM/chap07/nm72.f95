PROGRAM nm72
!---Theta-Method for Linear ODEs---
 USE precision
 IMPLICIT NONE
 INTEGER::j,n,nsteps
 REAL(iwp)::h,k0,k1,l0,l1,m0,m1,one=1.0_iwp,theta,x,y1
 REAL(iwp),ALLOCATABLE::y(:)
 OPEN(10,FILE='nm95.dat')
 OPEN(11,FILE='nm95.res')
 READ(10,*)n,nsteps,h,theta
 ALLOCATE(y(n))
 READ(10,*)x,y
 SELECT CASE(n)
 CASE(1)
   WRITE(11,'(A)')"---Theta-Method for First Order Linear ODEs---"
   WRITE(11,'(/,A,I2)')"      x            y"
   DO j=0,nsteps
     WRITE(11,'(10E13.5)')x,y
     CALL f72(x,k0,l0)
     CALL f72(x+h,k1,l1)
     y=(y+h*((one-theta)*(k0+l0*y)+theta*k1))/(one-h*theta*l1)
     x=x+h
   END DO
 CASE(2)
   WRITE(11,'(A)')"---Theta-Method for Second Order Linear ODEs---"
   WRITE(11,'(/,A,I2)')"      x            y            y'"
   DO j=0,nsteps
     WRITE(11,'(10E13.5)')x,y
     CALL f72(x,k0,l0,m0)
     CALL f72(x+h,k1,l1,m1)
     y1=y(1)*(one-h*theta*m1)+h*y(2)*(one-h*theta*m1*(1-theta))
     y1=y1+h**2.*theta*((1-theta)*(k0+l0*y(1)+m0*y(2))+theta*k1)
     y1=y1/(one-h*theta*m1-h**2.*theta**2.*l1)
     y(2)=(y1-y(1))/(h*theta)-(one-theta)/theta*y(2)
     y(1)=y1
     x=x+h
   END DO
 END SELECT
 CONTAINS

 SUBROUTINE f72(x,k,l,m)
  IMPLICIT NONE
  REAL(iwp),INTENT(IN)::x
  REAL(iwp),INTENT(OUT)::k,l
  REAL(iwp),INTENT(OUT),OPTIONAL::m
  k= x**3.
  l= -2._iwp*x
!  k =  0._iwp
!  l = -(3._iwp+x**2)
!  m =  2._iwp*x
  RETURN
 END SUBROUTINE f72

END PROGRAM nm72
