PROGRAM nm71
!---One-Step Methods for Systems of ODEs---
!   itype= 1 (Euler Method)   itype= 2 (Modified Euler Method)
!   itype= 3 (Mid-Point Method) itype= 4 (4th order Runge-Kutta Method)
 USE precision
 IMPLICIT NONE
 INTEGER::i,itype,j,n,nsteps
 REAL(iwp)::d6=6.0_iwp,h,two=2.0_iwp,x
 REAL(iwp),ALLOCATABLE::k0(:),k1(:),k2(:),k3(:),y(:)
 OPEN(10,FILE='nm95.dat')
 OPEN(11,FILE='nm95.res')
 READ(10,*)itype,n,nsteps,h
 ALLOCATE(k0(n),k1(n),k2(n),k3(n),y(n))
 READ(10,*)x,y
 WRITE(11,'(A)')"---One-Step Methods for Systems of ODEs---"
 SELECT CASE(itype)
 CASE(1)
   WRITE(11,'(/,A)')"************ EULER METHOD ************"
   WRITE(11,'(/,A,I2)')"      x            y(i) , i = 1,",n
   DO j=0,nsteps
     WRITE(11,'(10E13.5)')x,y
     k0=h*f71(x,y)
     y=y+k0
     x=x+h
   END DO
 CASE(2)
   WRITE(11,'(/,A)')"******* MODIFIED EULER METHOD ********"
   WRITE(11,'(/,A,I2)')"      x            y(i) , i = 1,",n
   DO j=0,nsteps
     WRITE(11,'(10E13.5)')x,y
     k0=h*f71(x,y)
     k1=h*f71(x+h,y+k0)
     y=y+(k0+k1)/two
     x=x+h
   END DO
 CASE(3)
   WRITE(11,'(/,A)')"********** MID-POINT METHOD ***********"
   WRITE(11,'(/,A,I2)')"      x            y(i) , i = 1,",n
   DO j=0,nsteps
     WRITE(11,'(10E13.5)')x,y
     k0=h*f71(x,y)
     k1=h*f71(x+h/two,y+k0/two)
     y=y+k1
     x=x+h
   END DO
 CASE(4)
   WRITE(11,'(/,A)')"***** 4TH ORDER RUNGE-KUTTA METHOD ****"
   WRITE(11,'(/,A,I2)')"      x            y(i) , i = 1,",n
   DO j=0,nsteps
     WRITE(11,'(10E13.5)')x,y
     k0=h*f71(x,y)
     k1=h*f71(x+h/two,y+k0/two)
     k2=h*f71(x+h/two,y+k1/two)
     k3=h*f71(x+h,y+k2)
     y=y+(k0+two*k1+two*k2+k3)/d6
     x=x+h
   END DO
 END SELECT
 CONTAINS

 FUNCTION f71(x,y)
  IMPLICIT NONE
  REAL(iwp),INTENT(IN)::x,y(:)
  REAL(iwp)::f71(SIZE(y,1))
  f71(1)=(x+y(1))**2
!  f71(1)=(x+y(1))**2.
!  f71(1)=3._iwp*x*y(2)+4._iwp
!  f71(2)=x*y(1)-y(2)-EXP(x)
!  f71(1)=y(2)
!  f71(2)=2._iwp*y(1)-3._iwp*y(2)+3._iwp*x**2.
  RETURN
 END FUNCTION f71

END PROGRAM nm71
