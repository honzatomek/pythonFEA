PROGRAM nm74
!---Shooting Method for Second Order ODEs---
 USE precision
 IMPLICIT NONE
 INTEGER::i,iters,j,limit,nsteps
 REAL(iwp)::astar,a0(2),d6=6.0_iwp,h,k0(2),k1(2),k2(2),k3(2),tol,         &
   two=2.0_iwp,x,xa,xb,y(2),ya,yb,zero=0.0_iwp
 REAL(iwp),ALLOCATABLE::y0(:,:),ystar(:)
 OPEN(10,FILE='nm95.dat')
 OPEN(11,FILE='nm95.res')
 READ(10,*)nsteps,xa,ya,xb,yb,a0,tol,limit
 ALLOCATE(y0(0:nsteps,2),ystar(0:nsteps))
 WRITE(11,'(A)')"---Shooting Method for Second Order ODEs---"
 h=(xb-xa)/nsteps
 DO j=1,2
   x=xa
   y(1)=ya
   y(2)=a0(j)
   DO i=0,nsteps
     y0(i,j)=y(1)
     k0=h*f74(x,y)
     k1=h*f74(x+h/two,y+k0/two)
     k2=h*f74(x+h/two,y+k1/two)
     k3=h*f74(x+h,y+k2)
     y=y+(k0+two*k1+two*k2+k3)/d6
     x=x+h
   END DO
 END DO
 IF((y0(nsteps,1)-yb)*(y0(nsteps,2)-yb)>zero)   &
   WRITE(11,'(A)')"Try new gradients....?"
 iters=0
 DO
   iters=iters+1
   astar=a0(1)+(yb-y0(nsteps,1))*(a0(2)-a0(1))/(y0(nsteps,2)-y0(nsteps,1))
   x=xa
   y(1)=ya
   y(2)=astar
   DO i=0,nsteps
     ystar(i)=y(1)
     k0=h*f74(x,y)
     k1=h*f74(x+h/two,y+k0/two)
     k2=h*f74(x+h/two,y+k1/two)
     k3=h*f74(x+h,y+k2)
     y=y+(k0+two*k1+two*k2+k3)/d6
     x=x+h
   END DO
   IF(ABS((ystar(nsteps)-yb)/yb)<tol)THEN
     WRITE(11,'(/,A,I2)')"      x            y"
     WRITE(11,'(2E13.5)')(xa+i*h,ystar(i),i=0,nsteps)
     WRITE(11,'(/,A,/,I5)')"Iterations to Convergence",iters
     EXIT
   END IF
   IF((ystar(nsteps)-yb)*(y0(nsteps,1)-yb)>zero)THEN
     y0(:,1)=ystar
     a0(1)=astar
     ELSE
     y0(:,2)=ystar
     a0(2)=astar
   END IF
 END DO
 CONTAINS

 FUNCTION f74(x,y)
  IMPLICIT NONE
  REAL(iwp),INTENT(IN)::x,y(2)
  REAL(iwp)::f74(2)
  f74(1)=y(2)
  f74(2)=3._iwp*x**2+4._iwp*y(1)
!  f74(1)=y(2)
!  f74(2)=-2._iwp*y(2)**2/y(1)
  RETURN
 END FUNCTION f74

END PROGRAM nm74
