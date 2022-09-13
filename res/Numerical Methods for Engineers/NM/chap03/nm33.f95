PROGRAM nm33
!---False Position Method for a Single Root---
 USE nm_lib  
 USE precision  
 IMPLICIT NONE
 INTEGER::iters,limit  
 REAL(iwp)::tol,xi,xip1,xnew,xold,zero=0.0_iwp
 OPEN(10,FILE='nm95.dat')  
 OPEN(11,FILE='nm95.res')
 WRITE(11,'(A)')"---False Position Method for a Single Root---"
 READ(10,*)xi,xip1,tol,limit
  IF(f33(xi)*f33(xip1)>zero)THEN
   WRITE(11,'(/,A)')"Root not captured, try again."
   STOP
 END IF   
WRITE(11,'(/,A,/,E12.4,A,E12.4)')"Starting Range",xi," to", xip1  
 WRITE(11,'(/,A)')"First Few Iterations"  
 iters=0  
 xold=xi 
 DO  
   iters=iters+1  
   xnew=xi-f33(xi)*(xip1-xi)/(f33(xip1)-f33(xi))
   IF(f33(xi)*f33(xnew)<zero)THEN  
     xip1=xnew  
   ELSE  
     xi=xnew  
   END IF
   IF(iters<5)WRITE(11,'(3E12.4)')xnew
   IF(check(xnew,xold,tol).OR.iters==limit)EXIT  
   xold=xnew  
 END DO
 WRITE(11,'(/,A,/,I5)')"Iterations to Convergence",iters
 WRITE(11,'(/,A,/,E12.4)')"Solution",xnew
 CONTAINS
!
 FUNCTION f33(x)
  IMPLICIT NONE
  REAL(iwp),INTENT(IN)::x  
  REAL(iwp)::f33
  f33=x**3-x-1.0_iwp  
  RETURN
 END FUNCTION f33
!
END PROGRAM nm33
