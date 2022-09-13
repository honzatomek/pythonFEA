PROGRAM nm35
!---Modified Newton-Raphson Method for a Single Equation---
 USE nm_lib  
 USE precision  
 IMPLICIT NONE
 INTEGER::iters,limit  
 REAL(iwp)::fd,tol,x0,x1
 OPEN(10,FILE='nm95.dat')  
 OPEN(11,FILE='nm95.res')
 WRITE(11,'(A,A)')"---Modified Newton-Raphson Method for a ",   &
   "Single Root---"
 READ(10,*)x0,tol,limit
 WRITE(11,'(/,A,/,E12.4)')"Guessed Starting Value",x0
 WRITE(11,'(/,A)')"First Few Iterations"  
 iters=0  
 fd=f35dash(x0) 
 DO  
   iters=iters+1
   x1=x0-f35(x0)/fd 
   IF(check(x1,x0,tol).OR.iters==limit)EXIT  
   x0=x1  
   IF(iters<5)WRITE(11,'(3E12.4)')x1
 END DO
 WRITE(11,'(/,A,/,I5)')"Iterations to Convergence",iters
 WRITE(11,'(/,A,/,E12.4)')"Solution",x1
 CONTAINS
!
 FUNCTION f35(x)
  IMPLICIT NONE 
  REAL(iwp),INTENT(IN)::x  
  REAL(iwp)::f35
  f35=x**3-x-1.0_iwp  
  RETURN
 END FUNCTION f35
! 
 FUNCTION f35dash(x)
  IMPLICIT NONE 
  REAL(iwp),INTENT(IN)::x  
  REAL(iwp)::f35dash
  f35dash=3.0_iwp*x**2-1.0_iwp
  END FUNCTION f35dash
!
END PROGRAM nm35
