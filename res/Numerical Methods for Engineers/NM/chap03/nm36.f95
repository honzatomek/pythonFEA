PROGRAM nm36
!---Iterative Substitution for Systems of Equations---
 USE nm_lib  
 USE precision  
 IMPLICIT NONE
 INTEGER::iters,limit,n  
 REAL(iwp)::tol  
 REAL(iwp),ALLOCATABLE::x(:)
 OPEN(10,FILE='nm95.dat')  
 OPEN(11,FILE='nm95.res')
 WRITE(11,'(A)')"---Iterative Substitution for Systems of Equations---"
 READ(10,*)n  
 ALLOCATE(x(n))  
 READ(10,*)x,tol,limit
 WRITE(11,'(/,A)')"Guesssed Starting Vector"
 WRITE(11,'(6E12.4)')x
 WRITE(11,'(/,A)')"First Few Iterations"  
 iters=0
 DO  
   iters=iters+1 
   IF(checkit(f36(x),x,tol).OR.iters==limit)EXIT
   x=f36(x)  
   IF(iters<5)WRITE(11,'(6E12.4)')x
 END DO
 WRITE(11,'(/,A,/,I5)')"Iterations to Convergence",iters
 WRITE(11,'(/,A)')"Solution Vector"
 WRITE(11,'(6E12.4)')x
 CONTAINS
!
 FUNCTION f36(x)
  IMPLICIT NONE
  REAL(iwp),INTENT(IN)::x(:)  
  REAL(iwp)::f36(UBOUND(x,1))
  f36(1)=SQRT(2.16_iwp-0.5_iwp*x(2)*x(2)) 
  f36(2)=x(1)
 END FUNCTION f36
!
END PROGRAM nm36
