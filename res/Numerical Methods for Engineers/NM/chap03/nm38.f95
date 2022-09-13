PROGRAM nm38
!---Modified Newton-Raphson for Systems of Equations---
 USE nm_lib  
 USE precision  
 IMPLICIT NONE
 INTEGER::iters,limit,n  
 REAL(iwp)::tol
 REAL(iwp),ALLOCATABLE::inv(:,:),x0(:),x1(:)
 OPEN(10,FILE='nm95.dat')  
 OPEN(11,FILE='nm95.res')
 WRITE(11,'(A)')"---Modified Newton-Raphson for Systems of Equations---"
 READ(10,*)n  
 ALLOCATE(x0(n),x1(n),inv(n,n))  
 READ(10,*)x0,tol,limit
 WRITE(11,'(/,A)')"Guesssed Starting Vector"
 WRITE(11,'(6E12.4)')x0
 WRITE(11,'(/,A)')"First Few Iterations"  
 iters=0  
 inv=inverse(f38dash(x0))
 DO  
   iters=iters+1
   x1=x0-MATMUL(inv,f38(x0)) 
   IF(checkit(x1,x0,tol).OR.iters==limit)EXIT  
   x0=x1
   IF(iters<5)WRITE(11,'(6E12.4)')x0
 END DO
 WRITE(11,'(/,A,/,I5)')"Iterations to Convergence",iters
 WRITE(11,'(/,A)')"Solution Vector"
 WRITE(11,'(6E12.4)')x0
 CONTAINS
!
 FUNCTION f38(x)
  IMPLICIT NONE
  REAL(iwp),INTENT(IN)::x(:)  
  REAL(iwp)::f38(UBOUND(x,1))
  f38(1)=2.0_iwp*x(1)*x(1)+x(2)*x(2)-4.32_iwp
  f38(2)=x(1)*x(1)-x(2)*x(2)
 END FUNCTION f38
!
 FUNCTION f38dash(x)
  IMPLICIT NONE
  REAL(iwp),INTENT(IN)::x(:)
  REAL(iwp)::f38dash(UBOUND(x,1),UBOUND(x,1))
  f38dash(1,1)=4.0_iwp*x(1)
  f38dash(1,2)=2.0_iwp*x(2)
  f38dash(2,1)=2.0_iwp*x(1)
  f38dash(2,2)=-2.0_iwp*x(2)
 END FUNCTION f38dash
!
END PROGRAM nm38
