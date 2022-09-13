PROGRAM nm37
!---Newton-Raphson for Systems of Equations---
 USE nm_lib  
 USE precision  
 IMPLICIT NONE
 INTEGER::iters,limit,n  
 REAL(iwp)::tol 
 REAL(iwp),ALLOCATABLE::x0(:),x1(:)
 OPEN(10,FILE='nm95.dat')  
 OPEN(11,FILE='nm95.res')
 WRITE(11,'(A)')"---Newton-Raphson for Systems of Equations---"
 READ(10,*)n  
 ALLOCATE(x0(n),x1(n))  
 READ(10,*)x0,tol,limit
 WRITE(11,'(/,A)')"Guesssed Starting Vector"
 WRITE(11,'(6E12.4)')x0
 WRITE(11,'(/,A)')"First Few Iterations"  
 iters=0
 DO  
   iters=iters+1
   x1=x0-MATMUL(inverse(f37dash(x0)),f37(x0))
   IF(checkit(x1,x0,tol).OR.iters==limit)EXIT  
   x0=x1
   IF(iters<5)WRITE(11,'(6E12.4)')x0
 END DO
 WRITE(11,'(/,A,/,I5)')"Iterations to Convergence",iters
 WRITE(11,'(/,A)')"Solution Vector"
 WRITE(11,'(6E12.4)')x0
 CONTAINS
!
 FUNCTION f37(x)
  IMPLICIT NONE
  REAL(iwp),INTENT(IN)::x(:)  
  REAL(iwp)::f37(UBOUND(x,1))
  f37(1)=x(1)**2+x(1)*x(2)-10.0_iwp
  f37(2)=x(2)+3.0_iwp*x(1)*x(2)**2-57.0_iwp
 END FUNCTION f37
!
 FUNCTION f37dash(x)
  IMPLICIT NONE
  REAL(iwp),INTENT(IN)::x(:)
  REAL(iwp)::f37dash(UBOUND(x,1),UBOUND(x,1))
  f37dash(1,1)=2.0_iwp*x(1)+x(2)  
  f37dash(1,2)=x(1)
  f37dash(2,1)=3.0_iwp*x(2)**2  
  f37dash(2,2)=1.0_iwp+6.0_iwp*x(2)*x(1)
 END FUNCTION f37dash
!
END PROGRAM nm37
