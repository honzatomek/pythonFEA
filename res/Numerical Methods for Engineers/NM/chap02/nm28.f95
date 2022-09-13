PROGRAM nm28
!---Jacobi Iteration For Linear Simultaneous Equations---
 USE nm_lib  
 USE precision  
 IMPLICIT NONE        
 INTEGER::i,iters,limit,n  
 REAL(iwp)::diag,tol,zero=0.0_iwp 
 REAL(iwp),ALLOCATABLE::a(:,:),b(:),x(:),xnew(:)
 OPEN(10,FILE='nm95.dat')  
 OPEN(11,FILE='nm95.res')
 READ(10,*)n  
 ALLOCATE(a(n,n),b(n),x(n),xnew(n))
 READ(10,*)a  
 READ(10,*)b  
 READ(10,*)x  
 READ(10,*)tol,limit
 WRITE(11,'(A)')"---Jacobi Iteration For Linear Simultaneous Equations---"
 WRITE(11,'(/,A)')"Coefficient Matrix" 
 a=TRANSPOSE(a)  
 DO i=1,n  
   WRITE(11,'(6E12.4)')a(i,:)  
 END DO
 WRITE(11,'(/,A)')"Right Hand Side Vector"  
 WRITE(11,'(6E12.4)')b
 WRITE(11,'(/,A)')"Guessed Starting Vector"  
 WRITE(11,'(6E12.4)')x
 DO i=1,n  
   diag=a(i,i)  
   a(i,:)=a(i,:)/diag  
   b(i)=b(i)/diag  
 END DO
 a=-a  
 DO i=1,n  
   a(i,i)=zero  
 END DO
 WRITE(11,'(/,A)')"First Few Iterations"  
 iters=0
 DO  
   iters=iters+1
   xnew=b+MATMUL(a,x)  
   IF(iters<5)WRITE(11,'(6E12.4)')x
   IF(checkit(xnew,x,tol).OR.iters==limit)EXIT  
   x=xnew
 END DO
 WRITE(11,'(/,A,/,I5)')"Iterations to Convergence",iters
 WRITE(11,'(/,A)')"Solution Vector"  
 WRITE(11,'(6E12.4)')x
END PROGRAM nm28
