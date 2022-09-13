PROGRAM nm42
!---Shifted Vector Iteration for Eigenvalue and its Eigenvector---
 USE nm_lib
 USE precision
 IMPLICIT NONE
 INTEGER::i,iters,limit,n
 REAL(iwp)::big,l2,shift,tol,zero=0.0_iwp
 REAL(iwp),ALLOCATABLE::a(:,:),x(:),x1(:)
 OPEN(10,FILE='nm95.dat')
 OPEN(11,FILE='nm95.res')
 READ(10,*)n
 ALLOCATE(a(n,n),x(n),x1(n))
 READ(10,*)a
 READ(10,*)shift
 READ(10,*)x
 READ(10,*)tol,limit
 WRITE(11,'(A)')                                                          &
   "---Shifted Vector Iteration Eigenvalue and its Eigenvector---"
 WRITE(11,'(/,A)')"Coefficient Matrix"
 a=TRANSPOSE(a)
 DO i=1,n
   WRITE(11,'(6E12.4)')a(i,:)
 END DO
 WRITE(11,'(/,A,/,E12.4)')"Shift",shift
 WRITE(11,'(/,A)')"Guessed Starting Vector"
 WRITE(11,'(3E12.4)')x
 DO i=1,n
   a(i,i)=a(i,i)+shift
 END DO
 WRITE(11,'(/,A)')"First Few Iterations"
 iters=0
 DO
   iters=iters+1
   x1=MATMUL(a,x)
   big=zero
   DO i=1,n
     IF(ABS(x1(i))>ABS(big))big=x1(i)
   END DO
   x1=x1/big
   IF(checkit(x1,x,tol).OR.iters==limit)EXIT
   x=x1
   IF(iters<5)WRITE(11,'(6E12.4)')x
 END DO
 l2=norm(x1)
 x1=x1/l2
 WRITE(11,'(/,A,/,I5)')"Iterations to Convergence",iters
 WRITE(11,'(/,A,/,F16.8)')"Eigenvalue",big-shift
 WRITE(11,'(/,A)')"Corresponding Eigenvector"
 WRITE(11,'(6E12.4)')x1
END PROGRAM nm42
