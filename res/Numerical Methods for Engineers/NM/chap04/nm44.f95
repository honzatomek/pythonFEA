PROGRAM nm44
!---Vector Iteration for Kx=lambda*Mx---
 USE nm_lib
 USE precision
 IMPLICIT NONE
 INTEGER::i,iters,limit,n
 REAL(iwp)::big,l2,one=1.0_iwp,tol,zero=0.0_iwp
 REAL(iwp),ALLOCATABLE::a(:,:),d(:),k(:,:),m(:,:),x(:),x1(:)
 OPEN(10,FILE='nm95.dat')
 OPEN(11,FILE='nm95.res')
 READ(10,*)n
 ALLOCATE(a(n,n),k(n,n),m(n,n),x(n),x1(n),d(n))
 DO i=1,n
   READ(10,*)k(i,i:n)
   k(i:n,i)=k(i,i:n)
 END DO
 DO i=1,n
   READ(10,*)m(i,i:n)
   m(i:n,i)=m(i,i:n)
 END DO
 READ(10,*)x
 READ(10,*)tol,limit
 WRITE(11,'(A)')"---Vector Iteration for Kx=lambda*M­x---"
 WRITE(11,'(/,A)')"Matrix K"
 DO i=1,n
   WRITE(11,'(6E12.4)')k(i,:)
 END DO
 WRITE(11,'(/,A)')"Matrix M"
 DO i=1,n
   WRITE(11,'(6E12.4)')m(i,:)
 END DO
 WRITE(11,'(/,A)')"Guessed Starting Vector"
 WRITE(11,'(6E12.4)')x
 CALL ldlt(k,d)
 WRITE(11,'(/,A)')"First Few Iterations"
 iters=0
 DO
   iters=iters+1
   a=k
   x1=MATMUL(m,x)
   CALL ldlfor(a,x1)
   DO i=1,n
     a(i,:)=a(i,:)/d(i)
   END DO
   CALL subbac(a,x1)
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
 WRITE(11,'(/,A,/,E12.4)')"Smallest Eigenvalue of Inv(M)*K",one/big
 WRITE(11,'(/,A)')"Corresponding Eigenvector"
 WRITE(11,'(6E12.4)')x1
END PROGRAM nm44
