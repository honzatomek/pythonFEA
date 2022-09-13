PROGRAM nm46
!---Jacobi Diagonalization for Eigenvalues of Symmetrical Matrices---
 USE nm_lib  
 USE precision  
 IMPLICIT NONE
 INTEGER::i,iters,j,limit,n,nc,nr 
 REAL(iwp)::alpha,big,ct,den,d2=2.0_iwp,d4=4.0_iwp,hold,l2,one=1.0_iwp,   &
   penalty=1.E20_iwp,pi,st,small=1.E-20_iwp,tol,zero=0.0_iwp
 REAL(iwp),ALLOCATABLE::a(:,:),a1(:,:),a2(:,:),enew(:),eold(:),p(:,:),x(:)
 OPEN(10,FILE='nm95.dat')  
 OPEN(11,FILE='nm95.res')
 READ(10,*)n  
 ALLOCATE(a(n,n),a1(n,n),a2(n,n),enew(n),eold(n),p(n,n),x(n))
 DO i=1,n  
   READ(10,*)a(i,i:n)  
   a(i:n,i)=a(i,i:n)  
 END DO  
 a2=a
 READ(10,*)tol,limit  
 pi=d4*ATAN(one)
 WRITE(11,'(A)')                                                          &
   "---Jacobi Diagonalization for Eigenvalues of Symmetrical Matrices---"
 WRITE(11,'(/,A)')"Matrix A"  
 DO i=1,n  
   WRITE(11,'(6E12.4)')a(i,:)  
 END DO
 WRITE(11,'(/,A)')"First Few Iterations"  
 iters=0  
 eold=zero 
 DO  
   iters=iters+1  
   big=zero
   DO i=1,n  
     DO j=i+1,n
       IF(ABS(a(i,j))>big)THEN
         big=ABS(a(i,j))  
         hold=a(i,j)  
         nr=i  
         nc=j
       END IF
     END DO  
   END DO
   IF(ABS(big)<small)EXIT
   den=a(nr,nr)-a(nc,nc)
   IF(ABS(den)<small)THEN
     alpha=pi/d4  
     IF(hold<zero)alpha=-alpha
   ELSE
     alpha=ATAN(d2*hold/den)/d2
   END IF
   ct=COS(alpha)  
   st=SIN(alpha)  
   p=zero  
   DO i=1,n  
     p(i,i)=one  
   END DO
   p(nr,nr)=ct  
   p(nc,nc)=ct  
   p(nr,nc)=-st  
   p(nc,nr)=st
   a=MATMUL(MATMUL(TRANSPOSE(p),a),p)
   IF(iters<5)THEN 
     DO i=1,n  
       WRITE(11,'(6E12.4)')a(i,:)  
     END DO  
     WRITE(11,*)
   END IF
   DO i=1,n  
     enew(i)=a(i,i)  
   END DO
   IF(checkit(enew,eold,tol).OR.iters==limit)EXIT  
   eold=enew
 END DO
 WRITE(11,'(A,/,I5)')"Iterations to Convergence",iters
 WRITE(11,'(/,A)')"Final Transformed Matrix A"
 DO i=1,n  
   WRITE(11,'(6E12.4)')a(i,:)  
 END DO  
 DO i=1,n  
   a1=a2  
   DO j=1,n  
     a1(j,j)=a1(j,j)-a(i,i)  
   END DO
   x=zero  
   a1(i,i)=penalty  
   x(i)=penalty  
   x=eliminate(a1,x) 
   l2=norm(x)
   WRITE(11,'(/,A,E12.4)')"Eigenvalue  ",a(i,i)
   WRITE(11,'(A)')"Eigenvector"
   WRITE(11,'(6E12.4)')x/l2  
 END DO
END PROGRAM nm46
