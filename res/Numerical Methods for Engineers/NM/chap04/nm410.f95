PROGRAM nm410
!---Characteristic Polynomial Method---
!---for Eigenvalues of Symmetrical Tridiagonal Matrix---
 USE nm_lib  
 USE precision  
 IMPLICIT NONE
 INTEGER::i,iters,j,limit,n,number
 REAL(iwp)::al,almax,aold,half=0.5_iwp,oldl,one=1.0_iwp,sign,             &
   small=1.E-20_iwp,tol
 REAL(iwp),ALLOCATABLE::alpha(:),beta(:),det(:)
 OPEN(10,FILE='nm95.dat')  
 OPEN(11,FILE='nm95.res')
 WRITE(11,'(A)')"---Characteristic Polynomial Method---"
 WRITE(11,'(A)')"---for Eigenvalues of Symmetric Tridiagonal Matrix---"
 READ(10,*)n  
 ALLOCATE(alpha(n),beta(n-1),det(0:n))
 READ(10,*)alpha  
 READ(10,*)beta
 READ(10,*)j,al,almax,tol,limit
 WRITE(11,'(/,A)')"Main Diagonal"  
 WRITE(11,'(6E12.4)')alpha
 WRITE(11,'(/,A)')"Off-Diagonal"  
 WRITE(11,'(6E12.4)')beta
 WRITE(11,'(/,A)')"Eigenvalue required, 1=largest,2=second largest etc."
 WRITE(11,'(I8)')j
 WRITE(11,'(/,A)')"Eigenvalue   Determinant  Number of roots less than"
 iters=0  
 det(0)=one 
 aold=almax
 DO  
   iters=iters+1  
   det(1)=alpha(1)-al
   DO i=2,n
     det(i)=(alpha(i)-al)*det(i-1)-beta(i-1)*beta(i-1)*det(i-2)
   END DO  
   number=0
   DO i=1,n
     IF(ABS(det(i))<small)CYCLE
     IF(ABS(det(i-1))<small)THEN
       sign=det(i)*det(i-2)
     ELSE
       sign=det(i)*det(i-1)
     END IF
     IF(sign<small)number=number+1
   END DO
   IF(number<=n-j)THEN
     oldl=al  
     al=half*(al+almax)
   ELSE
     almax=al  
     al=half*(oldl+al)
   END IF
   IF(det(n)<small)number=number-1  
   IF(MOD(j,2)==0)number=number-1
   WRITE(11,'(2E12.4,I15)')al,det(n),number
   IF(check(al,aold,tol).OR.iters==limit)EXIT  
   aold=al 
 END DO
 WRITE(11,'(/,A,/,I5)')"Iterations to Convergence",iters
END PROGRAM nm410
