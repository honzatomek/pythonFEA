PROGRAM nm49
!---LR Transformation for Eigenvalues---
 USE nm_lib  
 USE precision  
 IMPLICIT NONE
 INTEGER::i,iters,limit,n  
 REAL(iwp)::tol,zero=0.0_iwp
 REAL(iwp),ALLOCATABLE::a(:,:),enew(:),eold(:),lower(:,:),upper(:,:)
 OPEN(10,FILE='nm95.dat')  
 OPEN(11,FILE='nm95.res')
 READ(10,*)n  
 ALLOCATE(a(n,n),upper(n,n),lower(n,n),eold(n),enew(n))
 READ(10,*)a,tol,limit  
 WRITE(11,*)"---LR Transformation for Eigenvalues---"
 WRITE(11,'(/,A)')"Coefficient Matrix"
 a=TRANSPOSE(a)  
 DO i=1,n  
   WRITE(11,'(6E12.4)')a(i,:)  
 END DO
 WRITE(11,'(/,A)')"First Few Iterations"  
 iters=0  
 eold=zero
 DO  
   iters=iters+1  
   CALL lufac(a,lower,upper)
   a=MATMUL(upper,lower) 
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
 WRITE(11,'(/A)')"Final Transformed Matrix"
 DO i=1,n  
   WRITE(11,'(6E12.4)')a(i,:)  
 END DO
END PROGRAM nm49
