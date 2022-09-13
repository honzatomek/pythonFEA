PROGRAM nm45
!---Conversion of Kx=lambda*Mx to Symmetrical Standard Form--- 
 USE nm_lib  
 USE precision  
 IMPLICIT NONE
 INTEGER::i,j,n 
 REAL(iwp),ALLOCATABLE::c(:,:),d(:),e(:),k(:,:),m(:,:),s(:,:)
 OPEN(10,FILE='nm95.dat')  
 OPEN(11,FILE='nm95.res')
 READ(10,*)n  
 ALLOCATE(c(n,n),d(n),e(n),k(n,n),m(n,n),s(n,n))
 DO i=1,n  
   READ(10,*)k(i,i:n)  
   k(i:n,i)=k(i,i:n)  
 END DO 
 DO i=1,n  
   READ(10,*)m(i,i:n)  
   m(i:n,i)=m(i,i:n)  
 END DO 
 WRITE(11,'(A)')"---Conversion of Kx=lambda*Mx to Symmetrical Standard Form---"
 WRITE(11,'(/,A)')"Matrix K"
 DO i=1,n  
   WRITE(11,'(6E12.4)')k(i,:)  
 END DO
 WRITE(11,'(/,A)')"Matrix M"
 DO i=1,n  
   WRITE(11,'(6E12.4)')m(i,:)  
 END DO
 CALL ldlt(m,d)  
 d=SQRT(d)
 DO j=1,n  
   DO i=j,n  
     m(i,j)=m(i,j)/d(j)  
   END DO  
 END DO
 DO j=1,n  
   e=k(:,j)  
   CALL subfor(m,e)  
   c(:,j)=e  
 END DO
 WRITE(11,'(/,A)')"Matrix C"  
 DO i=1,n  
   WRITE(11,'(6E12.4)')c(i,:)  
 END DO
 DO j=1,n  
   e=c(j,:)  
   CALL subfor(m,e)  
   s(:,j)=e  
 END DO
 WRITE(11,'(/,A)')"Final symmetrical matrix S"
 DO i=1,n  
   WRITE(11,'(6E12.4)')s(i,:)  
 END DO
END PROGRAM nm45
