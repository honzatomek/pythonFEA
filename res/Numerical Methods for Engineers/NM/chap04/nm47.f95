PROGRAM nm47
!---Householder Reduction of Symmetrical Matrix to Tridiagonal Form---
 USE precision  
 IMPLICIT NONE
 INTEGER::i,k,l,n  
 REAL(iwp)::h,one=1.0_iwp,r,zero=0.0_iwp 
 REAL(iwp),ALLOCATABLE::a(:,:),a1(:,:),p(:,:),v(:,:)
 OPEN(10,FILE='nm95.dat')  
 OPEN(11,FILE='nm95.res')
 READ(10,*)n  
 ALLOCATE(a(n,n),a1(n,n),p(n,n),v(n,1))
 DO i=1,n  
   READ(10,*)a(i,i:n)  
   a(i:n,i)=a(i,i:n)  
 END DO
 WRITE(11,'(A,A)')"---Householder Reduction of Symmetrical Matrix to ",     &
   "Tridiagonal Form---"
 WRITE(11,'(/,A)')"Coefficient Matrix" 
 DO i=1,n  
   WRITE(11,'(6E12.4)')a(i,:)  
 END DO
 DO k=1,n-2
   r=zero  
   DO l=k,n-1  
     r=r+a(k,l+1)*a(k,l+1)  
   END DO 
   r=SQRT(r)  
   IF(r*a(k,k+1)>zero)r=-r  
   h=-one/(r*r-r*a(k,k+1))  
   v=zero
   v(k+1,1)=a(k,k+1)-r  
   DO l=k+2,n  
     v(l,1)=a(k,l)  
   END DO
   p=MATMUL(v,TRANSPOSE(v))*h  
   DO l=1,n  
     p(l,l)=p(l,l)+one  
   END DO
   a1=MATMUL(a,p)  
   a=MATMUL(p,a1)
 END DO
 WRITE(11,'(/,A)')"Transformed Main Diagonal" 
 WRITE(11,'(6E12.4)')(a(i,i),i=1,n)
 WRITE(11,'(/,A)')"Transformed Off-Diagonal"
 WRITE(11,'(6E12.4)')(a(i-1,i),i=2,n)
END PROGRAM nm47
