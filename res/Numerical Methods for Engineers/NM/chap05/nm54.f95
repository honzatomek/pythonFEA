PROGRAM nm54
!---Curve Fitting by Least Squares---
 USE nm_lib  
 USE precision  
 IMPLICIT NONE
 INTEGER::i,ic,j,k,nc,np,nv  
 INTEGER,ALLOCATABLE::kdiag(:)
 REAL(iwp)::es,my,r2,sd,yi,zero=0.0_iwp
 REAL(iwp),ALLOCATABLE::c(:),f(:),kv(:),x(:,:),y(:)
 OPEN(10,FILE='nm95.dat')  
 OPEN(11,FILE='nm95.res')
 READ(10,*)np,nv,nc
 ALLOCATE(kdiag(nc),kv(nc*(nc+1)/2),f(nc),c(nc),x(np,nv),y(np))
 READ(10,*)(x(i,:),y(i),i=1,np)
 WRITE(11,'(A)')"---Curve Fitting by Least Squares---"
 c=zero  
 kv=zero  
 my=SUM(y)/np
 DO i=1,np
   CALL f54(x(i,:),f)  
   ic=0
   DO j=1,nc
     c(j)=c(j)+f(j)*y(i)
     DO k=1,j  
     ic=ic+1  
     kv(ic)=kv(ic)+f(j)*f(k)  
     END DO
   END DO
 END DO
 DO i=1,nc  
   kdiag(i)=i*(i+1)/2  
 END DO
 CALL sparin(kv,kdiag)  
 CALL spabac(kv,c,kdiag)
 WRITE(11,'(/A,/,5E12.4)')"  Optimized Function Coefficients",c
 WRITE(11,'(/A)')"  Data Points and Fitted Point"
 WRITE(11,'(A,I1,A)')"  (x(i),i=1,",nv,"), y, yi"
 sd=zero  
 es=zero
 DO i=1,np  
   CALL f54(x(i,:),f)  
   yi=DOT_PRODUCT(c,f)
   sd=sd+(y(i)-my)**2  
   es=es+(y(i)-yi)**2
   WRITE(11,'(5E12.4)')x(i,:),y(i),yi
 END DO
 r2=(sd-es)/sd
 WRITE(11,'(/A,/,E12.4)')"  r-squared",r2
 CONTAINS
!
 SUBROUTINE f54(x,f)
  IMPLICIT NONE
  REAL(iwp),INTENT(IN)::x(:)
  REAL(iwp),INTENT(OUT)::f(:)
  f(1)=1.0_iwp
  f(2)=LOG(x(1))
!  f(2)=x(1)
!  f(3)=x(2)
  RETURN
 END SUBROUTINE f54
!
END PROGRAM nm54
