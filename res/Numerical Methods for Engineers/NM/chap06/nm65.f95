PROGRAM p65
!---Multiple Integrals by Gauss-Legendre Rules---
 USE nm_lib  
 USE precision  
 IMPLICIT NONE
 INTEGER::i,ndim,nod,nsp  
 REAL(iwp)::res,zero=0.0_iwp
 REAL(iwp),ALLOCATABLE::coord(:,:),der(:,:),fun(:),samp(:,:),wt(:)
 OPEN(10,FILE='nm95.dat')  
 OPEN(11,FILE='nm95.res')
 READ(10,*)ndim  
 IF(ndim==1)nod=2  
 IF(ndim==2)nod=4  
 IF(ndim==3)nod=8
 ALLOCATE(coord(nod,ndim),der(ndim,nod),fun(nod))
 READ(10,*)(coord(i,:),i=1,nod),nsp  
 ALLOCATE(samp(nsp,ndim),wt(nsp))
 CALL gauss_legendre(samp,wt)  
 res=zero
 DO i=1,nsp
   CALL fun_der(fun,der,samp,i)
   res=res+determinant(MATMUL(der,coord))*wt(i)*f65(MATMUL(fun,coord))
 END DO
 WRITE(11,'(A)')"---Multiple Integrals by Gauss-Legendre Rules---"
 WRITE(11,'(/A,I5/)' )"Number of dimensions     ",ndim
 DO i=1,nod
   WRITE(11,'(A,3F12.4)' )'Coordinates (x,y[,z])',coord(i,:)
 END DO
 WRITE(11,'(/A,I5)' )"Number of sampling points",nsp
 WRITE(11,'(/A,F12.4)' )"Computed result        ",res
 CONTAINS
!
 FUNCTION f65(point)
  IMPLICIT NONE
  REAL(iwp),INTENT(IN)::point(:)
  REAL(iwp)::x,y,z,f65
  x=point(1)  
  y=point(2)  
  z=point(3)
  f65=x**2*y**2
!  f65=x**3-2*y*z
  RETURN
 END FUNCTION f65
!
END PROGRAM p65
