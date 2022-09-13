PROGRAM p62
!---Repeated Gauss-Legendre Rules---
 USE nm_lib  
 USE precision  
 IMPLICIT NONE
 INTEGER::i,j,nr,nsp
 REAL(iwp)::a,area,b,cr,hr,pt5=0.5_iwp,wr,zero=0.0_iwp
 REAL(iwp),ALLOCATABLE::samp(:,:),wt(:)
 OPEN(10,FILE='nm95.dat')  
 OPEN(11,FILE='nm95.res')
 READ(10,*)a,b,nsp,nr  
 ALLOCATE(samp(nsp,1),wt(nsp))
 CALL gauss_legendre(samp,wt)  
 wr=(b-a)/nr  
 hr=pt5*wr  
 area=zero
 DO i=1,nr
   cr=a+(i-1)*wr+hr
   DO j=1,nsp  
     area=area+wt(j)*hr*f62(cr+samp(j,1)*hr)  
   END DO
 END DO
 WRITE(11,'(A)')"---Repeated Gauss-Legendre Rules---"
 WRITE(11,'(/,A,2F12.4)')"Limits of Integration",a,b
 WRITE(11,'(A,I7)')"Gauss-Legendre Rule  ",nsp
 WRITE(11,'(A,I7)')"Number of Repetitions",nr
 WRITE(11,'(/,A,F12.4)' )"Computed Result      ",area
 CONTAINS
!
 FUNCTION f62(x)
  IMPLICIT NONE
  REAL(iwp),INTENT(IN)::x  
  REAL(iwp)::f62
  f62=x*COS(x)
  RETURN
 END FUNCTION f62
!
END PROGRAM p62
