PROGRAM p64
!---Gauss-Laguerre Rules---
 USE nm_lib  
 USE precision  
 IMPLICIT NONE
 INTEGER::i,nsp  
 REAL(iwp)::area,zero=0.0_iwp
 REAL(iwp),ALLOCATABLE::samp(:,:),wt(:)
 OPEN(10,FILE='nm95.dat')  
 OPEN(11,FILE='nm95.res')
 READ(10,*)nsp  
 ALLOCATE(samp(nsp,2),wt(nsp))
 CALL gauss_laguerre(samp,wt)  
 area=zero
 DO i=1,nsp  
   area=area+wt(i)*f64(samp(i,1))  
 END DO
 WRITE(11,'(A)')"---Gauss-Laguerre Rules---"
 WRITE(11,'(/,A,I7)')"Gauss-Laguerre's Rule  ",nsp
 WRITE(11,'(/,A,F12.4)' )"Computed result      ",area
 CONTAINS
!
 FUNCTION f64(x)
  IMPLICIT NONE
  REAL(iwp),INTENT(IN)::x  
  REAL(iwp)::f64
  f64=SIN(x)
  RETURN
 END FUNCTION f64
!
END PROGRAM p64
