PROGRAM p63
!---Adaptive Gauss-Legendre Rules---
 USE nm_lib
 USE precision
 IMPLICIT NONE
 INTEGER::cf,ct,i,j,inew,nr1=1,nr2,nsp1,nsp2
 REAL(iwp)::a,abserr,ans,area,area1,area2,b,errest,hr,relerr,one=1.0_iwp, &
   pt5=0.5_iwp,tol,tot_err,wr,zero=0.0_iwp
 LOGICAL::verdict
 REAL(iwp),ALLOCATABLE::answer1(:),err1(:),limits1(:,:),samp1(:,:),wt1(:)
 REAL(iwp),ALLOCATABLE::answer2(:),err2(:),limits2(:,:),samp2(:,:),wt2(:)
 LOGICAL,ALLOCATABLE::conv1(:),conv2(:)
 OPEN(10,FILE='nm95.dat')
 OPEN(11,FILE='nm95.res')
 ALLOCATE(answer1(nr1),err1(nr1),conv1(nr1),limits1(nr1,2))
 ALLOCATE(answer2(nr1),err2(nr1),conv2(nr1),limits2(nr1,2))
!---Rule 2 Should be More Accurate than Rule 1 so nsp2 >> nsp1
 READ(10,*)limits1,abserr,relerr,nsp1,nsp2
 ALLOCATE(samp1(nsp1,1),samp2(nsp2,1),wt1(nsp1),wt2(nsp2))
 CALL gauss_legendre(samp1,wt1)
 CALL gauss_legendre(samp2,wt2)
 WRITE(11,'(A)')"---Adaptive Gauss-Legendre Rules---"
 WRITE(11,'(/A,E12.4)')'Absolute Error Tolerance        ',abserr
 WRITE(11,'(A,E12.4)') 'Relative Error Tolerance        ',relerr
 WRITE(11,'(/A,2F12.4)')'Limits of Integration       ',limits1
 WRITE(11,'(A,I5)')'Low  Order Gauss-Legendre Rule',nsp1
 WRITE(11,'(A,I5)')'High Order Gauss-Legendre Rule',nsp2
 conv1=.FALSE.
 DO
   area=zero
   tot_err=zero
   ct=0
   cf=0
   DO i=1,nr1
     IF(.NOT.conv1(i))THEN
       a=limits1(i,1)
       b=limits1(i,2)
       nsp1=UBOUND(samp1,1)
       nsp2=UBOUND(samp2,1)
       wr=b-a
       hr=pt5*wr
       area1=zero
       area2=zero
       DO j=1,nsp1
         area1=area1+wt1(j)*hr*f63(a+hr*(one-samp1(j,1)))
       END DO
       DO j=1,nsp2
         area2=area2+wt2(j)*hr*f63(a+hr*(one-samp2(j,1)))
       END DO
       errest=area1-area2
       tol=MAX(abserr,relerr*ABS(area2))
       ans=area2
       verdict=.FALSE.
       IF(ABS(errest)<tol)verdict=.TRUE.
       answer1(i)=ans
       conv1(i)=verdict
       err1(i)=errest
     END IF
     IF(conv1(i))THEN
     ct=ct+1
     ELSE
     cf=cf+1
     END IF
     area=area+answer1(i)
     tot_err=tot_err+err1(i)
   END DO
   IF(cf==0)THEN
     WRITE(11,'(A,I5)')"Number of Repetitions         ",nr1
     WRITE(11,'(/A)' )"  *****Strip Limits*****    Strip Area        Error"
     DO i=1,nr1
       WRITE(11,'(2E12.4,2E16.8)')limits1(i,:),answer1(i),err1(i)
     END DO
     WRITE(11,'(/A,2E16.8)')"Solution and Total Error",area,tot_err
     EXIT
   END IF
   limits2=limits1
   answer2=answer1
   conv2=conv1
   err2=err1
   nr2=nr1
   nr1=ct+2*cf
   DEALLOCATE(answer1,conv1,err1,limits1)
   ALLOCATE(answer1(nr1),conv1(nr1),err1(nr1),limits1(nr1,2))
   conv1=.FALSE.
   inew=0
   DO i=1,nr2
     IF(conv2(i))THEN
       inew=inew+1
       limits1(inew,:)=limits2(i,:)
       answer1(inew)=answer2(i)
       err1(inew)=err2(i)
       conv1(inew)=.TRUE.
     ELSE
       inew=inew+1
       limits1(inew,1)=limits2(i,1)
       limits1(inew,2)=(limits2(i,1)+limits2(i,2))*pt5
       inew=inew+1
       limits1(inew,1)=(limits2(i,1)+limits2(i,2))*pt5
       limits1(inew,2)=limits2(i,2)
     END IF
   END DO
   DEALLOCATE(answer2,conv2,err2,limits2)
   ALLOCATE(answer2(nr1),conv2(nr1),err2(nr1),limits2(nr1,2))
 END DO
 CONTAINS
!
 FUNCTION f63(x)
  IMPLICIT NONE
  REAL(iwp),INTENT(IN)::x
  REAL(iwp)::f63
  f63=x**(1.0_iwp/7.0_iwp)/(x**2+1.0_iwp)
  RETURN
 END FUNCTION f63
!
END PROGRAM p63
