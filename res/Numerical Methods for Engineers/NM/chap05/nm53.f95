PROGRAM nm53
!---Interpolation by Cubic Spline Functions---
 USE nm_lib
 USE precision
 IMPLICIT NONE
 INTEGER::i,np
 REAL(iwp)::d6=6.0_iwp,xi,yi,two=2.0_iwp,zero=0.0_iwp
 REAL(iwp),ALLOCATABLE::diffx(:),diffy(:),kv(:),rhs(:),x(:),y(:)
 INTEGER,ALLOCATABLE::kdiag(:)
 OPEN(10,FILE='nm95.dat')
 OPEN(11,FILE='nm95.res')
 READ(10,*)np
 ALLOCATE(diffx(0:np-2),diffy(0:np-2),kdiag(np-2),kv(2*(np-2)-1),         &
   rhs(np-2),x(0:np-1),y(0:np-1))
 READ(10,*)(x(i),y(i),i=0,np-1),xi
 WRITE(11,'(A/)')"---Interpolation by Cubic Spline Functions---"
 WRITE(11,'(A/A)')"       Data Points","      x           y"
 WRITE(11,'(2E12.4)')(x(i),y(i),i=0,np-1)
 DO i=0,np-2
   diffx(i)=x(i+1)-x(i)
   diffy(i)=y(i+1)-y(i)
 END DO
 rhs=zero
 DO i=1,np-2
   kdiag(i)=2*i-1
   kv(kdiag(i))=two*(diffx(i-1)+diffx(i))
   rhs(i)=d6*(diffy(i)/diffx(i)-diffy(i-1)/diffx(i-1))
 END DO
 DO i=1,np-3
   kv(2*i)=diffx(i)
 END DO
 CALL sparin(kv,kdiag)
 CALL spabac(kv,rhs,kdiag)
 WRITE(11,'(/A/A)')"   Interpolated Point","      x           y"
 DO i=1,np-1
   IF(xi<=x(i))THEN
     yi=(rhs(i-1)*(x(i)-xi)**3+rhs(i)*(xi-x(i-1))**3)/(d6*diffx(i-1))+    &
       (y(i-1)/diffx(i-1)-rhs(i-1)*diffx(i-1)/d6)*(x(i)-xi)+              &
       (y(i)/diffx(i-1)-rhs(i)*diffx(i-1)/d6)*(xi-x(i-1))
       EXIT
   END IF
 END DO
 WRITE(11,'(2E12.4)')xi,yi
END PROGRAM nm53
