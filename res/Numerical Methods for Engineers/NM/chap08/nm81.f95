PROGRAM nm81
!---Explicit Finite Differences in 1D---
 USE precision
 IMPLICIT NONE
 INTEGER::i,j,nres,nt,ntime,nz
 REAL(iwp)::area0,areat,beta,cv,dt,dz,layer,pt5=0.5_iwp,tmax,two=2.0_iwp, &
   zero=0.0_iwp
 REAL(iwp),ALLOCATABLE::a(:),b(:),c(:)
 CHARACTER(LEN=2)::bc
 OPEN(10,FILE='nm95.dat')
 OPEN(11,FILE='nm95.res')
 WRITE(11,'(A)')"---Explicit Finite Differences in 1D---"
 READ(10,*)layer,tmax,dz,dt,cv
 beta=cv*dt/dz**2
 IF(beta>pt5)THEN
   WRITE(11,'("beta too big")')
   STOP
 ENDIF
 nz=nint(layer/dz)
 nt=nint(tmax/dt)
 ALLOCATE(a(nz+1),b(nz+1),c(nz+1))
 READ(10,*)b,bc,nres,ntime
 area0=pt5*dz*(b(1)+b(nz+1))+dz*SUM(b(2:nz))
 WRITE(11,'(/,A,I3,A)')"    Time      Deg of Con  Pressure(grid pt",nres,")"
 WRITE(11,'(3E12.4)')zero,zero,b(nres)
 a=zero
 DO j=1,nt
   IF(bc=='uu'.OR.bc=='ud')a(1)=b(1)+two*beta*(b(2)-b(1))
   a(2:nz)=b(2:nz)+beta*(b(1:nz-1)-two*b(2:nz)+b(3:nz+1))
   IF(bc=='uu'.OR.bc=='du')a(nz+1)=b(nz+1)+two*beta*(b(nz)-b(nz+1))
   b=a
   IF(j==ntime)c=b
   areat=pt5*dz*(b(1)+b(nz+1))+dz*SUM(b(2:nz))
   WRITE(11,'(3E12.4)')j*dt,(area0-areat)/area0,b(nres)
 END DO
 WRITE(11,'(/,A,I3,A)')"    Depth     Pressure(after",ntime," steps)"
 WRITE(11,'(2E12.4)')(dz*(i-1),c(i),i=1,nz+1)
END PROGRAM nm81
