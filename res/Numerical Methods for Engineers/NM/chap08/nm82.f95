PROGRAM nm82
!---Simple FE analysis of Example 8.3---
 USE nm_lib
 USE precision
 IMPLICIT NONE
 INTEGER::g_num(4,16),i,iel,kdiag(25),num(4)
 REAL(iwp)::kc(4,4),loads(25)
 REAL(iwp),ALLOCATABLE::kv(:)
 OPEN(11,FILE='nm95.res')
 WRITE(11,'(A/)')"---Simple FE analysis of Example 8.3---"
 kc(1,:)=(/4.0_iwp,-1.0_iwp,-2.0_iwp,-1.0_iwp/)
 kc(2,:)=(/-1.0_iwp,4.0_iwp,-1.0_iwp,-2.0_iwp/)
 kc(3,:)=(/-2.0_iwp,-1.0_iwp,4.0_iwp,-1.0_iwp/)
 kc(4,:)=(/-1.0_iwp,-2.0_iwp,-1.0_iwp,4.0_iwp/)
 kc=kc/6.0_iwp
 kdiag=0
 DO iel=1,25
   num=geometry(iel)
   g_num(:,iel)=num
   CALL fkdiag(kdiag,num)
 END DO
 DO i=2,25
   kdiag(i)=kdiag(i)+kdiag(i-1)
 END DO
 ALLOCATE(kv(kdiag(25)))
 kv=0.0_iwp
 loads=0.0_iwp
 DO iel=1,25
   num=g_num(:,iel)
   CALL fsparv(kv,kc,num,kdiag)
 END DO
 kv(kdiag(1:4))=kv(kdiag(1:4))+1.E20_iwp
 kv(kdiag(6:16:5))=kv(kdiag(6:16:5))+1.E20_iwp
 kv(kdiag(10:20:5))=kv(kdiag(10:20:5))+1.E20_iwp
 kv(kdiag(22:25))=kv(kdiag(22:25))+1.E20_iwp
 loads(:4)=kv(kdiag(:4))*50.0_iwp
 loads(6:16:5)=kv(kdiag(6:16:5))*50.0_iwp
 CALL sparin(kv,kdiag)
 CALL spabac(kv,loads,kdiag)
 WRITE(11,'(A)') "  Node   Temperature"
 DO i=1,25
   WRITE(11,'(I5,F12.2)')i,loads(i)
 END DO
 CONTAINS

 FUNCTION geometry(iel)
!---Element Node Numbers for Example 8.3
!---4-Node only 4x4 mesh---
  IMPLICIT NONE
  INTEGER,INTENT(IN)::iel
  REAL(iwp)::geometry(4)
  INTEGER::ip,iq
  iq=(iel-1)/4+1
  ip=iel-(iq-1)*4
  geometry =(/iq*5+ip,(iq-1)*5+ip,(iq-1)*5+ip+1,iq*5+ip+1/)
 END FUNCTION geometry

END PROGRAM nm82
