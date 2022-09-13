PROGRAM nm27
!---Cholesky LLT Factorization With Skyline Storage---
!---Prescribed Solutions by Penalty Method---
 USE nm_lib
 USE precision
 IMPLICIT NONE
 INTEGER::ir,n,nfix
 REAL(iwp)::penalty=1.E20_iwp
 REAL(iwp),ALLOCATABLE::a(:),b(:),val(:)
 INTEGER,ALLOCATABLE::kdiag(:),no(:)
 OPEN(10,FILE='nm95.dat')
 OPEN(11,FILE='nm95.res')
 READ(10,*)n
 ALLOCATE(kdiag(n),b(n))
 READ(10,*)kdiag
 ir=kdiag(n)
 ALLOCATE(a(ir))
 READ(10,*)a,b,nfix
 ALLOCATE(no(nfix),val(nfix))
 IF(nfix>0)READ(10,*)no,val
 WRITE(11,'(A)')"---Cholesky Factorization With Skyline Storage---"
 WRITE(11,'(A)')"---Prescribed Solutions by Penalty Method---"
 WRITE(11,'(/,A)')"Coefficient Vector"
 WRITE(11,'(6E12.4)')a
 WRITE(11,'(/,A)')"Diagonal Locations"
 WRITE(11,'(8I5)')kdiag
 WRITE(11,'(/,A)')"Right Hand Side Vector"
 WRITE(11,'(6E12.4)')b
 WRITE(11,'(/,A)')"Prescribed Solution Number(s)"
 WRITE(11,'(8I5)')no
 WRITE(11,'(/,A)')"Prescribed Solution Value(s)"
 WRITE(11,'(6E12.4)')val
 a(kdiag(no(:)))=a(kdiag(no(:)))+penalty
 b(no)=a(kdiag(no(:)))*val(:)
 CALL sparin(a,kdiag)
 CALL spabac(a,b,kdiag)
 WRITE(11,'(/,A)')"Solution Vector"
 WRITE(11,'(6E12.4)')b
END PROGRAM nm27
