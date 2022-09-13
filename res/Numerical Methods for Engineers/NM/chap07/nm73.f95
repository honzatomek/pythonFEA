PROGRAM nm73
!---Fourth Order Predictor-Corrector Methods---
 USE precision
 IMPLICIT NONE
 INTEGER::i,itype,j,nsteps
 REAL(iwp)::d3=3.0_iwp,d4=4.0_iwp,d5=5.0_iwp,d9=9.0_iwp,d14=14.0_iwp,     &
   d19=19.0_iwp,d24=24.0_iwp,d29=29.0_iwp,d37=37.0_iwp,d55=55.0_iwp,      &
   d59=59.0_iwp,d270=270.0_iwp,e,h,two=2.0_iwp,x(-3:1),y(-3:1),y1
 OPEN(10,FILE='nm95.dat')
 OPEN(11,FILE='nm95.res')
 READ(10,*)itype,nsteps,h
 READ(10,*)(x(i),y(i),i=-3,0)
 SELECT CASE(itype)
 CASE(1)
   WRITE(11,'(A)')"---Milne-Simpson 4th Order P-C Methods---"
   WRITE(11,'(/,A)')"      x            y         Error"
   WRITE(11,'(2E13.5)')(x(i),y(i),i=-3,0)
   DO j=1,nsteps
     x(1)=x(0)+h
     y1=y(-3)+d4*h/d3*(two*f73(x(-2),y(-2))-f73(x(-1),y(-1))+             &
       two*f73(x(0),y(0)))
     y(1)=y(-1)+h/d3*(f73(x(-1),y(-1))+d4*f73(x(0),y(0))+f73(x(1),y1))
     e=-(y(1)-y1)/d29
     WRITE(11,'(3E13.5)')x(1),y(1),e
     y(-3:0)=y(-2:1)
     x(-3:0)=x(-2:1)
   END DO
 CASE(2)
   WRITE(11,'(A)')"---Adams-Bashforth-Moulton 4th Order P-C Methods---"
   WRITE(11,'(/,A)')"      x            y         Error"
   WRITE(11,'(2E13.5)')(x(i),y(i),i=-3,0)
   DO j=1,nsteps
     x(1)=x(0)+h
     y1=y(0)+h/d24*(-d9*f73(x(-3),y(-3))+d37*f73(x(-2),y(-2))-                &
       d59*f73(x(-1),y(-1))+d55*f73(x(0),y(0)))
     y(1)=y(0)+h/d24*(f73(x(-2),y(-2))-d5*f73(x(-1),y(-1))+                   &
       d19*f73(x(0),y(0))+d9*f73(x(1),y1))
     e=-d19*(y(1)-y1)/d270
     WRITE(11,'(3E13.5)')x(1),y(1),e
     y(-3:0)=y(-2:1)
     x(-3:0)=x(-2:1)
   END DO
 END SELECT
 CONTAINS

 FUNCTION f73(x,y)
  IMPLICIT NONE
  REAL(iwp),INTENT(IN)::x,y
  REAL(iwp)::f73
  f73=x*y**2+2._iwp*x**2
  RETURN
 END FUNCTION f73

END PROGRAM nm73
