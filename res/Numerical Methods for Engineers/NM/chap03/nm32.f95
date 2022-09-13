PROGRAM nm32
!---Bisection Method for a Single Root---
 USE nm_lib  
 USE precision  
 IMPLICIT NONE
 INTEGER::iters,limit 
 REAL(iwp)::half=0.5_iwp,tol,xmid,xi,xip1,xold,zero=0.0_iwp
 OPEN(10,FILE='nm95.dat')  
 OPEN(11,FILE='nm95.res')
 WRITE(11,'(A)')"---Bisection Method for a Single Root---"
 READ(10,*)xi,xip1,tol,limit
 IF(f32(xi)*f32(xip1)>zero)THEN
   WRITE(11,'(/,A)')"Root not captured, try again."
   STOP
 END IF   
 WRITE(11,'(/,A,/,E12.4,A,E12.4)')"Starting Range",xi," to", xip1 
 WRITE(11,'(/,A)')"First Few Iterations"  
 iters=0  
 xold=xi
 DO  
   iters=iters+1  
   xmid=half*(xi+xip1)
   IF(f32(xi)*f32(xmid)<zero)THEN  
     xip1=xmid  
   ELSE  
     xi=xmid  
   END IF
   IF(iters<5)WRITE(11,'(3E12.4)')xmid,f32(xmid)
   IF(check(xmid,xold,tol).OR.iters==limit)EXIT  
   xold=xmid  
 END DO
 WRITE(11,'(/,A,/,I5)')"Iterations to Convergence",iters
 WRITE(11,'(/,A,/,E12.4)')"Solution",xmid
 CONTAINS
!
 FUNCTION f32(x)
  IMPLICIT NONE
  REAL(iwp),INTENT(IN)::x  
  REAL(iwp)::f32
  f32=x**3-3.0_iwp*x+1.0_iwp  
 RETURN
 END FUNCTION f32
!
END PROGRAM nm32
