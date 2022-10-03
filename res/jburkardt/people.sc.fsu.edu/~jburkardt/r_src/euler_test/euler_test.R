#*****************************************************************************80
#
## euler_test tests euler.
#
#  Licensing:
#
#    Copyright 2016 James P. Howard, II
#
#    The computer code and data files on this web page are distributed under
#    https://opensource.org/licenses/BSD-2-Clause, the BSD-2-Clause license.
#
#  Modified:
#
#    23 March 2020
#
#  Author:
#
#    John Burkardt
#
cat ( date ( ), "\n" )
cat ( "\n" )
cat ( "euler_test\n" )
cat ( " ", version$version.string, "\n" )
cat ( "  euler uses the Euler method to solve an ordinary differential equation.\n" )

source ( "/home/burkardt/public_html/r_src/euler/euler.R" )  

f <- function ( x, y ) { y / ( 2.0 * x + 1.0 ) }
cat ( "\n" )
cat ( "  f(x,y) = y/(2*x+1)\n" )
cat ( "\n" )

a = 0.0
b = 1.0
h = 0.2
n = 5
solution <- euler ( f, a, b, h, n )
print ( solution )
#
#  Terminate.
#
cat ( "\n" )
cat ( "euler_test\n" )
cat ( "  Normal end of execution.\n" )
cat ( date ( ), "\n" )

quit ( )
