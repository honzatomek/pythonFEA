#*****************************************************************************80
#
## midpoint_explicit_test() tests midpoint_explicit.
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
#    24 March 2020
#
#  Author:
#
#    John Burkardt
#
cat ( date ( ), "\n" )
cat ( "\n" )
cat ( "midpoint_explicit_test\n" )
cat ( " ", version$version.string, "\n" )
cat ( "  midpoint_explicit uses the explicit midpoint method to solve \n" )
cat ( "  an ordinary differential equation.\n" )

source ( "/home/burkardt/public_html/r_src/midpoint_explicit/midpoint_explicit.R" )  

f <- function ( x, y ) { y / ( 2.0 * x + 1.0 ) }
cat ( "\n" )
cat ( "  f(x,y) = y/(2*x+1)\n" )
cat ( "\n" )

a = 0.0
b = 1.0
h = 0.2
n = 5
solution <- midpoint_explicit ( f, a, b, h, n )
print ( solution )
#
#  Terminate.
#
cat ( "\n" )
cat ( "midpoint_explicit_test\n" )
cat ( "  Normal end of execution.\n" )
cat ( date ( ), "\n" )

quit ( )
