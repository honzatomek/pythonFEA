#*****************************************************************************80
#
## midpoint_test() tests midpoint().
#
#  Licensing:
#
#    This code is distributed under the GNU LGPL license.
#
#  Modified:
#
#    28 April 2021
#
cat ( date ( ), "\n" )
cat ( "\n" )
cat ( "midpoint_test()\n" )
cat ( " ", version$version.string, "\n" )
cat ( "  Test midpoint() for solving ordinary differential equations.\n" )

source ( "/home/burkardt/public_html/r_src/midpoint_test/hund_midpoint.R" )
source ( "/home/burkardt/public_html/r_src/midpoint_test/stiff_midpoint.R" )

hund_midpoint ( )
stiff_midpoint ( )
#
#  Terminate.
#
cat ( "\n" )
cat ( "midpoint_test():\n" )
cat ( "  Normal end of execution.\n" )
cat ( date ( ), "\n" )

quit ( )

