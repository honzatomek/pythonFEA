function geometry_test213 ( )

%*****************************************************************************80
%
%% geometry_test213() tests triangle_xy_to_xsi(), triangle_xsi_to_xy().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    10 April 2007
%
%  Author:
%
%    John Burkardt
%
  dim_num = 2;

  t = [ ...
     4.0, 2.0; ...
     1.0, 5.0; ...
    -2.0, 2.0 ]';

  fprintf ( 1, '\n' );
  fprintf ( 1, 'TEST213\n' );
  fprintf ( 1, '  TRIANGLE_XY_TO_XSI converts XY to XSI coordinates.\n' );
  fprintf ( 1, '  TRIANGLE_XSI_TO_XY converts XSI to XY coordinates.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  We verify that (X,Y) -> (XSI1,XSI2,XSI3) -> (X,Y)\n' );
  fprintf ( 1, '  works properly.\n' );

  r8mat_transpose_print ( dim_num, 3, t, '  Triangle vertices:' );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Sample points:\n' );
  fprintf ( 1, '\n' );

  for j = 1 : 10

    if ( j == 1 )
      for i = 1 : dim_num
        p(i) = sum ( t(i,1:3) ) / 3.0;
      end
    elseif ( j == 2 )
      p(1) = 3.0;
      p(2) = 0.0;
    else
      p = triangle_sample ( t, 1 );
    end

    xsi = triangle_xy_to_xsi ( t, p );

    p2 = triangle_xsi_to_xy ( t, xsi );

    fprintf ( 1, '\n' );
    fprintf ( 1, '  %8f  %8f    %8f  %8f  %8f\n', ...
      p(1:dim_num), xsi(1:dim_num+1) );
    fprintf ( 1, '  %8f  %8f\n', p2(1:dim_num) );

  end

  return
end
