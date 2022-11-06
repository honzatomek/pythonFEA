function triangle_barycentric_test ( )

%*****************************************************************************80
%
%% triangle_barycentric_test() tests triangle_barycentric().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    21 July 2018
%
%  Author:
%
%    John Burkardt
%
  ntest = 7;

  p_test = [ ...
     0.25,   0.25; ...
     0.75,   0.25; ...
     1.00,   1.00; ...
    11.00,   0.50; ...
     0.00,   1.00; ...
     0.50, -10.00; ...
     0.60,   0.60 ]';
  t = [ ...
    0.0, 1.0; ...
    0.0, 0.0; ...
    1.0, 0.0 ]';

  fprintf ( 1, '\n' );
  fprintf ( 1, 'triangle_barycentric_test():\n' );
  fprintf ( 1, '  triangle_barycentric() converts XY coordinates\n' );
  fprintf ( 1, '  to barycentric XSI coordinates in a triangle;\n' );

  r8mat_transpose_print ( 2, 3, t, '  Triangle vertices:' );

  fprintf ( 1, '\n' );
  fprintf ( 1, '   X       Y     XSI\n' );
  fprintf ( 1, '\n' );

  for j = 1 : ntest

    p(1:2) = p_test(1:2,j);

    xsi = triangle_barycentric ( t, p );

    fprintf ( 1, '  %10f  %10f    %10f  %10f  %10f\n', p(1:2), xsi(1:3) );

  end

  return
end
