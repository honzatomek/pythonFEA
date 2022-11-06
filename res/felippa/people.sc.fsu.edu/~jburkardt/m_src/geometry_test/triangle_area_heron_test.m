function triangle_area_heron_test ( )

%*****************************************************************************80
%
%% triangle_area_heron_test() tests triangle_area_heron();
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    23 July 2018
%
%  Author:
%
%    John Burkardt
%
  t = [ ...
    1.0, 0.0, 0.0; ...
    0.0, 1.0, 0.0; ...
    0.0, 0.0, 1.0 ];

  fprintf ( 1, '\n' );
  fprintf ( 1, 'triangle_area_heron_test():\n' );
  fprintf ( 1, '  triangle_area_heron() computes the area of a triangle.\n' );

  r8mat_transpose_print ( 3, 3, t, '  Triangle vertices:' );

  for j = 1 : 3

    s(j) = 0.0;

    jp1 = mod ( j, 3 ) + 1;

    for i = 1 : 3
      s(j) = s(j) + ( t(i,j) - t(i,jp1) ).^2;
    end

    s(j) = sqrt ( s(j) );

  end

  r8vec_print ( 3, s, '  Side lengths:' );

  area = triangle_area_heron ( s );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  The area is %f\n', area );

  return
end
