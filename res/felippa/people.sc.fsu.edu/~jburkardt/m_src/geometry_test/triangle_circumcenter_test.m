function triangle_circumcenter_test ( )

%*****************************************************************************80
%
%% triangle_circumcenter_test() tests triangle_circumcenter().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    28 October 2010
%
%  Author:
%
%    John Burkardt
%
  m = 2;
  test_num = 4;
  t_test = cat ( 3, ...
    [ 10.0,  5.0; 
      11.0,  5.0; ...
      10.0,  6.0 ]', ...
    [ 10.0,  5.0; ...
      11.0,  5.0; ...
      10.5,  5.86602539 ]', ...
    [ 10.0,  5.0; ...
      11.0,  5.0; ...
      10.5, 15.0 ]', ...
    [ 10.0,  5.0; ...
      11.0,  5.0; ...
      20.0,   7.0 ]' );

  fprintf ( 1, '\n' );
  fprintf ( 1, 'triangle_circumcenter_test()::\n' );
  fprintf ( 1, '  triangle_circumcenter() computes the circumcenter of a triangle;\n' );
  fprintf ( 1, '  triangle_circumcenter_2() computes the circumcenter of a triangle;\n' );

  for test = 1 : test_num

    t(1:m,1:3) = t_test(1:m,1:3,test);

    r8mat_print ( m, 3, t, '  Triangle vertices ( columns )' );

    pc = triangle_circumcenter ( t );

    r8vec_print ( m, pc, '  triangle_circumenter:' );

    pc = triangle_circumcenter_2 ( t );

    r8vec_print ( m, pc, '  triangle_circumcenter_2:' );

  end

  return
end
