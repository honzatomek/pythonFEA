function r8vec_any_normal_test ( )

%*****************************************************************************80
%
%% r8vec_any_normal_test() tests r8vec_any_normal().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    13 January 2021
%
%  Author:
%
%    John Burkardt
%
  dim_num = 10;
  test_num = 5;

  fprintf ( 1, '\n' );
  fprintf ( 1, 'r8vec_any_normal_test():\n' );
  fprintf ( 1, '  r8vec_any_normal() computes a vector V2 that is normal\n' );
  fprintf ( 1, '  to a given vector V1.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '    Test    |V1|      |V2|        V1.V2\n' );
  fprintf ( 1, '\n' );

  for test = 1 : test_num

    v1 = rand ( dim_num, 1 );
    v1_length = norm ( v1 );
    v2 = r8vec_any_normal ( dim_num, v1 );
    v2_length = norm ( v2 );
    v1v2_dot = v1(1:dim_num)' * v2(1:dim_num);
    fprintf ( 1, '  %6d  %10g  %10g  %10g\n', ...
      test, v1_length, v2_length, v1v2_dot );
  end

  return
end
