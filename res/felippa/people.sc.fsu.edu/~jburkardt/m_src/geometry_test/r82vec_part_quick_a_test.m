function r82vec_part_quick_a_test ( )

%*****************************************************************************80
%
%% r82vec_part_quick_a_test() tests r82vec_part_quick_a().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    19 April 2009
%
%  Author:
%
%    John Burkardt
%
  n = 12;
  b = 0.0;
  c = 10.0;

  fprintf ( 1, '\n' );
  fprintf ( 1, 'r82vec_part_quick_a_test():\n' );
  fprintf ( 1, '  r82vec_part_quick_a() reorders an R82VEC\n' );
  fprintf ( 1, '  as part of a quick sort.\n' );

  a = 10.0 * rand ( 2, n );
 
  r82vec_print ( n, a, '  Before rearrangement:' );

  [ a, l, r ] = r82vec_part_quick_a ( n, a );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Rearranged array\n' );
  fprintf ( 1, '  Left index =  %d\n', l );
  fprintf ( 1, '  Key index =   %d\n', l+1 );
  fprintf ( 1, '  Right index = %d\n', r );
  fprintf ( 1, '\n' );

  r82vec_print ( l,     a(1:2,1:l),   '  Left half:' );
  r82vec_print ( 1,     a(1:2,l+1),   '  Key:' );
  r82vec_print ( n-l-1, a(1:2,l+2:n), '  Right half:' );

  return
end
