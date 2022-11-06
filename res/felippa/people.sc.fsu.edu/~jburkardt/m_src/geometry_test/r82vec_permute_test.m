function r82vec_permute_test ( )

%*****************************************************************************80
%
%% r82vec_permute_test() tests r82vec_permute().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    02 August 2018
%
%  Author:
%
%    John Burkardt
%
  n = 5;

  p = [ 2, 4, 5, 1, 3 ];
  xy = [ ...
    1.0,  2.0,  3.0,  4.0,  5.0; ...
   11.0, 22.0, 33.0, 44.0, 55.0 ];

  fprintf ( 1, '\n' );
  fprintf ( 1, 'r82vec_permute_test():\n' );
  fprintf ( 1, '  r82vec_permute() permutes an R82VEC.\n' );

  r82vec_print ( n, xy, '  Original array XY[]:' )

  i4vec_print ( n, p, '  Permutation vector P[]:' )

  xy = r82vec_permute ( n, xy, p );

  r82vec_print ( n, xy, '  Permuted array X[P[]]:' );

  return
end
