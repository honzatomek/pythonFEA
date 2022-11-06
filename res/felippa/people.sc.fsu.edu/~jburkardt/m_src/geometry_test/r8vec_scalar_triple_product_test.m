function r8vec_scalar_triple_product_test ( )

%*****************************************************************************80
%
%% r8vec_scalar_triple_product_test() tests r8vec_scalar_triple_product().
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
  fprintf ( 1, '\n' );
  fprintf ( 1, 'r8vec_scalar_triple_product_test():\n' );
  fprintf ( 1, '  r8vec_scalar_triple_product() computes the \n' );
  fprintf ( 1, '  scalar triple product of three R8VEC''s.\n' );
  fprintf ( 1, '  S = V1 dot ( V2 x V3 ).\n' );

  v1 = [ -1.0; 3.0;  3.0 ];
  v2 = [ -2.0; 3.0;  1.0 ];
  v3 = [  0.0;  4.0;  0.0 ];

  r8vec3_print ( v1, v2, v3, '  V1, V2 and V3:' );

  s = r8vec_scalar_triple_product ( v1, v2, v3 );

  sexact = -20.0;
 
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Computed = %g, Exact = %g\n', s, sexact );

  return
end
