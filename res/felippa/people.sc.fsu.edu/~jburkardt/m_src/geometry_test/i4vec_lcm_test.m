function i4vec_lcm_test ( )

%*****************************************************************************80
%
%% i4vec_lcm_test() tests i4vec_lcm().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    31 July 2018
%
%  Author:
%
%    John Burkardt
%
  n = 4;
  i4vec = [ 2^3*3  *5  *7*11  *13, ...
            2  *3^2*5  *7*11  *13, ...
            2  *3  *5^3*7*11  *13, ...
            2  *3  *5  *7*11^2*13];

  fprintf ( 1, '\n' );
  fprintf ( 1, 'i4vec_lcm_test():\n' );
  fprintf ( 1, '  i4vec_lcm() computes the least common multiple of the\n' );
  fprintf ( 1, '  entries in an I4VEC.\n' );

  i4vec_print ( n, i4vec, '  The I4VEC:' );

  value = i4vec_lcm ( n, i4vec ); 

  fprintf ( 1, '\n' );
  fprintf ( 1, '  LCM = %d\n', value );

  return
end
