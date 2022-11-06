function hermite_gk16_set_test ( )

%*****************************************************************************80
%
%% hermite_gk16_set_test() tests hermite_gk16_set().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    21 June 2015
%
%  Author:
%
%    John Burkardt
%
  l_max = 8;

  n_list = [ 1, 3, 7, 9, 17, 19, 31, 33, 35 ]';

  fprintf ( 1, '\n' );
  fprintf ( 1, 'HERMITE_GK16_SET_TEST\n' );
  fprintf ( 1, '  HERMITE_GK16_SET sets up a nested rule\n' );
  fprintf ( 1, '  for the Hermite integration problem.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Index       X             W\n' );

  for l = 0 : l_max

    n = n_list(l+1);

    [ x, w ] = hermite_gk16_set ( n );

    fprintf ( 1, '\n' );

    for i = 1 : n
      fprintf ( 1, '  %2d  %24.16g  %24.16g\n', i, x(i), w(i) );
    end

  end
  return
end
