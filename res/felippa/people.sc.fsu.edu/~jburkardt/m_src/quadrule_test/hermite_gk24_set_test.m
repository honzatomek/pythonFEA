function hermite_gk24_set_test ( )

%*****************************************************************************80
%
%% hermite_gk24_set_test() tests hermite_gk24_set().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    20 June 2015
%
%  Author:
%
%    John Burkardt
%
  l_max = 4;

  n_list = [ 1, 3, 9, 19, 43 ]';

  fprintf ( 1, '\n' );
  fprintf ( 1, 'HERMITE_GK24_SET_TEST\n' );
  fprintf ( 1, '  HERMITE_GK24_SET sets up a nested rule\n' );
  fprintf ( 1, '  for the Hermite integration problem.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Index       X             W\n' );

  for l = 0 : l_max

    n = n_list(l+1);

    [ x, w ] = hermite_gk24_set ( n );

    fprintf ( 1, '\n' );

    for i = 1 : n
      fprintf ( 1, '  %2d  %24.16g  %24.16g\n', i, x(i), w(i) );
    end

  end
  return
end
