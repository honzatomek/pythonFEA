function pyramid_rule_test ( )

%*****************************************************************************80
%
%% pyramid_rule_test() tests pyramid_rule().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    04 March 2019
%
%  Author:
%
%    John Burkardt
%
  addpath ( '../pyramid_rule' )

  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'pyramid_rule_test():\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '  Test pyramid_rule().\n' );

  legendre_order = 4;
  jacobi_order = 3;
  filename = 'pyramid_l4_j3';
  pyramid_rule ( legendre_order, jacobi_order, filename );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'pyramid_rule_test():\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  rmpath ( '../pyramid_rule' )

  return
end
function timestamp ( )

%*****************************************************************************80
%
%% timestamp() prints the current YMDHMS date as a timestamp.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    14 February 2003
%
%  Author:
%
%    John Burkardt
%
  t = now;
  c = datevec ( t );
  s = datestr ( c, 0 );
  fprintf ( 1, '%s\n', s );

  return
end

