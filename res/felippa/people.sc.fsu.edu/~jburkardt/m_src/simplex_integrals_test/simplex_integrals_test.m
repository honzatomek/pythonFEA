function simplex_integrals_test ( )

%*****************************************************************************80
%
%% simplex_integrals_test() tests simplex_integrals().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    15 March 2019
%
%  Author:
%
%    John Burkardt
%
  addpath ( '../simplex_integrals' );

  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'simplex_integrals_test():\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '  Test simplex_integrals().\n' );

  simplex_integrals_test01 ( );
  simplex_integrals_test02 ( );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'simplex_integrals_test():\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  rmpath ( '../simplex_integrals' );

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

