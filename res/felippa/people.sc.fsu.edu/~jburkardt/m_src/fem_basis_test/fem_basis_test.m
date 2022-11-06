function fem_basis_test ( )

%*****************************************************************************80
%
%% fem_basis_test() tests fem_basis().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    16 January 2019
%
%  Author:
%
%    John Burkardt
%
  addpath ( '../fem_basis' );

  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'fem_basis_test():\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '  Test fem_basis().\n' );

  fem_basis_test01 ( );
  fem_basis_test02 ( );
  fem_basis_test03 ( );
%
%  Repeat the tests, now using FEM_BASIS_MD.
%
  fem_basis_test04 ( );
  fem_basis_test05 ( );
  fem_basis_test06 ( );
%
%  Tests for triangular prism.
%
  fem_basis_test07 ( );
  fem_basis_test08 ( );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'fem_basis_test():\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  rmpath ( '../fem_basis' );

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

