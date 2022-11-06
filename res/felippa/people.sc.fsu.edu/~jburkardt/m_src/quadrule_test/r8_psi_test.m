function r8_psi_test ( )

%*****************************************************************************80
%
%% r8_psi_test() tests r8_psi().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    19 October 2008
%
%  Author:
%
%    John Burkardt
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'R8_PSI_TEST:\n' );
  fprintf ( 1, '  R8_PSI evaluates the Psi function.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '      X       Psi(X)                    Psi(X)                  DIFF\n' );
  fprintf ( 1, '             (Tabulated)               (R8_PSI)\n' );
  fprintf ( 1, '\n' );

  n_data = 0;

  while ( true )

    [ n_data, x, fx ] = psi_values ( n_data );

    if ( n_data == 0 )
      break
    end

    fx2 = r8_psi ( x );

    fprintf ( 1, '  %6.2f  %24.16e  %24.16e  %10.4e\n', x, fx, fx2, abs ( fx - fx2 ) );

  end

  return
end
