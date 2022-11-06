function check = gegenbauer_alpha_check ( alpha )

%*****************************************************************************80
%
%% gegenbauer_alpha_check() checks the value of ALPHA.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    23 November 2015
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real ALPHA, a parameter which is part of the definition of
%    the Gegenbauer polynomials.  It must be greater than -0.5.
%
%  Output:
%
%    integer CHECK.
%    1, ALPHA is acceptable.
%    0, ALPHA is not acceptable. 
%
  squawk = 0;

  if ( -0.5 < alpha )
    check = 1;
  else
    check = 0;
    if ( squawk )
      fprintf ( 1, '\n' );
      fprintf ( 1, 'GEGENBAUER_ALPHA_CHECK - Fatal error!\n' );
      fprintf ( 1, '  Illegal value of ALPHA.\n' );
      fprintf ( 1, '  ALPHA = %g\n', alpha );
      fprintf ( 1, '  but ALPHA must be greater than -0.5.\n' );
    end
  end

  return
end

