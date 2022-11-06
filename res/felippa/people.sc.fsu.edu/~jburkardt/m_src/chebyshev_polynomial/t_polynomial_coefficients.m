function c = t_polynomial_coefficients ( d )

%*****************************************************************************80
%
%% t_polynomial_coefficients(): coefficients of the d-th Chebyshev polynomial.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    19 November 2019
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Milton Abramowitz, Irene Stegun,
%    Handbook of Mathematical Functions,
%    US Department of Commerce, 1964.
%
%  Input:
%
%    integer d: the degree.
%
%  Output:
%
%    real c(1:d+1): the coefficients.
%
  ckm1 = [];
  c = [];

  for k = 0 : d

    ckm2 = ckm1;
    ckm1 = c;

    if ( k == 0 )
      c = [ 1.0 ];
    elseif ( k == 1 )
      c = [ 0.0; 1.0 ];
    else
      c = zeros ( k + 1, 1 );
      c(2:k+1) = 2.0 * ckm1(1:k);
      c(1:k-1) = c(1:k-1) - ckm2(1:k-1);
    end

  end

  return
end


