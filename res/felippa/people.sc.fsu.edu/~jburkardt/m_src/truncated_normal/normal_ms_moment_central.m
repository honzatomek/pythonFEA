function value = normal_ms_moment_central ( order, mu, sigma )

%*****************************************************************************80
%
%% normal_ms_moment_central() evaluates central moments of the Normal PDF.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    30 August 2013
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer ORDER, the order of the moment.
%    0 <= ORDER.
%
%    real MU, the mean of the distribution.
%
%    real SIGMA, the standard deviation of the distribution.
%
%  Output:
%
%    real VALUE, the value of the central moment.
%
  if ( mod ( order, 2 ) == 0 )
    value = r8_factorial2 ( order - 1 ) * sigma ^ order;
  else
    value = 0.0;
  end

  return
end
