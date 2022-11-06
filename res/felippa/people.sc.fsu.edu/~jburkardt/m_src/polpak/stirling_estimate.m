function value = stirling_estimate ( n )

%*****************************************************************************80
%
%% stirling_estimate() estimates log(n!) by Stirling's approximation. 
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    28 May 2022
%
%  Author:
%
%    Paul Nahin
%
%  Reference:
%
%    Paul Nahin,
%    Dueling Idiots and Other Probability Puzzlers,
%    Princeton, 2012,
%    ISBN: 978-0691155005.
%
%  Input:
%
%    integer N: the argument.
%
%  Output:
%
%    real value: Stirling's estimate for log(n!).
%
  if ( n == 0 )
    value = 0.0;
  else
    value = 0.5 * log ( 2.0 * pi * n ) + n * ( log ( n ) - 1.0 );
  end

  return
end
