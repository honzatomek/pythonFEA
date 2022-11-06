function w_polynomial_plot ( index, filename )

%*****************************************************************************80
%
%% w_polynomial_plot() plots Chebyshev polynomials W(n,x).
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    21 July 2015
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer INDEX(*), the orders of 1 or more Chebyshev polynomials
%    to be plotted together.
%
%    string FILENAME, the name into which the graphics information is
%    to be stored.  Note that the PNG format will be used.
%
  a = -1.0;
  b = +1.0;
  m = 501;
  x = linspace ( a, b, m );
  x = x';
  index_num = length ( index );

  clf ( );
  hold ( 'on' );
  for i = 1 : index_num
    n = index(i);
    y = w_polynomial ( m, n, x );
    plot ( x, y(:,n+1), 'LineWidth', 2 );
  end
  grid on
  xlabel ( '<--- X --->' )
  ylabel ( '<--- W(n,x) --->' )
  title ( 'Chebyshev polynomials W(n,x)' )
  hold off
  print ( '-dpng', filename )

  return
end
