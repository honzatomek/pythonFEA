function annulus_rule_plot ( center, r1, r2, nr, nt, w, x, y, filename, label )

%*****************************************************************************80
%
%% annulus_rule_plot() plots a quadrature rule for an annulus.
%
%  Integration region:
%
%    Points (X,Y) such that
%
%      R1^2 <= ( X - CENTER(1) )^2 + ( Y - CENTER(2) )^2 <= R2^2
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    06 July 2018
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    William Peirce,
%    Numerical Integration Over the Planar Annulus,
%    Journal of the Society for Industrial and Applied Mathematics,
%    Volume 5, Issue 2, June 1957, pages 66-73.
%
%  Input:
%
%    real CENTER(2), the center of the annulus.
%
%    real R1, R2, the inner and outer radius.
%
%    integer NR, the number of points in the radial rule.
%
%    integer NT, the number of angles to use.
%
%    real W(NR*NT), the weights for the rule.
%    (W is not actually needed by this function.)
%
%    real X(NR*NT), Y(NR*NT), the points for the rule.
%
%    string FILENAME, a name for the PNG graphics file
%    to be created.
%
%    string LABEL, a title for the plot.
%
  np = 33;
  x1 = zeros ( np );
  y1 = zeros ( np );
  x2 = zeros ( np );
  y2 = zeros ( np );

  for i = 1 : np
    theta = ( i - 1 ) * 2.0 * pi / ( np - 1 );
    x1(i) = center(1) + r1 * cos ( theta );
    y1(i) = center(2) + r1 * sin ( theta );
    x2(i) = center(1) + r2 * cos ( theta );
    y2(i) = center(2) + r2 * sin ( theta );
  end

  clf ( );
  hold ( 'on' );
  plot ( x1, y1, 'k-', 'Linewidth', 2 );
  plot ( x2, y2, 'k-', 'Linewidth', 2 );
  plot ( x, y, 'r.', 'Markersize', 20 );
  grid ( 'on' );
  xlabel ( '<--- X --->' );
  ylabel ( '<--- Y --->' );
  title ( label );
  axis ( 'equal' );
  hold ( 'off' );

  print ( '-dpng', filename );
  fprintf ( 1, '  Graphics saved as "%s"\n', filename );

  return
end
