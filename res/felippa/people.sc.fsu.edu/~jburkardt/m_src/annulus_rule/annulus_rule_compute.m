function [ w, x, y ] = annulus_rule_compute ( center, r1, r2, nr, nt )

%*****************************************************************************80
%
%% annulus_rule_compute() computes a quadrature rule for an annulus.
%
%  Discussion:
%
%    The integration region is points (X,Y) such that
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
%    The value NT=4*NR is recommended.
%
%  Output:
%
%    real W(NR*NT), the weights for the rule.
%
%    real X(NR*NT), Y(NR*NT), the points for the rule.
%

%
%
%  Get the Legendre rule for [-1,+1].
%
  [ ra, rw ] = legendre_ek_compute ( nr );
%
%  Adjust the rule from [-1,+1] to [r1^2,r2^2].
%
  a = -1.0;
  b = +1.0;
  c = r1 ^ 2;
  d = r2 ^ 2;
  [ ra, rw ] = rule_adjust ( a, b, c, d, nr, ra, rw );
%
%  Convert from R^2 to R.
%
  ra(1:nr) = sqrt ( ra(1:nr) );
  rw(1:nr) = rw(1:nr) / ( r2 + r1 ) / ( r2 - r1 );
%
%  Set the angular weight.
%
  tw = 1.0 / nt;
%
%  Get area of annulus.
%
  area = annulus_area ( center, r1, r2 );
%
%  Form the abscissa and weight vectors.
%
  x = zeros ( nr * nt, 1 );
  y = zeros ( nr * nt, 1 );
  w = zeros ( nr * nt, 1 );

  k = 0;
  for i = 0 : nt - 1
    t = 2.0 * pi * i / nt;
    for j = 1 : nr
      k = k + 1;
      x(k) = center(1) + ra(j) * cos ( t );
      y(k) = center(2) + ra(j) * sin ( t );
      w(k) = area * tw * rw(j);
    end
  end

  return
end

