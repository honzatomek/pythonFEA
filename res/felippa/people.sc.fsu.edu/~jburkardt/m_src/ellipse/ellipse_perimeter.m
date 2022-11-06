function value = ellipse_perimeter ( a, b )

%*****************************************************************************80
%
%% ellipse_perimeter() computes the perimeter of an ellipse.
%
%  Discussion:
%
%    The ellipse has major and minor semi-axes a and b.  In particular, it
%    could have the form:
%
%      (x/a)^2 + (y/b)^2 = 1
%
%    Computing the exact value requires evaluating the complete elliptic
%    integral of the second kind.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    24 March 2021
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    John D Cook,
%    Simple approximation of the perimeter of an ellipse,
%    24 March 2021
%    https://www.johndcook.com/blog/2021/03/24/perimeter-of-an-ellipse/
%
%  Input:
%
%    real A, B: the major and minor semi-axes.
%
%  Output:
%
%    real VALUE: the length of the perimeter.
%
  a = abs ( a );
  b = abs ( b );

  if ( a < b )
    t = a;
    a = b;
    b = t;
  end

  ecc_sq = 1.0 - ( b / a )^2;

  [ K, E ] = ellipke ( ecc_sq );

  value = 4.0 * a * E;

  return
end
