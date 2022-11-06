function value = ellipse_aspect_ratio ( a, b )

%*****************************************************************************80
%
%% ellipse_aspect_ratio() computes the aspect ratio of an ellipse.
%
%  Discussion:
%
%    The ellipse has major and minor semi-axes a and b.  In particular, it
%    could have the form:
%
%      (x/a)^2 + (y/b)^2 = 1
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    14 October 2022
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    John D Cook,
%    Eccentricity, Flattening, and Aspect Ratio,
%    https://www.johndcook.com/blog/2022/10/14/eccentricity-flatness-aspect/
%    Posted 14 October 2022.
%
%  Input:
%
%    real A, B, the major and minor semi-axes.
%
%  Output:
%
%    real VALUE, the aspect ratio of the ellipse.
%
  a = abs ( a );
  b = abs ( b );

  if ( a < b )
    t = a;
    a = b;
    b = t;
  end

  value = b / a;

  return
end
