function value = ellipsoid_area ( a, b, c )

%*****************************************************************************80
%
%% ellipsoid_area() returns the surface area of an ellipsoid.
%
%  Discussion:
%
%    The ellipsoid may be represented by the equation
%
%      (x/a)^2 + (y/b)^2 + (z/c)^2 = 1
%
%    with a => b => c
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
%    Ellipsoid surface area,
%    6 July 2014,
%    https://www.johndcook.com/blog/2014/07/06/ellipsoid-surface-area/
%
%  Input:
%
%    real A, B, C, the semi-axes of the ellipsoid.
%
%  Output:
%
%    real VALUE, the surface area of the ellipsoid.
%
  a = abs ( a );
  b = abs ( b );
  c = abs ( c );

  if ( a < b )
    t = a;
    a = b;
    b = t;
  end

  if ( a < c )
    t = a;
    a = c;
    c = t;
  end

  if ( b < c )
    t = b;
    b = c;
    c = t;
  end

  phi = acos ( c / a );

  if ( a^2 - c^2 == 0 )
    m = 1;
  else
    m = ( a^2 * ( b^2 - c^2 ) ) / ( b^2 * ( a^2 - c^2 ) );
  end

  temp = elliptic_inc_em ( phi, m ) * ( sin ( phi ) )^2 ...
       + elliptic_inc_fm ( phi, m ) * ( cos ( phi ) )^2;

  if ( sin ( phi ) == 0.0 )
    temp2 = 1.0;
  else
    temp2 = temp / sin ( phi );
  end

  value = 2.0 * pi * ( c^2 + a * b * temp2 );
  
  return
end

