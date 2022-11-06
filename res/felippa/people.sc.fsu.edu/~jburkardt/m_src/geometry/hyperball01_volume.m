function volume = hyperball01_volume ( m )

%*****************************************************************************80
%
%% hyperball01_volume() returns the volume of the unit hyperball in M dimensions.
%
%  Discussion:
%
%     M  Volume
%
%     1    2
%     2    1        * PI
%     3  ( 4 /   3) * PI
%     4  ( 1 /   2) * PI^2
%     5  ( 8 /  15) * PI^2
%     6  ( 1 /   6) * PI^3
%     7  (16 / 105) * PI^3
%     8  ( 1 /  24) * PI^4
%     9  (32 / 945) * PI^4
%    10  ( 1 / 120) * PI^5
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    08 April 2019
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer M, the spatial dimension.
%
%  Output:
%
%    real VOLUME, the volume of the unit ball.
%
  volume = ( 2.0 * gamma ( 3.0 / 2.0 ) ) ^ m  / gamma ( ( m + 2.0 ) / 2.0 );

  return
end

