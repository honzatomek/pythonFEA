function value = euler_mascheroni ( )

%*****************************************************************************80
%
%% euler_mascheroni() returns the value of the Euler-Mascheroni constant.
%
%  Discussion:
%
%    The Euler-Mascheroni constant is often denoted by a lower-case
%    Gamma.  Gamma is defined as
%
%      Gamma = limit ( M -> oo )
%        ( sum ( 1 <= N <= M ) 1 / N ) - log ( M )
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    21 May 2022
%
%  Author:
%
%    John Burkardt
%
%  Output:
%
%    real VALUE, the value of the Euler-Mascheroni constant.
%
  value = 0.577215664901532860606512090082402431042;

  return
end
