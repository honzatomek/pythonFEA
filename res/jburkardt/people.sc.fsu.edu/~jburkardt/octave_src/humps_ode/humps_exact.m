function y = humps_exact ( x )

%*****************************************************************************80
%
%% humps_exact evaluates the humps function.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    15 June 2019
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real x(): the evaluation points.
%
%  Output:
%
%    real y(): the function values.
%
  y = 1.0 ./ ( ( x - 0.3 ).^2 + 0.01 ) ...
    + 1.0 ./ ( ( x - 0.9 ).^2 + 0.04 ) ...
    - 6.0;

  return
end

