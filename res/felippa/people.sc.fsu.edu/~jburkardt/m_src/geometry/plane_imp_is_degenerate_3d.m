function value = plane_imp_is_degenerate_3d ( a, b, c )

%*****************************************************************************80
%
%% plane_imp_is_degenerate_3d() is TRUE if an implicit plane is degenerate.
%
%  Discussion:
%
%    The implicit form of a plane in 3D is:
%
%      A * X + B * Y + C * Z + D = 0
%
%    The implicit plane is degenerate if A = B = C = 0.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    09 November 2016
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real A, B, C, the implicit plane parameters.
%
%  Output:
%
%    logical VALUE, is TRUE if the plane is degenerate.
%
  if ( a == 0.0 & b == 0.0 & c == 0.0 )
    value = true;
  else
    value = false;
  end

  return
end
