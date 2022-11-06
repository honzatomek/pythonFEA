function det = r8mat_det_2d ( a )

%*****************************************************************************80
%
%% r8mat_det_2d() computes the determinant of a 2 by 2 matrix.
%
%  Discussion:
%
%    The determinant is the area spanned by the vectors making up the rows
%    or columns of the matrix.
%
%    value = A(1,1) * A(2,2) - A(1,2) * A(2,1).
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    31 January 2005
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real A(2,2), the matrix whose determinant is desired.
%
%  Output:
%
%    real DET, the determinant of the matrix.
%
  det = a(1,1) * a(2,2) - a(1,2) * a(2,1);

  return
end
