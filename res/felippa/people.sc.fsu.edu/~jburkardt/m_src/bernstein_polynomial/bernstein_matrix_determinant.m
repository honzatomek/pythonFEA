function value = bernstein_matrix_determinant ( n )

%*****************************************************************************80
%
%% bernstein_matrix_determinant() returns the determinant of the BERNSTEIN matrix.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    11 January 2021
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the order of the matrix.
%
%  Output:
%
%    real VALUE, the determinant.
%
  value = 1.0;
  for i = 0 : n - 1
    value = value * nchoosek ( n - 1, i );
  end

  return
end
