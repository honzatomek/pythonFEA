function ref = reference_tet4_sample ( n )

%*****************************************************************************80
%
%% reference_tet4_sample(): sample points in the reference tetrahedron.
%
%  Discussion:
%
%    This sampling method is not uniform.  The algorithm is simple.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    10 August 2009
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the number of points.
%
%  Output:
%
%    real REF(3,N), points in the reference tetrahedron.
%
  for j = 1 : n
    c = rand ( 4, 1 );
    c_sum = sum ( c(1:4) );
    ref(1:3,j) = c(1:3) / c_sum;
  end

  return
end
