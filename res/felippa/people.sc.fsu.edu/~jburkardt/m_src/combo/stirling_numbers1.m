function s = stirling_numbers1 ( m, n )

%*****************************************************************************80
%
%% stirling_numbers1() computes Stirling numbers of the first kind.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    28 July 2011
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Donald Kreher, Douglas Simpson,
%    Combinatorial Algorithms,
%    CRC Press, 1998,
%    ISBN: 0-8493-3988-X,
%    LC: QA164.K73.
%
%  Input:
%
%    integer M, the maximum row to compute.
%    M must be nonnegative.
%
%    integer N, the maximum column to compute.
%    N must be nonnegative.
%
%  Output:
%
%    integer S(1:M+1,1:N+1), the first M+1 rows and N+1 columns
%    of the table of Stirling numbers of the first kind.
%
  s = zeros ( m + 1, n + 1 );

  s(1,1) = 1;

  for i = 1 : m
    for j = 1 : n
      if ( j <= i )
        s(i+1,j+1) = s(i,j) - ( i - 1 ) * s(i,j+1);
      end
    end
  end

  return
end
