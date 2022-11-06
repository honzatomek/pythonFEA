function ntab = tableau_enum ( n )

%*****************************************************************************80
%
%% tableau_enum() enumerates the 2 by N standard tableaus.
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
%    integer N, the number of columns in the tableau.
%    N must be nonnegative.
%
%  Output:
%
%    integer NTAB, the number of 2 by N standard tableaus.
%
  ntab = nchoosek ( 2 * n, n ) / ( n + 1 );

  return
end
