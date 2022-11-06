function rank = rgf_rank ( m, f )

%*****************************************************************************80
%
%% rgf_rank() ranks a restricted growth function.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    26 January 2011
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
%    integer M, the domain of the RGF is the integers
%    from 1 to M.  M must be positive.
%
%    integer F(M), the restricted growth function.
%
%  Output:
%
%    integer RANK, the rank of the restricted growth
%    function.
%

%
%  Check.
%
  check = rgf_check ( m, f );

  if ( ~ check )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'RGF_RANK - Fatal error!\n' );
    fprintf ( 1, '  The input array is illegal!\n' );
    error ( 'RGF_RANK - Fatal error!' );
  end
%
%  Get the generalized restricted growth function table.
%
  offset = 1;
  d = rgf_g_table ( m );

  rank = 0;
  j = 1;
  for i = 2 : m
    rank = rank + ( f(i) - 1 ) * d(m-i+offset,j+offset);
    j = max ( j, f(i) );
  end

  return
end
