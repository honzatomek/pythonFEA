function a = npart_rsf_lex_random ( n, npart )

%*****************************************************************************80
%
%% npart_rsf_lex_random() returns a random RSF NPART partition.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    25 January 2011
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the integer to be partitioned.
%    N must be positive.
%
%    integer NPART, the number of parts of the partition.
%    1 <= NPART <= N.
%
%  Output:
%
%    integer A(NPART), contains the partition.
%    A(1) through A(NPART) contain the nonzero integers which
%    sum to N.
%
  npartitions = npart_enum ( n, npart );

  rank = randi ( [ 1, npartitions ], 1, 1 );

  a = npart_rsf_lex_unrank ( rank, n, npart );

  return
end
