function f = setpart_to_rgf ( m, nsub, s, index )

%*****************************************************************************80
%
%% setpart_to_rgf() converts a set partition to a restricted growth function.
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
%    integer M, the number of elements of the set.
%    M must be positive.
%
%    integer NSUB, the number of nonempty subsets into
%    which the set is partitioned.  1 <= NSUB <= M.
%
%    integer INDEX(NSUB), lists the location in S of the
%    last element of each subset.  Thus, the elements of subset 1
%    are S(1) through S(INDEX(1)), the elements of subset 2
%    are S(INDEX(1)+1) through S(INDEX(2)) and so on.
%
%    integer S(M), contains the integers from 1 to M,
%    grouped into subsets as described by INDEX.
%
%  Output:
%
%    integer F(M), the restricted growth function from
%    M to NSUB.
%

%
%  Check.
%
  check = setpart_check ( m, nsub, s, index );

  if ( ~ check )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'SETPART_TO_RGF - Fatal error!\n' );
    fprintf ( 1, '  The input array is illegal.\n' );
    error ( 'SETPART_TO_RGF - Fatal error!' );
  end

  khi = 0;
  for i = 1 : nsub
    klo = khi + 1;
    khi = index(i);
    for k = klo : khi
      f(s(k)) = i;
    end
  end

  return
end
