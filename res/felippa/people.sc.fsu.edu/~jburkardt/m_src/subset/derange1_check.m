function check = derange1_check ( n, a )

%*****************************************************************************80
%
%% derange1_check() determines whether a permutation is a derangement.
%
%  Discussion:
%
%    A derangement of N objects is a permutation which leaves no object
%    unchanged.
%
%    A derangement of N objects is a permutation with no fixed
%    points.  If we symbolize the permutation operation by "P",
%    then for a derangment, P(I) is never equal to I.
%
%    The number of derangements of N objects is sometimes called
%    the subfactorial function, or the derangement number D(N).
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    02 June 2015
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the number of objects permuted.
%
%    integer A(N), a permutation of the integers 1 through N.
%
%  Output:
%
%    logical CHECK, is TRUE if A is a derangement, and
%    FALSE otherwise.
%

%
%  Values must be between 1 and N.
%
  for i = 1 : n
    if ( a(i) < 1 || n < a(i) )
      check = 0;
      return
    end
  end
%
%  Every value must be represented.
%
  for j = 1 : n
    check = 0;
    for i = 1 : n
      if ( a(i) == j )
        check = 1;
        break
      end
    end
    if ( ~ check )
      return
    end
  end
%
%  Values must be deranged.
%
  for i = 1 : n
    if ( a(i) == i )
      check = 0;
      return
    end
  end

  check = 1;

  return
end
