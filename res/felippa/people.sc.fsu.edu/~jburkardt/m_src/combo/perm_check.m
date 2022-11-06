function check = perm_check ( n, p )

%*****************************************************************************80
%
%% perm_check() checks that a vector represents a permutation.
%
%  Discussion:
%
%    The routine verifies that each of the integers from 1
%    to N occurs among the N entries of the permutation.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    02 January 2004
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the number of entries.
%
%    integer P(N), the array to check.
%
%  Output:
%
%    integer CHECK.
%    1, the data is legal.
%    0, the data is not legal.
%
  check = 1;

  for seek = 1 : n

    check = 0;

    for find = 1 : n
      if ( p(find) == seek )
        check = 1;
        break;
      end
    end

    if ( ~ check )
      return;
    end

  end

  return
end
