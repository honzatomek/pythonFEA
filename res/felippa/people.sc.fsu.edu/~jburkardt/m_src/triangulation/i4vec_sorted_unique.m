function [ n_unique, a_unique ] = i4vec_sorted_unique ( n, a )

%*****************************************************************************80
%
%% i4vec_sorted_unique() finds the unique elements in a sorted I4VEC.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    29 April 2004
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the number of elements in A.
%
%    integer A(N), the sorted integer array.
%
%  Output:
%
%    integer N_UNIQUE, the number of unique elements in A.
%
%    integer A_UNIQUE[N_UNIQUE], the unique elements.
%
  n_unique = 0;
  a_unique = [];

  if ( n <= 0 )
    return;
  end

  n_unique = 1;
  a_unique = [ a_unique a(1) ];

  for i = 2 : n

    if ( a(i) ~= a_unique(n_unique) )
      n_unique = n_unique + 1;
      a_unique = [ a_unique a(i) ];
    end

  end

  return
end

