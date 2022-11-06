function p = perm_random ( n )

%*****************************************************************************80
%
%% perm_random() selects a random permutation of 1, ..., N.
%
%  Discussion:
%
%    The algorithm is known as the Fisher-Yates or Knuth shuffle.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    23 May 2015
%
%  Author:
%
%    John Burkardt.
%
%  Reference:
%
%    Albert Nijenhuis, Herbert Wilf,
%    Combinatorial Algorithms,
%    Academic Press, 1978, second edition,
%    ISBN 0-12-519260-6.
%
%  Input:
%
%    integer N, the number of objects to be permuted.
%
%  Output:
%
%    integer P(N), a permutation of ( 1, 2, ..., N ), in standard index form.
%
  p = ( 1 : n );

  for i = 1: n - 1

    j = randi ( [ i, n ], 1, 1 );

    temp = p(i);
    p(i) = p(j);
    p(j) = temp;

  end

  return
end
