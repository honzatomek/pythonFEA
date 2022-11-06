function a = compnz_random ( n, k )

%*****************************************************************************80
%
%% compnz_random() selects a random composition of the integer N into K nonzero parts.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    01 July 2021
%
%  Author:
%
%    John Burkardt
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
%    integer N, the integer to be decomposed.
%
%    integer K, the number of parts in the composition.
%    K must be no greater than N.
%
%  Output:
%
%    integer A(K), the parts of the composition.
%
  if ( n < k )
    a(1:k) = -1;
    return
  end

  if ( 1 < k )
    a = ksub_random ( n-1, k-1 );
  end

  a(k) = n;
  l = 0;

  for i = 1 : k
    m = a(i);
    a(i) = a(i) - l - 1;
    l = m;
  end

  a(1:k) = a(1:k) + 1;

  return
end
