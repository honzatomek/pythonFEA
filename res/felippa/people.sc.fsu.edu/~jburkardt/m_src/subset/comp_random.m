function a = comp_random ( n, k )

%*****************************************************************************80
%
%% comp_random() selects a random composition of the integer N into K parts.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    01 Julyl 2021
%
%  Author:
%
%    Original FORTRAN77 version by Albert Nijenhuis and Herbert Wilf
%    MATLAB version by John Burkardt
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
%
%  Output:
%
%    integer A(K), the parts of the composition.
%
  b = ksub_random2 ( n + k - 1, k - 1 );

  a = zeros ( k, 1 );

  a(1:k-1) = b(1:k-1);
  a(k) = n + k;

  l = 0;

  for i = 1 : k
    m = a(i);
    a(i) = a(i) - l - 1;
    l = m;
  end

  return
end
