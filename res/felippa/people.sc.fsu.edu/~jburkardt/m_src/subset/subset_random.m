function a = subset_random ( n )

%*****************************************************************************80
%
%% subset_random() selects a random subset of an N-set.
%
%  Example:
%
%    N = 4
%
%    0 0 1 1
%    0 1 0 1
%    1 1 0 1
%    0 0 1 0
%    0 0 0 1
%    1 1 0 0
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
%    Original FORTRAN77 version by Albert Nijenhuis, Herbert Wilf.
%    MATLAB version by John Burkardt.
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
%    integer N, the size of the full set.
%
%  Output:
%
%    integer A(N).  A vector to hold the information about
%    the set chosen.  On return, if A(I) = 1, then
%    I is in the random subset, otherwise, A(I) = 0
%    and I is not in the random subset.
%
  a = randi ( [ 0, 1 ], n );

  return
end
