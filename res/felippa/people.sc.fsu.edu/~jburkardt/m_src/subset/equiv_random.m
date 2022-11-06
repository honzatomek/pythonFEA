function [ npart, a ] = equiv_random ( n )

%*****************************************************************************80
%
%% equiv_random() selects a random partition of a set.
%
%  Discussion:
%
%    The user does not control the number of parts in the partition.
%
%    The equivalence classes are numbered in no particular order.
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
%    integer N, the number of elements in the set to be partitioned.
%
%  Output:
%
%    integer NPART, the number of classes or parts in the 
%    partition.  NPART will be between 1 and N.
%
%    integer A(N), indicates the class to which each element
%    is assigned.
%
  b = zeros ( n, 1 );

  b(1) = 1.0;

  for l = 1 : n - 1

    sum1 = 1.0 / l;
    for k = 1 : l - 1
      sum1 = ( sum1 + b(k) ) / ( l - k );
    end

    b(l+1) = ( sum1 + b(l) ) / ( l + 1 );

  end

  a = zeros ( n, 1 );

  m = n;
  npart = 0;

  while ( true )

    z = rand ( 1, 1 );
    z = m * b(m) * z;
    k = 0;
    npart = npart + 1;

    while ( 0.0 <= z )

      a(m) = npart;
      m = m - 1;

      if ( m == 0 )
        break
      end

      z = z - b(m);
      k = k + 1;
      z = z * k;

    end

    if ( m == 0 )
      break
    end

  end
%
%  Randomly permute the assignments.
%
  for i = 1 : n - 1
    j = randi ( [ i, n ], 1 );
    t    = a(i);
    a(i) = a(j);
    a(j) = t;
  end

  return
end
