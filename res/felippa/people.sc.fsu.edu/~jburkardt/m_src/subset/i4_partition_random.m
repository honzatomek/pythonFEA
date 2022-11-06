function [ a, mult, npart ] = i4_partition_random ( n, table )

%*****************************************************************************80
%
%% i4_partition_random() selects a random partition of the integer N.
%
%  Discussion:
%
%    Note that some elements of the partition may be 0.  The partition is
%    returned as (MULT(I),I), with NPART nonzero entries in MULT, and
%
%      N = sum ( 1 <= I <= N ) MULT(I) * I.
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
%    integer N, the integer to be partitioned.
%
%    integer TABLE(N), the number of partitions of the integers 1 through N.
%    This table may be computed by I4_PARTITION_COUNT2.
%
%  Output:
%
%    integer A(N), contains in A(1:NPART) the parts of the partition.
%
%    integer MULT(N), contains in MULT(1:NPART) the multiplicity
%    of the parts.
%
%    integer NPART, the number of parts in the partition chosen,
%    that is, the number of integers I with nonzero multiplicity MULT(I).
%
  m = n;
  npart = 0;
  mult = zeros ( n, 1 );

  while ( 0 < m )

    z = rand ( 1, 1 );
    z = m * table(m) * z;
    id = 1;
    i1 = m;
    j = 0;

    while ( true )

      j = j + 1;
      i1 = i1 - id;

      if ( i1 < 0 )
        id = id + 1;
        i1 = m;
        j = 0;
        continue
      end

      if ( i1 == 0 )
        z = z - id;
        if ( 0.0 < z )
          id = id + 1;
          i1 = m;
          j = 0;
          continue
        else
          break
        end
      end

      if ( 0 < i1 )
        z = z - id * table(i1);
        if ( z <= 0.0 )
          break
        end
      end

    end

    mult(id) = mult(id) + j;
    npart = npart + j;
    m = i1;

  end
%
%  Reformulate the partition in the standard form.
%  NPART is the number of distinct parts.
%
  npart = 0;

  for i = 1 : n
    if ( mult(i) ~= 0 )
      npart = npart + 1;
      a(npart) = i;
      mult(npart) = mult(i);
    end
  end

  mult(npart+1:n) = 0;

  return
end
