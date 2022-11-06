function a = ksub_random2 ( n, k )

%*****************************************************************************80
%
%% ksub_random2() selects a random subset of size K from a set of size N.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    13 December 2003
%
%  Author:
%
%    FORTRAN77 original version by Albert Nijenhuis, Herbert Wilf.
%    MATLAB version by John Burkardt.
%
%  Reference:
%
%    A Nijenhuis, H Wilf,
%    Combinatorial Algorithms,
%    Academic Press, 1978, second edition,
%    ISBN 0-12-519260-6.
%
%  Input:
%
%    integer N, the size of the set.
%
%    integer K, the size of the subset, between 0 and N.
%
%  Output:
%
%    integer A(K), the indices of the selected elements.
%
  if ( k < 0 | n < k )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'KSUB_RANDOM2 - Fatal error!\n' );
    fprintf ( 1, '  N = %d\n', n );
    fprintf ( 1, '  K = %d\n', k );
    fprintf ( 1, '  but 0 <= K <= N is required!\n' );
    error ( 'KSUB_RANDOM2 - Fatal error: Illegal K.' );
  end

  if ( k == 0 )
    a = [];
    return;
  end

  need = k;
  have = 0;

  available = n;
  candidate = 0;

  while ( true )

    candidate = candidate + 1;

    r = rand ( 1, 1 );

    if ( available * r <= need )

      need = need - 1;
      have = have + 1;
      a(have) = candidate;

      if ( need <= 0 )
        break;
      end

    end

    available = available - 1;

  end

  return
end
