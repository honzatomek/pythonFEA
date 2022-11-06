function a = r8mat_perm1 ( n, a, p )

%*****************************************************************************80
%
%% r8mat_perm1() permutes the rows and columns of a square R8MAT.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    27 June 2004
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
%    integer N, the order of the matrix.
%
%    real A(N,N), the matrix to be permuted.
%
%    integer P(N), a permutation to be applied to the rows
%    and columns.  P(I) is the new number of row and column I.
%
%  Output:
%
%    real A(N,N), the permuted matrix.
%
  [ p, is, nc ] = perm1_cycle ( n, 1, p );

  for i = 1 : n

    i1 = -p(i);

    if ( 0 < i1 )

      lc = 0;

      while ( true )

        i1 = p(i1);
        lc = lc + 1;

        if ( i1 <= 0 )
          break;
        end

      end

      i1 = i;

      for j = 1 : n

        if ( p(j) <= 0 )

          j2 = j;
          k = lc;

          while ( true )

            j1 = j2;
            it = a(i1,j1);

            while ( true )

              i1 = abs ( p(i1) );
              j1 = abs ( p(j1) );

              temp     = a(i1,j1);
              a(i1,j1) = it;
              it       = temp;

              if ( j1 ~= j2 )
                continue;
              end

              k = k - 1;

              if ( i1 == i )
                break
              end

            end

            j2 = abs ( p(j2) );

            if ( k == 0 )
              break
            end

          end
        end
      end
    end
  end

  return
end
