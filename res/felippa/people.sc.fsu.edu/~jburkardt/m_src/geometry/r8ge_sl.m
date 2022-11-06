function x = r8ge_sl ( n, a_lu, pivot, b, job )

%*****************************************************************************80
%
%% r8ge_sl() solves a system factored by R8GE_FA.
%
%  Discussion:
%
%    The R8GE storage format is used for a general M by N matrix.  A storage 
%    space is made for each logical entry.  The two dimensional logical
%    array is mapped to a vector, in which storage is by columns.
%
%    R8GE_SL is a simplified version of the LINPACK routine SGESL.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    27 March 2004
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the order of the matrix.
%    N must be positive.
%
%    real A_LU(N,N), the LU factors from R8GE_FA.
%
%    integer PIVOT(N), the pivot vector from R8GE_FA.
%
%    real B(N), the right hand side vector.
%
%    integer JOB, specifies the operation.
%    0, solve A * x = b.
%    nonzero, solve A' * x = b.
%
%  Output:
%
%    real X(N), the solution vector.
%
  x(1:n) = b(1:n);
%
%  Solve A * x = b.
%
  if ( job == 0 )
%
%  Solve PL * Y = B.
%
    for k = 1 : n-1

      l = pivot(k);

      if ( l ~= k )
        t    = x(l);
        x(l) = x(k);
        x(k) = t;
      end

      x(k+1:n) = x(k+1:n) + a_lu(k+1:n,k)' * x(k);

    end
%
%  Solve U * X = Y.
%
    for k = n : -1 : 1
      x(k) = x(k) / a_lu(k,k);
      x(1:k-1) = x(1:k-1) - a_lu(1:k-1,k)' * x(k);
    end
%
%  Solve A' * X = B.
%
  else
%
%  Solve U' * Y = B.
%
    for k = 1 : n
      x(k) = ( x(k) - x(1:k-1) * a_lu(1:k-1,k) ) / a_lu(k,k);
    end
%
%  Solve ( PL )' * X = Y.
%
    for k = n-1 : -1 : 1

      x(k) = x(k) + x(k+1:n) * a_lu(k+1:n,k);

      l = pivot(k);

      if ( l ~= k )
        t    = x(l);
        x(l) = x(k);
        x(k) = t;
      end

    end

  end

  return
end
