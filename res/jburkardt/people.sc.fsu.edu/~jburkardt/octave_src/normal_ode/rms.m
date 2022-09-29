function value = rms ( x )

%*****************************************************************************80
%
%% rms returns the root-mean-square norm of a vector.
%
%  Discussion:
%
%    If X is a matrix, the RMS norm of each column is returned in a row result.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    14 September 2020
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real X(N) or X(M,N): the vector or matrix.
%
%  Output:
%
%    real VALUE or VALUE(1,N): the RMS of the vector, 
%    or of each column of the matrix.
%
  [ m, n ] = size ( x );

  if ( m == 1 )
    x = x';
    [ m, n ] = size ( x );
  end

  value = sqrt ( sum ( x.^2 ) / m );

  return
end

