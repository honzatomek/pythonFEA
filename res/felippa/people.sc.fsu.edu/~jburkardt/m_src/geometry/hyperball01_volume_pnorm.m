function volume = hyperball01_volume_pnorm ( m, p )

%*****************************************************************************80
%
%% hyperball01_volume_pnorm(): p-norm volume of unit M-dimensional hyperball. 
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    08 April 2019
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    John D Cook,
%    Ratio of Lebesgue norm ball volumes,
%    https://www.johndcook.com/blog/
%    07 April 2019.
%
%  Input:
%
%    integer M, the spatial dimension.
%    1 <= M.
%
%    real P, chooses the P-norm.
%    0 < P <= Inf.
%    P = 2 is the Euclidean norm.
%
%  Output:
%
%    real VOLUME, the volume.
%
  if ( m < 1 )
    volume = 0.0;
    return;
  end

  if ( p <= 0.0 )
    volume = 0.0;
    return;
  end

  if ( p == Inf )
    volume = 2.0 ^ m;
  else
    volume = ( 2.0 * gamma ( ( p + 1.0 ) / p ) ) ^ m / gamma ( ( p + m ) / p );
  end

  return
end

