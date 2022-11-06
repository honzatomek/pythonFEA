function value = domino_tiling_num ( m, n )

%*****************************************************************************80
%
%% domino_tiling_num() counts tilings of an MxN rectangle by dominoes.
%
%  Discussion:
%
%    An 8x8 chessboard has 12,988,816 such tilings.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    19 June 2018
%
%  Author:
%
%    Original Python version by John D Cook.
%    MATLAB version by John Burkardt.
%
%  Input:
%
%    integer M, N, the number of rows and columns.
%
%  Output:
%
%    integer VALUE, the number of tilings.
%
  value = 1.0;

  for k = 1 : m
    for l = 1 : n
      value = value * 2 * (       cos ( pi * k / ( m + 1 ) ) ...
                            + i * cos ( pi * l / ( n + 1 ) ) );
    end
  end

  value = round ( sqrt ( abs ( value ) ) );

  return
end
