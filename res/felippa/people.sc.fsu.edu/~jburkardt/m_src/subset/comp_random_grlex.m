function [ xc, rank ] = comp_random_grlex ( kc, rank1, rank2 )

%*****************************************************************************80
%
%% comp_random_grlex(): random composition with degree less than or equal to N.
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
%    John Burkardt
%
%  Input:
%
%    integer KC, the number of parts in the composition.
%
%    integer RANK1, RANK2, the minimum and maximum ranks.
%    1 <= RANK1 <= RANK2.
%
%  Output:
%
%    integer XC(KC), the random composition.
%
%    integer RANK, the rank of the composition.
%

%
%  Ensure that 1 <= KC.
%
  if ( kc < 1 )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'COMP_RANDOM_GRLEX - Fatal error!\n' );
    fprintf ( 1, '  KC < 1\n' );
    error ( 'COMP_RANDOM_GRLEX - Fatal error!' );
  end
%
%  Ensure that 1 <= RANK1.
%
  if ( rank1 < 1 )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'COMP_RANDOM_GRLEX - Fatal error!\n' );
    fprintf ( 1, '  RANK1 < 1\n' );
    error ( 'COMP_RANDOM_GRLEX - Fatal error!' );
  end
%
%  Ensure that RANK1 <= RANK2.
%
  if ( rank2 < rank1 )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'COMP_RANDOM_GRLEX - Fatal error!\n' );
    fprintf ( 1, '  RANK2 < RANK1\n' );
    error ( 'COMP_RANDOM_GRLEX - Fatal error!' );
  end
%
%  Choose RANK between RANK1 and RANK2.
%
  rank = randi ( [ rank1, rank2 ], 1 );
%
%  Recover the composition of given RANK.
%
  xc = comp_unrank_grlex ( kc, rank );

  return
end
