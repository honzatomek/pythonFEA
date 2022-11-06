function [ blocks, goal ] = count_pose_random ( )

%*****************************************************************************80
%
%% count_pose_random() poses a problem for the game "The Count is Good"
%
%  Discussion:
%
%    The French television show "The Count is Good" has a game that goes
%    as follows:
%
%      A number is chosen at random between 100 and 999.  This is the GOAL.
%
%      Six numbers are randomly chosen from the set 1, 2, 3, 4, 5, 6, 7, 8,
%      9, 10, 25, 50, 75, 100.  These numbers are the BLOCKS.
%
%      The player must construct a formula, using some or all of the blocks,
%      (but not more than once), and the operations of addition, subtraction,
%      multiplication and division.  Parentheses should be used to remove
%      all ambiguity.  However, it is forbidden to use subtraction in a
%      way that produces a negative result, and all division must come out
%      exactly, with no remainder.
%
%    This routine poses a sample problem from the show.  The point is,
%    to determine how to write a program that can solve such a problem.
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
%  Reference:
%
%    Raymond Seroul,
%    Programming for Mathematicians,
%    Springer Verlag, 2000, page 355-357.
%
%  Output:
%
%    integer BLOCKS(6), the six numbers available for the formula.
%
%    integer GOAL, the goal number.
%
  stuff = [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 25, 50, 75, 100 ];

  goal = randi ( [ 100, 999 ], 1 );

  ind = ksub_random ( 14, 6 );

  blocks(1:6) = stuff(ind(1:6));

  return
end
