function [ a, done, active, dir, change ] = vec_gray_next ( n, base, ...
  a, done, active, dir )

%*****************************************************************************80
%
%% vec_gray_next() computes the elements of a product space.
%
%  Discussion:
%
%    The elements are produced one at a time.
%
%    This routine handles the case where the number of degrees of freedom may
%    differ from one component to the next.
%
%    A method similar to the Gray code is used, so that successive
%    elements returned by this routine differ by only a single element.
%
%    A previous version of this routine used internal static memory.
%
%  Example:
%
%    N = 2, BASE = ( 2, 3 ), DONE = TRUE
%
%     A    DONE  CHANGE
%    ---  -----  ------
%    0 0  FALSE    1
%    0 1  FALSE    2
%    0 2  FALSE    2
%    1 2  FALSE    1
%    1 1  FALSE    2
%    1 0  FALSE    2
%    1 0   TRUE   -1  
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    16 May 2015
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Dennis Stanton, Dennis White,
%    Constructive Combinatorics,
%    Springer, 1986,
%    ISBN: 0387963472,
%    LC: QA164.S79.
%
%  Input:
%
%    integer N, the number of components.
%
%    integer BASE(N), contains the number of degrees of
%    freedom of each component.  The output values of A will
%    satisfy 0 <= A(I) < BASE(I).
%
%    integer A(N).  On the first call, the input value of A doesn't 
%    matter.  Thereafter, it should be the same as
%    its output value from the previous call. 
%
%    logical DONE.  On the first call, the user must
%    set DONE to TRUE.  Thereafter, the input value DONE should simply
%    be the output on the previous call. 
%
%    integer ACTIVE(N), DIR(N), work arrays needed by the function.  
%    The user should create them before the first call, but 
%    thereafter should not change their values, passing in the output values 
%    of the previous call for the next call.
%
%  Output:
%
%    integer A(N), the next vector. 
%
%    logical DONE.  If DONE is FALSE, then the program has computed another 
%    entry in A.  If the output value of DONE is TRUE, then there are no 
%    more entries.
%
%    integer ACTIVE(N), DIR(N), work arrays needed by the function.
%
%    integer CHANGE, the index of the element whose
%    value was changed.  On return from the first call, CHANGE
%    is 1, even though all the elements have been "changed".  On
%    return with DONE equal to TRUE, CHANGE is -1.
%

%
%  The user is calling for the first time.
%
  if ( done )

    done = false;
    a(1:n) = 0;

    dir(1:n) = 1;
    active(1:n) = 1;

    for i = 1 : n

      if ( base(i) < 1 )
        fprintf ( 1, '\n' );
        fprintf ( 1, 'VEC_GRAY_NEXT - Warning!\n' );
        fprintf ( 1, '  For index I = %d\n', i );
        fprintf ( 1, '  the nonpositive value of BASE(I) = %d\n',  base(i) );
        fprintf ( 1, '  which was reset to 1!\n' );
        base(i) = 1;
        active(i) = 0;
      elseif ( base(i) == 1 )
        active(i) = 0;
      end

    end

    change = 1;

    return

  end
%
%  Find the maximum active index.
%
  change = -1;

  for i = 1 : n
    if ( active(i) ~= 0 )
      change = i;
    end
  end
%
%  If there are NO active indices, we have generated all vectors.
%
  if ( change == -1 )
    done = true;
    return
  end
%
%  Increment the element with maximum active index.
%
  a(change) = a(change) + dir(change);
%
%  If we attained a minimum or maximum value, reverse the direction
%  vector, and deactivate the index.
%
  if ( a(change) == 0 || a(change) == base(change) - 1 )
    dir(change) = -dir(change);
    active(change) = 0;
  end
%
%  Activate all subsequent indices.
%
  for i = change + 1 : n
    if ( 1 < base(i) ) 
      active(i) = 1;
    end
  end

  return
end
