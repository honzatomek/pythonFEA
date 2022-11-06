function [ n_data, order, number ] = polyomino_chiral_count_values ( n_data )

%*****************************************************************************80
%
%% polyomino_chiral_count_values() counts chiral polyominoes (allowing holes).
%
%  Discussion:
%
%    Polyominoes are connected planar shapes formed by adjoining unit squares.
%
%    The number of unit squares in a polyomino is its order.
%
%    If we do not ignore reflections, but ignore rotations when comparing then 
%    we are considering the class of "chiral" polyominoes.  In that case,
%    for instance, there are 18 chiral polyominoes of order 5.
%
%    As the order increases, the number of polyominoes grows very rapidly.
%    The list offered here goes no further than order 28, but the later
%    numbers in the list are too large to represent as 32 byte integers. 
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    18 May 2018
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Solomon Golomb,
%    Polyominoes: Puzzles, Patterns, Problems, and Packings,
%    Princeton University Press, 1996,
%    ISBN: 9780691024448
%
%  Input:
%
%    integer N_DATA.  The user sets N_DATA to 0 before the first call.  
%    Thereafter, it should simply be the value returned by the previous call.
%
%  Output:
%
%    integer N_DATA.  On each call, the routine increments N_DATA by 1, and
%    returns the corresponding data; when there is no more data, the
%    output value of N_DATA will be 0 again.
%
%    integer ORDER, the order of a polyomino.
%
%    integer NUMBER, the number of chiral polyominos of this order.
%
  n_max = 31;

  order_vec = [ ...
    0, ...
    1,  2,  3,  4,  5, ...
    6,  7,  8,  9, 10, ...
   11, 12, 13, 14, 15, ...
   16, 17, 18, 19, 20, ...
   21, 22, 23, 24, 25, ...
   26, 27, 28, 29, 30 ];

  number_vec = [  ...
    1, ...
    1, 1, 2, 7, 18, ...
    60, 196, 704, 2500, 9189, ...
    33896, 126759, 476270, 1802312, 6849777, ...
    26152418, 100203194, 385221143, 1485200848, 5741256764, ...
    22245940545, 86383382827, 336093325058, 1309998125640, 5114451441106, ...
    19998172734786, 78306011677182, 307022182222506, 1205243866707468, 4736694001644862 ];

  if ( n_data < 0 )
    n_data = 0;
  end

  n_data = n_data + 1;

  if ( n_max < n_data )
    n_data = 0;
    order = 0;
    number = 0;
  else
    order = order_vec(n_data);
    number = number_vec(n_data);
  end

  return
end
