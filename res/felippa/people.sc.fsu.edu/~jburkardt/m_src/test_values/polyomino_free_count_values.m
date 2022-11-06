function [ n_data, order, number ] = polyomino_free_count_values ( n_data )

%*****************************************************************************80
%
%% polyomino_free_count_values() counts free polyominoes (allowing holes).
%
%  Discussion:
%
%    Polyominoes are connected planar shapes formed by adjoining unit squares.
%
%    The number of unit squares in a polyomino is its order.
%
%    If we ignore reflections and rotations when comparing polyominoes,
%    then we are considering the class of "free" polyominoes.  In that case,
%    for instance, there are just 12 free polyominoes of order 5, the
%    so called "pentominoes".
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
%    09 April 2018
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
%    integer NUMBER, the number of free polyominos of this order.
%
  n_max = 29;

  order_vec = [ ...
    0, ...
    1,  2,  3,  4,  5, ...
    6,  7,  8,  9, 10, ...
   11, 12, 13, 14, 15, ...
   16, 17, 18, 19, 20, ...
   21, 22, 23, 24, 25, ...
   26, 27, 28 ];

  number_vec = [  ...
    1, ...   
    1,  1,  2,  5,  12, ...
    35,  108,  369,  1285,  4655, ...
    17073,  63600,  238591,  901971,  3426576, ...
    13079255,  50107909,  192622052,  742624232,  2870671950, ...
    11123060678,  43191857688,  168047007728,  654999700403,  2557227044764, ...
    9999088822075,  39153010938487,  153511100594603 ];

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
