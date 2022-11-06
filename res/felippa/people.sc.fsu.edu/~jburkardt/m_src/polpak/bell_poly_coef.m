function c = bell_poly_coef ( n )

%*****************************************************************************80
%
%% bell_poly_coef(): Coefficients of a Bell polynomial.
%
%  First terms:
%
%    N    0    1    2    3    4    5    6    7    8
%
%    0    1
%    1    0    1    
%    2    0    1    1
%    3    0    1    3    1
%    4    0    1    7    6    1
%    5    0    1   15   25   10    1
%    6    0    1   31   90   65   15    1
%    7    0    1   63  301  350  140   21    1
%    8    0    1  127  966 1701 1050  266   28    1
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    03 March 2018
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the order of the polynomial.
%
%  Output:
%
%    integer C(1:N+1), the coefficients.
%
  c(1) = 1;
  c(2:n+1) = 0;
 
  for i = 1 : n
    for j = i : -1 : 1
      c(j+1) = j * c(j+1) + c(j);
    end
    c(1) = 0;
  end
 
  return
end
