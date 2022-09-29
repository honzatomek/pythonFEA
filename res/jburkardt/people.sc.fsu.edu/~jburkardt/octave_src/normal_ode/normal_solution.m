function value = normal_solution ( t )

%*****************************************************************************80
%
%% normal_solution: the normal PDF function.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    28 September 2020
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real T: the current time.
%
%  Output:
%
%    real Y: the value of the function at t.
%    
  value = exp ( - t.^2 / 2.0 ) / sqrt ( 2.0 * pi );

  return
end

