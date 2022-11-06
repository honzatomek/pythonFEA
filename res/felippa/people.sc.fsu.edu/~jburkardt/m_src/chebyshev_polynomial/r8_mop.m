function value = r8_mop ( i )

%*****************************************************************************80
%
%% r8_mop() returns the I-th power of -1 as an R8 value.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    07 November 2007
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer I, the power of -1.
%
%  Output:
%
%    real VALUE, the I-th power of -1.
%
  if ( mod ( i, 2 ) == 0 )
    value = + 1.0;
  else
    value = - 1.0;
  end

  return
end
