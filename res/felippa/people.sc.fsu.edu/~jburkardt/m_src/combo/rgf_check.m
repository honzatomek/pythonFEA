function check = rgf_check ( m, f )

%*****************************************************************************80
%
%% rgf_check() checks a restricted growth function.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    26 January 2011
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Donald Kreher, Douglas Simpson,
%    Combinatorial Algorithms,
%    CRC Press, 1998,
%    ISBN: 0-8493-3988-X,
%    LC: QA164.K73.
%
%  Input:
%
%    integer M, the domain of the RGF is the integers
%    from 1 to M.  M must be positive.
%
%    integer F(M), the restricted growth function.
%
%  Output:
%
%    integer CHECK.
%    1, the data is legal.
%    0, the data is not legal.
%
  check = 1;

  if ( m <= 0 )
    check = 0;
    return
  end

  fmax = 0;
  for i = 1 : m
    if ( f(i) <= 0 | fmax + 1 < f(i) )
      check = 0;
      return
    end
    fmax = max ( fmax, f(i) );
  end

  return
end
