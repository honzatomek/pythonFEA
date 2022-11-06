function [ nc, kc, ac ] = ksub_to_compnz ( ns, ks, as )

%*****************************************************************************80
%
%% ksub_to_compnz() converts a K-subset to a nonzero composition.
%
%  Discussion:
%
%    There is a bijection between K subsets and nonzero compositions.
%
%    Let AS be a KS subset of a set of the integers 1 through NS.
%
%    Then let 
%      NC = NS + 1
%      KC = KS + 1, 
%    and define
%      AC(1) = AS(1);
%      AC(2:KC-1) = AS(2:KC-1) - AS(1:KC-2);
%      AC(KC) = NC - AS(KS).
%
%    Then AC is a composition of NC into KC parts.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    24 May 2015
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer NS, the size of the set.
%
%    integer KS, the size of the subset.
%
%    integer AS(KS), the entries of the K-subset, in increasing order.
%
%  Output:
%
%    integer NC, the composition sum.
%
%    integer KC, the number of parts of the composition.
%
%    integer AC(KC), the parts of the composition.
%
  nc = ns + 1;
  kc = ks + 1;
  ac = zeros ( kc, 1 );

  ac(1) = as(1);
  for i = 2 : kc - 1;
    ac(i) = as(i) - as(i-1);
  end
  ac(kc) = nc - as(ks);

  return
end


