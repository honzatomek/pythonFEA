function [ ns, ks, as ] = compnz_to_ksub ( nc, kc, ac )

%*****************************************************************************80
%
%% compnz_to_ksub() converts a (nonzero) composition to a K-subset.
%
%  Discussion:
%
%    There is a bijection between K subsets and nonzero compositions.
%
%    Let AC be a nonzero composition of NC into KC parts.
%
%    Then let
%      NS = NC - 1
%      KS = KC - 1
%    and define
%      AS(I) = sum ( AC(1:I) ), for I = 1 : KS.
%      
%    Then AS is a KS subset of the integers 1 through NS.
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
%    integer NC, the composition sum.
%
%    integer KC, the number of parts of the composition.
%
%    integer AC(KC), the parts of the composition.
%
%  Output:
%
%    integer NS, the size of the set.
%
%    integer KS, the size of the subset.
%
%    integer AS(KS), the entries of the K-subset, in increasing order.
%
  ns = nc - 1;
  ks = kc - 1;
  as = cumsum ( ac(1:kc-1) );

  return
end


