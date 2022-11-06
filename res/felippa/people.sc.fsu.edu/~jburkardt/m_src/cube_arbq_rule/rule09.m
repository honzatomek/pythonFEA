function [ x, w ] = rule09 ( n )

%*****************************************************************************80
%
%% rule09() returns the rule of degree 9.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    08 July 2014
%
%  Author:
%
%    Original FORTRAN77 version by Hong Xiao, Zydrunas Gimbutas.
%    This MATLAB version by John Burkardt.
%
%  Reference:
%
%    Hong Xiao, Zydrunas Gimbutas,
%    A numerical algorithm for the construction of efficient quadrature
%    rules in two and higher dimensions,
%    Computers and Mathematics with Applications,
%    Volume 59, 2010, pages 663-676.
%
%  Input:
%
%    integer N, the number of nodes.
%
%  Output:
%
%    real X(3,N), the coordinates of the nodes.
%
%    real W(N), the weights.
%
  xs = [ ...
    0.7677593774006314E+00, ...
    -0.7677593774006314E+00, -0.3207821329562814E+00, ...
    0.3207821329562814E+00, -0.4136209586513314E+00, ...
    0.4136209586513314E+00, -0.6852061275740796E+00, ...
    0.6852061275740795E+00, 0.9244405033583697E+00, ...
    -0.9244405033583697E+00, -0.6411525460395265E+00, ...
    0.6411525460395265E+00, -0.6524611766154725E+00, ...
    0.6524611766154725E+00, -0.9637289125097334E+00, ...
    0.9637289125097335E+00, -0.4042812911589966E-01, ...
    0.4042812911589966E-01, 0.7010911265568170E+00, ...
    -0.7010911265568170E+00, -0.4018375553483048E+00, ...
    0.4018375553483048E+00, 0.2427999428831116E+00, ...
    -0.2427999428831116E+00, -0.8888214064543165E+00, ...
    0.8888214064543165E+00, 0.5686958127555934E+00, ...
    -0.5686958127555934E+00, -0.1007305440999530E+00, ...
    0.1007305440999530E+00, 0.9627142008988805E+00, ...
    -0.9627142008988805E+00, 0.5575105029618763E+00, ...
    -0.5575105029618763E+00, -0.2006401852932052E+00, ...
    0.2006401852932052E+00, 0.1276245748755967E+00, ...
    -0.1276245748755967E+00, 0.5324626645500558E+00, ...
    -0.5324626645500558E+00, -0.8230657430079429E+00, ...
    0.8230657430079429E+00, -0.9171428680981173E+00, ...
    0.9171428680981173E+00, 0.9753289529764423E+00, ...
    -0.9753289529764423E+00, 0.7278991004245323E+00, ...
    -0.7278991004245322E+00, 0.9084671271535661E+00, ...
    -0.9084671271535661E+00 ];
  ys = [ ...
    -0.5215705733515856E-02, ...
    0.5215705733515811E-02, -0.9255213178288733E+00, ...
    0.9255213178288733E+00, 0.3254858593442050E+00, ...
    -0.3254858593442050E+00, -0.8673453037549068E+00, ...
    0.8673453037549068E+00, 0.1651688473834196E+00, ...
    -0.1651688473834196E+00, 0.9161123256468909E+00, ...
    -0.9161123256468909E+00, 0.8789047081433894E+00, ...
    -0.8789047081433894E+00, 0.7092728961533591E+00, ...
    -0.7092728961533591E+00, 0.5224015774226531E+00, ...
    -0.5224015774226531E+00, 0.4986074979684522E+00, ...
    -0.4986074979684522E+00, -0.5887106445666494E-01, ...
    0.5887106445666495E-01, 0.4841822578088601E+00, ...
    -0.4841822578088601E+00, -0.7052476161004777E+00, ...
    0.7052476161004776E+00, -0.7991952932799359E-01, ...
    0.7991952932799361E-01, 0.2884264730944422E+00, ...
    -0.2884264730944421E+00, -0.5964266662509132E+00, ...
    0.5964266662509132E+00, -0.7120073930331048E+00, ...
    0.7120073930331048E+00, 0.8586009498349154E+00, ...
    -0.8586009498349154E+00, 0.9369688457657286E+00, ...
    -0.9369688457657286E+00, 0.6794094006908223E+00, ...
    -0.6794094006908225E+00, 0.4202573751253162E+00, ...
    -0.4202573751253164E+00, -0.1970879922320003E+00, ...
    0.1970879922320003E+00, 0.8523907907745764E+00, ...
    -0.8523907907745764E+00, 0.9938423815326598E+00, ...
    -0.9938423815326598E+00, -0.9848158730090135E+00, ...
    0.9848158730090135E+00 ];
  zs = [ ...
    -0.9944420260442561E+00, ...
    0.9944420260442561E+00, 0.9672106847608946E+00, ...
    -0.9672106847608946E+00, -0.9450653904792801E+00, ...
    0.9450653904792801E+00, -0.9208144764213119E+00, ...
    0.9208144764213120E+00, 0.8857214694746194E+00, ...
    -0.8857214694746194E+00, 0.8845112554423256E+00, ...
    -0.8845112554423256E+00, -0.8251225389279271E+00, ...
    0.8251225389279271E+00, -0.8150208317048079E+00, ...
    0.8150208317048079E+00, 0.8824401782407778E+00, ...
    -0.8824401782407778E+00, 0.5971438826916258E+00, ...
    -0.5971438826916258E+00, -0.5274658210290475E+00, ...
    0.5274658210290475E+00, -0.7239973446191893E+00, ...
    0.7239973446191893E+00, 0.7834569443713458E+00, ...
    -0.7834569443713458E+00, -0.6010889720637044E+00, ...
    0.6010889720637044E+00, 0.5589543903569408E-01, ...
    -0.5589543903569408E-01, -0.7052432618789399E+00, ...
    0.7052432618789399E+00, -0.2951319795385468E+00, ...
    0.2951319795385468E+00, -0.4212601390804162E+00, ...
    0.4212601390804162E+00, 0.5022536274483731E+00, ...
    -0.5022536274483730E+00, -0.3981899691586369E-01, ...
    0.3981899691586369E-01, -0.3419107086504177E+00, ...
    0.3419107086504178E+00, 0.1910346742820620E+00, ...
    -0.1910346742820620E+00, 0.4032363262946108E+00, ...
    -0.4032363262946109E+00, -0.3785656001274115E+00, ...
    0.3785656001274115E+00, 0.8086398500198494E-02, ...
    -0.8086398500198474E-02 ];
  ws = [ ...
    0.2538811854621882E-01, ...
    0.2538811854621883E-01, 0.1762240779978733E-01, ...
    0.1762240779978734E-01, 0.4976713220360957E-01, ...
    0.4976713220360960E-01, 0.2355615731022458E-01, ...
    0.2355615731022460E-01, 0.2724595042792551E-01, ...
    0.2724595042792553E-01, 0.2403649336578707E-01, ...
    0.2403649336578707E-01, 0.2946300694577264E-01, ...
    0.2946300694577266E-01, 0.1432705784656010E-01, ...
    0.1432705784656010E-01, 0.7802654597787825E-01, ...
    0.7802654597787825E-01, 0.6022424126025965E-01, ...
    0.6022424126025969E-01, 0.1068682771411129E+00, ...
    0.1068682771411130E+00, 0.9993782502170498E-01, ...
    0.9993782502170505E-01, 0.3727854515440352E-01, ...
    0.3727854515440351E-01, 0.9828312978563304E-01, ...
    0.9828312978563307E-01, 0.1296245426718050E+00, ...
    0.1296245426718050E+00, 0.3099443142302383E-01, ...
    0.3099443142302385E-01, 0.9399083827155912E-01, ...
    0.9399083827155913E-01, 0.8169878897651819E-01, ...
    0.8169878897651825E-01, 0.5623588432426985E-01, ...
    0.5623588432426987E-01, 0.1056612200118129E+00, ...
    0.1056612200118130E+00, 0.9074609788069554E-01, ...
    0.9074609788069565E-01, 0.6975340566577869E-01, ...
    0.6975340566577869E-01, 0.2198686280707488E-01, ...
    0.2198686280707488E-01, 0.2302735019253151E-01, ...
    0.2302735019253151E-01, 0.1846925136114678E-01, ...
    0.1846925136114678E-01 ];

  x(1,1:n) = xs(1:n);
  x(2,1:n) = ys(1:n);
  x(3,1:n) = zs(1:n);
  w(1:n) = ws(1:n);

  return
end
