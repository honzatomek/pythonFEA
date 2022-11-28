function [ x, y, w ] = rule25 ( )

%*****************************************************************************80
%
%% rule25() returns the rule of degree 25.
%
%  Discussion:
%
%    Order 25 (120 pts)
%    1/6 data for 25-th order quadrature with 25 nodes.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    26 June 2014
%
%  Author:
%
%    Original FORTRAN77 version by Hong Xiao, Zydrunas Gimbutas.
%    This MATLAB version by John Burkardt.
%
%  Output:
%
%    real X(*), Y(*), the coordinates of the nodes.
%
%    real W(*), the weights.
%
  x = [ ...
      0.0000000000000000000000000000000000, ...
      0.0000000000000000000000000000000000, ...
     -0.1173472784070389885314628951496541, ...
      0.0000000000000000000000000000000000, ...
     -0.6450240460256727525710559747446056, ...
      0.0000000000000000000000000000000000, ...
     -0.5664343384798589665228955002024030, ...
      0.0000000000000000000000000000000000, ...
     -0.3910189989265832790334726502623686, ...
     -0.2012181263167401097079073382399585, ...
     -0.2038066062734541936977294977052958, ...
     -0.7946055588153782250820470421122720, ...
     -0.1972942521563999602645401846428416, ...
      0.0000000000000000000000000000000000, ...
     -0.6355041410448193873418685582169463, ...
     -0.2629485129580234408600628587611446, ...
     -0.7847262192370077797696941981117842, ...
     -0.3957476856294985173592517287113179, ...
      0.0000000000000000000000000000000000, ...
     -0.4622691203344359376572172601919067, ...
      0.0000000000000000000000000000000000, ...
     -0.9106735335147376498998363730530587, ...
      0.0000000000000000000000000000000000, ...
      0.0000000000000000000000000000000000, ...
     -0.4402761583839805030337834115210799 ];
    y = [ ... ...
     -0.1881308452404751474791770362222539, ...
      0.4237594812020736794258174198266686, ...
     -0.5741998997668708578748305284336055, ...
      0.1172287234795072247760841771164101, ...
     -0.5133334261186957616800512329664083, ...
      0.1025756540328684422937411013290944E+01, ...
     -0.4407640888171437954387566505919770, ...
      0.6520273733414071456865438634979216, ...
     -0.4581028502386777788723229469606161, ...
     -0.3764327086872663044277454612227560, ...
     -0.4936620181125184332135804336351929, ...
     -0.5650036663011180322305141381076653, ...
     -0.2245437989161187688779317294631657, ...
     -0.3167088545245427646354993582346312, ...
     -0.5648168693903931904010878260331929, ...
     -0.5549827672775567477688079143971306, ...
     -0.5120728253301845715218718158194338, ...
     -0.3400477140537724278182036849058156, ...
     -0.4464373961278773726299668315862868, ...
     -0.5348456380201123672101389788730482, ...
      0.8327133265910420576580234570528976, ...
     -0.5648988533934863981864107760207050, ...
      0.1127558109594723670980720926142653E+01, ...
     -0.5393815319272739058039649892539370, ...
     -0.5758062076985843575874824269557718 ];
    w = [ ... ...
      0.9008428931929272978284555982082135E-02, ...
      0.7624848013076671474981932002250592E-02, ...
      0.2204184225038700283242061056342968E-02, ...
      0.1185627435111220996327302583177423E-01, ...
      0.8306372211706409316335445471098323E-02, ...
      0.2235547623030763175546447504392690E-02, ...
      0.1252247261761103193500179756735477E-01, ...
      0.7561849278446502447261825323255299E-02, ...
      0.1432466758007433215998170125431021E-01, ...
      0.2084707600211569027917629357552928E-01, ...
      0.1400325214573775947713571136961477E-01, ...
      0.3349765842027190344172740201222808E-02, ...
      0.2357591414900995147064953536612743E-01, ...
      0.1047023089196828804219791222697079E-01, ...
      0.4295322980586838056654945868959874E-02, ...
      0.7178707806144740205045671854952271E-02, ...
      0.6939081726476119924677787292839119E-02, ...
      0.1808296563518180275001928371748139E-01, ...
      0.8985018370003820265092102638348177E-02, ...
      0.9626213990805757770654565513598145E-02, ...
      0.6042636212818408481919357354311341E-02, ...
      0.2228091765228852260396032167229650E-02, ...
      0.5307136158214250141007056133594507E-03, ...
      0.5556521038676869804672708399129014E-02, ...
      0.1989511820786002256062338480298376E-02 ];

  return
end