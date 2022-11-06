function [ x, w ] = rule11( n )

%*****************************************************************************80
%
%% rule11() returns the rule of degree 11.
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
    0.2499788650260321E+00, 0.3921769824433600E+00, ...
    -0.7499317903601719E+00, 0.1970279946664021E+00, ...
    -0.4574811189815491E+00, 0.7920994458748165E+00, ...
    0.9496878040642897E+00, 0.5494991726029024E+00, ...
    -0.9415698074379264E+00, -0.1549790418658323E+00, ...
    0.9014007983667046E+00, -0.4493885995928424E+00, ...
    -0.7095579485335214E+00, -0.8736487328036152E+00, ...
    -0.3264862525811954E-01, -0.1103424065156252E+00, ...
    -0.5637539669697259E+00, 0.2945721905451988E+00, ...
    -0.8603062445393469E+00, -0.7507058748066501E+00, ...
    -0.2780639680677569E+00, 0.7047555601238907E+00, ...
    -0.6295944388807833E+00, -0.9095323899548270E+00, ...
    0.2964917498074751E+00, 0.8851424078101294E+00, ...
    -0.7219563370216030E+00, -0.9230518045006708E+00, ...
    0.5631064783902135E+00, -0.9323802418890973E+00, ...
    0.9285744095618478E+00, -0.3446313474069602E-01, ...
    0.5285124795902665E+00, -0.6564153211279398E+00, ...
    0.9670556758395562E+00, 0.9746827810441796E+00, ...
    -0.1497145509400533E+00, 0.5126157047506474E-01, ...
    0.5771162069337331E+00, -0.9882152664587444E+00, ...
    0.8664710684798440E+00, 0.1554480104048762E+00, ...
    -0.2499788650260319E+00, -0.3921769824433600E+00, ...
    0.7499317903601719E+00, -0.1970279946664021E+00, ...
    0.4574811189815492E+00, -0.7920994458748165E+00, ...
    -0.9496878040642897E+00, -0.5494991726029023E+00, ...
    0.9415698074379265E+00, 0.1549790418658323E+00, ...
    -0.9014007983667046E+00, 0.4493885995928425E+00, ...
    0.7095579485335214E+00, 0.8736487328036152E+00, ...
    0.3264862525811960E-01, 0.1103424065156252E+00, ...
    0.5637539669697259E+00, -0.2945721905451988E+00, ...
    0.8603062445393470E+00, 0.7507058748066501E+00, ...
    0.2780639680677569E+00, -0.7047555601238906E+00, ...
    0.6295944388807833E+00, 0.9095323899548271E+00, ...
    -0.2964917498074751E+00, -0.8851424078101294E+00, ...
    0.7219563370216030E+00, 0.9230518045006707E+00, ...
    -0.5631064783902135E+00, 0.9323802418890973E+00, ...
    -0.9285744095618478E+00, 0.3446313474069607E-01, ...
    -0.5285124795902665E+00, 0.6564153211279398E+00, ...
    -0.9670556758395561E+00, -0.9746827810441796E+00, ...
    0.1497145509400534E+00, -0.5126157047506470E-01, ...
    -0.5771162069337331E+00, 0.9882152664587445E+00, ...
    -0.8664710684798441E+00, -0.1554480104048762E+00 ];
  ys = [ ...
    0.9380477277286925E+00, -0.3903208068416588E+00, ...
    0.5539681565347837E+00, -0.9087002846713521E+00, ...
    -0.6321385589135142E+00, 0.3182961009431599E+00, ...
    -0.8236851175776945E+00, -0.8104327207737747E+00, ...
    0.9326434060163864E+00, -0.4047646882920218E+00, ...
    0.9551344910189569E+00, -0.8100975319542825E+00, ...
    -0.2881465340470265E+00, 0.9393091372522897E+00, ...
    0.7616291888714266E+00, -0.3792679791993436E+00, ...
    -0.8198273601663221E+00, -0.6277875734879305E+00, ...
    0.3601156105251353E+00, -0.6500193633731873E+00, ...
    -0.1345149716413504E+00, 0.5453436457811436E+00, ...
    0.9440747796649167E+00, -0.9592627915208141E+00, ...
    -0.7303420755929871E-01, 0.8946123590730062E+00, ...
    0.6073216328066294E-01, 0.2144997262319605E+00, ...
    -0.1311566771466323E+00, -0.7186229429207273E+00, ...
    -0.6749210564844365E+00, 0.9691840758487141E+00, ...
    -0.6464321973320226E+00, 0.9661037973313142E+00, ...
    0.5022924249551910E+00, 0.2011928667597957E+00, ...
    -0.3711890466829925E+00, -0.7861623327708182E+00, ...
    0.9831486697529307E+00, 0.1008010491222713E+00, ...
    -0.6108831589158809E+00, 0.9546709186773804E+00, ...
    -0.9380477277286926E+00, 0.3903208068416588E+00, ...
    -0.5539681565347837E+00, 0.9087002846713521E+00, ...
    0.6321385589135142E+00, -0.3182961009431599E+00, ...
    0.8236851175776946E+00, 0.8104327207737747E+00, ...
    -0.9326434060163864E+00, 0.4047646882920218E+00, ...
    -0.9551344910189569E+00, 0.8100975319542825E+00, ...
    0.2881465340470266E+00, -0.9393091372522897E+00, ...
    -0.7616291888714266E+00, 0.3792679791993436E+00, ...
    0.8198273601663221E+00, 0.6277875734879305E+00, ...
    -0.3601156105251352E+00, 0.6500193633731873E+00, ...
    0.1345149716413503E+00, -0.5453436457811436E+00, ...
    -0.9440747796649167E+00, 0.9592627915208141E+00, ...
    0.7303420755929881E-01, -0.8946123590730062E+00, ...
    -0.6073216328066286E-01, -0.2144997262319605E+00, ...
    0.1311566771466323E+00, 0.7186229429207273E+00, ...
    0.6749210564844365E+00, -0.9691840758487141E+00, ...
    0.6464321973320226E+00, -0.9661037973313142E+00, ...
    -0.5022924249551909E+00, -0.2011928667597957E+00, ...
    0.3711890466829925E+00, 0.7861623327708182E+00, ...
    -0.9831486697529307E+00, -0.1008010491222714E+00, ...
    0.6108831589158809E+00, -0.9546709186773804E+00 ];
  zs = [ ...
    0.8462611120890736E+00, 0.3547611988896818E+00, ...
    0.5262314749145942E-02, 0.7237196107659919E+00, ...
    -0.6425814432975854E-01, 0.1803373124222549E+00, ...
    -0.7073699162717987E+00, -0.3664557776165722E+00, ...
    0.1016191298800923E+00, -0.4999025408268907E+00, ...
    0.9145661354846110E+00, 0.6962396504425696E+00, ...
    0.4859552045824018E+00, -0.9060323834426769E+00, ...
    -0.1822470452558573E+00, 0.7016603041053766E+00, ...
    -0.6424102177565745E+00, -0.7635438798952285E+00, ...
    0.5612530704657699E+00, 0.9279296695895975E+00, ...
    -0.8932000387343114E+00, 0.9433964716928596E+00, ...
    -0.3855860228559991E+00, 0.8267083885708849E+00, ...
    -0.2085191046423283E+00, 0.2867455231288707E+00, ...
    -0.6303696136436402E+00, -0.9480808897684662E+00, ...
    -0.8851760571455803E+00, 0.3677243413181205E+00, ...
    0.5707827021209619E+00, 0.3308168458013439E+00, ...
    0.8942058612668059E+00, 0.8491966568619232E+00, ...
    0.6997888056053100E+00, -0.8582587623738921E+00, ...
    0.9980904532339944E+00, -0.9929983508078530E+00, ...
    -0.2607372382203097E+00, -0.2706521005603409E-01, ...
    -0.9886712504226799E+00, -0.9981234658396176E+00, ...
    -0.8462611120890736E+00, -0.3547611988896818E+00, ...
    -0.5262314749145976E-02, -0.7237196107659919E+00, ...
    0.6425814432975864E-01, -0.1803373124222550E+00, ...
    0.7073699162717988E+00, 0.3664557776165722E+00, ...
    -0.1016191298800922E+00, 0.4999025408268908E+00, ...
    -0.9145661354846109E+00, -0.6962396504425696E+00, ...
    -0.4859552045824018E+00, 0.9060323834426768E+00, ...
    0.1822470452558573E+00, -0.7016603041053766E+00, ...
    0.6424102177565745E+00, 0.7635438798952286E+00, ...
    -0.5612530704657699E+00, -0.9279296695895975E+00, ...
    0.8932000387343114E+00, -0.9433964716928596E+00, ...
    0.3855860228559991E+00, -0.8267083885708848E+00, ...
    0.2085191046423283E+00, -0.2867455231288708E+00, ...
    0.6303696136436402E+00, 0.9480808897684662E+00, ...
    0.8851760571455803E+00, -0.3677243413181204E+00, ...
    -0.5707827021209619E+00, -0.3308168458013439E+00, ...
    -0.8942058612668059E+00, -0.8491966568619232E+00, ...
    -0.6997888056053101E+00, 0.8582587623738921E+00, ...
    -0.9980904532339944E+00, 0.9929983508078530E+00, ...
    0.2607372382203098E+00, 0.2706521005603400E-01, ...
    0.9886712504226799E+00, 0.9981234658396176E+00 ];
  ws = [ ...
    0.1322946412898770E-01, 0.7207978700091233E-01, ...
    0.5018350497921869E-01, 0.2510540976305218E-01, ...
    0.6381617344667434E-01, 0.5297826014608787E-01, ...
    0.1107387819925981E-01, 0.4482499482382119E-01, ...
    0.1156633883765873E-01, 0.8060095080318432E-01, ...
    0.4983228901803450E-02, 0.3768915302119238E-01, ...
    0.6105445139039273E-01, 0.6907527307462437E-02, ...
    0.6642161794579643E-01, 0.6883290145149486E-01, ...
    0.3765112561164340E-01, 0.5084567296013873E-01, ...
    0.4165679702303787E-01, 0.1924373869332567E-01, ...
    0.4590245342578355E-01, 0.2073261975977018E-01, ...
    0.2506136162764897E-01, 0.6618443166679632E-02, ...
    0.1133796667663198E+00, 0.2117476937280500E-01, ...
    0.6105702586211274E-01, 0.1302086249789518E-01, ...
    0.4287461768786696E-01, 0.2726768060699983E-01, ...
    0.2693097474826901E-01, 0.2634041203981413E-01, ...
    0.3577055224953832E-01, 0.1180436625397797E-01, ...
    0.1895051399977877E-01, 0.1378259645552230E-01, ...
    0.1414623939342679E-01, 0.1101406610125121E-01, ...
    0.1973564062736466E-01, 0.2303337346288448E-01, ...
    0.8809606694498163E-02, 0.6060743137742029E-02, ...
    0.1322946412898769E-01, 0.7207978700091235E-01, ...
    0.5018350497921865E-01, 0.2510540976305218E-01, ...
    0.6381617344667437E-01, 0.5297826014608786E-01, ...
    0.1107387819925980E-01, 0.4482499482382119E-01, ...
    0.1156633883765873E-01, 0.8060095080318433E-01, ...
    0.4983228901803448E-02, 0.3768915302119238E-01, ...
    0.6105445139039275E-01, 0.6907527307462438E-02, ...
    0.6642161794579643E-01, 0.6883290145149484E-01, ...
    0.3765112561164340E-01, 0.5084567296013873E-01, ...
    0.4165679702303786E-01, 0.1924373869332567E-01, ...
    0.4590245342578356E-01, 0.2073261975977018E-01, ...
    0.2506136162764896E-01, 0.6618443166679627E-02, ...
    0.1133796667663198E+00, 0.2117476937280501E-01, ...
    0.6105702586211271E-01, 0.1302086249789518E-01, ...
    0.4287461768786696E-01, 0.2726768060699984E-01, ...
    0.2693097474826901E-01, 0.2634041203981412E-01, ...
    0.3577055224953834E-01, 0.1180436625397797E-01, ...
    0.1895051399977878E-01, 0.1378259645552230E-01, ...
    0.1414623939342678E-01, 0.1101406610125121E-01, ...
    0.1973564062736465E-01, 0.2303337346288442E-01, ...
    0.8809606694498165E-02, 0.6060743137742026E-02 ];

  x(1,1:n) = xs(1:n);
  x(2,1:n) = ys(1:n);
  x(3,1:n) = zs(1:n);
  w(1:n) = ws(1:n);

  return
end
