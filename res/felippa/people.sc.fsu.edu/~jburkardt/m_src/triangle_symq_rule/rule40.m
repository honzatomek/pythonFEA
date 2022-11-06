function [ x, y, w ] = rule40 ( )

%*****************************************************************************80
%
%% rule40() returns the rule of degree 40.
%
%  Discussion:
%
%    Order 40 (295 pts)
%    1/6 data for 40-th order quadrature with 58 nodes.
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
     -0.66411637423956578739874309909381, ...
     -0.24207197863738398306862447542249, ...
     -0.67514797177612497189134399646036, ...
     -0.87225015944191233374717005469166, ...
     -0.76868567879223402156192530043099, ...
     -0.30999131154608929623930725445334, ...
     -0.85323240841065682042462033708838, ...
     -0.53727030292133569609030595551578, ...
     -0.76757602207091515344003545816282, ...
     -0.41709706260058898440960904281784, ...
     -0.64250324642862691442102353901370, ...
     -0.73395730528035636744565078095249, ...
     -0.43310798704792516669286002220314, ...
     -0.84693242965372475993997325395966, ...
     -0.52988575538367881737400733664267, ...
     -0.81395203621001148192221956701526, ...
     -0.13262238321158207908085298608017, ...
      0.00000000000000000000000000000000, ...
      0.00000000000000000000000000000000, ...
      0.00000000000000000000000000000000, ...
     -0.12565893500209059730381606002675, ...
     -0.25582836243545168466615122195358, ...
     -0.41982097163871183619680245728246, ...
     -0.39584244571866434364968812226225, ...
     -0.13831881077136084719064254716209, ...
     -0.55558835329547161475407470013715, ...
     -0.26904113625079335044301504651780, ...
     -0.38617376855258266995224135587201, ...
     -0.90712290003268365546639318057557, ...
     -0.13211254083412060410003056730975, ...
     -0.13532200006090754449420198166522, ...
     -0.51005162143835566342423604409501, ...
     -0.51567412876653561433770558683539, ...
     -0.72835797984670651201858245642167, ...
     -0.62260740206958077067832393367003, ...
     -0.63528538940605775324683142950171, ...
     -0.13545478402323854515885777980547, ...
      0.00000000000000000000000000000000, ...
     -0.91834711208585899367680729069800, ...
     -0.26841456178680462127694701451016, ...
      0.00000000000000000000000000000000, ...
      0.00000000000000000000000000000000, ...
      0.00000000000000000000000000000000, ...
      0.00000000000000000000000000000000, ...
     -0.29450956974500233083745593494225, ...
      0.00000000000000000000000000000000, ...
      0.00000000000000000000000000000000, ...
      0.00000000000000000000000000000000, ...
     -0.15766005001997588398000791219776, ...
      0.00000000000000000000000000000000, ...
     -0.39500420341927114646355151929679, ...
      0.00000000000000000000000000000000, ...
      0.00000000000000000000000000000000, ...
     -0.26583541850389618832328064074218, ...
      0.00000000000000000000000000000000, ...
      0.00000000000000000000000000000000, ...
      0.00000000000000000000000000000000, ...
     -0.96488163632640580772795528150790 ];
  y = [ ... ...
     -0.57470378380535839956317699309199, ...
     -0.55141831198597603831507286345131, ...
     -0.56556494724168897227639164041706, ...
     -0.51618467078933129623213777000412, ...
     -0.57460672418387407371421627702399, ...
     -0.54008229992950442561714306153720, ...
     -0.57384241303948358177304914166735, ...
     -0.55074388383078986548196008116722, ...
     -0.55974233776493979903089310270512, ...
     -0.52536474950509016897082596544373, ...
     -0.54449319151536051271909078020569, ...
     -0.53103387228266569983647483538750, ...
     -0.57587141183196674121843247917653, ...
     -0.55611797625038840229584838026927, ...
     -0.51296985906932015988541254967364, ...
     -0.52158827793142738546379723447015, ...
     -0.56187932687266342953378211324032, ...
      0.11003011687008237336733434466617E+01, ...
      0.13663584323344421793456675059387, ...
     -0.12737340589454779203738364883373, ...
     -0.18929257322289204287244570649377, ...
     -0.25237212852983271455023246046020, ...
     -0.56044801444994193030345087522119, ...
     -0.47566799110898290470517971819636, ...
     -0.52410356380692977799609815127570, ...
     -0.57170940642241625229835788381429, ...
     -0.49908301034879271805709825139645, ...
     -0.31612494675971333985536967359669, ...
     -0.54847061860786860395005750661379, ...
     -0.29657575362028796765551235884180, ...
     -0.46558820109446506663286885353751, ...
     -0.37923882528380845099383801241958, ...
     -0.45684803758863608291625036678218, ...
     -0.48029591121869609066590186543256, ...
     -0.43603282267888446054028374651391, ...
     -0.50005009951296356150064101928238, ...
     -0.38891721146901775531494918295119, ...
      0.27933482596036362512959496310960, ...
     -0.57179705694461496519432143619911, ...
     -0.43363067211489722444853507200367, ...
      0.42523996391594593545924154391851, ...
      0.11441090108726184667750585003180E+01, ...
     -0.24234470017351667426024672589549, ...
     -0.49681561132122740420073482664742, ...
     -0.57125217727233967706758863119402, ...
      0.93003191476975548336483377359380, ...
      0.56962631638217381356419889136285, ...
      0.00000000000000000000000000000000, ...
     -0.57631399232172626259918827974601, ...
      0.82111436271392253740638163140260, ...
     -0.40469300239711789646100476636550, ...
     -0.54513955979531860983983136957607, ...
     -0.57203141564320759086953238463017, ...
     -0.35075660264700114997055999049565, ...
     -0.34273351742727767767145394888763, ...
      0.70353037724633104142734855780744, ...
     -0.42794958420782980247198112865939, ...
     -0.57216175902539330683834475050253 ];
  w = [ ... ...
      0.49688685986076335500813888089767E-03, ...
      0.20421322651151844017762046611226E-02, ...
      0.11376335398163363442669464305672E-02, ...
      0.13123691749460771142584854517446E-02, ...
      0.53837565018711308131679874030211E-03, ...
      0.26896172405115405725156808433702E-02, ...
      0.54307048276333835762068445369337E-03, ...
      0.25198826411461542126409010399677E-02, ...
      0.15319142782715693899177741951315E-02, ...
      0.38411906228772124799394024016653E-02, ...
      0.25562607929590933659755695536622E-02, ...
      0.26896816019905783851160754457047E-02, ...
      0.60036514084449199244017297704017E-03, ...
      0.14715716460982722722064995019804E-02, ...
      0.39962224319702302486375553437309E-02, ...
      0.25534279525811038857142356722120E-02, ...
      0.25201810346020069847466083850254E-02, ...
      0.41167230724256272754400665111648E-03, ...
      0.56555181082450911185900550967659E-02, ...
      0.56586789117358307508299653903984E-02, ...
      0.10834097702542315480967100797076E-01, ...
      0.99225669815767172945689578107284E-02, ...
      0.24415471132838240081093498781218E-02, ...
      0.57970709422402784901464121587120E-02, ...
      0.50105635706238936498550178535846E-02, ...
      0.12758608174913493052074050132044E-02, ...
      0.55592023555177532864401800823748E-02, ...
      0.87115223434972838093595378352456E-02, ...
      0.13179261092062112504683572450985E-02, ...
      0.99446646389628687350930319624097E-02, ...
      0.69734880107002948832233705466873E-02, ...
      0.73103828906969970566980130701137E-02, ...
      0.59302091834307146929707082953472E-02, ...
      0.40971162551422433954212787599193E-02, ...
      0.57773519974100858251570872273170E-02, ...
      0.42621746164839825812632309788527E-02, ...
      0.86522628980919465385085382556473E-02, ...
      0.52900487244341074555301014519952E-02, ...
      0.60794404513723632450482344169488E-03, ...
      0.73967700759539224320693784348360E-02, ...
      0.47456468096285949486134939736546E-02, ...
      0.80914786877822581165281163168211E-04, ...
      0.53082561926097671621877127564791E-02, ...
      0.30789395107446736330440294150100E-02, ...
      0.16224159778341893367110818294581E-02, ...
      0.17880830179715579824918122606522E-02, ...
      0.41061127743554988075477642960788E-02, ...
      0.19270579632685484848497884675987E-02, ...
      0.59093070603955750104585123796686E-03, ...
      0.25603623606255944146797327158473E-02, ...
      0.74860701764472599200155271263176E-02, ...
      0.19931737634037231520593140572046E-02, ...
      0.79295305880649344360913599297743E-03, ...
      0.88807475399574394813610532572560E-02, ...
      0.47542421811652855221590656198943E-02, ...
      0.33799358355588133678392461897413E-02, ...
      0.40037958142737030132660946973599E-02, ...
      0.36660639965830984726685636110253E-03 ];

  return
end
