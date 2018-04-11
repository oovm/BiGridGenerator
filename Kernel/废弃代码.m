Block[{}, def = "1,2,0.2,2,5,10,3,64Pi,140";

list = {1, 1, 2, 3, 5, 8};
TraditionalForm[
  an = Factor@InterpolatingPolynomial[Join[list, {9527}], n]]
  Table[an, {n, 1, 20}]
  eqn = {y'[x] == y[x] (1 - y[x]/27)};
  Flatten@Trace[DSolve[eqn, y[x], x], Solve[_, y[x]],
    TraceInternal -> True];
  ans = Extract[%[[1]], {1, 1}];
TraditionalForm[Column@eqn -> "   "  ans]


  f[t_] := {RandomInteger[{-9, 9}, 2], 1}
  g[{{a_, b_}, c_}] :=
   ImplicitRegion[(x + a)^2 + (y + b)^2 <= c^2, {x, y}]
  data[t_] := g /@ f /@ Range[t];
  area[t_] := RegionUnion[data[t]] // DiscretizeRegion // RegionMeasure

-----------------------------------------------------------------------------------------------------------------------------------
Clear["`*"]
DumpSave[FileNameJoin[{NotebookDirectory[], "plot.mx"}], "Global`"];
Quit[];
FileNameJoin[{NotebookDirectory[], "plot.mx"}]
------------------------------------------------------------------------------------------------------------------------------------
VoronoiMesh[AnglePath[Table[{Sqrt[k], GoldenAngle}, {k, 800}]],
  PlotTheme -> "Lines"]

param[x_, m_, t_] :=
    Block[{f, n = Length[x], nf},
      f = Chop[Fourier[x]][[;; Ceiling[Length[x]/2]]];
      nf = Length[f];
      Total[Rationalize[
        2 Abs[f]/Sqrt[n] Sin[
          Pi/2 - Arg[f] + 2. Pi Range[0, nf - 1] t], .01][[;;
          Min[m, nf]]]]]

tocurve[Line[data_], m_, t_] := param[#, m, t] & /@ Transpose[data]

img = Import["http://i.stack.imgur.com/wtJoA.png"];

img = Binarize[img~ColorConvert~"Grayscale"~ImageResize~500~Blur~3]~
    Blur~3;
lines = Cases[
  Normal@ListContourPlot[Reverse@ImageData[img],
    Contours -> {0.5}], _Line, -1];

ParametricPlot[Evaluate[tocurve[#, 500, t] & /@ lines], {t, 0, 1},
  Frame -> True, Axes -> False]
------------------------------------------------------------------------------------------------------------------------------------

ToZeta[exp_] := Block[{time},
  time = Log[Pi, #] & @@ Cases[exp, Pi^_Integer];
  Inactivate[Zeta[time]] exp/Zeta[time]]
ToZeta[31 Pi^6/5040]

ToZeta[exp_] := Module[{time},
	time = Log[Pi, #] & @@ Cases[exp, Pi^_Integer];
	Inactivate[Zeta[time]] exp/Zeta[time]]
ToZeta[31 Pi^6/5040]


TwoAxisListLinePlot[{f_, g_}] :=
    Block[{fgraph, ggraph, frange, grange, fticks,
      gticks}, {fgraph, ggraph} =
        MapIndexed[
          ListLinePlot[#, Axes -> True, PlotStyle -> ColorData[1][#2[[1]]],
            ImageSize -> Full,
            PlotLegends -> Placed[{"资产总和", "忍受度总和"}, Above]] &, {f,
          g}]; {frange, grange} =
        Last[PlotRange /. AbsoluteOptions[#, PlotRange]] & /@ {fgraph,
          ggraph};
    fticks =
        Last[Ticks /.
            AbsoluteOptions[fgraph,
              Ticks]] /. _RGBColor | _GrayLevel | _Hue :> ColorData[1][1];
    gticks = (MapAt[Function[r, Rescale[r, grange, frange]], #, {1}] & /@
        Last[Ticks /.
            AbsoluteOptions[ggraph,
              Ticks]]) /. _RGBColor | _GrayLevel | _Hue -> ColorData[1][2];
    Show[fgraph,
      ggraph /.
          Graphics[graph_, s___] :>
              Graphics[
                GeometricTransformation[graph,
                  RescalingTransform[{{0, 1}, grange}, {{0, 1}, frange}]], s],
      Axes -> False, Frame -> True,
      FrameStyle -> {ColorData[1] /@ {1, 2}, {Automatic, Transparent}},
      FrameTicks -> {{fticks, gticks}, {Automatic, Automatic}}]]
TwoAxisListLinePlot[{Total /@ zc, Total /@ rsd}]



da1[{a_, d_}] := If[a >= d, a - d, 0]
da2[{a_, d_}] := If[a <= 0, 0, If[d > 0, 100*a/(100 + d), a*(2*d - 50)/(d - 50)]]
da3[{a_, d_}] := If[a <= 0, 0, If[d > 0, a^2/(a + d), (2 a/Pi)*(Pi + ArcTan[a/d])]]
Manipulate[
  Plot[{da1[{a, d}], da2[{a, d}], da3[{a, d}]}, {d, -100, 150},
    PlotRange -> {0, 360}, PlotLegends -> {"加法公式", "乘法公式", "除法公式"},
    PlotLabels -> Placed["攻击力" <> ToString@a, {0.25}],
    AspectRatio -> 1/2, ImageSize -> Medium,
    AxesLabel -> {"防御力", "伤害值"}], {a, -10, 200, 10}]

da1[{a_, d_}] := If[a >= d, a - d, 0]
da2[{a_, d_}] :=
    If[a <= 0, 0, If[d > 0, 100*a/(100 + d), a*(2*d - 50)/(d - 50)]]
da3[{a_, d_}] :=
    If[a <= 0, 0, If[d > 0, a^2/(a + d), (2 a/Pi)*(Pi + ArcTan[a/d])]]
dda1[{a_, d_}] := Abs@D[da1[{a, d}], d]
dda2[{a_, d_}] := Abs@D[da2[{a, d}], d]
dda3[{a_, d_}] := Abs@D[da3[{a, d}], d]
a = 500;
Plot[{da1[{a, d}], da2[{a, d}], da3[{a, d}]}, {d, -100, 250},
  PlotRange -> {0, 800}, PlotLegends -> {"加法公式", "乘法公式", "除法公式"},
  PlotLabels -> Placed["攻击力" <> ToString@a, {0.25}],
  AspectRatio -> 1/2, ImageSize -> Large, AxesLabel -> {"防御力", "伤害值"}];
Plot[Evaluate@{dda1[{a, d}], dda2[{a, d}], dda3[{a, d}]}, {d, -250,
  500}, PlotLegends -> {"加法公式", "乘法公式", "除法公式"}, AspectRatio -> 1/2,
  ImageSize -> Large, AxesLabel -> {"防御力", "防御边际效益(攻击力500)"},
  Axes -> { True,  True, True}, PlotRange -> {Automatic, 10},
  ScalingFunctions -> "Log"];
Column[{%%, %}]
Export["s.png", %]


Clear["`*"]
    ------------------------------------------------------------------------------------------------------------------------------------
好像是求随机游走遍历的?

G = Graph[{1 <-> 2, 1 <-> 3, 2 <-> 3, 2 <-> 4, 3 <-> 4}]
A = MatrixForm@AdjacencyMatrix[G];
M = Flatten[Quiet@Inner[Divide, Total /@ A, A, List], 1] /.
    ComplexInfinity -> Infinity;
WeightedAdjacencyGraph[M]


CoverTime[GridGraph[{3,3}],5]


f[1]=1;f[2]:=1;f[n_]:=f/@{n-1,n-2};TreeForm[f[6]]
Sum[Sum[Fibonacci[i]*Fibonacci[1-i+n],{i,1,n-2}],{n,4,32,2}]
Sum[Sum[Fibonacci[i],{i,1,n-2}],{n,4,32,2}]



------------------------------------------------------------------------------------------------------------------------------------
僵尸大逃杀!
nc = 15; nr = 3;
cx = Table[ToExpression[StringJoin["cx", ToString[i]]], {i, 1, nc}];
cy = Table[ToExpression[StringJoin["cy", ToString[i]]], {i, 1, nc}];
rx = Table[ToExpression[StringJoin["rx", ToString[i]]], {i, 1, nr}];
ry = Table[ToExpression[StringJoin["ry", ToString[i]]], {i, 1, nr}];
coordList = Flatten[{Transpose[{cx, cy}], Transpose[{rx, ry}]}];
cspeed = 1;
rspeed = 1.1;

eqns = Flatten[
  {Table[
    {{Derivative[1][cx[[i]]][t], Derivative[1][cy[[i]]][t]} ==
        cspeed*Normalize[Sum[{rx[[j]][t] - cx[[i]][t], ry[[j]][t] - cy[[i]][t]}/((cx[[i]][t] - rx[[j]][t])^2 +
            (cy[[i]][t] - ry[[j]][t])^2), {j, 1, nr}]],
      cx[[i]][0] == RandomReal[{-30, 30}],
      cy[[i]][0] == RandomReal[{-30, 30}]
    },
    {i, 1, nc}
  ],
    Table[
      {{Derivative[1][rx[[i]]][t], Derivative[1][ry[[i]]][t]} ==
          rspeed*Normalize[Sum[{rx[[i]][t] - cx[[j]][t], ry[[i]][t] - cy[[j]][t]}/((cx[[j]][t] - rx[[i]][t])^2 +
              (cy[[j]][t] - ry[[i]][t])^2), {j, 1, nc}]],
        rx[[i]][0] == RandomReal[{-5, 5}],
        ry[[i]][0] == RandomReal[{-5, 5}]
      },
      {i, 1, nr}
    ]
  }
];

soln = NDSolve[eqns, coordList, {t, 0, 200}, MaxSteps -> 200000, PrecisionGoal -> 2][[1]];

coordListFn[t_] := Flatten[Table[{coordList[[i]][t], coordList[[i + 1]][t]}, {i, 1, 2*(nc + nr), 2}]]
cops[t_] := Evaluate[coordListFn[t][[1 ;; 2*nc]] /. soln[[1 ;; 2*nc]]]
robbers[t_] := Evaluate[coordListFn[t][[2*nc + 1 ;; 2*(nc + nr)]] /. soln[[2*nc + 1 ;; 2*(nc + nr)]]]

Manipulate[
  Show[
    ParametricPlot[
      {cops[t], robbers[t]}, {t, 0, tmax},
      PlotRange -> All, PlotStyle -> {Darker[Red], {Darker[Green]}}
    ],
    ListPlot[
      {Partition[cops[0], 2], Partition[robbers[0], 2], Partition[cops[tmax], 2], Partition[robbers[tmax], 2]},
      PlotStyle -> {Directive[PointSize[Medium], Black], Directive[PointSize[Medium], Black], Darker[Red], Darker[Green]}
    ],
    Axes -> False
  ],
  {tmax, 0.001, 200, 1.5}
]
-----------------------------------------------------------------------------------------------------------------------------------
这干嘛的?
Graphics@Table[BlockMap[{RandomColor[],Rectangle[{#[[1]],y},{#[[2]],y+1}]}&,Accumulate@Prepend[0]@RandomPartition[100,16],2,1],{y,100}]



LU分解?忘记干嘛的了
n = 3; G = GridGraph[{n, n}];
M = AdjacencyMatrix@G + SparseArray[{Band[{1, 1}] -> 1}, {n^2, n^2}];
U = Normal@
    SparseArray[{Band[{1, 1}] -> 1, Band[{1, 2}] -> 1,
      Band[{2, 1}] -> 1}, {n, n}];
I1 = IdentityMatrix[n];
mu = Normal@
    SparseArray[{Band[{1, 1}] -> u, Band[{1, 2}] -> 1,
      Band[{2, 1}] -> 1}, {n, n}];
mc = ConstantArray[c, n];
LinearSolve[mu, mc]
MatrixForm /@ LUDecomposition[M, Modulus -> 2]
------------------------------------------------------------------------------------------------------------------------------------
一堆心形曲面
ops = {PlotRange -> All, Boxed -> False, Axes -> False,
  ContourStyle -> Directive[Red, Specularity[White, 10]],
  PlotRange -> All, ViewPoint -> {1, 1, 0.2}}
Nordstrand[x_, y_, z_] :=
    (2*((4/3)*x)^2 + 2*y^2 + z^2 - 1)^3 - ((4/3)*x)^2*(z^3/10) -
        y^2*z^3
Kuska[x_, y_,
  z_] := (2*x^2 + y^2 + z^2 - 1)^3 - ((1/10)*x^2)*z^3 - y^2*z^3

Taubin[x_, y_, z_] := (x^2 + (3/2)^2*y^2 + z^2 - 1)^3 - x^2*z^3 -
    (((3/2)^2/20)*y^2)*z^3

Trott[x_, y_, z_] :=
    320*((x^2 + (3/2)^2*y^2 + z^2 - 1)^3 -
        x^2*z^3 - (((3/2)^2/20)*y^2)*z^3)
ContourPlot3D[Kuska[x, y, z] == 0,
  {x, -0.9, 0.9}, {y, -1.2, 1.2}, {z, -1.2, 1.4}]
ContourPlot3D[Taubin[x, y, z] == 0,
  {x, -2, 2}, {y, -2, 2}, {z, -2, 2}]
ContourPlot3D[
  Trott[x, y, z] == 0, {x, -3/2, 3/2}, {y, -1, 1}, {z, -3/2,
  3/2}, #] & @@ ops
ContourPlot3D[Nordstrand[x, y, z] == 0,
  {x, -2, 2}, {y, -2, 2}, {z, -2, 2}, #] & @@ ops





记得似乎是个红包模拟器
Function[{total, num},
  Differences@
      Sort[1 + (RandomInteger[{0, 100 total - num}, num - 1]~
          Join~{0, 100 total - num})]/100.0]





Clear["`*"];
谢宾斯基三角上的随机游走
n = 5;
steps = 1000;
dir = {{0, 0}, {1, 0}, {0, 1}};

start = FromDigits[#, 2] & /@
    Transpose@Append[RandomChoice[dir, n - 1], RandomInteger[1, 2]];

move[pt_] :=
    With[{c = RandomChoice@Pick[-dir, BitAnd @@ (# + pt) & /@ -dir, 0]},
      pt + c + RandomChoice@DeleteCases[dir, -c]];
move[{2^n, 0}] := {2^n - 1, RandomInteger@1};
move[{0, 2^n}] := {RandomInteger@1, 2^n - 1};
move[{0, 0}] := RandomChoice@Rest@dir;

ListLinePlot[{#1 + #2/2, #2*Sqrt@3/2} & @@@
    NestList[move, start, steps],
  PlotRange -> {{0, 2^n}, {0, 2^n*Sqrt[3]/2}},
  AspectRatio -> Sqrt[3]/2]



------------------------------------------------------------------------------------------------------------------------------------
CCP蒙特卡洛
Manipulate[SeedRandom[sr];
With[{jj$ = Table[RandomInteger[{1, coupons}], {20*coupons}]},
  With[{store$ =
      Table[Length[Union[Take[jj$, k]]], {k, 1, Length[jj$]}]},
    With[{list$ =
        Flatten[Table[First[Position[store$, n]], {n, 1, coupons}]]},

      ListPlot[list$, PlotRange -> All, Frame -> True, Axes -> False,

        FrameLabel -> {Style["distinct coupons bought", "Label"],
          Style["total coupons bought", "Label"]},
        PlotLabel -> Style[StringJoin[ToString[Last[list$]],

          " coupons had to be bought for the complete set of ",
          ToString[coupons],
          ".
        The expected number was ",
          ToString[Round[coupons*HarmonicNumber[coupons]]], "."],
          "Label"],
        ImagePadding -> {{35, 25}, {35, 35}},
        ImageSize -> {475, 375}]]]],
  {{coupons, 50, "number of distinct coupons"}, 6, 100, 1,
    Appearance -> "Labeled"},
  {{sr, 1, "seed random"}, 1, 400, 1, Appearance -> "Labeled"},
  ControllerLinking -> True]











一堆砖块?
    Block[{hmax = 34, vmax = 21, d},
  Graphics[{EdgeForm[{Thin, Black}],
    Table[d = Min[i, j, hmax - i - 1, vmax - j - 1]/Max[hmax, vmax];
    {ColorData["FallColors", 2.2 d^0.7],
      Translate[
        Rotate[Rectangle[{i, j}], 2 d*RandomReal[{0, 1}]*\[Pi]/2],
        400 E^(-6 (1 - d)) RandomReal[{-0.1, 0.1}, 2]]}, {i, 0,
      hmax - 1}, {j, 0, vmax - 1}]}, Background -> Black]]



参数曲线动画
circle[t_] := {Sin[Pi t], Cos[Pi t]};
dMax = 1.5;
Animate[ParametricPlot[circle[t], {t, Max[0, u - .2], u},
  PlotRange -> {{-dMax, dMax}, {-dMax, dMax}},
  ColorFunction -> Function[{x, y, w}, Opacity[w, Blue]],
  Frame -> True, Axes -> True, AxesOrigin -> {0, 0},
  PlotPoints -> 100,
  Epilog -> {Black, PointSize -> 0.015, Point[circle[u]]}], {u,
  0. + $MachineEpsilon, 6}, AnimationRate -> 1,
  AnimationRunning -> False]

















wikiTranslation[word_, language_: "Chinese"] :=
    language /. WikipediaData[word, "TitleTranslationRules"]
namelist = {"Takaaki Kajita", "Arthur B. McDonald", "Isamu Akasaki",
  "Hiroshi Amano", "Shuji Nakamura"}
wikiTranslation /@ namelist
webAppURL =
    "https://script.google.com/macros/s/alphaNumericSequence/exec"
onlineTran[textList_, languageCode_] :=
    URLExecute[
      webAppURL, {"textlist" -> textList, "language" -> languageCode}]
onlineTran[{"Takaaki Kajita", "Arthur B. McDonald", "Isamu Akasaki",
  "Hiroshi Amano", "Shuji Nakamura"}, "zh-CN"]



quan = Flatten[ConstantArray @@@ Transpose[{{15, 100}, {10, 90}}]];
BinCounts[RandomInteger[Total@quan, 1000], {{0}~Join~Accumulate@quan}]






LSSB[list_] := Block[{base}, ClearAll[n];
base = Function[t, Table[E^(2 m n Pi I/t), {m, 1, t}, {n, 1, t}]]@
    Length[list];
Inner[Times, LinearSolve[base, list], base[[1]]^n, Plus]]
FullSimplify@LSSB[{1, 4, 9}]
InterpolatingPolynomial[{1, 4, 9, 16}, x]



T[list_] := Total@Boole@OddQ[list]
add[list_] := list~Join~Switch[Mod[T[list], 4],
  0, {(Total[list^2] - 1)/2, (Total[list^2] + 1)/2},
  1, {(Total[list^2] - 1)/2, (Total[list^2] + 1)/2},
  2, {},
  3, {Total[list^2]/4 - 1, Total[list^2]/4 + 1}]



IO玄学
    // Normal // Print; SelectionMove[
  EvaluationNotebook[], Previous, Cell]; FrontEndExecute[
  FrontEndToken["CopySpecial", "PlainText"]]; SelectionMove[
  EvaluationNotebook[], After, CellGroup, 2]; FrontEndExecute[
  FrontEndToken["Paste"]];


(*UDCL:UniformDistributionCompositionList*)
UDCL={{"和分布",Piecewise[{{2 - z, 1 < z < 2}, {z,0<z<=1}}]},
  {"差分布",Piecewise[{{1 - z, 0 < z < 1}, {1 + z,-1<z<=0}}]},
  {"积分布",Piecewise[{{-Log[z], 0 < z < 1}}]},
  {"商分布",Piecewise[{{1/2, 0 <= z <= 1}, {1/(2*z^2), z > 1}}]},
  {"幂分布",Piecewise[{{-LogIntegral[z]/z,0<=z<1}}]},
  {"根分布",Piecewise[{{(1 - z + z*Log[z])/(z*Log[z]^2),0<=z}}]},
  {"对数分布",Piecewise[{{1/(1 + z)^2,0<z}}]},
  {"调和分布",Piecewise[{{1/2, z == 0}, {(2*(-1 + z - 2*z*ArcTanh[1 - z]
      + z^2*ArcTanh[1 - z]))/(-2 + z), 0< z < 1}}]}};
Just01=Plot[Evaluate@UDCL[[{3,5,8},2]],{z,0,1},PlotTheme->"Monochrome",
  PlotLegends->{UDCL[[{3,5,8},1]]},Background->None]
Not01=Plot[Evaluate@UDCL[[{1,2,4,6,7},2]],{z,-1,4},
  PlotTheme->{"VibrantColor","Detailed","Monochrome","ThickLines"},
  PlotLegends->{UDCL[[{1,2,4,6,7},1]]},PlotRange->{0,1.2},AspectRatio->1,Background->None]
Export["UDCL.png",Grid[{{Just01},{Not01}}],Background->None,ImageResolution->200];


基本运算符={Plus,Subtract,Times,Divide,Power,Surd,Log};
均值运算符={HarmonicMean,GeometricMean,Mean,RootMeanSquare,ContraharmonicMean};
统计运算符={Mean,Variance,Total,Max,Min};
自定义算符={#1Sin[#2],#1Cos[#2],Gamma[#1,#2]};
二元算符=Function/@Join[Through[基本运算符[#1,#2]]
  ,Through[均值运算符[{#1,#2}]],Through[统计运算符[{#1,#2}]],自定义算符];
算符重整化={#1+#2&,#1-#2&,#1*#2&,#1/#2&,#1^#2&,#1^(1/#2)&,Log[#2]/Log[#1]&,
  (2*#1*#2)/(#1+#2)&,Sqrt[#1*#2]&,(#1+#2)/2&,Sqrt[(#1^2+#2^2)/2]&,
  (#1^2+#2^2)/(#1+#2)&,(#1+#2)/2&,
  (1/2)*(Conjugate[#1]-Conjugate[#2])*(#1-#2)&,#1+#2&,
  (#1+#2)/2+Sqrt[(#1-#2)^2]/2&,(1/2)*(#1-Sqrt[(#1-#2)^2]+#2)&}


(*NDCL:NormalDistributionCompositionList*)
NDCL={{"和分布",1/(E^(z^2/4)*(2*Sqrt[Pi]))},
  {"差分布",1/(E^(z^2/4)*(2*Sqrt[Pi]))},
  {"积分布",BesselK[0, Abs[z]]/Pi},
  {"商分布",1/(Pi*(1 + z^2))}};
All2=Plot[Evaluate@NDCL[[All,2]],{z,-2,2},
  PlotTheme->{"VibrantColor","Monochrome","ThickLines"},
  PlotLegends->{NDCL[[All,1]]},PlotRange->{0,0.5},Background->None]
Export["All2.png",All2,Background->None,ImageResolution->200];




IMethod[n_]:=NIntegrate[(1-Product[Subscript[x,i](1-Subscript[x,i]),{i,1,n}])^(-1),##]&@@Array[{Subscript[x,#],0,1}&,n];
HMethod[n_]:=N@HypergeometricPFQ[ConstantArray[1,n+1],ConstantArray[3/2,n],1/4^n]
Table[NMethod[i]-HMethod[i],{i,1,20}]

solvedata[nn_]:=Table[ListPlot[Style[{Re[#],Im[#]}&/@Join[{n},
  Table[-n /Log[n] ProductLog[1/n (-1)^(i/n) Log[n]],{i,0,n}]],Hue[n/10]],
  PlotStyle->PointSize[Large],PlotRange->All,PlotLegends->Style[TraditionalForm[n^x==x^n],Hue[n/10]],Background->None],{n,2,nn}];
指数函数=Plot[Evaluate@Table[n^x-x^n,{n,1,10}],{x,-2.5,2.5},PlotLegends->Table[ToString@TraditionalForm[n^x==x^n],{n,1,10}],
  PlotRange->{-10,20},AspectRatio->1/2,PlotTheme->{"Classic","Frame"},ImageSize->500,Background->None];
Export["指数函数.png",Grid[{{Show[solvedata[11],PlotRange->All,ImageSize->500]},{指数函数}}],Background->None,ImageResolution->100];

data=Transpose@Table[{
  Integrate[Floor[t],{t,0,n}],
  Integrate[Ceiling[t],{t,0,n}],
  Integrate[Round[t],{t,0,n}],
  Integrate[SawtoothWave[t],{t,0,n}]
},{n,0,10,0.1}];
png=GraphicsGrid[{{Plot[{Floor[t],Ceiling[t],Round[t],SawtoothWave[t]},{t,0,10},AspectRatio->1/2,PlotTheme->"Web",PlotLegends->{"Floor","Ceiling","Round","SawtoothWave"},ImageSize->{400,400}]},{ListLinePlot[data,AspectRatio->1/2,PlotTheme->"Web",PlotLegends->{"\[Integral]Floor","\[Integral]Ceiling","\[Integral]Round","\[Integral]SawtoothWave"},ImageSize->400]}},AspectRatio->1/2,ImageSize->Large]
Export["Guass_Integrate.png",png,Background->None];






Clear["`*"]
s=5;p=0.4;n=20;
(*蒙特卡洛大法*)
t=0;
Do[f=RandomChoice[{p,1-p}->{1,0},n+s-1];
Do[If[f[[i]]==1,f[[i+1;;i+s-1]]=0],{i,1,n}];
f=f[[1;;n]];
g=ConstantArray[0,n+s-1];
Do[If[f[[i]]==1,g[[i;;i+s-1]]=1],{i,1,n}];
t+=Total@g[[1;;n]],{j,1,10000}]
"非覆盖理论值 \[TildeTilde] "<>ToString[n s p/(p (s-1)+1)]
"非覆盖模拟值 = "<>ToString[t/10000.0]
t=0;
Do[g=ConstantArray[0,n+s-1];
f=RandomChoice[{p,1-p}->{1,0},n];
Do[If[f[[i]]==1,g[[i;;i+s-1]]=1],{i,1,n}];
t+=Total@g[[1;;n]],{j,1,10000}]
"覆盖理论值   = "<>ToString[(-1+p+n p+(1-p)^s (1+p (-1-n+s)))/p]
"覆盖模拟值   = "<>ToString[t/10000.0]



can=Rest[Union@@(Range[#,100,#]&/@{7,13})];
ass=Gather[IntegerDigits[can],First[#1]==First[#2]&][[All,All,2]];
foo=Function[p,Join[p,{#}]&/@ass[[Last@p]]];
findpath[path_]:=Select[Flatten[foo/@path,1],DuplicateFreeQ];
Array[Quiet@Nest[findpath,{{#}},8]&,9]//RepeatedTiming


半傅里叶变换
Sqrt[1 - I Cot[a]] E^(I Pi Cot[a] u^2) NIntegrate[
  Exp[-2 Pi I (Csc[a] u x - Cot[a] x^2/2)] f[x], {x, -Infinity,
    Infinity}];
foo = FullSimplify[Abs[% /. {a -> 2 Pi a/4}],
Assumptions -> u > 0 && a > 0]
g[u_, a_] := Quiet@foo;
g[u_, 0] := f[u]; g[u_, 2 Pi] := f[u];




index = Compile[{{p, _Integer}, {r, _Integer}, {n, _Integer}},
  If[p <= r,
    r + Quotient[n, r] - p,
    Quotient[n, p]
  ],
  CompilationTarget -> "C",
  Parallelization -> True,
  RuntimeOptions -> "Speed"
];

PrimeSum = Compile[{{n, _Integer}},
  Module[{r = Floor[Sqrt[n]], V, S, p = 2, ind = 0, i = 1, v = 0},
    V = Quotient[n, Range[r]] ~Join~ Range[Quotient[n, r] - 1, 1, -1];
    S = Quotient[#(#+1), 2] - 1 & /@ V;
    For[p = 2, p <= r, ++p,
      If[S[[index[p, r, n]]] > S[[index[p-1, r, n]]],
        i = 1;
        While[(v = V[[i++]]) >= p^2,
          ind = index[v, r, n];
          S[[ind]] -= p (S[[index[Quotient[v, p], r, n]]] - S[[index[p-1, r, n]]])
        ];
      ]
    ];
    First[S]
  ],
  CompilationTarget -> "C",
  Parallelization -> True,
  RuntimeOptions -> "Speed",
  CompilationOptions -> {"InlineCompiledFunctions" -> True, "InlineExternalDefinitions" -> True}
];



game24[input_List, result_: 24] :=
	Quiet@Block[{add, sub, mul, div},
		With[{oprules = {add -> Plus, sub -> Subtract, mul -> Times,
			div -> Divide},
			specifics = {div[x_, 1] :> x, mul[x_, 1] :> x, mul[1, x_] :> x,
				add[2, 2] -> mul[2, 2]}},
			ParallelMap[RightComposition[
				Hold, ReplaceAll[oprules], ToString[#, InputForm] &,
				StringDelete[{"Hold[", "]"}],
				StringReplace[{"*" -> "\[Times]", "/" -> "\[Divide]"}]
			],
				Union[
					Select[result == (# /. oprules) &]@
						Groupings[Permutations@input, {add, sub, mul, div} -> 2],
					SameTest -> (0 ===
						Simplify[
							sub[#1, #2] //. specifics /.
								Prepend[oprules, k_Integer :> ToString[k]]] &)
				]
			]
		]
	];

RootHypergeometric[n_Integer,m_,t_]:=Sum[
	(-(1/((n-1)*k!)))*t^k*E^((2*Pi*I*(k-1)*m)/(n-1))*
		Pochhammer[(k-1)/(n-1)+1,k-1]*HypergeometricPFQ[Range[n-1]/n+(k-1)/(n-1),
		Delete[Range[k+1,k+n-1],-k+n-1]/(n-1),n*((n*t)/(n-1))^(n-1)],{k,0,n-2}
];
Root[#1^5-#1+1&,2]//N
N@RootHypergeometric[5,1,1]//Chop




{Subscript[l, 1], Subscript[l, 2], Subscript[l, 3], Subscript[l, 4]} =
	{y - a/3 == Subscript[k, 1] (x - a/2),
		y - a/2 == Subscript[k, 2] (x - 2 a/3),
		y - 2 a/3 == Subscript[k, 2] (x - a/2),
		y - a/2 == Subscript[k, 4] (x - a/3)}


{#, Style["中文测试 & English Test",
	FontFamily -> #]} & /@ $FontFamilies // TableForm

Subscript[p, A] = {0, 0};
Subscript[p, B] = {x, y} /.
	First@Solve[{y == 0, Subscript[l, 4]}, {x, y}];
Subscript[p, C] = {x, y} /.
	First@Solve[{y == 0, Subscript[l, 2]}, {x, y}];
Subscript[p, D] = {a, 0};
Subscript[p, E] = {x, y} /.
	First@Solve[{x == a, Subscript[l, 1]}, {x, y}];
Subscript[p, F] = {x, y} /.
	First@Solve[{x == a, Subscript[l, 3]}, {x, y}];
Subscript[p, G] = {a, a};
Subscript[p, H] = {x, y} /.
	First@Solve[{y == a, Subscript[l, 2]}, {x, y}];
Subscript[p, I] = {x, y} /.
	First@Solve[{y == a, Subscript[l, 4]}, {x, y}];
Subscript[p, J] = {0, a};
Subscript[p, K] = {x, y} /.
	First@Solve[{x == 0, Subscript[l, 3]}, {x, y}];
Subscript[p, L] = {x, y} /.
	First@Solve[{x == 0, Subscript[l, 1]}, {x, y}];
Subscript[p, M] = {x, y} /.
	First@Solve[{Subscript[l, 1], Subscript[l, 4]}, {x, y}];
Subscript[p, N] = {x, y} /.
	First@Solve[{Subscript[l, 1], Subscript[l, 2]}, {x, y}];
Subscript[p, O] = {x, y} /.
	First@Solve[{Subscript[l, 2], Subscript[l, 3]}, {x, y}];
Subscript[p, P] = {x, y} /.
	First@Solve[{Subscript[l, 3], Subscript[l, 4]}, {x, y}];


Tupper::prime = "`1` is not a prime!";
Options[TupperPlot] = {Prime -> 17, Times -> 6};
Options[TupperK] = {Prime -> 17, Times -> 6};
TupperK[img_, OptionsPattern[]] := Block[{i, m, r, p},
	p = OptionValue[Prime];
	If[! PrimeQ[p], Message[Tupper::prime, p]; Return[]];
	i = Rasterize[img, ImageSize -> {OptionValue[Times] p, p}];
	m = Map[Boole[Max@# < 1] &, ImageData[i], {2}];
	r = p FromDigits[Flatten@Reverse@Transpose[m], 2]]
TupperPlot[k_, OptionsPattern[]] := Block[{bin, p, t},
	p = OptionValue[Prime]; t = OptionValue[Times] ;
	bin = IntegerDigits[k/p, 2, t p^2 - 1];
	ArrayPlot[Reverse /@ Transpose@Partition[bin, p]]]


PDF[WeibullDistribution[1, 100, 0], t]
PDF[ExponentialDistribution[1/100], t]
InverseCDF[ExponentialDistribution[1/100], t]
Integrate[-100 Log[1 - t], {t, 0, x}, Assumptions -> 0 < x < 1]/
	Integrate[-100 Log[1 - t], {t, 0, 1}]
img = Plot[{Style[x, Dashed, Thickness[0.01]],
	Style[x - (-1 + x) Log[1 - x], Thickness[0.01]]}, {x, 0, 1},
	Filling -> {2 -> {1}}, PlotRange -> {0, 1}, AspectRatio -> 1,
	Axes -> False,
	Frame -> True,
	FrameTicks -> {Table[i, {i, 0, 1, 0.1}], Automatic},
	FrameLabel -> {"劳伦兹曲线:\!\(\*TagBox[\(TraditionalForm\`x + \((1 - \
x)\)\\\ \(log(1 - x)\)\),
TraditionalForm,\nEditable->True]\)", None, "基尼系数=0.5"},
	BaseStyle -> FontSize -> 12]


rule=Thread[Array[Subscript[a,#]&,5]->{2,3,4,5,6}];
\[Sigma][n_,k_:5]:=Plus@@Times@@@(Map[Subscript[a,#]&,Subsets[Range[k],{n}],{2}]^2)
u=16K^2;
Subscript[t, 2]=u-4\[Sigma][2]+\[Sigma][1]^2;
Subscript[t, 3]=8\[Sigma][3]+\[Sigma][1]Subscript[t, 2];
Subscript[t, 4]=-64\[Sigma][4]+Subscript[t, 2]^2;
Subscript[t, 5]=128\[Sigma][5];
eqn=Subsuperscript[t, 3, 2] Subsuperscript[t,4, 2]+u Subsuperscript[t, 4, 3]-16 Subsuperscript[t, 3, 3] Subscript[t, 5]-18 u Subscript[t, 3] Subscript[t, 4] Subscript[t, 5]-27 u^2 Subsuperscript[t, 5, 2]==0;
sols=Solve[{eqn/.rule,K>0},Reals];
Max[K/.sols]//ToRadicals//N
sol=Reduce[{Product[I i/(2R)+Sqrt[1-(i/(2R))^2],{i,{a,b,c,d}}]==-1}]
S=Sum[i Sqrt[(R/.Solve[{sol,R>0}])^2-i^2/4],{i,{a,b,c,d}}]/2//RootReduce





f[1, k_] := 1
f[n_, k_] := Mod[f[n - 1, k] + k, n, 1]
f[10, 2]
<< Combinatorica`
Tr@Position[Josephus@##2, 1 + #2 - #] &[5, 20, 3]
Range@1000 //. {x_, y_, z___} :> {z, x}
If[# < 2, 1, Mod[#0[# - 1, #2] + #2, #, 1]] &[1*^3, 2]
2 # - 2^IntegerLength[#, 2] + 1 &[1000]
2 # - 2*2^Floor@Log2@# + 1 &@1*^3


n=8;m=2;

GridsSelect[n_,m_]:=Block[{M=ConstantArray[0,{n-m+1,n-m+1}],drop,points,p},
	drop[{x_,y_}]:=M[[Max[x-m+1,1];;Min[x+m-1,n-m+1],
		Max[y-m+1,1];;Min[y+m-1,n-m+1]]]=1;
	points=Position[M,0];p={};
	While[points!={},
		AppendTo[p,RandomChoice@Position[M,0]];
		drop[Last@p];
		points=Position[M,0]];
	Association[{"Points"->p,"Area"->1.-Length[p]m^2/n^2}]];

try[n_,m_]:=Mean[(GridsSelect[n,#]&/@ConstantArray[m,100])[[All,"Area"]]]
try[n_,1]:=1;
AbsoluteTiming[data=Table[N@try[24,i],{i,1,24}]]
ListLinePlot[data,PlotTheme->"Detailed",LabelingFunction->(#1 &)]



path=NotebookDirectory[];ok=FileNames["*",path];
in=Transpose[{Range[#//Length],#}]&@CharacterRange["一","龥"];
Img[{n_,char_}]:=Block[{name,text},
	name=StringJoin[ToString/@{path,n,"-",char,".jpg"}];
	If[MemberQ[ok,name],Return[]];
	text=Binarize@Rasterize[Style[char,FontFamily->"楷体"],RasterSize->1024,ImageSize->Large];
	Export[name,text]]

ok = FileNames["*", NotebookDirectory[]];
AbsoluteTiming[ParallelMap[Img, in];]


Nest[(Defer[#1] & )[#1/\[Placeholder]] & , 1, 30]
ToCharacterCode["Hello human!"]
StringJoin@Riffle[StringJoin[ToString/@#]&/@PadRight[IntegerDigits[#,2]&/@%]," "]





	---



	$\displaystyle \prod _{n>0} \cos \left(\frac{t}{2^n}\right) \sim \text{sinc}(x)$

$\displaystyle \prod _{n>0} \cos \left({\frac{t}{n}}\right) \sim e^{-\pi^2 x^2/64}\ \text{sinc}(2x)$


General case

	---

**Background**

	I was reading [Random Harmonic Series](http://www.stat.ualberta.ca/people/schmu/preprints/rhs.pdf).

It talks about $\displaystyle \sum _{n>0}  \xi_n \frac{1}{n}$, where $\xi_n$ is a random variable.

	Consider it's characteristic function.

	If we can find a function that approximation enough to the oragin, by calculate it's inverse fourier transform, we can get an approximate function.

	And I found : $\displaystyle F(t)=\exp\left(-\frac{\pi^2 t^2}{64}\right)\ \text{sinc}(2t)$ is a good approximation.

[enter image description here][1]

And the IFT is : $\displaystyle f(x)=\frac{1 }{8}\left[\text{erf}\left(\frac{8+4x}{\pi }\right)+\text{erf}\left(\frac{8-4 x}{\pi }\right)\right]$

This indeed close to the experimental results.

[enter image description here][2]

I tried to use this trick on other situation, but $\displaystyle \prod _{n>0} \cos \left(\frac{t}{a^b}\right) $ looks chaos. I can't figure it out.



[1]: https://i.stack.imgur.com/ijmv2.png
[2]: https://i.stack.imgur.com/1GTI2.png





b[0]:=-1/2;
b[n_]:=b[n]=-w[n,n+1]/n;
w[0,m_]:=0;
w[n_,m_]:=w[n,m]=a[m-1]+w[n-1,m]+Sum[a[j]w[n-1,m-j-1],{j,0,m-2}];
a[j_]:=u[0,j+1];
u[n_,k_]:=u[n,k]=Which[
	2^n-1==k,1,
	2^n-1>k,Sum[u[n-1,j]u[n-1,k-j],{j,0,k}],
	2^(n+1)-1>k,0,
	True,(u[n+1,k]-Sum[u[n,j]u[n,k-j],{j,k-1}])/2
];
Block[
	{$RecursionLimit=Infinity},
	(areas=Table[\[Pi](1-Sum[n b[n]^2,{n,nmax}]),{nmax,200}])
];
ListPlot[areas,PlotJoined->True,PlotStyle->Red,AxesLabel->TraditionalForm/@{n,Subscript[A, n]}]

CC
n=1000;
cost[i_]:=Block[
	{P=1,D=1,S=2,V=1,l},
	l=Drop[Divisors[i],-1];
	Join[
		Transpose[{Thread[l->i],S+i/l V}],
		{{i-1->i,P},{i+1->i,D}}
	]
]
raw=Drop[Flatten[Table[cost[i],{i,2,n+10}],1],-1];
{dir,wei}=Transpose[(SortBy[#,Last]&/@GatherBy[raw,First])[[All,1]]];
G=Graph[dir,EdgeWeight->wei,PlotTheme->"Monochrome",GraphLayout->"CircularEmbedding"];
ways=FindShortestPath[G,1,All]/@Range[n];
dis=GraphDistance[G,1,#]&/@Range[n]//Round;
\[Alpha]=a/.FindFit[dis,a Log[x],a,x]
Show[ListLinePlot@dis,Plot[a Log[x]/. {a->\[Alpha]},{x,1,n},PlotStyle->Red]]

trans[a_,b_]:=Join[
	Table[{st[a,b]->st[a,j],2},{j,b+1,a}],
	{{st[a,b]->st[a+b,b],1},{st[a,b]->st[a+1,b],1}}
];
trans[a_,0]:=Append[Table[{st[a,0]->st[a,j],2},{j,1,a}],{st[a,0]->st[a+1,0],1}];
trans[n_]:=Flatten[Table[trans[n,i],{i,0,n}],1];
GetPath[G_,n_]:=Block[{list,min,path},
	list=Table[GraphDistance[G,st[0,0],st[n,i]],{i,0,n}];
	min=Round@Flatten@Position[list,Min@list];
	path=Table[FindShortestPath[G,st[0,0],All][st[10,i]],{i,min}];
	Association["Min"->Round@Min@list,"Path"->path]
];



	>100];