BeginPackage["Main`"]

DigitalCycle::usage = "DigitalCycle可以生成螺旋数字图.;"
TriPainting::usage = "TriPainting可以将图片转变为三角画风;"


Begin["`Private`"];
PlayList[n_] := Module[{foo, up},
  foo[p_] := Range[5 (-1 + p) p, p (4 + 5 p), p];
  up = Floor[-(2/5) + 1/5 Sqrt[4 + 5 n]];
  Drop[Flatten@{foo /@ Range[up], Range[5 up (up + 1) , n , up + 1]},1]];



fontstyle = ((Translate[#1, {-4.5, -10}] &) /@
    First[First[ImportString[ExportString[
          Style[#1, FontSize -> 24, FontFamily -> "Arial"], "PDF"],
        "PDF", "TextMode" -> "Outlines"]]] &) /@
    Join[{"."}, CharacterRange["0", "9"]];
DigitalCycle[num_, digits_: 5000, start_: Pi/4, fontsize_: 0.0655] :=
    Graphics[MapIndexed[
      With[{angle = (-(#2[[1]] - 2) +Switch[#2[[1]], 1, -0.1, 2, 0, _, 0.6])*fontsize},
        With[{scale = (1 - 1.5*fontsize)^(-angle/(2*Pi))},
          GeometricTransformation[fontstyle[[#1 + 2]],
            ScalingTransform[{1, 1}*0.1*fontsize*scale] /*
                TranslationTransform[{0, scale}] /*
                RotationTransform[start + angle]]]] &,
      list = Insert[First[RealDigits[num, 10, digits]], -1, 2]],
      PlotRange -> {{-1.1, 1.1}, {-1.1, 1.1}}];

Venn[n_, ineqs_: {}] :=
    Module[{i, r = .6, R = 1, v, grouprules, x, y, x1, x2, y1, y2, ve},
      v = Table[Circle[r {Cos[#], Sin[#]} &[2 Pi (i - 1)/n], R], {i, n}];
      {x1, x2} = {Min[#], Max[#]} &[Flatten@Replace[v,Circle[{xx_, yy_}, rr_] :> {xx - rr, xx + rr}, {1}]];
      {y1, y2} = {Min[#], Max[#]} &[Flatten@Replace[v,Circle[{xx_, yy_}, rr_] :> {yy - rr, yy + rr}, {1}]];
      ve[x_, y_, i_] :=v[[i]] /. Circle[{xx_, yy_}, rr_] :> (x - xx)^2 + (y - yy)^2 < rr^2;
      grouprules[x_, y_] = ineqs /.Table[With[{is = i}, Subscript[_, is] :> ve[x, y, is]], {i, n}];
      Show[If[MatchQ[ineqs, {} | False], {},
        RegionPlot[grouprules[x, y], {x, x1, x2}, {y, y1, y2},Axes -> False]], Graphics[v],
        PlotLabel ->TraditionalForm[Replace[ineqs, {} | False -> \[EmptySet]]], Frame -> False]];

ReflectFunction[f_, {left_: - 4, right_: 4, n_: 20}] :=
    Manipulate[ With[{df = D[f, x]},
        Plot[f, {x, left, right},
          Filling -> -100,
          PlotRange -> {{left, right}, Automatic},
          FillingStyle -> RGBColor[0.848, 0.848, 0.92],
          Prolog -> {Lighter[Orange],
            Table[{Line[{{xi, f /. x -> xi}, {xi, Max[10, f /. x -> xi]}}],
              Orange, Line[{{xi, f /. x -> xi},
              N[{xi - length*(2*(df/(1 + df^2))),f + length*(-1 + 2/(1 + df^2))}]} /. x -> xi]},
            {xi,left, right, (right - left)/n}]}]], {{length, 16, "反射线长度"}, 0,16}];

ReflectCircle =
    Module[{p, q, inref},inref[pt1_, pt2_,k_] := ({Re[#1], Im[#1]} &)[(#2*(#2/#1)^k &)[pt1.{1, I},pt2.{1, I}]];
      Manipulate[Dynamic[p = {-1, 0}; q = {Cos[x], Sin[x]};
      With[{inrefs = (inref[p, q, #1] &) /@ Range[-1, s]},
        Graphics[Flatten[{GrayLevel[0.3], Disk[], White, Line[inrefs]}]]]],
        {{x,0, "入射点"}, -Pi, Pi}, {{s, 120, "反射次数"}, 0, 120, 1}]];

ReflectEllipse[aa_, bb_] :=Manipulate[ellipseMultiReflectionGraphics[\[CurlyPhi], \[Alpha],n, {aa, bb}],  {{\[CurlyPhi], -Pi}, -Pi, Pi}, {{n, 1}, 1, 50,1}, {{\[Alpha], \[Pi]/4}, -Pi/2, Pi/2},
      Initialization :> {ellipseMultiReflectionGraphics[\[CurlyPhi]_, \[Alpha]_, n_, {a_, b_}] :=
            Module[{parameters, pathData, pathPoints},
              parameters = N[{\[CurlyPhi], \[Alpha], n, {a, b}}, 20 + 4*n];
              pathData = multiReflections @@ parameters;
              pathPoints = N[First /@ pathData];
              Graphics[{{GrayLevel[0.9], Disk[{0, 0}, {a, b}]}, {Black,PointSize[0.016],
                N[If[a >= b,Point[{{Sqrt[a^2 - b^2], 0}, {-Sqrt[a^2 - b^2], 0}}],
                  Point[{{0,Sqrt[b^2 - a^2]}, {0, -Sqrt[b^2 - a^2]}}]]]}, {Black,Thickness[0.006], Point[pathPoints[[1]]]},
                {Black, PointSize[0.02], Circle[{0, 0}, {a, b}]}, {Thickness[0.001],
                  MapIndexed[{Hue[0.3*(#2[[1]]/n) + 0.6], Line[#1]} & , Partition[pathPoints, 2, 1]]}}]],
        multiReflections[\[CurlyPhi]_, \[Alpha]_, n_, {a_, b_}] := NestList[ propagate[#1, {a, b}] & , {{a*Cos[\[CurlyPhi]],b*Sin[\[CurlyPhi]]}, {a*Cos[\[Alpha]],b*Sin[\[Alpha]]}}, Round[n]],
        propagate[{p_, dir_}, {a_, b_}] := reflect[{nextReflectionPoint[{p, dir}, {a, b}], dir}, {a, b}],
        nextReflectionPoint[{p_, dir_}, {a_, b_}] := Module[{solxy, distances, pos, x, y},
              solxy = {x, y} /.Quiet[Solve[{x == p[[1]] + s*dir[[1]], y == p[[2]] + s*dir[[2]], x^2/a^2 + y^2/b^2 == 1}, {s, x, y}]];
              distances = (Norm[p - #1] & ) /@ solxy;
              pos = Position[distances, Max[distances]][[1]]; (solxy[[#1]] & ) @@ pos],
        reflect[{p_, dir_}, {a_, b_}] :=Module[{normalDir, parallelDir, normalComponent, paralleComponent},
              normalDir = (-(#1/Norm[#1]) & )[p/{a^2, b^2}];
              parallelDir = Reverse[normalDir]*{-1, 1};
              normalComponent = normalDir . dir;
              paralleComponent = parallelDir . dir;
              {p, (-normalComponent)*normalDir +paralleComponent*parallelDir}]}];

Catacaustic[f_, {left_, right_, down_, up_}, n_: 200] :=
    Module[{refl$y$, sol$, F},F[x_, y_, a_,g_] := (2*Derivative[1][g][a])*(y -g[a]) - (Derivative[1][g][a]^2 - 1)*(x - a);
      With[{},sol$ = Simplify[First[Solve[{F[x, y, t, f] == 0, D[F[x, y, t, f], t] == 0}, {x,y}]]];
              refl$y$ = y /. First[Solve[F[x, y, t, f] == 0, y]];
        Show[Graphics[{Opacity[0.2], White,Line[Table[{{a, up + 1}, {a,f[a]}, {(-Sign[Derivative[1][f][a]])*5,
            refl$y$ /. {x -> (-Sign[Derivative[1][f][a]])*5,t -> a}}}, {a, left + (right - left)/(2.*n),right, (right - left)/n}]]}],
          Sequence @@ (ParametricPlot[Tooltip[{x, y} /. sol$], {t, #1[[1]], #1[[2]]},
            PlotStyle -> {White, Thickness[Medium]}] &) /@ {{-10, 10}},
          Plot[Tooltip[f[x]], {x, left, right},PlotStyle -> {Thick, ColorData[1][2]}],
          PlotRange -> {{left, right}, {up, down}}, Axes -> False,Background -> Black]]];

Catacaustic2[f_, {left_, right_, n_: 100}, a_: 1] :=
    Module[{}, point = N@Range[left, right, (right - left)/n];
    line[x_] := {HalfLine[{{x, f[x]}, {x, f[x] + 1}}],
      HalfLine[{{x, f[x]}, {x - a Sin[2 ArcTan[Derivative[1][f][x]]],
        f[x] + a Cos[2 ArcTan[Derivative[1][f][x]]]}}]};
    p1 = Plot[f[x], {x, left, right}, AspectRatio -> 1,PlotTheme -> "Business"];
    curve = {t -Derivative[1][f][t]/Derivative[2][f][t], (1 - Derivative[1][f][t]^2)/(2*Derivative[2][f][t]) + f[t]};
    p2 = ParametricPlot[curve, {t, 0, 10},PlotStyle -> Directive[Red, Thick]];
    p3 = Graphics[{Thin, Flatten[line /@ point]}];
    Show[p1, p2, p3]];

TangentComplex[function_] := Module[{f0,f1,f2},
  f0 = FromPolarCoordinates[{function, \[Theta]}];
  f1 = FullSimplify[Last[f0] + D[Last[f0], \[Theta]]*(x - First[f0])/D[First[f0], \[Theta]]];
  f2 = Simplify[N[Table[f1, {\[Theta], 0, 10, 0.1}]]];
  Plot[f2, {x, -1, 4}, PlotRange -> 3,
    PlotStyle -> Directive[Black, Thin], AspectRatio -> 1,
    Axes -> False, PlotLabel -> y == f1]];

Epicycloid[n_] := Manipulate[
  Show[Graphics[Style[Line[Partition[Riffle[Table[{-Cos[i], Sin[i]}, {i, 0, ((n + 1)*lin)*(Pi/32), Pi/32}],
    Table[{Cos[i/(n + 1)], -Sin[i/(n + 1)]}, {i, 0, ((n + 1)*lin)*(Pi/32), Pi/32}]], 2]], Darker[Purple]]],
    ParametricPlot[{Sin[f], Cos[f]},{f, 0, 2*Pi},
    PlotStyle -> {Hue[1/3, 1, 0.4, 0.9], Thickness[0.006]}],
    PlotLabel ->FullSimplify[{(n/(n + 2) + 1/(n + 2))*Cos[t] - (1/(n + 2))* Cos[(n + 1)*t], (n/(n + 2) + 1/(n + 2))*Sin[t] - (1/(n + 2))*Sin[(n + 1)*t]}],
    ImageSize -> {400, 400}],   {{lin, 64, "lines"}, 0, 64, 1, Appearance -> "Labeled"}]

MineLayout[{m_, n_, k_}] := Module[{M, foo, bar, list},
  M = ConstantArray[0, {m + 2, n + 2}];
  foo[{x_, y_}] := M[[x - 1 ;; x + 1, y - 1 ;; y + 1]] += 1;
  bar[{x_, y_}] := M[[x, y]] = 10;
  list = RandomSample[Tuples[{Range[2, m + 1], Range[2, n + 1]}]][[1 ;; k]];
  Do[foo@list[[i]], {i, k}]; bar /@ list; M[[2 ;; -2, 2 ;; -2]]];
MineDistribution[m_, n_, k_, p_] :=
    Transpose@{Range[0, 10],BinCounts[Flatten[MineLayout /@ ConstantArray[{m, n, k}, p]], {-0.5, 10.5,1}]/p + 0.0};

(*Mondrian[大小,复杂度,比例]*)
Mondrian[p_, complex_: 6, ratio_: 0.7] := Module[{splitx, splity, f, cols},
      splitx[Rectangle[{x0_, y0_}, {x1_, y1_}]] :=
          Module[{a = RandomInteger[{x0 + 1, x1 - 1}]}, {Rectangle[{x0, y0}, {a, y1}],Rectangle[{a, y0}, {x1, y1}]}];
      splity[Rectangle[{x0_, y0_}, {x1_, y1_}]] :=
          Module[{a = RandomInteger[{y0 + 1, y1 - 1}]}, {Rectangle[{x0, y0}, {x1, a}],Rectangle[{x0, a}, {x1, y1}]}];
      f = ReplaceAll[r : Rectangle[{x0_, y0_}, {x1_, y1_}] :> RandomChoice[{(x1 - x0)^2, (y1 - y0)^2, 5} -> {splitx, splity,Identity}]@r];
      cols = MapThread[Darker, {{Black, White, Yellow, Red, Blue}, {0, 0.1, 0.1, 0.15,0.3}}];
      Graphics[{EdgeForm@Thickness[0.012], {FaceForm@RandomChoice@cols, #} & /@
          Flatten@Nest[f, Rectangle[{0, 0}, {p, p}], complex]},AspectRatio -> ratio]];





MaxPlot[{Sin[x], Cos[x], 0.2}, {x, -10, 10}]
MinPlot[{Sin[x], Cos[x], 0.2}, {x, -1, 1}]
MaxPlot3D[{Sin[x] Cos[y], Cos[x] Sin[y]}, {x, -Pi, Pi}, {y, -Pi, Pi},
  ViewPoint -> {0, 0, \[Infinity]}]
MinPlot3D[{Sin[x] Cos[y], Cos[x] Sin[y]}, {x, -Pi, Pi}, {y, -Pi, Pi},
  ViewPoint -> {0, 0, \[Infinity]}]


SingleElectronGrid[k_, way_: Re, ops___] :=
    GraphicsGrid[
      Table[SphericalPlot3D[
        Evaluate@
            way@SphericalHarmonicY[l, m, \[Theta], \[Phi]], {\[Theta], 0,
          Pi}, {\[Phi], 0, 2 Pi}, PlotRange -> All, Mesh -> None,
        Boxed -> False, Axes -> None, ColorFunction -> "Rainbow",
        ops], {l, 0, k - 1}, {m, 0, l}], Frame -> All]
SingleElectron[l_, m_, ops___] :=
    GraphicsRow[
      SphericalPlot3D[#[
          SphericalHarmonicY[l, m, \[Theta], \[CurlyPhi]]], {\[Theta], 0,
        Pi}, {\[CurlyPhi], 0, 2 Pi}, Boxed -> False, Axes -> None,
        ColorFunction -> "TemperatureMap", ops] & /@ {Re, Abs},
      Frame -> All]






End[];
Protect[DigitalCycle,TriPainting];

EndPackage[]