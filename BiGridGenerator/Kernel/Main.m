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










End[];
Protect[DigitalCycle,TriPainting];

EndPackage[]