(* Mathematica Package *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)

(* :Title: StylePainting *)
(* :Context: StylePainting` *)
(* :Author: 28059 *)
(* :Date: 2016-11-14 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: *)
(* :Copyright: (c) 2016 28059 *)
(* :Keywords: *)
(* :Discussion: *)

BeginPackage["StylePainting`"]
(* Exported symbols added here with SymbolName::usage *)

Begin["`Private`"];
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
End[] (* `Private` *)

EndPackage[]