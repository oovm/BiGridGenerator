(* Mathematica Package *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)

(* :Title: ExRandomWalk *)
(* :Context: ExRandomWalk` *)
(* :Author: 28059 *)
(* :Date: 2016-11-20 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: *)
(* :Copyright: (c) 2016 28059 *)
(* :Keywords: *)
(* :Discussion: *)

BeginPackage["ExRandomWalk`"]
ExRandomWalk[max_, "2DGridSelfAvoiding"] :=
    Module[{i = 0, nmax = max, pts = {{0, 0}},
      moves = {{-1, 0}, {0, 1}, {1, 0}, {0, -1}}},
      While[i < nmax &&
          Not@(And @@ (MemberQ[pts, #] & /@
              Table[pts[[-1]] + moves[[i]], {i, 1, 4}])), i++;
      AppendTo[pts,
        RandomChoice[
          Select[Table[pts[[-1]] + moves[[i]], {i, 1, 4}],
            Not@MemberQ[pts, #] &]]]];
      TemporalData[Transpose@#, {Range@Length@#}] &@pts];
ExRandomWalk[max_, "3DGridSelfAvoiding"] :=
    Module[{notvisitedQ, SARW, pts}, notvisitedQ[_] := True;
    SARW = {#1, Select[Flatten[Outer[Plus, {#1}, {{1, 0, 0}, {-1, 0, 0},
      {0, 1, 0}, {0, -1, 0}, {0, 0, 1}, {0,
        0, -1}}, 1], 1], notvisitedQ]} & ;
    pts = NestWhileList[SARW[
      notvisitedQ[#1[[1]]] = False;
      RandomChoice[#1[[2]]]] & ,
      {{0, 0,
        0}, {{1, 0, 0}, {-1, 0, 0}, {0, 1, 0}, {0, -1, 0}, {0, 0,
        1}, {0, 0, -1}}},
      #1[[2]] =!= {} & , 1, max - 1];
    TemporalData[Transpose@#, {Range@Length@#}] &@    (First /@ pts)]
RandomWalkPlot[in_] := Module[{pts = Transpose@in["ValueList"]},
  dim2 = Graphics[Line@pts,
    Epilog -> {PointSize[Large], RGBColor[.6, .74, .36],
      Point[{0, 0}], RGBColor[.9, .42, .17], Point[Last@pts],
      PointSize[Medium], Black,
      Table[Point[pts[[i]]], {i, 2, Length[pts] - 1}]},
    PlotRange -> All];
  dim3 = Graphics3D[{Thick, Gray, Line[pts],
    RGBColor[0.6, 0.74, 0.36], Sphere[pts[[1]], 1],
    RGBColor[0.9, 0.42, 0.17], Sphere[pts[[-1]], 1]},
    PlotLabel ->
        Style[If[Length[pts] < max, StringJoin["STUCK in ",
          ToString[Length[pts] - 1],
          If[Length[pts] == 1, " move!!", " moves!!"]],

          StringJoin["free after ", ToString[max - 1],
            If[Length[pts] - 1 == 1, " move",
              " moves"]]], "Label", 12],
    ImageSize -> {500, 500}, PlotRange -> All,
    Boxed -> False];
  If[Length@pts[[1]] === 2, dim2, dim3]]
ExRandomWalk[2000, "3DGridSelfAvoiding"]
RandomWalkPlot[%]

Begin["`Private`"]

End[] (* `Private` *)

EndPackage[]