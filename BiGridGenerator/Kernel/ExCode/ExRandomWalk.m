(* Mathematica Package *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)

(* :Title: ExRandomWalk *)
(* :Context: ExRandomWalk` *)
(* :Author: GalAster *)
(* :Date: 2016-11-20 *)

(* :Package Version: 0.1 *)
(* :Update: 2016-11-20 *)
(* :Mathematica Version: 11.0+ *)
(* :Copyright:该软件包遵从CC协议:BY+NA+NC(署名、非商业性使用、相同方式共享） *)
(* :Keywords: *)
(* :Discussion: *)

BeginPackage["ExRandomWalk`"];
Begin["`Private`"];
ExRandomWalk[max_,"2DGridSelfAvoiding"]:=Module[{i=0,pts={{0,0}},moves={{-1,0},{0,1},{1,0},{0,-1}}},
  While[i<max&&Not@(And@@(MemberQ[pts,#]&/@Table[pts[[-1]]+moves[[i]],{i,1,4}])),i++;
  AppendTo[pts,RandomChoice[Select[Table[pts[[-1]]+moves[[i]],{i,1,4}],Not@MemberQ[pts,#]&]]]];
  TemporalData[Transpose@#,{Range@Length@#}]&@pts];
ExRandomWalk[max_,"3DGridSelfAvoiding"]:=Module[{notvisitedQ,SARW,pts},
  notvisitedQ[_]:=True;
  SARW={#1,Select[Flatten[Outer[Plus,{#1},{{1,0,0},{-1,0,0},{0,1,0},{0,-1,0},{0,0,1},{0,0,-1}},1],1],notvisitedQ]}&;
  pts=NestWhileList[SARW[notvisitedQ[#1[[1]]]=False;
  RandomChoice[#1[[2]]]]&,{{0,0,0},{{1,0,0},{-1,0,0},{0,1,0},{0,-1,0},{0,0,1},{0,0,-1}}},#1[[2]]=!={}&,1,max-1];
  TemporalData[Transpose@#,{Range@Length@#}]&@(First/@pts)];

SetAttributes[RandomWalkPlot,HoldAll];
RandomWalkPlot[ExRandomWalk[max_,"2DGridSelfAvoiding"]]:=
    Module[{pts=Transpose[ExRandomWalk[max,"2DGridSelfAvoiding"]["ValueList"]]},
      Graphics[Line@pts,Epilog->{PointSize[Large],RGBColor[.6,.74,.36],Point[{0,0}],RGBColor[.9,.42,.17],
        Point[Last@pts],PointSize[Medium],Black,Table[Point[pts[[i]]],{i,2,Length[pts]-1}]},PlotRange->All]];
RandomWalkPlot[ExRandomWalk[max_,"3DGridSelfAvoiding"]]:=
    Module[{pts=Transpose[ExRandomWalk[max,"3DGridSelfAvoiding"]["ValueList"]]},
      Graphics3D[{Thick,Gray,Line[pts],RGBColor[0.6,0.74,0.36],Sphere[pts[[1]],1],RGBColor[0.9,0.42,0.17],Sphere[pts[[-1]],1]},
        PlotLabel->Style[If[Length[pts]<max,StringJoin[ToString[Length[pts]-1],"步后卡住了!!!"],StringJoin[ToString[max-1],"步后逃逸!"]],"Label",12],
        PlotRange->All,Boxed->False]];

End[];

EndPackage[];