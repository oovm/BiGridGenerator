(* Mathematica Package *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)

(* :Title: LightOut *)
(* :Context: LightOut` *)
(* :Author: 28059 *)
(* :Date: 2016-11-13 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: *)
(* :Copyright: (c) 2016 28059 *)
(* :Keywords: *)
(* :Discussion: *)

BeginPackage["LightOut`"]
(* Exported symbols added here with SymbolName::usage *)

Begin["`Private`"]
A = ConstantArray[0, {7, 7}];
DynamicModule[{pt = {0, 0}},
  Dynamic@EventHandler[
    ArrayPlot[A, ImageSize -> 512, Mesh -> True,
      MeshStyle ->
          Black], {{"MouseDown",
      1} :> (A[[Ceiling[7 - MousePosition["Graphics"][[2]]],
        Ceiling[MousePosition["Graphics"][[1]]]]] = 1), {"MouseDown",
      2} :> (A[[Ceiling[7 - MousePosition["Graphics"][[2]]],
        Ceiling[MousePosition["Graphics"][[1]]]]] = 0)}]]
Dynamic[B = {Length[A[[1]]], FromDigits[#, 2] & /@ A}]
NonogramList = B
ListToMatrix[list_] := IntegerDigits[list[[2]], 2, list[[1]]]
ArrayPlot[NonogramMatrix = ListToMatrix@NonogramList]
SplitNM = Map[Split, NonogramMatrix];
SplitMN = Map[Split, Transpose[NonogramMatrix]];
ListLift =
    Map[Length,
      Table[Select[SplitNM[[i]], MemberQ[#, 1] &], {i, 1,
        Length[SplitNM]}], {2}]
ListAbove =
    Map[Length,
      Table[Select[SplitMN[[i]], MemberQ[#, 1] &], {i, 1,
        Length[SplitMN]}], {2}]
End[] (* `Private` *)

EndPackage[]