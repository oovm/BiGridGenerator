(* Mathematica Package *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)

(* :Title: ExMatrix *)
(* :Context: ExMatrix` *)
(* :Author: GalAster *)
(* :Date: 2016-11-22 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 11.0+ *)
(* :Copyright:该软件包遵从CC协议:BY+NA+NC(署名、非商业性使用、相同方式共享） *)
(* :Keywords: *)
(* :Discussion: *)

BeginPackage["ExMatrix`"];


Begin["`Private`"];
SpiralMatrix[n_?OddQ]:=Permute[Range[n^2],Accumulate@Take[Join[{n^2+1}/2,
  Flatten@Table[(-1)^ji,{j,n},{i,{-1,n}},{j}]],n^2]]~Partition~n;
SpiralMatrix[n_]:=SpiralMatrix[n+1][[1;;-2,2;;-1]];
MagicMatrix[n_]:=MagicSquare`Magic[n];



End[];

EndPackage[];