(* Mathematica Package *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)

(* :Title: CardGames *)
(* :Context: CardGames` *)
(* :Author: GalAster *)
(* :Date: 2016-12-16 *)

(* :Package Version: 0.1 *)
(* :Update: 2016-12-16 *)
(* :Mathematica Version: 11.0+ *)
(* :Copyright:该软件包遵从CC协议:BY+NA+NC(署名、非商业性使用、相同方式共享） *)
(* :Keywords: *)
(* :Discussion: *)

BeginPackage["CardGames`"];
ShowHardQ::usage = "给出一个梭哈牌局的胜负向量";

Begin["`Private`"];
(*FCS=Five Card Stud*)
SetAttributes[FCScore,Orderless];
FCSass1=Union@Differences[List@@#1[[All,1]]]=={1}&;
FCSass2={#2,Max@@#[[All,1]]}&;
FCSrule={
  w:FCScore[{_,t_}..]?FCSass1:>FCSass2[w,9],
  FCScore[{a_,_}..,_]:>{8,a},
  w:FCScore[{a_|b_,_}..]:>FCSass2[w,7],
  w:FCScore[{_,t_}..]:>FCSass2[w,6],
  w_FCScore?FCSass1:>FCSass2[w,5],FCScore[{a_,_}..,_,_]:>{4,a},
  FCScore[{a_|b_,_}..,_]:>{3,a~Max~b},
  FCScore[{a_,_}..,_,_,_]:>{2,a},w_FCScore:>FCSass2[w,1]};
ShowHardQ[case_]:=If[OrderedQ[Apply[FCScore,Characters@Partition[case,5]/.
    Thread[Characters@"23456789TJQKA"->Range[2,14]], {1}] /. FCSrule], {1, 0}, {0, 1}];


End[];

EndPackage[];