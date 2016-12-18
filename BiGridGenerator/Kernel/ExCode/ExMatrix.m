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


(*本程序包中Path的格式为双重表{{x,y}},而非Mathematica中使用的列表{a,b,c}*)
Options[CiclePath]={Radius->0.25,FontSize->Scaled[1/20]};
CiclePath[matrix_,path_,OptionsPattern[]]:=
    Graphics[{MapIndexed[{Circle[#2,OptionValue[Radius]],Text[#,#2]}&,matrix,{2}],
      Arrow[#,OptionValue[Radius]]&/@Partition[path,2,1]},BaseStyle->{FontSize->OptionValue[FontSize]}];



MinPathF[mat_][i_,j_]:=MinPathF[mat][i,j]=
    mat[[i,j]]+Piecewise[{
      {Min[MinPathF[mat][i+1,j],MinPathF[mat][i,j+1]],
        i<Length[mat]&&j<Length[mat[[i]]]},
      {MinPathF[mat][i+1,j],i<Length[mat]},
      {MinPathF[mat][i,j+1],j<Length[mat[[i]]]}},0];
nextF[mat_][{i_,j_}]:=If[i<Length[mat]&&j<Length[mat[[i]]],
  If[MinPathF[mat][i+1,j]<MinPathF[mat][i,j+1],{i+1,j},{i,j+1}],
  If[i<Length[mat],{i+1,j},If[j<Length[mat[[i]]],{i,j+1},{}]]];
(*LRC=LowerRightCorner 右下角*)
Options[MatrixPathLRC]={Return->Graphics,Style->{Dividers->All,Spacings->{1.5,1.5}}};
MatrixPathLRC[mat_,Start_:{1,1},OptionsPattern[]]:=Switch[OptionValue[Return],
  Path,Most@NestWhileList[nextF[mat],Start,!#==={}&],
  Graphics,Grid[grid,#]&@Join[{Background->{Automatic,Automatic,
    Thread[MatrixPathLRC[mat,Start,Return->Path]->Red]}},OptionValue[Style]]];





End[];

EndPackage[];