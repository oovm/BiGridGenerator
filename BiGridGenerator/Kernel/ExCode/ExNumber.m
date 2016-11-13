(* Mathematica Package *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)

(* :Title: ExNumber *)
(* :Context: ExNumber` *)
(* :Author: GalAster *)
(* :Date: 2016-11-11 *)

(* :Package Version: 0.2 *)
(* :Mathematica Version: 11.0+ *)
(* :Copyright:该软件包遵从CC协议:BY+NA+NC(署名、非商业性使用、相同方式共享） *)
(* :Keywords: *)
(* :Discussion: *)

BeginPackage["ExNumber`"];
MultPrime::usage = "PlusPrime[n]生成n以内所有可以由两个素数相乘得到的整数";
PlusPrime::usage = "PlusPrime[n]生成n以内所有可以由两个素数相加得到的整数";
ManyPrime::usage = "ManyPrime[n]生成n以内所有可以由s个素数相乘得到的整数";
DisplaySum::usage = "DisplaySum[f[n],{n,a,b}]显示这个级数的和";
ImproperSum::usage = "ImproperSum[f[n]]尝试各种手段对f[n]进行无穷求和";



Begin["`Private`"];
MultPrime[n_]:=Union@@Table[p*TakeWhile[Prime[Range[PrimePi[n]]],p*#1<n&],{p,TakeWhile[l,#1<Sqrt[n]&]}];
PlusPrime[n_]:=Union@@Table[p+TakeWhile[Prime[Range[PrimePi[n]]],p*#1<n&],{p,TakeWhile[l,#1<Sqrt[n]&]}];
ManyPrime[n_,s_]:=Select[Range[n],PrimeOmega[#1]==s&];
DisplaySum[a_,{n_,n1_,n2_},opts:OptionsPattern[]]/;n1<=n2:=
    Module[{nf=Min[n1+OptionValue["Terms"]-1,n2]},Row[{Defer[Sum[a,{n,n1,n2}]],
      Composition[Defer,Plus]@@Append[Table[a,{n,n1,nf}],If[n2===\[Infinity],"\[CenterEllipsis]",Nothing]],Sum[a,{n,n1,n2}]},"="]];
Summation[fun_]:=Sum[fun,{n,1,Infinity},Regularization->#]&/@{"None","Abel","Euler","Cesaro","Dirichlet","Borel"};
RamanujanSummation[fun_]:=Module[{f},
  f=Function[Evaluate@Variables[Level[fun,{-1}]],Evaluate@fun];
  -(f[0]/2)+I*Integrate[(f[I*t]-f[(-I)*t])/(E^(2*Pi*t)-1),{t,0,Infinity}]]
ImproperSum[function_]:=Module[{ans,name},
  ans=Join[Summation[function],{RamanujanSummation[function]}];
  name={"Cauchy","Abel","Euler","Cesaro","Dirichlet","Borel","Ramanujan"};
  TableForm@Transpose[{name,If[Head@#===Sum,"Undefinited",#]&/@ans}]];








End[] ;

EndPackage[];