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
  -(f[0]/2)+I*Integrate[(f[I*t]-f[(-I)*t])/(E^(2*Pi*t)-1),{t,0,Infinity}]];
ImproperSum[function_]:=Module[{ans,name},
  ans=Join[Summation[function],{RamanujanSummation[function]}];
  name={"Cauchy","Abel","Euler","Cesaro","Dirichlet","Borel","Ramanujan"};
  TableForm@Transpose[{name,If[Head@#===Sum,"Undefinited",#]&/@ans}]];
Options[ColorForm]={Form->StandardForm,Color->"TemperatureMap"};
ColorForm[expr_,OptionsPattern[]]:=With[{colored=DisplayForm[ToBoxes[expr,OptionValue[Form]]/.s_String:>
    With[{x=Quiet@ToExpression@s},RowBox@List@StringReplace[ToBoxes@s,
      (ToString[#]->"\*\n"<>ToString@ToBoxes@Style[#,ColorData[OptionValue[Color]]
      [If[NumberQ[OptionValue[Color]],#,#/10]]])&/@Range[0,9]]/;MatchQ[x,_Real|_Integer]]]},Interpretation[colored,expr]];
fontstyle=((Translate[#1,{-4.5,-10}]&)/@First[First[ImportString[ExportString[
  Style[#1,FontSize->24,FontFamily->"Arial"],"PDF"],
  "PDF","TextMode"->"Outlines"]]]&)/@Join[{"."},CharacterRange["0","9"]];
DigitalCycle[num_,digits_:5000,start_:Pi/4,fontsize_:0.0655]:=Module[{list},
  list=Insert[#[[1]],-1,1+#[[2]]]&@RealDigits[num,10,digits];
  Graphics[MapIndexed[With[{angle=(-(#2[[1]]-2)+Switch[#2[[1]],1,-0.1,2,0,_,0.6])*fontsize},
    With[{scale=(1-1.5*fontsize)^(-angle/(2*Pi))},GeometricTransformation[fontstyle[[#1+2]],
      ScalingTransform[{1,1}*0.1*fontsize*scale]/*TranslationTransform[{0,scale}]/*
          RotationTransform[start+angle]]]]&,list],PlotRange->{{-1.1,1.1},{-1.1,1.1}}]];
(*数盘*)
clusterSector[gap_][{{xmin_,xmax_},y_},rest___]:=
    Block[{ngap=Min[(xmax-xmin)/2,gap]},{EdgeForm[White],
      ChartElementData["Sector"][{{xmin+ngap,xmax-ngap},y},rest]}];
iCoord[{i_,j_},bin_:60]:=Through[{Cos,Sin}[Pi/2-\[Pi]/5i-(\[Pi]/5)/bin(j-1)-0.025]];
iCurve[{x_,y_},rad_:15,bin_:60,colorf_:ColorData[35]]:=
    Block[{s,t,range,c1,c2},{s,t}=iCoord[#,bin]&/@{x,y};
    {c1,c2}=colorf/@{x[[1]],y[[1]]};
    range=Range[0,1,.1];
    Line[BezierFunction[rad{s,{0,0}+.4Normalize[(s+t)],t}]/@range,
      VertexColors->(Blend[{c1,c2},#]&/@range)]];
DigitalSector[num_,digits_:1000,style_:35,fontsize_:30]:=
    Module[{digit,count,cdigits,curves},
      digit=First@RealDigits[num,10,digits];
      count=Association[Thread[Range[0,9]->Table[1,10]]];
      cdigits=Partition[{#,count[#]++}&/@digit,2,1];
      curves=iCurve[#,15.5,Max[cdigits],ColorData[style]]&/@cdigits;
      Show[{PieChart[Table[1,10],
        SectorOrigin->{{Pi/2,"Clockwise"},16},PerformanceGoal->"Speed",
        ChartElementFunction->clusterSector[0.02],
        ChartLabels->Placed[Table[Rotate[Style[i,15,White,FontFamily->"Arials"],-(18+36i)Degree],
          {i,0,9}],{1/2,1.8}],ChartStyle->style,Background->Black],
        Graphics[{{Opacity[.4],curves},Text[Style[ToString[num,StandardForm],
          White,fontsize,Bold],{0,0}]}]}]];
(*取模覆写优化*)
Unprotect[Mod,PowerMod];
Mod[x_,y_]:=1/;x==1||y==1;
PowerMod[x_,y_,z_]:=1/;x==1||z==1;
(*ET=ExponentialTower*)
ModEt[{a_},n_]:=Mod[a,n];
ModEt[{a_,b_},n_]:=PowerMod[a,b,n];
ModEt[list_?VectorQ,n_]:=Block[
  {a=First@list,bc=Rest@list,g,d,f},
  g=GCD[a,n];d=a/g;f=n/g;
  If[a==1||n==1,Return[1]];
  If[g==1,Return[PowerMod[a,ModEt[bc,EulerPhi[n]],n]]];
  Mod[g Mod[PowerMod[g,Mod[(ModEt[bc,EulerPhi[f]]-1),
    EulerPhi[f]],f]PowerMod[d,ModEt[bc,EulerPhi[f]],f],f],n]];
ModBi::win="错误的输入,请保证所有数都是正数且a>b";
ModBi[r_?IntegerQ,t_,n_]:=Block[
  {{a,b}={r,Min[t,r-t]}},
  If[0<=r<=t,None,Message[ModBi::win]];
  If[PrimeQ@n,Return@LucasTheorem[a,b,n]];
  If[SquareFreeQ@n,Return@SquareFreeMod[a,b,n]];
  AndrewMod[a,b,n]];
LucasTheorem[a_,b_,p_?PrimeQ]:=Block[
  {pn=IntegerDigits[#,p]&/@{a,b},sn},
  sn=PadLeft[#,Max[Length/@pn]]&/@pn;
  sn=PadLeft[#,Max[Length/@pn]]&/@pn;
  Mod[Times@@Mod[Binomial@@@Transpose@sn,p],p]];
(*(*卢卡斯定理正确性检验*)
a=Table[Mod[Binomial[i,j],13],{i,1,100},{j,1,100}];
b=Table[LucasTheorem[i,j,13],{i,1,100},{j,1,100}];
a\[Equal]b*)
SquareFreeMod[a_,b_,p_?SquareFreeQ]:=Block[
  {pp=FactorInteger[p][[All,1]],xx},
  xx=LucasTheorem[a,b,#]&/@pp;
  ChineseRemainder[xx,pp]];
(*(*无平方因子正确性验证*)
a=Table[SquareFreeMod[i,50,230],{i,50,200}];
b=Table[Mod[Binomial[i,50],230],{i,50,200}];
a\[Equal]b*)
ModFa[n_,p_]:=Fold[Mod[#1 #2,p]&,Range[n]];



End[] ;

EndPackage[];