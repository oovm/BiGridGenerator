(* Mathematica Package *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)

(* :Title: ExForm *)
(* :Context: ExForm` *)
(* :Author: GalAster *)
(* :Date: 2016-12-06 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 11.0+ *)
(* :Copyright:该软件包遵从CC协议:BY+NA+NC(署名、非商业性使用、相同方式共享） *)
(* :Keywords: *)
(* :Discussion: *)

BeginPackage["ExForm`"];


Begin["`Private`"];
Attributes[LispForm]=HoldAll;
LispForm[exp_]:=Block[{ml,str,aaa,bbb,ccc,ddd,eee},
  ml=ImportString@ExportString[FullForm[Hold@exp],"MathML"];
  str=Cases[ml,_String,-1];
  aaa={#-1,#}&@@@Position[str,"["];
  bbb=SequenceCases[str,{a_,"["}:>{"[",a}];
  ccc=Rule@@@Partition[Flatten[Transpose/@Transpose[{aaa,bbb}]],2];
  ddd=Insert[ReplacePart[str,ccc]," ",{#+1}&@@@Position[str,"["]];
  eee=ToLowerCase@Insert[ddd," ",Position[ddd,"]"]];
  StringJoin[(eee/.{"["->"(","]"->")",","->" ","hold"->""})[[4;;-3]]]];
trans=If[Head@WolframLanguageData[ToString@#,"Translations"]===Missing,Nothing,
  ToString@#->Entity["WritingScript","SimplifiedChinese::zzc7y"]/.WolframLanguageData[ToString@#,"Translations"]]&;
LispForm[exp_,"匿天算"]:=Block[{ml,str,tra,trap,aaa,bbb,ccc,ddd,eee},
  ml=ImportString@ExportString[FullForm[Hold@exp],"MathML"];
  str=Cases[ml,_String,-1];
  tra=trans/@DeleteCases[Quiet@Union[ToExpression/@str],Hold];
  trap=tra~Join~{"["->"阴","]"->"阳",","->" ","Hold"->"匿天演算式"};
  aaa={#-1,#}&@@@Position[str,"["];
  bbb=SequenceCases[str,{a_,"["}:>{"[",a}];
  ccc=Rule@@@Partition[Flatten[Transpose/@Transpose[{aaa,bbb}]],2];
  ddd=Insert[ReplacePart[str,ccc]," ",{#+1}&@@@Position[str,"["]];
  eee=Insert[ddd," ",Position[ddd,"]"]];
  StringJoin[eee/.trap]];
Options[TriangleForm]={ColorFunction->ColorDataFunction["Black","Gradients",{0,1},If[#1<0,#1,Black]&]};
(*将一个三角式的多重表显示出来,比如杨辉三角*)
TriangleForm[triArray_List,OptionsPattern[]]:=Module[{n=Length[triArray]},
  Graphics[MapIndexed[Text[Style[#1,Large,OptionValue[ColorFunction]
  [(Min[triArray]-#1)/Subtract@@MinMax[triArray]]],
    {Sqrt[3]*(n-1+#2.{-1,2}),3*(n-First[#2]+1)}/2]&,triArray,{2}]]];



End[];

EndPackage[];