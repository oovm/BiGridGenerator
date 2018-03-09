(* ::Package:: *)
(* ::Title:: *)
(*Example(样板包)*)
(* ::Subchapter:: *)
(*程序包介绍*)
(* ::Text:: *)
(*Mathematica Package*)
(*Created by Mathematica Plugin for IntelliJ IDEA*)
(*Establish from GalAster's template*)
(**)
(* ::Text:: *)
(*Author:我是作者*)
(*Creation Date:我是创建日期*)
(*Copyright: Mozilla Public License Version 2.0*)
(* ::Program:: *)
(*1.软件产品再发布时包含一份原始许可声明和版权声明。*)
(*2.提供快速的专利授权。*)
(*3.不得使用其原始商标。*)
(*4.如果修改了源代码，包含一份代码修改说明。*)
(**)
(* ::Text:: *)
(*这里应该填这个函数的介绍*)
(* ::Section:: *)
(*函数说明*)
BeginPackage["ExSequence`"];
NestOrbit::usage = "";
OrbitLength::usage = "";
OrbitLink::usage = "";
(* ::Section:: *)
(*程序包正体*)
(* ::Subsection::Closed:: *)
(*主设置*)
ExSequence::usage = "程序包的说明,这里抄一遍";
Begin["`Private`"];
(* ::Subsection::Closed:: *)
(*主体代码*)
ExSequence$Version="V1.0";
ExSequence$LastUpdate="2016-11-11";
(* ::Subsubsection:: *)
(*功能块 1*)
Options[NestOrbit]={MaxIterations->1024};
Options[OrbitLink]={MaxIterations->1024};
NestOrbit[f_Symbol,n_Integer,OptionsPattern[]]:=NestWhileList[f,n,UnsameQ,All,OptionValue[MaxIterations]];
OrbitLength[f_Symbol,n_Integer,OptionsPattern[]]:=Length@NestOrbit[f,n,MaxIterations->OptionValue[MaxIterations]]-1;
OrbitLink[f_Symbol,n_Integer,OptionsPattern[]]:=Block[
	{set,nest=NestOrbit[f,#,MaxIterations->OptionValue[MaxIterations]]&},
	set=nest/@Range[n];
	Union[DirectedEdge@@@Join@@(Partition[#,2,1]&/@set)]
];



(* ::Subsubsection:: *)
(*功能块 2*)
ExampleFunction[2]="我就是个示例函数,什么功能都没有";


(* ::Subsection::Closed:: *)
(*附加设置*)
End[] ;
SetAttributes[
	{ },
	{Protected,ReadProtected}
];
EndPackage[];
