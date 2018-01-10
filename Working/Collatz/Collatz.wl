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
BeginPackage["CollatzFunction`"];
Collatz::usage = "";
(* ::Section:: *)
(*程序包正体*)
(* ::Subsection::Closed:: *)
(*主设置*)
CollatzFunction::usage = "程序包的说明,这里抄一遍";
Begin["`Private`"];
(* ::Subsection::Closed:: *)
(*主体代码*)
Collatz$Version="V1.0";
Collatz$LastUpdate="2016-11-11";
(* ::Subsubsection:: *)
(*功能块 1*)
Collatz[n_?EvenQ]:=n/2;
Collatz[n_?OddQ]:=3*n+1;
Collatz[z_?InexactNumberQ]:=(2+7 z-(2+5 z) Cos[Pi*z])/4;
Collatz/: Derivative[1][Collatz]:=(7-5 Cos[\[Pi] #]+\[Pi] (2+5#) Sin[\[Pi] #])/4&;
CollatzFix=Compile[
	{{z0,_Complex,0}},
	Module[
		{iter=0,max=3000,z=z0},
		While[iter++<max,If[Abs[z=(1+4 z-(1+2 z) Cos[Pi z])/4]>200.,Break[]]];
		z
	],
	CompilationTarget->"C",
	Parallelization->True,
	RuntimeAttributes->{Listable}
];
Collatz[z_,Fix->True]:=If[Chop[Round[#]-#]==0,Round[#],#]&[CollatzFix@z];






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
