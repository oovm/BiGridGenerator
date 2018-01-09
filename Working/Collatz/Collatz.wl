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



b[0]:=-1/2;
b[n_]:=b[n]=-w[n,n+1]/n;
w[0,m_]:=0;
w[n_,m_]:=w[n,m]=a[m-1]+w[n-1,m]+Sum[a[j]w[n-1,m-j-1],{j,0,m-2}];
a[j_]:=u[0,j+1];
u[n_,k_]:=u[n,k]=Which[
	2^n-1==k,1,
	2^n-1>k,Sum[u[n-1,j]u[n-1,k-j],{j,0,k}],
	2^(n+1)-1>k,0,
	True,(u[n+1,k]-Sum[u[n,j]u[n,k-j],{j,k-1}])/2
];
Block[
	{$RecursionLimit=Infinity},
	(areas=Table[\[Pi](1-Sum[n b[n]^2,{n,nmax}]),{nmax,200}])
];
ListPlot[areas,PlotJoined->True,PlotStyle->Red,AxesLabel->TraditionalForm/@{n,Subscript[A, n]}]

CC
n=1000;
cost[i_]:=Block[
	{P=1,D=1,S=2,V=1,l},
	l=Drop[Divisors[i],-1];
	Join[
		Transpose[{Thread[l->i],S+i/l V}],
		{{i-1->i,P},{i+1->i,D}}
	]
]
raw=Drop[Flatten[Table[cost[i],{i,2,n+10}],1],-1];
{dir,wei}=Transpose[(SortBy[#,Last]&/@GatherBy[raw,First])[[All,1]]];
G=Graph[dir,EdgeWeight->wei,PlotTheme->"Monochrome",GraphLayout->"CircularEmbedding"];
ways=FindShortestPath[G,1,All]/@Range[n];
dis=GraphDistance[G,1,#]&/@Range[n]//Round;
\[Alpha]=a/.FindFit[dis,a Log[x],a,x]
Show[ListLinePlot@dis,Plot[a Log[x]/. {a->\[Alpha]},{x,1,n},PlotStyle->Red]]


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
