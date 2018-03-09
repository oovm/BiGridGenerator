BeginPackage["Phyllotaxis`"];
PhyllotaxicSurface::usage = "";
PhyllotaxicStripedSurface::usage = "";
(* ::Section:: *)
(*程序包正体*)
(* ::Subsection::Closed:: *)
(*主设置*)
Phyllotaxis::usage = "程序包的说明,这里抄一遍";
Begin["`Private`"];
(* ::Subsection::Closed:: *)
(*主体代码*)
Phyllotaxis$Version="V1.0";
Phyllotaxis$LastUpdate="2016-11-11";
(* ::Subsubsection:: *)
(*Constant*)
\[Gamma] = 2 \[Pi] (1 - 1/GoldenRatio);
PolarCoordinate[r_,\[Theta]_]:= r{Cos[\[Theta]],Sin[\[Theta]]};
FRSphericalCoordinate[\[Phi]_,\[Theta]_]:= {Sin[\[Phi]] Cos[\[Theta]], Sin[\[Phi]] Sin[\[Theta]], Cos[\[Phi]]};
area[{x_,z_},{t0_,t1_}] :=Abs[NIntegrate[2 \[Pi] x[t]Sqrt[(x'[t])^2+(z'[t])^2],{t,t0,t1}]];
areaToTFunction[{x_,z_},{t0_,t1_},n_:100]:=Interpolation[Table[{area[{x,z},{t0,t}],t},{t,t0,t1,N[(t1-t0)/n]}]];

(* ::Subsubsection:: *)
(*PhyllotaxicSurface*)
SetAttributes[PhyllotaxicSurface,HoldAll];
PhyllotaxicSurface[{x_,z_},{t_,t0_,t1_},density_:1,radius_:.5]:=
	Module[{xFunc,zFunc,tFunc,totalArea,n},
		xFunc=Function[t,x];
		zFunc=Function[t,z];
		tFunc = areaToTFunction[{xFunc,zFunc},{t0,t1}];
		totalArea = area[{xFunc,zFunc}, {t0,t1}];
		n = Floor[density totalArea];
		Graphics3D[{
			Sphere[Table[With[{t=tFunc[totalArea i/n],\[Theta]=i \[Gamma]},{xFunc[t] Cos[\[Theta]],xFunc[t] Sin[\[Theta]],zFunc[t]}],{i,1,n}],radius]
		},Boxed->False,ViewAngle->0.3`]
	];
Atop[a_,b_]:=MapThread[If[#1===Null,#2,#1]&, {a,b}];
Colors[rawColorSpec_,n_]:=
	Module[{colorSpec,indices=Range[n], specList},
		colorSpec = Take[Append[#,1],3]& /@ rawColorSpec;
		specList=With[{c=#[[1]],m=#[[2]],o=#[[3]]},(If[Mod[#-o,m]==0,c,Null]& /@indices)]&/@ colorSpec;
		Fold[Atop,Last[specList],Rest[Reverse[specList]]]
	];
(*Colors[{{1,3,2},{2,2,1}},10]*)
ToIndices[colors_]:=
	Module[{allColors = Complement[Union[colors],{Null}]},
		{#,First /@ Position[colors,#]}& /@ allColors
	];
(*ToIndices[Colors[{{Red,3},{Black,2},{5,5}},10]]*)

(* ::Subsubsection:: *)
(*PhyllotaxicStripedSurface*)
SetAttributes[PhyllotaxicStripedSurface,HoldAll];
PhyllotaxicStripedSurface[{x_,y_},{t_,t0_,t1_},density_:1,radius_:.5,colorSpec_:{{White,1}}]:=Module[
	{xFunc,yFunc,tFunc,totalArea,n},
	xFunc=Function[t,x];
	yFunc=Function[t,y];
	tFunc = areaToTFunction[{xFunc,yFunc},{t0,t1}];
	totalArea = area[{xFunc,yFunc}, {t0,t1}];
	n = Floor[density totalArea];
	Graphics3D[With[{color=#[[1]],indices=#[[2]]},
		{color,Sphere[
			With[{t=tFunc[totalArea #/n],\[Theta]=# \[Gamma]},
				{xFunc[t] Cos[\[Theta]],xFunc[t] Sin[\[Theta]],yFunc[t]}
			]&/@indices,radius]}]&/@ToIndices[Colors[colorSpec,n]],
		Boxed->False,ViewAngle->0.3`
	]
];

(* ::Subsection::Closed:: *)
(*附加设置*)
End[] ;
SetAttributes[
	{PhyllotaxicSurface,PhyllotaxicStripedSurface},
	{Protected,ReadProtected}
];
EndPackage[];
