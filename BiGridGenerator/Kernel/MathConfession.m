(* Mathematica Package *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)

(* :Title: MathConfession *)
(* :Context: MathConfession` *)
(* :Author: 28059 *)
(* :Date: 2016-10-18 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: *)
(* :Copyright: (c) 2016 28059 *)
(* :Keywords: *)
(* :Discussion: *)

BeginPackage["MathConfession`"];
(* Exported symbols added here with SymbolName::usage *)
MathLove[2]:=Module[{vals,funs},
  {vals,funs}=NDEigensystem[{-Laplacian[u[x,y],{x,y}],
        DirichletCondition[u[x,y]==0,x==0]},
        u[x,y],{x,y}\[Element]RegionSymmetricDifference[Disk[{0,0},5],Disk[{0,0},3]],1];
  Quiet@Plot3D[funs,{x,-5,5},{y,-5,5},PlotRange->All,PlotTheme->"Marketing",
    PlotLegends->Placed["-\!\(\*TemplateBox[{RowBox[{\"u\",\"(\",\n\
        RowBox[{\"x\",\",\",\"y\"}],\")\"}],RowBox[{\"{\",\n\
        RowBox[{\"x\",\",\",\"y\"}],\"}\"}]},\n\
        \"Laplacian\"]\),3<\!\(\*SuperscriptBox[\(x\),\
        \(2\)]\)+\!\(\*SuperscriptBox[\(y\),\(2\)]\)<5",Below]]];






Begin["`Private`"]

End[] (* `Private` *)

EndPackage[]