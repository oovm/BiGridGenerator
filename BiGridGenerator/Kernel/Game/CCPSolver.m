(* Mathematica Package *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)

(* :Title: CCPSolver *)
(* :Context: CCPSolver` *)
(* :Author: 28059 *)
(* :Date: 2016-11-10 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: *)
(* :Copyright: (c) 2016 28059 *)
(* :Keywords: *)
(* :Discussion: *)

BeginPackage["CCPSolver`"]
(* Exported symbols added here with SymbolName::usage *)

Begin["`Private`"];
step=Function[list,Ramp[list-#&/@IdentityMatrix@Length@list]];
fix=Function[list,Union@Flatten[FixedPointList[Union@Flatten[step/@#,1]&,{list}],1]];
add=Function[list,(list->#&/@step@list)~Join~{list->list}];
ass=Function[list,Reverse[Union@Flatten[add/@fix@list]]];
pos=Function[{a,b},If[a===b,(If[#[[1]]===0,1,0]&/@Transpose@{a,b})~Join~{1},(a-b)~Join~{0}]];
Wei[{x_,m_}]:=Chop[Total/@(Append[x,1-Total@x]*#&/@pos@@@ass@m)];
CCPGraph[{x_?VectorQ,m_?VectorQ},"3D"]:=Graph3D[
  Delete[ass@m,Position[Wei[{x,m}],0]],PlotTheme->"Web",
      GraphLayout->"SpringEmbedding",
      EdgeWeight->Delete[Wei[{x,m}],Position[Wei[{x,m}],0]],
      EdgeLabels->Placed["EdgeWeight",Center],
      EdgeLabelStyle->Directive[Lighter@Blue,16,Bold],
      VertexLabelStyle->Directive[Black,16,Bold],
      VertexLabels->Placed["Name",Center]];
CCPGraph[{x_?VectorQ,m_?VectorQ}]:=Graph[
  Function[{a,b},Property[DirectedEdge@@a,EdgeStyle->{Thick,b,Dashed}]]@@@Transpose@{Delete[ass@m,Position[Wei[{x,m}],0]],
    ColorData["Rainbow"]/@Delete[Wei[{x,m}],Position[Wei[{x,m}],0]]},
  PlotTheme->"Minimal",GraphLayout->"LayeredDigraphEmbedding",
  VertexShapeFunction->"Diamond",VertexSize->Large,
  VertexLabelStyle->Directive[Black,16,Bold],
  VertexLabels->Placed["Name",Center]];
CCPMatrix[{x_?VectorQ,m_?VectorQ}]:=WeightedAdjacencyMatrix[Graph[ass@m,EdgeWeight->Wei[{x,m}]]];
CCPMarkov[{x_?VectorQ,m_?VectorQ}]:=DiscreteMarkovProcess[1,CCPMatrix[{x,m}]];



End[]; (* `Private` *)

EndPackage[];