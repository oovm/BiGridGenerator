(* ::Package:: *)
(* ::Title:: *)
(*ExPlot(绘图增强包)*)
(* ::Subchapter:: *)
(*程序包介绍*)
(* ::Text:: *)
(*Mathematica Package*)
(*Created by Mathematica Plugin for IntelliJ IDEA*)
(*Establish from GalAster's template*)
(**)
(*Author:GalAster*)
(*Creation Date:2016-05-12*)
(*Copyright:CC4.0 BY+NA+NC*)
(**)
(*该软件包遵从CC协议:署名、非商业性使用、相同方式共享*)
(**)
(*这里应该填这个函数的介绍*)
(* ::Section:: *)
(*函数说明*)
BeginPackage["ExPlot`"];
InfinitePlot::usage = "一种把无穷函数映射到单位正方形的变换.";
InfiniteListPlot::usage = "同InfinitePlot,但是使用点绘图,对于波动大的函数效果较好,但是速度较慢.";
ComplexPlot::usage = ".";
Gray3DPlot::usage = ".";
WavePlot::usage = ".";
GEBPlot::usage = ".";
(* ::Section:: *)
(*程序包正体*)
(* ::Subsection::Closed:: *)
(*主设置*)
ExPlot$Version="V0.5";
ExPlot$Environment="V11.0+";
ExPlot$LastUpdate="2016-10-17";
ExPlot::usage = "程序包的说明,这里抄一遍";
Begin["`Private`"];
(* ::Subsection::Closed:: *)
(*主体代码*)



(* ::Subsubsection:: *)
(*无穷->单位框的映射*)
InfinitePlot[list_]:=Module[{funs,func,tick,axis,coor},
  funs=ArcTan[Function[x,#][Tan[x]]]&/@list;
  func=Plot[Evaluate@funs,{x,-Pi/2,Pi/2},AspectRatio->1,Axes->False,PlotLegends->list,PerformanceGoal->"Quality",MaxRecursion->15];
  tick=N@ConstantArray[Pi/2,12]-((Pi/2^#)&/@Range[12]);
  axis={{{0,0},{Pi/2.0,0}}}~Join~({{#,0.02},{#,-0.02}}&/@tick);
  coor=Graphics[Line/@((#.RotationMatrix[Pi/2])~Join~#)&@(axis~Join~-axis)];
  Show[func,coor,ImageSize->Large]];
InfiniteListPlot[list_]:=
    Module[{funs,xxx,data,func,tick,axis,coor},
      funs=ArcTan[Function[x,#][Tan[x]]]&/@list;
      xxx=Subdivide[-Pi/2,-Pi/4.0,800]~Join~Subdivide[-Pi/4,Pi/4.0,400]~Join~Subdivide[Pi/4,Pi/2.0,800];
      data=Transpose[{xxx,Function[x,#]/@xxx}]&/@funs;
      func=ListLinePlot[data,AspectRatio->1,Axes->False,PlotLegends->list,PlotRangeClipping->False];
      tick=N@ConstantArray[Pi/2,12]-((Pi/2^#)&/@Range[12]);
      axis={{{0,0},{Pi/2.0,0}}}~Join~({{#,0.02},{#,-0.02}}&/@tick);
      coor=Graphics[Line/@((#.RotationMatrix[Pi/2])~Join~#)&@(axis~Join~-axis)];
      Show[func,coor,ImageSize->Large]];



(* ::Subsubsection:: *)
(*作用于图片的绘图函数*)
Gray3DPlot[img_,points_:200]:=Module[{gray},
  gray=Reverse@ImageData@RemoveAlphaChannel@ColorConvert[img,"Grayscale"];
  ListPlot3D[gray,ColorFunction->GrayLevel,MaxPlotPoints->points,Boxed->False,Axes->False,Mesh->None]];
WavePlot[img_,mf_:5,md_:80,ops___]:=Module[{in,gray},
  in=ImageResize[MeanFilter[img,mf],{200,md}];
  gray=Reverse@ImageData@RemoveAlphaChannel@ColorConvert[in,"Grayscale"];
  ListPlot3D[gray,Mesh->{0,md},PlotStyle->Opacity[0],Boxed->False,Axes->False,ops]];
Options[letter]={ImageSize->100};
letter[s_,OptionsPattern[]]:=Binarize@Graphics[
  {EdgeForm[None],FaceForm[Black],
    First[First[ImportString[ExportString[Style[s,FontSize->100],"PDF"],
      "PDF","TextMode"->"Outlines"]]]},AspectRatio->1,
  ImageSize->OptionValue[ImageSize]];



(* ::Subsubsection:: *)
(*大小切换绘图*)
FunQ={ConstantArray[#1,Length@{##2}],{##2}}&@@@NestList[RotateLeft,#,Length@#-1]&;
FunGE[{a_,b_}]:=If[Inner[GreaterEqual,a,b,And],a[[1]],I];
FunLE[{a_,b_}]:=If[Inner[LessEqual,a,b,And],a[[1]],I];
MaxPlot[funcs_,range_,ops___]:=Plot[Evaluate[FunGE/@(FunQ@funcs)],range,ops];
MinPlot[funcs_,range_,ops___]:=Plot[Evaluate[FunLE/@(FunQ@funcs)],range,ops];
MaxPlot3D[funcs_,range_,ops___]:=Plot3D[Evaluate[FunGE/@(FunQ@funcs)],range,ops];
MinPlot3D[funcs_,range_,ops___]:=Plot3D[Evaluate[FunLE/@(FunQ@funcs)],range,ops];
DigitsPlot[x_,num_:100,dig_:10,ops___]:=ArrayPlot[Partition[RealDigits[x,dig,num][[1]],dig],Mesh->True,ops];
(*Thanks to @Apple*)


(* ::Subsubsection:: *)
(*未分类代码*)
ComplexPlot[f_]:=Module[{fun,data,RE,IM,ABS},
  fun=Table[f/.z->(xI+y),{x,-4,4,0.02},{y,-4,4,0.02}];
  data=2ArcTan[#@fun]/Pi&/@{Re,Im,Abs};
  {RE,IM,ABS}=ArrayPlot[#,ColorFunction->"TemperatureMap"]&/@data;
  GraphicsGrid[{{RE,ABS,SpanFromLeft},{IM,SpanFromAbove,SpanFromBoth}},ImageSize->Large]];
GEBPlot[str_String,res_Integer:100]:=Module[{X,Y,Z},
  {X,Y,Z}=(ImageData@letter[#,ImageSize->res])&/@StringPartition[str,1];
  Quiet@RegionPlot3D[X[[Round[i],Round[j]]]==0&&Y[[Round[i],Round[k]]]==0&&
      Z[[Round[j],Round[k]]]==0,{i,1,res},{j,1,res},{k,1,res},
    Boxed->False,Axes->False,Mesh->None,PlotPoints->res/10]];




(* ::Subsection::Closed:: *)
(*附加设置*)
End[] ;

EndPackage[];