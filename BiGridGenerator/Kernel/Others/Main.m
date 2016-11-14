BeginPackage["Main`"]

DigitalCycle::usage = "DigitalCycle可以生成螺旋数字图.;"
TriPainting::usage = "TriPainting可以将图片转变为三角画风;"


Begin["`Private`"];
PlayList[n_]:=Module[{foo,up},
  foo[p_]:=Range[5(-1+p)p,p(4+5p),p];
  up=Floor[-(2/5)+1/5Sqrt[4+5n]];
  Drop[Flatten@{foo/@Range[up],Range[5up(up+1),n,up+1]},1]];

Venn[n_,ineqs_:{}]:=
    Module[{i,r=.6,R=1,v,grouprules,x,y,x1,x2,y1,y2,ve},
      v=Table[Circle[r{Cos[#],Sin[#]}&[2Pi(i-1)/n],R],{i,n}];
      {x1,x2}={Min[#],Max[#]}&[Flatten@Replace[v,Circle[{xx_,yy_},rr_]:>{xx-rr,xx+rr},{1}]];
      {y1,y2}={Min[#],Max[#]}&[Flatten@Replace[v,Circle[{xx_,yy_},rr_]:>{yy-rr,yy+rr},{1}]];
      ve[x_,y_,i_]:=v[[i]]/.Circle[{xx_,yy_},rr_]:>(x-xx)^2+(y-yy)^2<rr^2;
      grouprules[x_,y_]=ineqs/.Table[With[{is=i},Subscript[_,is]:>ve[x,y,is]],{i,n}];
      Show[If[MatchQ[ineqs,{}|False],{},
        RegionPlot[grouprules[x,y],{x,x1,x2},{y,y1,y2},Axes->False]],Graphics[v],
        PlotLabel->TraditionalForm[Replace[ineqs,{}|False->\[EmptySet]]],Frame->False]];

MineLayout[{m_, n_, k_}] := Module[{M, foo, bar, list},
  M = ConstantArray[0, {m + 2, n + 2}];
  foo[{x_, y_}] := M[[x - 1 ;; x + 1, y - 1 ;; y + 1]] += 1;
  bar[{x_, y_}] := M[[x, y]] = 10;
  list = RandomSample[Tuples[{Range[2, m + 1], Range[2, n + 1]}]][[1 ;; k]];
  Do[foo@list[[i]], {i, k}]; bar /@ list; M[[2 ;; -2, 2 ;; -2]]];
MineDistribution[m_, n_, k_, p_] :=
    Transpose@{Range[0, 10],BinCounts[Flatten[MineLayout /@ ConstantArray[{m, n, k}, p]], {-0.5, 10.5,1}]/p + 0.0};

End[];
Protect[DigitalCycle,TriPainting];

EndPackage[]