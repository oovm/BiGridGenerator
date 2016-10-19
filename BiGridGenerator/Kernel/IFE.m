BeginPackage["IFE`"]

ExIFE::usage = "程序包IFE(Image To Fourier Expansion)可以将一张图片转换为参数形式的傅里叶正弦展开式"

Begin["`Private`"]

ExIFE[image_, method_: 1, range_: 2, threshold_: 0.2, random_: 2, 
     linenumber_: 10, terms_: 10, order_: 3, detail_: 64*Pi, 
     points_: 100] := Module[{}, 
     fourierComponentData[pointList_, nMax_, op_] := 
        Module[{\[CurlyEpsilon] = 1/10^3, \[Mu] = 2^14, M = 10000, s, 
     scale, \[CapitalDelta], 
            L, nds, sMax, if, \[ScriptX]\[ScriptY]Function, X, Y, 
     XFT, YFT, type}, 
          scale = 1.*Mean[Table[Max[fl /@ pointList] - 
                     Min[fl /@ pointList], {fl, {First, Last}}]]; 
           \[CapitalDelta] = EuclideanDistance[First[pointList], 
               Last[pointList]]; L = Which[op === "Closed", 
               type = "Closed"; If[First[pointList] === 
                    Last[pointList], pointList, Append[pointList, 
                    First[pointList]]], op === "Open", 
               type = "Open"; pointList, \[CapitalDelta] == 0., 
      type = "Closed"; 
                pointList, \[CapitalDelta]/scale < op, 
      type = "Closed"; 
                Append[pointList, First[pointList]], True, 
               type = "Open"; Join[pointList, 
                  
       Rest[Reverse[pointList]]]]; \[ScriptX]\[ScriptY]Function = 
             BSplineFunction[L, SplineDegree -> 4]; 
           nds = NDSolve[{Derivative[1][s][t] == 
                   
        Sqrt[Derivative[1][\[ScriptX]\[ScriptY]Function][t] . 
                       
          Derivative[1][\[ScriptX]\[ScriptY]Function][t]], 
       s[0] == 0}, s, 
               {t, 0, 1}, MaxSteps -> 10^5, PrecisionGoal -> 4]; 
           sMax = s[1] /. nds[[1]]; if = Interpolation[
               
      Table[{s[\[Sigma]] /. nds[[1]], \[Sigma]}, {\[Sigma], 0, 1, 
        1/M}]]; 
           X[t_Real] := BSplineFunction[L][
                 Max[Min[1, if[((t + Pi)*sMax)/(2*Pi)]], 0]][[1]]; 
           Y[t_Real] := BSplineFunction[L][
                 Max[Min[1, if[((t + Pi)*sMax)/(2*Pi)]], 0]][[2]]; 
           {XFT, 
      YFT} = (Fourier[Table[#1[N[t]], {t, \[CurlyEpsilon] - Pi, 
                        
           Pi - \[CurlyEpsilon], (2*Pi - 
              2*\[CurlyEpsilon])/\[Mu]}]] & ) /@ {X, Y}; 
           {type, (2*Pi*(Transpose[Table[({Re[#1], Im[#1]} & )[
                             E^(I*k*Pi)*#1[[k + 1]]], {k, 0, 
              nMax}]] & ) /@ 
                    {XFT, YFT})/Sqrt[\[Mu]]}]; 
      pointListToLines[pointList_, neighborhoodSize_: 6] := 
        Module[{L = DeleteDuplicates[pointList], NF, \[Lambda], 
     lineBag, 
            counter, seenQ, sLB, nearest, nearest1, nextPoint, 
            couldReverseQ, \[ScriptD], \[ScriptN], \[ScriptS]}, 
    NF = Nearest[L]; 
           \[Lambda] = Length[L]; Monitor[lineBag = {}; counter = 0; 
              
     While[counter < \[Lambda], sLB = {RandomChoice[DeleteCases[L, 
                         _?seenQ]]}; seenQ[sLB[[1]]] = True; counter++; 
                 couldReverseQ = True; While[
                   nearest = NF[Last[sLB], {Infinity, 
                          neighborhoodSize}]; nearest1 = 
                      SortBy[DeleteCases[nearest, _?seenQ], 
                        1.*EuclideanDistance[Last[sLB], #1] & ]; 
                    nearest1 =!= {} || couldReverseQ, 
                   If[nearest1 === {}, sLB = Reverse[sLB]; 
                      couldReverseQ = False, nextPoint = If[
                          Length[sLB] <= 3, nearest1[[1]], 
                          \[ScriptD] = 
           1.*Normalize[(1/2)*(sLB[[-2]] - sLB[[
                                        -3]]) + (sLB[[-1]] - 
                sLB[[-2]])]; 
                           \[ScriptN] = {-1, 1}*
            Reverse[\[ScriptD]]; \[ScriptS] = Sort[
                               ({Sqrt[(\[ScriptD] . (#1 - 
                    sLB[[-1]]))^2 + 
                                       
                  2*(\[ScriptN] . (#1 - sLB[[-1]]))^2], #1} & ) /@ 
                                 nearest1]; \[ScriptS][[1, 2]]]; 
        AppendTo[sLB, 
                        nextPoint]; seenQ[nextPoint] = True; 
                      counter++]]; AppendTo[lineBag, sLB]]; 
              Reverse[SortBy[Select[lineBag, Length[#1] > 12 & ], 
                  Length]], Grid[
               {{Text[Style["progress point joining", Darker[Green, 
                         0.66]]], 
        ProgressIndicator[counter/\[Lambda]]}, 
                 {Text[Style["number of segments", Darker[Green, 
                         0.66]]], Length[lineBag] + 1}}, 
               Alignment -> Left, Dividers -> Center]]]; 
      Options[fourierComponents] = {"MaxOrder" -> 500, 
          "OpenClose" -> 0.025}; fourierComponents[pointLists_, 
          OptionsPattern[]] := 
        Monitor[Table[fourierComponentData[pointLists[[k]], 
                (If[Head[#1] === List, #1[[k]], #1] & )[
                  OptionValue["MaxOrder"]], 
                (If[Head[#1] === List, #1[[k]], #1] & )[
                  OptionValue["OpenClose"]]], {k, 
       Length[pointLists]}], 
            Grid[
              {{Text[Style["progress calculating Fourier \
          coefficients", Darker[Green, 0.66]]], ProgressIndicator[
                    k/Length[pointLists]]}}, Alignment -> Left, 
              Dividers -> Center]] /; Depth[pointLists] === 4; 
      makeFourierSeries[{"Closed" | "Open", {{cax_, sax_}, 
              {cay_, say_}}}, t_, n_] := 
        {Sum[Cos[k*t]*If[k == 0, 1/2, 1]*cax[[k + 1]] + 
              sax[[k + 1]]*Sin[k*t], {k, 0, Min[n, Length[cax]]}], 
          Sum[Cos[k*t]*If[k == 0, 1/2, 1]*cay[[k + 1]] + 
              say[[k + 1]]*Sin[k*t], {k, 0, Min[n, Length[cay]]}]}; 
      makeFourierSeriesApproximationManipulate[fCs_, 
          maxOrder_: 60] := Manipulate[
          With[{opts = Sequence[PlotStyle -> Black, Frame -> True, 
                  Axes -> False, FrameTicks -> None, PlotRange -> All, 
                  ImagePadding -> 12]}, 
            Quiet[Show[{ParametricPlot[Evaluate[
                      (makeFourierSeries[#1, t, n] & ) /@ Cases[fCs, 
                          {"Closed", _}]], {t, -Pi, Pi}, opts], 
                  ParametricPlot[Evaluate[(makeFourierSeries[#1, t, 
                             n] & ) /@ 
           Cases[fCs, {"Open", _}]], {t, -Pi, 0}, 
                    opts]}]]], {{n, 1, "max series order"}, 1, 
     maxOrder, 
            1, Appearance -> "Labeled"}, TrackedSymbols :> True, 
          SaveDefinitions -> True]; 
      sinAmplitudeForm[kt_, {cF_, sF_}] := 
        With[{\[CurlyPhi] = phase[cF, sF]}, Sqrt[cF^2 + sF^2]*
            Sin[kt + \[CurlyPhi]]]; phase[cF_, sF_] := 
        With[{T = Sqrt[cF^2 + sF^2]}, 
          With[{g = Total[Abs[Table[cF*Cos[x] + sF*Sin[x] - 
                          T*Sin[x + ArcSin[cF/T]*#1 + #2], {x, 0, 1, 
                          0.1}]]] & }, If[g[1, 0] < g[-1, Pi], 
              ArcSin[cF/T], Pi - ArcSin[cF/T]]]]; 
      singleParametrization[fCs_, t_, n_] := 
        UnitStep[Sign[Sqrt[Sin[t/2]]]]*
          Sum[UnitStep[t - ((m - 1)*4*Pi - Pi)]*
              UnitStep[4*Pi*(m - 1) - t + 3*Pi]*
              {(1/2)*(Plus[fCs[[m, 2, 1, 1, 1]]]) + 
                  Sum[sinAmplitudeForm[k*t, {fCs[[m, 2, 1, 1, k + 1]], 
                        fCs[[m, 2, 1, 2, k + 1]]}], 
                    {k, Min[If[Head[n] === List, n[[m]], n], 
                        Length[fCs[[1, 2, 1, 1]]]]}], 
                (1/2)*(Plus[fCs[[m, 2, 2, 1, 1]]]) + 
                  Sum[sinAmplitudeForm[k*t, {fCs[[m, 2, 2, 1, k + 1]], 
                        fCs[[m, 2, 2, 2, k + 1]]}], 
                    {k, Min[If[Head[n] === List, n[[m]], n], 
                        Length[fCs[[1, 2, 1, 1]]]]}]}, {m, 
      Length[fCs]}]; 
      importedimage = Import[image]; pinkPantherImage = 
        importedimage; pinkPantherEdgeImage = 
        Thinning[EdgeDetect[ColorConvert[ImagePad[
                Image[Map[Most, ImageData[pinkPantherImage], {2}]], 
                20, White], "Grayscale"]]]; 
      (pinkPantherEdgeImage2 = Thinning[EdgeDetect[
               Image[importedimage, "Real"], range, threshold], 1])*
        Print[GraphicsGrid[{{pinkPantherImage, ColorNegate[
                  pinkPantherEdgeImage], ColorNegate[
                  pinkPantherEdgeImage2]}}, ImageSize -> 720]]; 
      If[method == 1, edgePoints = Apply[{#2, -#1} & , 
            Position[ImageData[pinkPantherEdgeImage], 1, {2}], {1}], 
        edgePoints = Apply[{#2, -#1} & , Position[
              ImageData[pinkPantherEdgeImage2], 1, {2}], {1}]]; 
      SeedRandom[random]; hLines = pointListToLines[edgePoints, 
          linenumber]; Print["线条数：", Length[hLines]]; 
      fCs = fourierComponents[hLines]; 
      Print[makeFourierSeriesApproximationManipulate[fCs, 100]]; 
      Print["最终表达式："]; finalCurve = 
        Rationalize[singleParametrization[fCs, t, terms], 
          10^(-order)]; Print[TraditionalForm[finalCurve]]; 
      Print[ParametricPlot[Evaluate[N[finalCurve]], 
          {t, 0, detail}, PlotPoints -> points]]; ]
End[]
EndPackage[]