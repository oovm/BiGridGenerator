$CharacterEncoding="UTF-8";
BiGridGenerator$path=FileNameJoin[{$InstallationDirectory,"AddOns","Applications","BiGridGenerator","Kernel",##}]&;
PrependTo[$Path,BiGridGenerator$path["ExCode"]];
PrependTo[$Path,BiGridGenerator$path["Game"]];
PrependTo[$Path,BiGridGenerator$path["Others"]];

Quiet[DeclarePackage@@@{
  {"ExCode`",{"ExCode","ExDecrypt","ExEncrypt","ListToPolish"}},
  {"ExExplorer`",{"AttractorExplorer","DecayExplorer","ExExplorer","TraceExplorer"}},
  {"ExForm`",{}},{"ExMatrix`",{}},
  {"ExNumber`",{"DigitReplacePrime","DisplaySum","ExNumber","ImproperSum","ManyPrime","MultPrime","PlusPrime","RTCount","SumProdNumber","SumProdPartitions"}},
  {"ExPainting`",{"ExPainting","GlassPainting","PloyPainting","PointPainting","ExPainting`TriPainting"}},
  {"ExPlot`",{"ComplexPlot","ExPlot","GEBPlot","Gray3DPlot","InfiniteListPlot","InfinitePlot","WavePlot"}},
  {"ExRandom`",{"AutoBiography","ExRandom","RandomExample"}},
  {"ExScience`",{}},
  {"ExLanguage`",{}},



  {"CardGames`",{}},
  {"CCPSolver`",{}},
  {"LightOut`",{}},
  {"MathConfession`",{"ExForm","ExForm$Environment","ExForm$LastUpdate","ExForm$Version","MathLove"}},
  {"OtherGames`",{"GameRPS","GraphRPS","MatrixRPS","OtherGames","RPSQ"}},
  {"WordGames`",{}},



  {"IrwinSums`",{"iPartialSum","iPartialSumThreshold","IrwinSum","IrwinSums","IrwinSum$","iSumFormatted","setPrintLevel"}},
  {"KempnerSums`",{"KempnerSum","KempnerSums","kPartialSum","kPartialSumThreshold","kSumFormatted","kSumGetA","kSumGetT","kSumSetDefaultDecimals","kSumShowA","kSumShowDefaultDecimals","kSumTimeAndMemory"}},
  {"Main`",{"DigitalCycle","TriPainting"}},
  {"QuinticEquation`",{}}
};];
(*以上代码为自动生成,人肉维护
path=FileNameJoin[{$InstallationDirectory,"AddOns","Applications","BiGridGenerator","Kernel",##}]&;
MakeDeclare[str_]:={str,Evaluate@Names[str<>"*"]}
Package$ExCode=Quiet@Flatten[StringCases[#,"ExCode\\"~~x__~~".m"\[Rule]x<>"`"]&/@FileNames["*",path["ExCode"]]];
Package$Game=Quiet@Flatten[StringCases[#,"Game\\"~~x__~~".m"\[Rule]x<>"`"]&/@FileNames["*",path["Game"]]];
Package$Others=Quiet@Flatten[StringCases[#,"Others\\"~~x__~~".m"\[Rule]x<>"`"]&/@FileNames["*",path["Others"]]];
MakeDeclare/@Join[Package$ExCode,Package$Game,Package$Others]
*)





(*暂时不会有变动的包*)




(*幻方包 V1.3*)
DeclarePackage["MagicSquare`",{
  "Magic","Magic3D","Magic3DQ","Magic3DShow","MagicQ","MagicSquare"  }];




(*排序算法可视化 V1.4*)
DeclarePackage["SortAlgorithm`",{
  "ShellSort", "BubbleSort", "InsertionSort", "CocktailSort",
  "BogoSort", "QuickSort", "SortPlay", "SortDraw", "SortShow",
  "SortPlot", "BeadSortStep", "BeadSort", "BeadPlay", "BatcherNet",
  "InsertionNet", "OddEvenTranspositionNet", "PairwiseNet",
  "OptimalNet", "NetEfficiency", "NetShow"}];
(*黎曼几何包 v2.0*)
DeclarePackage["RiemannSurfacePlot3D`",{
  "BranchPointOffset","Coloring","LogSheets","NDSolveOptions","RiemannSurfacePlot3D","StitchPatches"}];
