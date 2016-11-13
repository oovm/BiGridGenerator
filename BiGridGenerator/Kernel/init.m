$CharacterEncoding="UTF-8";
DeclarePackage["BiGridGenerator`MagicSquare`",
  {"Magic","Magic3D","Magic3DShow","MagicQ","Magic3DQ"}];
DeclarePackage["BiGridGenerator`RockPaperScissors`",
  {"GraphRPS","MatrixRPS","RPSQ","GameRPS"}];
DeclarePackage["BiGridGenerator`SortAlgorithm`",
  {"ShellSort","BubbleSort","InsertionSort","CocktailSort",
    "BogoSort","QuickSort","SortPlay","SortDraw","SortShow",
    "SortPlot","BeadSortStep","BeadSort","BeadPlay","PairSort",
    "MultiPairSort","ApplySorting","ApplySortingList",
    "NetworkGraphics","BatcherNet","ExchangeLoop","InsertionNet",
    "OddEvenTranspositionNet","PairwiseNet","OptimalNet",
    "NetEfficiency","NetShow"}];



Block[{path=FileNameJoin[{$InstallationDirectory,"AddOns","Applications","BiGridGenerator","Kernel",##}]&},
  Get[path["ExCode","ExCode.m"]];
  Get[path["ExCode","ExData.nb"]];
  Get[path["ExCode","ExExplorer.m"]];
  Get[path["ExCode","ExNumber.m"]];
  Get[path["ExCode","ExPainting.m"]];
  Get[path["ExCode","ExPlot.m"]];
  Get[path["ExCode","ExRandom.m"]];
  Get[path["ExCode","ExScience.m"]];

  Get[path["Game","CCPSolver.m"]];
  Get[path["Game","LightOut.m"]];
  Get[path["Game","MagicSquare.m"]];
  Get[path["Game","MathConfession.m"]];
  Get[path["Game","RockPaperScissors.m"]];

  Get[path["Others","IrwinSums.m"]];
  Get[path["Others","KempnerSums.m"]];
  Get[path["Others","Main.m"]];
  Get[path["Others","NumberAndLanguage.m"]];
  Get[path["Others","SortAlgorithm.m"]];
];