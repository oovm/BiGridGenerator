$CharacterEncoding="UTF-8";
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
  Get[path["Others","RiemannSurfacePlot3D.m"]];
];
DeclarePackage["BiGridGenerator`SortAlgorithm`",Names["BiGridGenerator`SortAlgorithm`*"]];
DeclarePackage["BiGridGenerator`MagicSquare`",Names["BiGridGenerator`MagicSquare`*"]];
DeclarePackage["BiGridGenerator`RockPaperScissors`",Names["BiGridGenerator`RockPaperScissors`"];