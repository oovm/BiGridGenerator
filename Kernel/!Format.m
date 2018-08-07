Clear["`*"];
ComplexPlot[f_]:=Module[
	{fun,data,RE,IM,ABS},
	fun=Table[f/.z->(x I+y),{x,-4,4,0.02},{y,-4,4,0.02}];
	data=2ArcTan[#@fun]/Pi&/@{Re,Im,Abs};
	{RE,IM,ABS}=ArrayPlot[#,ColorFunction->"TemperatureMap"]&/@data;
	GraphicsGrid[{{RE,ABS,SpanFromLeft},{IM,SpanFromAbove,SpanFromBoth}},ImageSize->Large]
];
ComplexPlot[Erf[z]];