Options[ASCIIfy] = {FontFamily -> "Inconsolata", ColorNegate -> False, Colorize -> True};
ASCIIfy[img_, grainchoice_, OptionsPattern[]] := Module[
	{},
(*Chars used in output that are not part of the alphabets*)
	charsbasic = {"$", "&", "'", ",", ".", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", ":", ";", "?"};
	chars = RandomSample[Join[CharacterRange[65, 122], charsbasic], UpTo[100]];
	
	(*calculates the dimensions of characters*)
	{truecharwidth , truecharheight} = Max /@ Transpose[ImageDimensions@Rasterize[
		Style[#, FontFamily -> OptionValue[FontFamily]]
	] & /@ chars];
	
	(*creates a table of the brightness levels of chars*)
	charsmean = Table[ImageMeasurements[ColorConvert[Rasterize[Pane[
		Style[chars[[a]], FontFamily -> OptionValue[FontFamily]],
		{truecharwidth, truecharheight}, Alignment -> Center], ImageSize -> 50], "Grayscale"], "Mean"], {a, 1, Length[chars]}];
	
	
	(*sets grain*)
	{charwidth, charheight} = If[
		grainchoice == "Auto",
		{truecharwidth, truecharheight},
		{grainchoice, Round[grainchoice * (truecharheight + 0) / truecharwidth]}
	];
	
	(*dark images will not be well represented by chars, as they have a white background, so before applying the program the image brightness is scales up accordingly*)
	bleach[im_] := im * (1 - Min[charsmean]) + Min[charsmean];
	
	(*creates a table of rasterized columns of strings that resemble frames of a gif*)
	
	graphicframe = If[OptionValue[ColorNegate] === True, ColorNegate[img], img];
	
	(*creates a partitioned black and white, bleached image*)
	pic = ImagePartition[ImageApply[bleach, {ColorConvert[graphicframe, "Grayscale"]}], {charwidth, charheight}];
	
	(*creates a partitioned colored image if coloredchoice is true*)
	If[OptionValue[Colorize] == True, picwc = ImagePartition[graphicframe, {charwidth, charheight}];, ""];
	
	(*calculated the dimensions of the partitioned image*)
	{height, width} = Dimensions[pic];
	
	(*calculates the average brightness for each piece of the image*)
	picmeans = Table[ImageMeasurements[pic[[y, x]], "Mean"], {x, 1, width}, {y, 1, height}];
	
	(*names a function that calculates the differences between a piece of the image and all chars*)
	diffs[rownumber_, placeinrow_] := Table[charsmean[[a]] - picmeans[[rownumber, placeinrow]], {a, 1, Length[chars]}]^2;
	
	(*picks the character whoose difference is the smallest*)
	best[x_, y_] := chars[[Position[diffs[x, y], Min[diffs[x, y]]][[1, 1]]]];
	
	(*creates a column of rows of colored best fitting letters, colored or black*)
	charpic = If[
		OptionValue[Colorize] == True,
		TableForm[Table[Style[best[testx, testy], FontColor -> RGBColor[ImageMeasurements[picwc[[testy, testx]], "Mean"]]], {testy, 1, height}, {testx, 1, width}], TableSpacing -> {0, 0}],
		TableForm[Table[Style[best[testx, testy], FontColor -> Black], {testy, 1, height}, {testx, 1, width}], TableSpacing -> {0, 0}]
	];
	(*rasterizes the column of strings*)
	If[
		OptionValue[ColorNegate] == True,
		ColorNegate[Rasterize[Style[TableForm[Map[Pane[#, {truecharwidth, truecharheight}, Alignment -> Center]&, charpic, {3}], TableSpacing -> {0, 1}], FontFamily -> OptionValue[FontFamily]]]],
		Rasterize[Style[TableForm[Map[Pane[#, {truecharwidth, truecharheight}, Alignment -> Center]&, charpic, {3}], TableSpacing -> {0, 1}], FontFamily -> OptionValue[FontFamily]]]
	]
]



