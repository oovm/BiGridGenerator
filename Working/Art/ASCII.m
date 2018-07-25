ASCIIfy[graphicchoice_, charchoice_, cbasic_, grainchoice_, coloredchoice_, fontfam_, fontsize_, blackbg_] := Module[

(*choose graphic: image or gif*)

(*choose alphabet here, "Auto" uses your location *)

(*choose grain, "Auto" uses size of characters*)

(*choose between colored or black characters*)
	
	
	{},

(*Is this a gif? if not then make it so*)
	If[ListQ[graphicchoice],
		graphic = graphicchoice,
		graphic = List[graphicchoice]];
	
	(*Chars used in output that are not part of the alphabets*)
	If[cbasic, charsbasic = {"$", "&", "'", ",", ".", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", ":", ";", "?"}, charsbasic = List[]];
	
	(*if you use setchars[auto] it will automatically use the characters used in your country*)
	auto = EntityValue[$GeoLocationCountry, "Languages"][[1]];
	
	(*Creates a list of chars to be used: lowercase, capital and special "charsbasic"*)
	setchars[lang_] := If[lang == "Auto", Join[Alphabet[auto], Capitalize[Alphabet[auto]], charsbasic], Join[Alphabet[lang], Capitalize[Alphabet[lang]], charsbasic]];
	
	(*temporary switch between languages, in the future will use a drop list*)
	chars = setchars[charchoice];
	
	(*calculates the dimensions of characters*)
	truecharwidth = Max[Table[Length[ImageData[Rasterize[Style[chars[[c]], FontFamily -> fontfam]]][[1]]], {c, 1, Length[chars]}]];
	truecharheight = Max[Table[Length[ImageData[Rasterize[Style[chars[[c]], FontFamily -> fontfam]]]], {c, 1, Length[chars]}]];
	
	
	(*creates a table of the brightness levels of chars*)
	charsmean = Table[ImageMeasurements[ColorConvert[Rasterize[Pane[Style[chars[[a]], FontFamily -> fontfam], {truecharwidth, truecharheight}, Alignment -> Center], ImageSize -> 50], "Grayscale"], "Mean"], {a, 1, Length[chars]}];
	
	
	(*sets grain*)
	If[grainchoice == "Auto", charwidth = truecharwidth;charheight = truecharheight, charwidth = grainchoice;charheight = Round[grainchoice * (truecharheight + 0) / truecharwidth]];
	
	(*dark images will not be well represented by chars, as they have a white background, so before applying the program the image brightness is scales up accordingly*)
	bleach[im_] := im * (1 - Min[charsmean]) + Min[charsmean];
	
	Monitor[
	
	(*creates a table of rasterized columns of strings that resemble frames of a gif*)
		giftable = Table[
		(*Should it color negate?*)
			If[blackbg,
				graphicframe = ColorNegate[graphic[[frame]]],
				graphicframe = graphic[[frame]]];
			
			(*creates a partitioned black and white, bleached image*)
			pic = ImagePartition[ImageApply[bleach,
				{ColorConvert[graphicframe, "Grayscale"]}], {charwidth, charheight}];
			
			(*creates a partitioned colored image if coloredchoice is true*)
			If[coloredchoice, picwc = ImagePartition[graphicframe, {charwidth, charheight}];, ""];
			
			(*calculated the dimensions of the partitioned image*)
			{height, width} = Dimensions[pic];
			
			(*calculates the average brightness for each piece of the image*)
			picmeans = Table[ImageMeasurements[pic[[y, x]], "Mean"], {x, 1, width}, {y, 1, height}];
			
			(*names a function that calculates the differences between a piece of the image and all chars*)
			diffs[rownumber_, placeinrow_] :=
				Table[charsmean[[a]] - picmeans[[rownumber, placeinrow]], {a, 1, Length[chars]}]^2;
			
			(*picks the character whoose difference is the smallest*)
			best[x_, y_] := chars[[Position[diffs[x, y], Min[diffs[x, y]]][[1, 1]]]];
			
			(*creates a column of rows of colored best fitting letters, colored or black*)
			If[coloredchoice,
				charpic = TableForm[
					Table[Style[best[testx, testy], FontColor -> RGBColor[ImageMeasurements[picwc[[testy, testx]], "Mean"]]], {testy, 1, height}, {testx, 1, width}], TableSpacing -> {0, 0}],
				charpic = TableForm[Table[Style[best[testx, testy], FontColor -> Black], {testy, 1, height}, {testx, 1, width}], TableSpacing -> {0, 0}]];
			
			(*rasterizes the column of strings*)
			If[blackbg, ColorNegate[Rasterize[Style[TableForm[Map[
				Pane[#, {truecharwidth, truecharheight}, Alignment -> Center]&, charpic, {3}], TableSpacing -> {0, 1}], FontFamily -> fontfam]]],
				
				Rasterize[Style[TableForm[Map[
					Pane[#, {truecharwidth, truecharheight}, Alignment -> Center]&, charpic, {3}], TableSpacing -> {0, 1}], FontFamily -> fontfam]]],
		
		(*repetition setting: as many times as the gif is long*)
			{frame, 1, Length[graphic]}],
	
	(*monitor setting: shows progress and total number of frames*)
		{(testy - 1) / height * 1. + (frame - 1), Length[graphic]}];
	
	(*exports the table of images into a gif or jpg*)
	If[ListQ[graphicchoice],
		Export["giftable.gif", giftable, "AnimationRepetitions" -> \[Infinity]],
		Export["giftable.jpg", giftable[[1]]]];
	
	(*opens the gif*)
	If[ListQ[graphicchoice],
		SystemOpen["giftable.gif"],
		SystemOpen["giftable.jpg"]]
]