InCircle[Polygon[GeoPosition[a_]]] := InCircle[Polygon[a /. {x_?NumericQ, y_?NumericQ} :> {y, x}]];
InCircle[Polygon[a_ /; Depth[a] == 4]] := Last@(SortBy[InCircle /@ a, Last]);
InCircle[a_List] := InCircle[Polygon@a];
InCircle[Polygon[a_ /; Depth[a] == 3]] := InCircle[Polygon[a]] = Module[
	{interior = BoundaryDiscretizeGraphics@Polygon[a]},
	{{x, y} /. #2, Abs@#1}& @@ NMinimize[SignedRegionDistance[interior, {x, y}], {x, y}\[Element]interior]
];
OutCircle[p_List] := List @@ BoundingRegion[p, "MinDisk"];
OutCircle[Polygon[GeoPosition[a_]]] := OutCircle[Polygon[a /. {x_?NumericQ, y_?NumericQ} :> {y, x}]];
OutCircle[Polygon[a_]] := OutCircle[Flatten[a, Depth[a] - 3]];
CountryCircumcircle[c_, proj_ : "Mercator"] := Circle[OutCircle@CountryData[c, {"SchematicPolygon", proj}]];
CountryIncircle[c_, proj_ : "Mercator"] := Circle[InCircle@CountryData[c, {"SchematicPolygon", proj}]];
CountryCircles[] := Block[
	{Countries = CountryData[#, "StandardName"]& /@ CountryData[]},
	Dataset@Monitor[
		Table[With[
			{poly = CountryData[Countries[[i]], {"SchematicPolygon", "Mercator"}]},
			<|
				"Name" -> CountryData[Countries[[i]], "Name"],
				"Out/In" -> Last[InCircle[poly]]^2 / Last[OutCircle[poly]]^2,
				"Area" -> CountryData[CountryData[Countries[[i]]], "Area"],
				"Graphics" -> Show[
					Graphics[{Blue, Disk @@ OutCircle[poly]}],
					Graphics@poly,
					Graphics[{Red, Disk @@ InCircle[poly]}]
				]
			|>
		], {i, Length@Countries}
		] // SortBy[#["Out/In"]&] // Reverse,
		Row[{ProgressIndicator[i / Length@Countries], Countries[[i]]}]
	]
];
SC[{lat_, lon_}] := With[{r = 6367.5}, r {Cos[lon \[Degree]] Cos[lat \[Degree]], Sin[lon \[Degree]] Cos[lat \[Degree]], Sin[lat \[Degree]]}];
CountryEarth[] := Graphics3D[{
	Opacity[.4], Sphere[{0, 0, 0}, 6367.5],
	Map[Line[Map[SC, CountryData[#, "SchematicCoordinates"], {-2}]]&, CountryData["Countries"]]
}, Boxed -> False, SphericalRegion -> True
];
CountryCircumball[c_, rgb_ : Red] := Block[
	{data, cdata, cty, ball},
	data = If[
		MemberQ[{Entity["Country", "China"], "China"}, c],
		Flatten[CountryData[#, {"FullPolygon", "Mercator"}][[1]]& /@ {"China", "Taiwan"}],
		Flatten[First@CountryData[c, {"FullPolygon", "Mercator"}]]
	];
	(*LatitudeLongitude[GeoGridPosition[{1,1},"Mercator"]]*)
	cdata = Function[{x, y}, {180 Gudermannian[\[Pi] y / 180] / Pi, x}] @@@ Partition[data, 2];
	cty = Graphics3D[{rgb, PointSize[Medium], Point[SC[#]]& /@ cdata}, Boxed -> False, SphericalRegion -> True, ViewAngle -> .3];
	Echo[ball = BoundingRegion[SC /@ cdata, "MinBall"], "pos: "];
	Show[cty, CountryEarth[], Graphics3D[{Opacity[.4], rgb, ball}]]
]