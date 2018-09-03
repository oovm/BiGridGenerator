MXNetLink`MXJSONPlot[expr_Association, OptionsPattern[]] := Block[
	{
		showTensors, rotated, vertexLabels, vertexOrder, edgeBundling, outputTensors,
		internalDimensions, nodes, argnodes, heads, MXNetLink`Visualization`PackagePrivate`$oldids, nameStrings, typeStrings,
		edges, nodeOps, longRange, opTypes, opNames, nullType, blank, maxIndex,
		name, nodeDims, edgeTooltips, nops, opStyles, opSizes, vertexTypeData, labels,
		infoGrids, nnodes
	},
	{
		showTensors, rotated, vertexLabels, vertexOrder, edgeBundling, outputTensors,
		internalDimensions
	} = OptionValue @ {
		"ShowTensors", "Rotated", "VertexLabels", "VertexOrder", "EdgeBundling", "OutputTensors",
		"InternalDimensions"
	};
	{nodes, argnodes, heads} = Lookup[expr, {"nodes", "arg_nodes", "heads"}, GeneralUtilities`Panic[]];
	MXNetLink`Visualization`PackagePrivate`$oldids = If[ListQ[vertexOrder],
		Map[Function[GeneralUtilities`IndexOf[vertexOrder, #name] - 1], nodes],
		Range[Length[nodes]] - 1
	];
	nodes = Map[
		Function[
			Append[#, "inputs1" -> (Part[#inputs, All, 1] + 1)]
		],
		nodes
	];
	nameStrings = Map[MXNetLink`Visualization`PackagePrivate`toVertexLabelString[#name]&, nodes];
	typeStrings = Map[MXNetLink`Visualization`PackagePrivate`toVertexTypeString[#op]&, nodes];
	AddTo[argnodes, 1];
	AddTo[heads, 1];
	edges = Apply[Join,
		MapIndexed[
			Function @ If[
				SameQ[#op, "null"],
				Nothing,
				If[showTensors,
					Thread @ Prepend[#2, #inputs1],
					Thread @ Prepend[#2, Complement[#inputs1, argnodes]]
				]
			],
			nodes
		]
	];
	edges = DeleteDuplicates @ edges;
	nodeOps = nodes[[All, "op"]];
	If[And[edgeBundling, !FreeQ[nodeOps, "Concat" | "SliceChannel"]],
		longRange = MXNetLink`Visualization`PackagePrivate`pickLongRangeEdges[edges, nodes],
		longRange = None;
	];
	{opTypes, opNames} = GeneralUtilities`Labelling @ nodeOps;
	nullType = GeneralUtilities`IndexOf[opNames, "null"];
	nodes = MapIndexed[Function[Append[#, "id" -> (First[#2] - 1)]],
		nodes
	];
	If[showTensors && ListQ[outputTensors],
		opTypes = Join[opTypes, ConstantArray[nullType, Length @ outputTensors]];
		argnodes = Join[argnodes, Range[Length[outputTensors]] + Max[edges]];
		nameStrings = Join[nameStrings, outputTensors];
		blank = ConstantArray["", Length @ outputTensors];
		MXNetLink`Visualization`PackagePrivate`$oldids = Join[MXNetLink`Visualization`PackagePrivate`$oldids, blank];
		typeStrings = Join[typeStrings, blank];
		nodes = Join[nodes, blank];
		maxIndex = Max @ edges;
		MapIndexed[
			Function[AppendTo[edges, {First @ #, First[#2] + maxIndex}]],
			heads
		]
	];
	edgeTooltips = If[SameQ[internalDimensions, None],
		None,
		nodeDims = Table[
			name = Internal`UnsafeQuietCheck[nodes[[i, "name"]], None];
			MXNetLink`Visualization`PackagePrivate`toDimLabel @ Lookup[
				internalDimensions, name, If[StringQ[name],
					Lookup[internalDimensions, StringJoin[name, "_output"], None],
					None
				]
			],
			{i, Length @ nodes}
		];
		((Part[nodeDims, #]&) @@@ edges) /. BatchSize -> "b"
	];
	nops = Length @ opNames;
	opStyles = Map[MXNetLink`Visualization`PackagePrivate`opColor, opNames];
	opSizes = ReplacePart[ConstantArray[6, nops], nullType -> 4];
	opStyles = ReplacePart[opStyles, nullType -> Gray];
	opNames = opNames /. "null" -> "Tensor";
	vertexTypeData = <|"VertexStyles" -> opStyles|>;
	If[showTensors,
		vertexTypeData = Join[vertexTypeData, <|"VertexSizes" -> opSizes|>]
	];
	labels = ReplaceAll[vertexLabels,
		{"Name" :> nameStrings, "ID" :> MXNetLink`Visualization`PackagePrivate`$oldids, "Type" :> typeStrings}
	];
	infoGrids = Map[MXNetLink`Visualization`PackagePrivate`nodeInfoGrid, nodes];
	nnodes = Length @ nodes;
	GeneralUtilities`LayerPlot[edges,
		"VertexLabels" -> labels, "VertexTooltips" -> infoGrids, "HiddenVertices" -> If[showTensors, None, argnodes],
		"VertexTypeData" -> vertexTypeData, "VertexTypeLabels" -> opNames, "MaximumImageSize" -> None,
		"VertexSizes" -> 4, "EdgeTooltips" -> edgeTooltips,
		"BaseLabelStyle" -> {FontSize -> 7},
		"DuplicateInputVertices" -> True, If[showTensors, "VertexTypes" -> opTypes, "VertexStyles" -> opTypes],
		"LongRangeEdges" -> longRange, "Rotated" -> False, "ArrowShape" -> "Chevron", "LegendLabelStyle" -> 8
	]
]
Options[MXNetLink`MXJSONPlot] = {
	"ShowTensors" -> True,
	"Rotated" -> False,
	"VertexLabels" -> Placed["ID", Above],
	"VertexOrder" -> Automatic,
	"EdgeBundling" -> True,
	"OutputTensors" -> None,
	"InternalDimensions" -> None
};