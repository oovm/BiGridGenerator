g4 = ReplacePart[RandomInteger[{-100, 100}, {5, 5, 7, 8, 4}], {3, 4, 5, 6} -> {100, 100, 100, 100}]

(*four player game with at least one manually inserted Nash equilibrium just so we can make sure it works*);

nashPos[g_] := Select[Position[g, _, {ArrayDepth[g] - 1}, Heads -> False], pos\[Function]And @@ Table[Extract[g, Append[pos, player]] >= Max[Extract[g, Table[Append[ReplacePart[pos, player -> choice], player], {choice, Range[Dimensions[g][[player]]]}]]], {player, Range[ArrayDepth[g] - 1]}]]

eliminate[g_, {startingAugmentedStrategyCombinations_, ns_}] := Module[
	{
		candidate = First[startingAugmentedStrategyCombinations],
		complementaryPattern, augmentedComplementaryStrategyCombinations,
		scores, best, bestQList, eliminateStrategyCombinations, eliminateAugmentedStrategyCombinations,
		strategyCombinationEliminationPatternList, scoreAugmentationPositions, nsAug, newNS,
		eliminationPatterns, newAugmentedStrategyCombinations
	},
	complementaryPattern = ReplacePart[candidate, Last[candidate] -> _];
	augmentedComplementaryStrategyCombinations = Cases[Position[g, _, {ArrayDepth[g]}, Heads -> False], complementaryPattern];
	scores = Extract[g, augmentedComplementaryStrategyCombinations];
	best = Max[scores];
	bestQList = Map[score\[Function]Equal[score, best], scores];
	eliminateStrategyCombinations = Map[asc\[Function]ReplacePart[asc, -1 -> _], Pick[augmentedComplementaryStrategyCombinations, bestQList, False]];
	eliminateAugmentedStrategyCombinations = Pick[augmentedComplementaryStrategyCombinations, bestQList, True];
	scoreAugmentationPositions = Map[asc\[Function]Most[asc], Pick[augmentedComplementaryStrategyCombinations, bestQList, True]];
	nsAug = SparseArray[Thread[Rule[scoreAugmentationPositions, 1]], Dimensions[ns]];
	newNS = Normal[nsAug] + ns;
	eliminationPatterns = Alternatives @@ Join[eliminateStrategyCombinations, eliminateAugmentedStrategyCombinations];
	newAugmentedStrategyCombinations = DeleteCases[startingAugmentedStrategyCombinations, Alternatives @@ eliminationPatterns];
	{newAugmentedStrategyCombinations, Normal[nsAug] + ns}
];

nashPos2[g_] := Position[NestWhile[eliminate[g, #]&, {Position[g, _, {ArrayDepth[g]}, Heads -> False], SparseArray[{}, Most@Dimensions[g]]}, x\[Function]Length[x[[1]]] > 0, 1, 1500][[2]], ArrayDepth[g] - 1]




{

}



(*
Nash[game_] := Block[
	{n, l, anew, MapD},
	If[Length[Dimensions[game]] != 3, Return["Not a two-player game!"]];
	n[i_] := Dimensions[game][[i]];
	l = Max[n[1], n[2]];
	If[2 != Dimensions[game][[3]], Return["Payoffs aren't defined for two players"]];
	sols = NashSq@If[n[1] != n[2], squareFill[game], game];
	SparseArray@*PadRight /@ sols
]
squareFill[m_] := Block[
	{w, h, l},
	{w, h} = Dimensions[m];
	l = Max[w, h];
	Table[If[i <= w && j <= h, m[[i, j]], {Min[m] - 1, Min[m] - 1}], {i, 1, l}, {j, 1, l}]
];
*)

NashQ[a_, S_] := Block[
	{m1, m2, Eu, br1, br2, t, l},
	l = Dimensions[a][[1]];
	Eu[2, st_] := N[S[[1]].a[[Range[1, l], st, 2]]];
	Eu[1, st_] := N[S[[2]].a[[st, Range[1, l], 1]]];
	m1 = Max[Table[Eu[1, t], {t, 1, l}]];
	m2 = Max[Table[Eu[2, t], {t, 1, l}]];
	br1 = Table[If[Eu[1, t] == m1, 0, 1], {t, 1, l}];
	br2 = Table[If[Eu[2, t] == m2, 0, 1], {t, 1, l}];
	If[br1.S[[1]] + br2.S[[2]] == 0, True, False]
];

NashSq[a_] := Block[
	{f, i, j, l, p, nq, pn, pp, t1, t2, t3, num, pos, ans1, ans2, eqn1, eqn2, bList, eqns1, eqns2, NashE, nList, sList, a1list, a2list},
	l = Dimensions[a][[1]];
	pp = Table[p[i], {i, 1, l}];
	a1list = {};
	a2list = {};
	bList = Table[Mod[Floor[j / 2^i], 2], {j, 1, 2^l - 1}, {i, 0, l - 1}];
	For[t1 = 1, t1 <= Length[bList], t1++,
		{ num = Apply[Plus, bList[[t1]]];
		f[x_] := If[Apply[Plus, x] == num, True, False];
		sList = Select[bList, f];
		For[t2 = 1, t2 <= Length[sList], t2++,
			{ pos = Flatten[Position[sList[[t2]], 1]];
			eqn1 = Table[(pp * bList[[t1]]).a[[Range[1, l], pos[[t3]], 2]], {t3, 1, num}];
			eqn2 = Table[(pp * bList[[t1]]).a[[pos[[t3]], Range[1, l], 1]], {t3, 1, num}];
			eqns1 = Table[eqn1[[i]] == eqn1[[i + 1]], {i, 1, num - 1}];
			eqns2 = Table[eqn2[[i]] == eqn2[[i + 1]], {i, 1, num - 1}];
			ans1 = Solve[Join[eqns1, {Apply[Plus, pp * bList[[t1]]] == 1}], pp];
			ans2 = Solve[Join[eqns2, {Apply[Plus, pp * bList[[t1]]] == 1}], pp];
			AppendTo[a1list, Flatten[(pp * bList[[t1]]) /. ans1]];
			AppendTo[a2list, Flatten[(pp * bList[[t1]]) /. ans2]];
			}]
		}];
	nq[x_] := Apply[And, Table[NumberQ[N[x[[i]]]], {i, 1, Length[x]}]];
	pn[x_] := Apply[And, Table[N[x[[i]]] >= 0 && N[x[[i]]] <= 1, {i, 1, Length[x]}]];
	a1list = Union[Select[Select[a1list, nq], pn]];
	a2list = Union[Select[Select[a2list, nq], pn]];
	nList = Flatten[Table[{a1list[[i]], a2list[[j]]}, {i, 1, Length[a1list]}, {j, 1, Length[a2list]}], 1];
	NashE = Select[nList, NashQ[a, #]&]
];


Nash[mx_, my_] := Block[
	{squareFill, game},
	squareFill[m_?MatrixQ] := Module[
		{d = Dimensions@m},
		If[Unequal @@ d, PadRight[m, Array[Max@d&, 2], Min@m - 1], m]
	];
	game = Transpose[squareFill /@ {mx, my}, {3, 1, 2}];
	SparseArray@*PadRight /@ Quiet@NashSq[game]
]