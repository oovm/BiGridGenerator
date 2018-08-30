IsNash[a_, S_] := Block[{l},
	l = Dimensions[a][[1]];
	Isnash[a, S]
];

Isnash[a_, S_] := Block[{m1, m2, Eu, br1, br2, t},
	Eu[2, st_] := N[S[[1]].a[[Range[1, l], st, 2]]];
	Eu[1, st_] := N[S[[2]].a[[st, Range[1, l], 1]]];
	m1 = Max[Table[Eu[1, t], {t, 1, l}]];
	m2 = Max[Table[Eu[2, t], {t, 1, l}]];
	br1 = Table[If[Eu[1, t] == m1, 0, 1], {t, 1, l}];
	br2 = Table[If[Eu[2, t] == m2, 0, 1], {t, 1, l}];
	If[br1.S[[1]] + br2.S[[2]] == 0, True, False]
];

squareFill[a_] := Block[{n, l},
	n[i_] := Dimensions[a][[i]];
	l = Max[n[1], n[2]];
	Table[ If[i <= n[1] && j <= n[2], a[[i, j]], {Min[a] - 1, Min[a] - 1}], {i, 1, l}, {j, 1, l}]
]

Nash[game_] := Block[
	{n, l, anew, MapD, Dropd, solns},
	If[Length[Dimensions[game]] != 3, Return["Not a two-player game!"]];
	n[i_] := Dimensions[game][[i]];
	l = Max[n[1], n[2]];
	If[2 != Dimensions[game][[3]], Return["Payoffs aren't defined for two players"]];
	sols = If[n[1] != n[2], anew = squareFill[game];
	solns = Nash[anew];
	Dropd[i_][x_] := Drop[x, n[i] - l];
	MapD[x_] := MapAt[Dropd[1], MapAt[Dropd[2], x, {{2}}], {{1}}];
	Map[MapD, solns]
		, NashSq[game]
	];
	SparseArray@*PadRight /@ sols
]
NashSq[a_] := Block[{t1, t2, t3, l, p, pp, a1list, a2list, blist, f, pos, nq, pn, nlist,
	eqn1, eqn2, eqns1, eqns2, ans1, ans2, i, j, NashE},
	l = Dimensions[a][[1]];
	pp = Table[p[i], {i, 1, l}]; a1list = {}; a2list = {};
	blist = Table[Mod[Floor[j / 2^i], 2], {j, 1, 2^l - 1}, {i, 0, l - 1}];
	For[t1 = 1, t1 <= Length[blist], t1++,
		{ num = Apply[Plus, blist[[t1]]];
		f[x_] := If[Apply[Plus, x] == num, True, False];
		slist = Select[blist, f];
		For[t2 = 1, t2 <= Length[slist], t2++,
			{ pos = Flatten[Position[slist[[t2]], 1]];
			eqn1 = Table[(pp * blist[[t1]]).a[[Range[1, l], pos[[t3]], 2]], {t3, 1, num}];
			eqn2 = Table[(pp * blist[[t1]]).a[[pos[[t3]], Range[1, l], 1]], {t3, 1, num}];
			eqns1 = Table[eqn1[[i]] == eqn1[[i + 1]], {i, 1, num - 1}];
			eqns2 = Table[eqn2[[i]] == eqn2[[i + 1]], {i, 1, num - 1}];
			ans1 = Solve[Join[eqns1, {Apply[Plus, pp * blist[[t1]]] == 1}], pp];
			
			ans2 = Solve[Join[eqns2, {Apply[Plus, pp * blist[[t1]]] == 1}], pp];
			
			AppendTo[a1list, Flatten[(pp * blist[[t1]]) /. ans1]];
			AppendTo[a2list, Flatten[(pp * blist[[t1]]) /. ans2]];
			}]}];
	nq[x_] := Apply[And, Table[NumberQ[N[x[[i]]]], {i, 1, Length[x]}]];
	pn[x_] := Apply[And, Table[N[x[[i]]] >= 0 && N[x[[i]]] <= 1, {i, 1, Length[x]}]];
	a1list = Union[Select[Select[a1list, nq], pn]];
	a2list = Union[Select[Select[a2list, nq], pn]];
	nlist = Flatten[Table[{a1list[[i]], a2list[[j]]}, {i, 1, Length[a1list]},
		{j, 1, Length[a2list]}], 1];
	INash[S_] := Isnash[a, S];
	NashE = Select[nlist, INash]
];