
game24[input_List, result_: 24] :=
	Quiet@Block[{add, sub, mul, div},
		With[{oprules = {add -> Plus, sub -> Subtract, mul -> Times,
			div -> Divide},
			specifics = {div[x_, 1] :> x, mul[x_, 1] :> x, mul[1, x_] :> x,
				add[2, 2] -> mul[2, 2]}},
			ParallelMap[RightComposition[
				Hold, ReplaceAll[oprules], ToString[#, InputForm] &,
				StringDelete[{"Hold[", "]"}],
				StringReplace[{"*" -> "\[Times]", "/" -> "\[Divide]"}]
			],
				Union[
					Select[result == (# /. oprules) &]@
						Groupings[Permutations@input, {add, sub, mul, div} -> 2],
					SameTest -> (0 ===
						Simplify[
							sub[#1, #2] //. specifics /.
								Prepend[oprules, k_Integer :> ToString[k]]] &)
				]
			]
		]
	];

RootHypergeometric[n_Integer,m_,t_]:=Sum[
	(-(1/((n-1)*k!)))*t^k*E^((2*Pi*I*(k-1)*m)/(n-1))*
		Pochhammer[(k-1)/(n-1)+1,k-1]*HypergeometricPFQ[Range[n-1]/n+(k-1)/(n-1),
		Delete[Range[k+1,k+n-1],-k+n-1]/(n-1),n*((n*t)/(n-1))^(n-1)],{k,0,n-2}
];
Root[#1^5-#1+1&,2]//N
N@RootHypergeometric[5,1,1]//Chop




{Subscript[l, 1], Subscript[l, 2], Subscript[l, 3], Subscript[l, 4]} =
	{y - a/3 == Subscript[k, 1] (x - a/2),
		y - a/2 == Subscript[k, 2] (x - 2 a/3),
		y - 2 a/3 == Subscript[k, 2] (x - a/2),
		y - a/2 == Subscript[k, 4] (x - a/3)}


{#, Style["中文测试 & English Test",
	FontFamily -> #]} & /@ $FontFamilies // TableForm

Subscript[p, A] = {0, 0};
Subscript[p, B] = {x, y} /.
	First@Solve[{y == 0, Subscript[l, 4]}, {x, y}];
Subscript[p, C] = {x, y} /.
	First@Solve[{y == 0, Subscript[l, 2]}, {x, y}];
Subscript[p, D] = {a, 0};
Subscript[p, E] = {x, y} /.
	First@Solve[{x == a, Subscript[l, 1]}, {x, y}];
Subscript[p, F] = {x, y} /.
	First@Solve[{x == a, Subscript[l, 3]}, {x, y}];
Subscript[p, G] = {a, a};
Subscript[p, H] = {x, y} /.
	First@Solve[{y == a, Subscript[l, 2]}, {x, y}];
Subscript[p, I] = {x, y} /.
	First@Solve[{y == a, Subscript[l, 4]}, {x, y}];
Subscript[p, J] = {0, a};
Subscript[p, K] = {x, y} /.
	First@Solve[{x == 0, Subscript[l, 3]}, {x, y}];
Subscript[p, L] = {x, y} /.
	First@Solve[{x == 0, Subscript[l, 1]}, {x, y}];
Subscript[p, M] = {x, y} /.
	First@Solve[{Subscript[l, 1], Subscript[l, 4]}, {x, y}];
Subscript[p, N] = {x, y} /.
	First@Solve[{Subscript[l, 1], Subscript[l, 2]}, {x, y}];
Subscript[p, O] = {x, y} /.
	First@Solve[{Subscript[l, 2], Subscript[l, 3]}, {x, y}];
Subscript[p, P] = {x, y} /.
	First@Solve[{Subscript[l, 3], Subscript[l, 4]}, {x, y}];


Tupper::prime = "`1` is not a prime!";
Options[TupperPlot] = {Prime -> 17, Times -> 6};
Options[TupperK] = {Prime -> 17, Times -> 6};
TupperK[img_, OptionsPattern[]] := Block[{i, m, r, p},
	p = OptionValue[Prime];
	If[! PrimeQ[p], Message[Tupper::prime, p]; Return[]];
	i = Rasterize[img, ImageSize -> {OptionValue[Times] p, p}];
	m = Map[Boole[Max@# < 1] &, ImageData[i], {2}];
	r = p FromDigits[Flatten@Reverse@Transpose[m], 2]]
TupperPlot[k_, OptionsPattern[]] := Block[{bin, p, t},
	p = OptionValue[Prime]; t = OptionValue[Times] ;
	bin = IntegerDigits[k/p, 2, t p^2 - 1];
	ArrayPlot[Reverse /@ Transpose@Partition[bin, p]]]


PDF[WeibullDistribution[1, 100, 0], t]
PDF[ExponentialDistribution[1/100], t]
InverseCDF[ExponentialDistribution[1/100], t]
Integrate[-100 Log[1 - t], {t, 0, x}, Assumptions -> 0 < x < 1]/
	Integrate[-100 Log[1 - t], {t, 0, 1}]
img = Plot[{Style[x, Dashed, Thickness[0.01]],
	Style[x - (-1 + x) Log[1 - x], Thickness[0.01]]}, {x, 0, 1},
	Filling -> {2 -> {1}}, PlotRange -> {0, 1}, AspectRatio -> 1,
	Axes -> False,
	Frame -> True,
	FrameTicks -> {Table[i, {i, 0, 1, 0.1}], Automatic},
	FrameLabel -> {"劳伦兹曲线:\!\(\*TagBox[\(TraditionalForm\`x + \((1 - \
x)\)\\\ \(log(1 - x)\)\),
TraditionalForm,\nEditable->True]\)", None, "基尼系数=0.5"},
	BaseStyle -> FontSize -> 12]


rule=Thread[Array[Subscript[a,#]&,5]->{2,3,4,5,6}];
\[Sigma][n_,k_:5]:=Plus@@Times@@@(Map[Subscript[a,#]&,Subsets[Range[k],{n}],{2}]^2)
u=16K^2;
Subscript[t, 2]=u-4\[Sigma][2]+\[Sigma][1]^2;
Subscript[t, 3]=8\[Sigma][3]+\[Sigma][1]Subscript[t, 2];
Subscript[t, 4]=-64\[Sigma][4]+Subscript[t, 2]^2;
Subscript[t, 5]=128\[Sigma][5];
eqn=\!\(
\*SubsuperscriptBox[\(t\), \(3\), \(2\)]\
\*SubsuperscriptBox[\(t\), \(4\), \(2\)]\)+u Subsuperscript[t, 4, 3]-16 Subsuperscript[t, 3, 3] Subscript[t, 5]-18 u Subscript[t, 3] Subscript[t, 4] Subscript[t, 5]-27 u^2 Subsuperscript[t, 5, 2]==0;
sols=Solve[{eqn/.rule,K>0},Reals];
Max[K/.sols]//ToRadicals//N
sol=Reduce[{Product[I i/(2R)+Sqrt[1-(i/(2R))^2],{i,{a,b,c,d}}]==-1}]
S=Sum[i Sqrt[(R/.Solve[{sol,R>0}])^2-i^2/4],{i,{a,b,c,d}}]/2//RootReduce



f[1, k_] := 1
f[n_, k_] := Mod[f[n - 1, k] + k, n, 1]
f[10, 2]
<< Combinatorica`
Tr@Position[Josephus@##2, 1 + #2 - #] &[5, 20, 3]
Range@1000 //. {x_, y_, z___} :> {z, x}
If[# < 2, 1, Mod[#0[# - 1, #2] + #2, #, 1]] &[1*^3, 2]
2 # - 2^IntegerLength[#, 2] + 1 &[1000]
2 # - 2*2^Floor@Log2@# + 1 &@1*^3


n=8;m=2;

GridsSelect[n_,m_]:=Block[{M=ConstantArray[0,{n-m+1,n-m+1}],drop,points,p},
	drop[{x_,y_}]:=M[[Max[x-m+1,1];;Min[x+m-1,n-m+1],
		Max[y-m+1,1];;Min[y+m-1,n-m+1]]]=1;
	points=Position[M,0];p={};
	While[points!={},
		AppendTo[p,RandomChoice@Position[M,0]];
		drop[Last@p];
		points=Position[M,0]];
	Association[{"Points"->p,"Area"->1.-Length[p]m^2/n^2}]];

try[n_,m_]:=Mean[(GridsSelect[n,#]&/@ConstantArray[m,100])[[All,"Area"]]]
try[n_,1]:=1;
AbsoluteTiming[data=Table[N@try[24,i],{i,1,24}]]
ListLinePlot[data,PlotTheme->"Detailed",LabelingFunction->(#1 &)]



path=NotebookDirectory[];ok=FileNames["*",path];
in=Transpose[{Range[#//Length],#}]&@CharacterRange["一","龥"];
Img[{n_,char_}]:=Block[{name,text},
	name=StringJoin[ToString/@{path,n,"-",char,".jpg"}];
	If[MemberQ[ok,name],Return[]];
	text=Binarize@Rasterize[Style[char,FontFamily->"楷体"],RasterSize->1024,ImageSize->Large];
	Export[name,text]]

ok = FileNames["*", NotebookDirectory[]];
AbsoluteTiming[ParallelMap[Img, in];]


Nest[(Defer[#1] & )[#1/\[Placeholder]] & , 1, 30]
ToCharacterCode["Hello human!"]
StringJoin@Riffle[StringJoin[ToString/@#]&/@PadRight[IntegerDigits[#,2]&/@%]," "]


DigitHis[a_, range_: 1000, digit_: 4] := Block[{data, title},
	data = FromDigits /@
		First /@ RealDigits[N[a]^Range[1000], 10, digit];
	title =
		StringJoin[
			ToString /@ {"底数", a, "  前", digit, "个数字  ", range, "样本"}];
	Histogram[data, Automatic, "Probability", PlotLabel -> title,
		LabelingFunction -> Above, PlotTheme -> "Classic"]]
GraphicsGrid@{{DigitHis[2],
	DigitHis[2, 10000]},
	{DigitHis[2, 1000, 5],
		DigitHis[233]}}
""


	---



	$\displaystyle \prod _{n>0} \cos \left(\frac{t}{2^n}\right) \sim \text{sinc}(x)$

$\displaystyle \prod _{n>0} \cos \left({\frac{t}{n}}\right) \sim e^{-\pi^2 x^2/64}\ \text{sinc}(2x)$


General case

	---

**Background**

	I was reading [Random Harmonic Series](http://www.stat.ualberta.ca/people/schmu/preprints/rhs.pdf).

It talks about $\displaystyle \sum _{n>0}  \xi_n \frac{1}{n}$, where $\xi_n$ is a random variable.

	Consider it's characteristic function.

	If we can find a function that approximation enough to the oragin, by calculate it's inverse fourier transform, we can get an approximate function.

	And I found : $\displaystyle F(t)=\exp\left(-\frac{\pi^2 t^2}{64}\right)\ \text{sinc}(2t)$ is a good approximation.

[enter image description here][1]

And the IFT is : $\displaystyle f(x)=\frac{1 }{8}\left[\text{erf}\left(\frac{8+4x}{\pi }\right)+\text{erf}\left(\frac{8-4 x}{\pi }\right)\right]$

This indeed close to the experimental results.

[enter image description here][2]

I tried to use this trick on other situation, but $\displaystyle \prod _{n>0} \cos \left(\frac{t}{a^b}\right) $ looks chaos. I can't figure it out.



[1]: https://i.stack.imgur.com/ijmv2.png
[2]: https://i.stack.imgur.com/1GTI2.png





b[0]:=-1/2;
b[n_]:=b[n]=-w[n,n+1]/n;
w[0,m_]:=0;
w[n_,m_]:=w[n,m]=a[m-1]+w[n-1,m]+Sum[a[j]w[n-1,m-j-1],{j,0,m-2}];
a[j_]:=u[0,j+1];
u[n_,k_]:=u[n,k]=Which[
	2^n-1==k,1,
	2^n-1>k,Sum[u[n-1,j]u[n-1,k-j],{j,0,k}],
	2^(n+1)-1>k,0,
	True,(u[n+1,k]-Sum[u[n,j]u[n,k-j],{j,k-1}])/2
];
Block[
	{$RecursionLimit=Infinity},
	(areas=Table[\[Pi](1-Sum[n b[n]^2,{n,nmax}]),{nmax,200}])
];
ListPlot[areas,PlotJoined->True,PlotStyle->Red,AxesLabel->TraditionalForm/@{n,Subscript[A, n]}]

CC
n=1000;
cost[i_]:=Block[
	{P=1,D=1,S=2,V=1,l},
	l=Drop[Divisors[i],-1];
	Join[
		Transpose[{Thread[l->i],S+i/l V}],
		{{i-1->i,P},{i+1->i,D}}
	]
]
raw=Drop[Flatten[Table[cost[i],{i,2,n+10}],1],-1];
{dir,wei}=Transpose[(SortBy[#,Last]&/@GatherBy[raw,First])[[All,1]]];
G=Graph[dir,EdgeWeight->wei,PlotTheme->"Monochrome",GraphLayout->"CircularEmbedding"];
ways=FindShortestPath[G,1,All]/@Range[n];
dis=GraphDistance[G,1,#]&/@Range[n]//Round;
\[Alpha]=a/.FindFit[dis,a Log[x],a,x]
Show[ListLinePlot@dis,Plot[a Log[x]/. {a->\[Alpha]},{x,1,n},PlotStyle->Red]]

trans[a_,b_]:=Join[
	Table[{st[a,b]->st[a,j],2},{j,b+1,a}],
	{{st[a,b]->st[a+b,b],1},{st[a,b]->st[a+1,b],1}}
];
trans[a_,0]:=Append[Table[{st[a,0]->st[a,j],2},{j,1,a}],{st[a,0]->st[a+1,0],1}];
trans[n_]:=Flatten[Table[trans[n,i],{i,0,n}],1];
GetPath[G_,n_]:=Block[{list,min,path},
	list=Table[GraphDistance[G,st[0,0],st[n,i]],{i,0,n}];
	min=Round@Flatten@Position[list,Min@list];
	path=Table[FindShortestPath[G,st[0,0],All][st[10,i]],{i,min}];
	Association["Min"->Round@Min@list,"Path"->path]
];