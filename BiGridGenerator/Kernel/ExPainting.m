(* Mathematica Package *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)

(* :Title: PolyPainting *)
(* :Context: PolyPainting` *)
(* :Author: GalAster *)
(* :Date: 2016-2-17 *)

(* :Package Version: 0.5 *)
(* :Update: 2016-10-17 *)
(* :Mathematica Version: 11.0+ *)
(* :Copyright:该软件包遵从CC协议:BY+NA+NC(署名、非商业性使用、相同方式共享） *)
(* :Keywords: *)
(* :Discussion: *)

BeginPackage["PolyPainting`"];
(* Exported symbols added here with SymbolName::usage *)

Begin["`Private`"];
TriPainting[i_, n_: 1000] :=
    Module[{x, y, pt, pts}, {x, y} = ImageDimensions[i];
    pt = Reverse /@RandomChoice[Flatten@ImageData@GradientFilter[i, 2] -> Tuples@{Range[y, 1, -1], Range[x]}, n];
    pts = Join[pt, {{0, 0}, {x, 0}, {x, y}, {0, y}}];
    Graphics[With[{col = RGBColor@ImageValue[i, Mean @@ #]}, {EdgeForm@col,col, #}] & /@ MeshPrimitives[DelaunayMesh@pts, 2]]];
PloyPainting[i_, n_: 1000] := Module[{x, y, gr, pt},{x, y} = ImageDimensions[i];
gr = ListDensityPlot[Transpose@{RandomReal[x, n], RandomReal[y, n], RandomReal[1, n]},
  InterpolationOrder -> 0, Frame -> False, Mesh -> All,AspectRatio -> Automatic];
pt = Polygon[#[[1]]] & /@ Cases[Normal@gr, _Polygon, \[Infinity]];
Graphics[With[{col = RGBColor@ImageValue[i, Mean @@ #]}, {EdgeForm@col,col, #}] & /@ pt, AspectRatio -> ImageAspectRatio[i]]];
CycPainting[img_, n_: 10000] := Module[{etf, sdf, map, mapdata, data, w, h, ch, spots},
  etf = EntropyFilter[img, 12] // ImageAdjust;
  sdf = ColorConvert[StandardDeviationFilter[img, 5], "GrayScale"] //ImageAdjust;
  map = ImageAdd[sdf, etf] // ImageAdjust;
  mapdata = ImageData[map];
  data = ImageData[img];
  {w, h} = ImageDimensions[img];
  ch = RandomChoice[(Flatten[mapdata] + 0.1)^1.7 ->Join @@ Table[{i, j}, {i, h}, {j, w}], 35000];
  spots =Reverse@SortBy[{data[[#1, #2]], {#2, -#1}, 15 (1.1 - mapdata[[#1, #2]])^1.8} & @@@ ch, Last];
  Graphics[{RGBColor[#1], Disk[#2, #3]} & @@@ spots, Background -> GrayLevel[0.75], PlotRange -> {{1, w}, {1, -h}}]];
TranslateObject[p_, {x_, y_}] := Map[{x, y} + # &, p, {2}];
HouseHexPolygon[s_, 0] :=Polygon[s*{{1/2, -1}, {3/2, 0}, {3/2, 1}, {-1/2, 1}, {-3/2,0}, {-3/2, -1}}];
HouseHexPolygon[s_, 1] :=Polygon[s*{{1, 1/2}, {0, 3/2}, {-1, 3/2}, {-1, -1/2}, {0, -3/2}, {1, -3/2}}];
HouseHexGrid[s_, imax_, jmax_] :=
    Block[{imod, jmod, k, m}, Flatten[Table[imod = Mod[2 i, 5];
    jmod = Mod[2 i + 3, 5];
    k = Range[0, Floor[(jmax - imod)/5]];
    m = Range[0, Floor[(jmax - jmod)/5]];
    {Table[TranslateObject[HouseHexPolygon[s, 0], s*{i + 1/2, j}], {j,jmod + 1/2 + 5*m}],
      Table[TranslateObject[HouseHexPolygon[s, 1], s*{i, j}], {j,imod + 5*k}]}, {i, 0, imax}], 2]];
HouseHexGridPerturbed[s_, h_, v_, r_] :=
    Block[{poly =Map[Round[#, 10.^-10] &, HouseHexGrid[N[s], h, v], {2}], pts,
      len, rules}, pts = DeleteDuplicates[Flatten[poly[[All, 1]], 1]];
    len = Length[pts];
    rules = Dispatch[Thread[pts -> Range[len]]];
    GraphicsComplex[pts + RandomReal[{-r, r}, {len, 2}],
      RandomSample[poly] /. rules]];
HexPainting[image_Image, s_, t_: 0, opts___] :=
    Block[{d, dim, g, centroids, colours, rr, gg, bb, greys},
      d = Reverse[ImageData[image]]; dim = Dimensions[d];
      g = HouseHexGridPerturbed[s, Floor[dim[[2]]/s - 3],
        Floor[dim[[1]]/s - 3], 0];
      centroids = Apply[Mean[g[[1, #]]] &, g[[2]], 1];
      colours = Apply[RGBColor, Extract[d, Map[Reverse, Round[centroids + s/2]]],1];
      greys = colours /. RGBColor[rr_, gg_, bb_] -> 0.299 rr + 0.587 gg + 0.114 bb;
      g[[2]] = Transpose[{colours, g[[2]]}][[Ordering[greys]]];
      If[t == 1, Graphics[{EdgeForm[{Thickness[0.0003], Black}], GraphicsComplex[g[[1]], g[[2]]]}, {opts}],
        Graphics[{GraphicsComplex[g[[1]], g[[2]]]}, {opts}]]];
(*见鬼,写完才发现Mathematica没有YUV的色彩空间,白写了我靠
Solve[{y,u,v}=={0.299*r+0.587*g+0.114*b,-0.169r-0.331g+0.5b+128,0.5r-0.419g-0.081b+128},{b,g,r}];
YUV2RGB=Function[{y,u,v},Evaluate@Flatten[{r,g,b}/.%]];
CYR=77;CYG=150;CYB=29;
CUR=-43;CUG=-85;CUB=128;
CVR=128;CVG=-107;CVB=-21;
{{CYR,CYG,CYB},{CUR,CUG,CUB},{CVR,CVG,CVB}}
y=BitShiftRight[CYR*r+CYG*g+CYB*b,8];
u=BitShiftRight[CUR*r+CUG*g+CUB*b,8];
v=BitShiftRight[CVR*r+CVG*g+CVB*b,8];
RGB2YUV=Function[{r,g,b},BitShiftRight[{29b+150g+77r,128b-85g-43r,-21b-107g+128r},8]+{0,128,128}];
Image[Nest[Floor@Apply[YUV2RGB,Apply[RGB2YUV,#,{2}],{2}]&,st,20],"Byte"]*)
(*手撸JPEG的压缩算法,只能压缩灰度图,而且还真心TM慢
DCT[x_?MatrixQ]:=Transpose[N@FourierDCTMatrix[8]].x.FourierDCTMatrix[8]
IDCT[x_?MatrixQ]:=Transpose[FourierDCTMatrix[8,3]].x.FourierDCTMatrix[8,3]
jpeg[img_]:=ImageAssemble[Map[Image[Threshold[DCT[ImageData[#]],{"LargestValues",8}]]&,ImagePartition[ImageSubtract[img],8],{2}]]
ImageAssemble[Map[Image[IDCT[ImageData[#]]]&,ImagePartition[jpeg[img],8],{2}]];*)
(*伪造的劣化绿绿算法,再编译加速下好了
RGB2RGB=Function[{r,g,b},{RandomReal[{0.9,1.0}]r,RandomReal[{0.99,1.01}]g,RandomReal[{0.9,1.0}]b}];*)
RGB2RGB=Compile[{{r,_Integer},{g,_Integer},{b,_Integer}},{RandomReal[{0.9,1.0}]r,RandomReal[{0.99,1.01}]g,RandomReal[{0.9,1.0}]b}];
TiebaPainting[image_,qua_:0.5,time_:20]:=Module[{img,w},
  img=image;
  w=ImageDimensions[img][[1]];
  img=ImageResize[img,201];
  img=Nest[Image[Apply[RGB2RGB,ImageData[#,"Byte"],{2}],"Byte"]&,img,time];
  Do[Export["baidu.jpeg",img,"CompressionLevel"->(1-qua)];
  img=Import["baidu.jpeg"]
    ,{n,1,time}];
  img=ImageResize[img,w]]
ExpandPainting[img_,neigh_:20,samp_:1000]:=Module[{dims,canvas,mask},
      dims=ImageDimensions[img];
      canvas=ImageCrop[starryNight,2*dims,Padding->White];
      mask=ImageCrop[ConstantImage[Black,dims],2*dims,Padding->White];
      Inpaint[canvas,mask,Method->{"TextureSynthesis","NeighborCount"->30,"MaxSamples"->1000}]];
(*LineWebPainting[图像,细腻度:100]*)
LineWebPainting[img_,k_:100]:=Module[{radon,lhalf,inverseDualRadon,lines},
      radon=Radon[ColorNegate@ColorConvert[img,"Grayscale"]];
      {w,h}=ImageDimensions[radon];
      lhalf=Table[N@Sin[\[Pi] i/h],{i,0,h-1},{j,0,w-1}];
      inverseDualRadon=Image@Chop@InverseFourier[lhalf Fourier[ImageData[radon]]];
      lines=ImageApply[With[{p=Clip[k#,{0,1}]},RandomChoice[{1-p,p}->{0,1}]]&,inverseDualRadon];
      ColorNegate@ImageAdjust[InverseRadon[lines,ImageDimensions[img],Method->None],0,{0,k}]];









End[];




EndPackage[];