(* Mathematica Package *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)

(* :Title: ExRandom *)
(* :Context: ExRandom` *)
(* :Author: GalAster *)
(* :Date: 2016-03-15 *)

(* :Package Version: 0.3 *)
(* :Update: 2016-10-15 *)
(* :Mathematica Version: 11.0+ *)
(* :Copyright:该软件包遵从CC协议:BY+NA+NC(署名、非商业性使用、相同方式共享） *)
(* :Keywords: *)
(* :Discussion: *)



BeginPackage["ExRandom`"];
RandomExample::usage = "RandomExample[]随机给出一个Mathematica的巧妙范例!";
AutoBiography::usage = "Biography[Name]可以随机生成Name的自传哦!";



Begin["`Private`"];
RandomExample[]:=
    Module[{dir,file,inputs,output,cap,i=0,j=1,in},
      dir=DirectoryName[FindFile["ExamplePages/CreateMolecularGraphs.nb"]];
      file=RandomChoice[FileNames["*",dir]];
      output=Import[file,{"Cells","Output"}][[1]];
      cap=CellLabel/.Options[output];
      If[!StringQ[cap],Return[$Failed]];
      cap=ToExpression[StringReplace[cap,"Out["~~x__~~"]"~~__:>x]];
      inputs=Import[file,{"Cells","Input"}];
      CellPrint[TextCell[StringReplace[file,__~~"ExamplePages":>"ExamplePages"],"Subsubsection"]];
      CellPrint[Reap[While[i<cap&&j<=Length[inputs],
        in=CellLabel/.Options[inputs[[j]]];
        If[StringQ[in],i=ToExpression[StringReplace[in,"In["~~x__~~"]"~~__:>x]]];
        Sow[inputs[[j++]]]]][[-1,1]]];
      CellPrint[output];];
RandomPartition[n_,p_?IntegerQ]:=Module[{r},r=RandomSample[Range[1,n],p-1]//Sort;
    AppendTo[r,n];Prepend[r//Differences,r[[1]]]];
RandomPartition[n_,V_?VectorQ]:=Module[{r,s},r=RandomInteger[V,n];
    s=Select[Accumulate@r,#<n&]~Join~{n};Append[Differences@s,s[[1]]]];
(*Random cosmic background radiation*)
InvMollweide[{x_,y_}]:=With[{theta=ArcSin[y]},{Pi(x)/(2Cos[theta]),ArcSin[(2theta+Sin[2theta])/Pi]}];
RandomCBR[res_:64]:=Module[{Alms,fieldN,dat,im},
  Do[Alms[l,m]=(Random[NormalDistribution[]]+I Random[NormalDistribution[]])/Sqrt[(l+2)(l+1)];
  Alms[l,-m]=(-1)^m Conjugate@Alms[l,m];,{l,0,48},{m,0,l}];
  Do[Alms[l,0]=(Random[NormalDistribution[]])/Sqrt[(l+2)(l+1)];,{l,0,48}];
  fieldN=Compile[{\[Theta],\[Phi]},Evaluate@Sum[Alms[l,m]SphericalHarmonicY[l,m,\[Theta],\[Phi]],{l,0,48},{m,-l,l}]];
  dat=ParallelTable[fieldN[\[Theta],\[Phi]],{\[Theta],0.0,Pi,Pi/res},{\[Phi],0.0,2Pi,Pi/res}];
  im=Re[dat]//Image//ImageAdjust//Colorize[#,ColorFunction->"LightTemperatureMap"]&;
  ImageTransformation[im,InvMollweide,DataRange->{{-Pi,Pi},{-Pi/2,Pi/2}},PlotRange->{{-2,2},{-1,1}},Padding->White]];
RandomPebble[n_,sc_:0.95]:=With[{data=MapIndexed[Flatten[{##1}]&,RandomReal[1,{n,2}]]},
  Normal[ListDensityPlot[data,InterpolationOrder->0,
    ColorFunction->Hue,Mesh->All,
    Background->Lighter[Hue[RandomReal[]],.75],Frame->False,
    ImageSize->400]]/.Polygon[l_,v_]:>Scale[{Hue[RandomReal[]],
    FilledCurve[BSplineCurve[l,SplineClosed->True,SplineDegree->3]]},sc]];






种族表 = {人类, 暗夜精灵, 矮人, 侏儒, 德莱尼, 狼人, 牛头人, 巨魔, 亡灵, 血精灵, 地精, 熊猫人, 恐惧魔王, 深渊领主, 艾瑞达, 泰坦, 龙族, 野猪人, 鹰身人, 娜迦, 鱼人, 其拉虫族, 虚灵, 戈隆, 食人魔, 阿努比萨斯, 维库, 托维尔, 锦鱼人, 魔古族, 蜥蜴人, 纳鲁};
等级表 = Range@100;
武器表 = {提洛许 - 世纪梦魇, 戈拉德 - 巨龙之暮, 巨龙之怒, 影之哀伤, 瓦兰奈尔 - 远古王者之锤, 灵弦长弓, 宇宙灌注者,毁灭, 瓦解法杖, 无尽之刃, 迁跃切割者, 索利达尔 - 群星之怒, 埃辛诺斯战刃, 安杜尼蘇斯 - 靈魂的收割者,埃提耶什 - 守护者的传说之杖, 雷霆之怒 - 逐风者的祝福之剑, 萨弗拉斯 - 炎魔拉格纳罗斯之手, 霜之哀伤, 灰烬使者};
必杀技表 = {回到过去, 魂之挽歌, 恩赐解脱, 梦境缠绕, 星体禁锢, 灵魂超度, 黄泉颤抖, 永恒冰壁, 浴火重生, 时光倒流,海妖之歌, 灵魂之矛, 幻化之锋, 月之祝福, 月之暗面, 月神之箭, 群星坠落, 神之力量, 圆月之舞};
战斗力表 = Range@(10^6);
死亡时间表 = Range[500, 1500];
死亡事件表 = {太过鬼畜, 偷看基友洗澡, 太过脑残, 金坷垃洗脑, 看B站视频, 码代码};

AutoBiography[姓名_] := Module[{choose},
  SeedRandom[Hash[姓名, "CRC32"]];
  choose[表_] := 表[[RandomInteger[{1, Length[表]}]]];
  {种族, 等级, 武器, 必杀技, 战斗力, 死亡时间, 死亡事件} = choose /@ {种族表, 等级表, 武器表, 必杀技表, 战斗力表, 死亡时间表, 死亡事件表};
  Print[ToString@姓名 ~~ "是一个" ~~ ToString@等级 ~~ "级的" ~~ ToString@种族 ~~ ",成名武器是" ~~ ToString@武器 ~~ ",必杀技是" ~~ ToString@必杀技 ~~ "," ~~ ToString@死亡时间 ~~ "年死于" ~~ ToString@死亡事件 ~~ "."]]




End[];
Protect[AutoBiography];

EndPackage[]