(* Mathematica Package *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)

(* :Title: ExRandom *)
(* :Context: ExRandom` *)
(* :Author: GalAster *)
(* :Date: 2016-03-15 *)

(* :Package Version: 0.4 *)
(* :Update: 2016-11-22 *)
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








RandomName
(*https://zhuanlan.zhihu.com/p/21258963*)
百家姓=StringPartition["王李张刘陈杨黄赵周吴徐孙马胡朱郭何罗高林郑梁谢唐许冯宋韩邓彭曹曾田于萧潘袁董叶杜丁蒋程余吕魏蔡苏任卢沈姜姚钟崔陆谭汪石傅贾范金方韦\
夏廖侯白孟邹秦尹江熊薛邱阎段雷季史陶毛贺龙万顾关郝孔向龚邵钱武扬黎汤戴严文常牛莫洪米康温代赖施覃安",1];
复姓={"欧阳","上官","司马","东方","诸葛","令狐","南宫","慕容","公孙","司徒","皇甫","夏候","万俟","宇文","轩辕","东郭","南门","西门","尉迟"};
通用字=StringPartition["天地日月星辰金木水火土风云雨声雷电山丘陵平原台江河川池湖海洋冰沙泽深涵春夏秋冬立分至谷梧桐银赤红丹朱黄绿碧青蓝紫黑白墨苍初旭光\
荣宝玉藏陶艺家宙代纪世期时宇界系统阶带和清温润森林亭楼阁欢迎宜宾客友恩华飞升圣少子心真人学敬敏精湛景影喜一二三四五六七八九十百千万兆里元鑫甘\
乐方圆周维望诺冠亚季殿尤其迪化靖瑞睿易奕熙然司令群奇遇繁衍",1];
男用字=StringPartition["高大岩石勇猛刚强雄威豪震壮阔宏伟杰建国庆筑基健康毅启明亮道德忠仁义礼智信诚谦贤良俊通达广长泰昌盛富贵吉祥洪烈照鸿浩波涛浪源潮渊\
隐逸超越特勤东南西北中甲乙丙丁戊己庚辛壬癸阳顶立辉耀军兵将帅武力本主钢铁锋峰钟鼎铭印玄太远上中近古继承松柏根柱梁成龙虎豹彪骏鹏鹤斌郎公士夫生\
汉伯叔之胜利赢权益人民团结正定安兴举直凯旋征旅守卫福禄寿奋斗跃进步同志向全恒衡鸣",1];
女用字=StringPartition["晓小妹姑娘妃姬后好妙妮娃爱媛婵娟娇媚婉婷妍嫣贝璧环琪璇玑珍珠玲珑佩琼瑶瑜琳珊晶莹花草兰英灵芝萱芸茗芬芳蓓蕾翠丽萍莲荷莉蔷薇茜莎\
蓉菱梅杏杨柳桂椿榆枫凤雉燕鸽莺鹃蝶锦绣绫罗绮衣裳慧惠香穗颖芒彩艳素秀美悦雯露霞雪虹涟溪思绪念想盼希梦境淑贞端庄宁静倩巧楚琴笛竹棋书诗文语音韵\
歌曲如若晴曦馨欣颜优雅怡暖满可佳嘉",1];



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