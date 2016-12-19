(* ::Package:: *)
(* ::Title:: *)
(*Example(样板包)*)
(* ::Subchapter:: *)
(*程序包介绍*)
(* ::Text:: *)
(*Mathematica Package*)
(*Created by Mathematica Plugin for IntelliJ IDEA*)
(*Establish from GalAster's template*)
(**)
(*Author:GalAster*)
(*Creation Date:2016-11-11*)
(*Copyright:CC4.0 BY+NA+NC*)
(**)
(*该软件包遵从CC协议:署名、非商业性使用、相同方式共享*)
(**)
(*这里应该填这个函数的介绍*)
(* ::Section:: *)
(*函数说明*)
BeginPackage["ExCode`"];
ExEncrypt::usage="ExEncrypt[Str,Way]以方式Way给出输入代码Str的超编码";
ExDecrypt::usage="ExDecrypt[Str,Way]以方式Way给出输入代码Str的超解码";
(* ::Section:: *)
(*程序包正体*)
(* ::Subsection::Closed:: *)
(*主设置*)
ExCode$Version="V0.1";
ExCode$Environment="V11.0+";
ExCode$LastUpdate="2016-11-11";
ExCode::usage = "程序包的说明,这里抄一遍";
Begin["`Private`"];
(* ::Subsection::Closed:: *)
(*主体代码*)









(* ::Subsubsection:: *)
(*超加密、超解密*)
SetAttributes[{CodeToCipher,ExEncrypt},HoldAll];
CodeToCipher[Str_]:=Module[{密匙,输出},
  密匙=GenerateSymmetricKey[Method-><|"Cipher"->"AES256",
    "InitializationVector"->ByteArray[{1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16}]|>];
  输出=Join[Normal@密匙["Key"],Normal@Encrypt[密匙,Compress@Hold@Str]["Data"]]];
CipherToCode[Str_,Safe_:False]:=Module[{破译密匙,破译内容},
  破译密匙=SymmetricKey[<|"Cipher"->"AES256","BlockMode"->"CBC","Key"->ByteArray[Str[[1;;32]]],
    "InitializationVector"->ByteArray["AQIDBAUGBwgJCgsMDQ4PEA=="]|>];
  破译内容=EncryptedObject[<|"Data"->ByteArray[Str[[33;;-1]]],
    "InitializationVector"->ByteArray["AQIDBAUGBwgJCgsMDQ4PEA=="],"OriginalForm"->String|>];
  If[Safe,Uncompress@Decrypt[破译密匙,破译内容],ReleaseHold@Uncompress@Decrypt[破译密匙,破译内容]]];
CharSet[Str_]:=StringPartition[Str,1];
CharAss[Way_]:=AssociationThread[Range@Length[CharSet[Way]]->CharSet[Way]];
CharAnti[Way_]:=AssociationThread[CharSet[Way]->Range@Length[CharSet[Way]]];
ExEncrypt[Str_,Way_]:=StringJoin[Map[CharAss[Way],IntegerDigits[FromDigits[CodeToCipher@Str,256],Length@CharSet[Way]]+1]];
ExDecrypt[Str_String,Way_,Safe_:False]:=CipherToCode[IntegerDigits[FromDigits[Map[CharAnti[Way],
  StringPartition[Str,1]]-1,Length@CharSet[Way]],256],Safe];
(*StringJoin@Union@StringPartition[%,1]*)
CharSet[Language->"MarySue"]:=StringPartition[
  "丝丹丽之乐云亚仪伊优伤佳依俏倩倾兮兰冰凌凝凡凤凪利千华卿可叶吉君咏哀嘉园城基塔墨夏多奥如妍妖妙妮妲姆姣姬娅娜娣娥娴婉婵婷媛嫩宁安宜寂\
寇寒岚巧希幻幽弥彩影御心思怡恋恩悠悦情慕慧拉文斯春昭晓晗晶曦曼月朵枝枫柒柔格桂梅梦樱欢欣殇残毓沫泪洁洛浅海涅淑清温渺滢澜澪灵烟然燕燢爱爽玉玖\
玛玥玫环玲珊珍珠琉琦琪琬琰琳琴琼瑗瑞瑟瑰瑶瑷璃璎璐璧白百盘眉真碎离秀秋筱米素紫红纨纯纱绯缈美羽翠翼育舒舞艳艺艾芊芝芬花芳芸苏苑英茉茗茜茹荔荷\
莉莎莲莳莹莺菁菲萌萍萝萦萨落蒂蓉蓓蓝蔷蕊蕴蕾薇薰蝶融血裳语贞迷邪铃银锦阳陌雁雅雨雪霄霜霞霭露青静音韵颖颜风飘香馥馨魂魅魑鸢黎黛",1];
(* https://github.com/atonasting/marysue-encoder
CharSet[Language\[Rule]"MarySue"]:=StringPartition[
"薰璃安莹洁莉樱殇雪羽晗灵血娜丽魑魅塔利亚伤梦儿海蔷玫瑰泪邪凡多姆威恩夏影琉舞雅蕾玥瑷曦月瑟薇蓝岚紫蝶馨琦洛凤颜鸢希玖兮雨烟叶兰凝冰伊如落心\
语凌爱陌悠千艳优花晶墨阳云筱残莲沫渺琴依然丝可茉黎幽幻银韵倾乐慕文思蕊清碎音芊黛怡莎苏香城萌美迷离白嫩风霜萝妖百合珠喃之倩情恋弥绯芸茜魂澪琪\
欣呗缈娅吉拉斯基柔惠朵茹妙铃裳纱颖蕴燢浅萦璎糜凪莳娥寂翼巧哀俏涅盘辰芝艾柒曼妲眉御寇妮米菲奥格萨温蒂",1];
*)
ExEncrypt[Str_,Language->"MarySue"]:=Module[{ans,ins},
  ans=IntegerDigits[FromDigits[CodeToCipher@Str,256],Length@CharSet[Language->"MarySue"]]+1;
  ins=Select[Accumulate[{RandomInteger[{2,8}]}~Join~RandomInteger[{1,9},Length@ans]],#<Length@ans&];
  StringInsert[StringJoin[CharAss[Language->"MarySue"]/@ans],"\[CenterDot]",ins]];
ExDecrypt[Str_String,Language->"MarySue",Safe_:False]:=Module[{input,res},
  input=CharAnti[Language->"MarySue"]/@StringPartition[StringDelete[Str,"\[CenterDot]"],1];
  res=IntegerDigits[FromDigits[input-1,Length@CharSet[Language->"MarySue"]],256];
  CipherToCode[res,Safe]];
CharSet[Language->name_]:=Alphabet[Language->name];
CharSet[Language->"Chinese"]:=StringPartition[FromCharacterCode[Range[13312,40869]],1];
CharSet[Language->"ASCII"]:=StringPartition[FromCharacterCode[Range[32,126]],1];




(* ::Subsubsection:: *)
(*功能块 2*)

(*空*)


(* ::Subsection::Closed:: *)
(*附加设置*)
End[] ;

EndPackage[];
