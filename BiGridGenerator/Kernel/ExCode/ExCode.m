(* Mathematica Package *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)

(* :Title: ExCode *)
(* :Context: ExCode` *)
(* :Author: GalAster *)
(* :Date: 2016-11-11 *)

(* :Package Version: 0.2 *)
(* :Mathematica Version: 11.0+ *)
(* :Copyright:该软件包遵从CC协议:BY+NA+NC(署名、非商业性使用、相同方式共享） *)
(* :Keywords: *)
(* :Discussion: *)



BeginPackage["ExCode`"];
ExEncrypt::usage="ExEncrypt[Str,Way]以方式Way给出输入代码Str的超编码";
ExDecrypt::usage="ExDecrypt[Str,Way]以方式Way给出输入代码Str的超解码";



Begin["`Private`"];
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
CharSet["MarySue"]:=StringPartition[
      "丝丹丽之乐云亚仪伊优伤佳依俏倩倾兮兰冰凌凝凡凤凪利千华卿可叶吉君咏哀嘉园城基塔墨夏多奥如妍妖妙妮妲姆姣姬娅娜娣娥娴婉婵婷媛嫩宁安宜寂\
寇寒岚巧希幻幽弥彩影御心思怡恋恩悠悦情慕慧拉文斯春昭晓晗晶曦曼月朵枝枫柒柔格桂梅梦樱欢欣殇残毓沫泪洁洛浅海涅淑清温渺滢澜澪灵烟然燕燢爱爽玉玖\
玛玥玫环玲珊珍珠琉琦琪琬琰琳琴琼瑗瑞瑟瑰瑶瑷璃璎璐璧白百盘眉真碎离秀秋筱米素紫红纨纯纱绯缈美羽翠翼育舒舞艳艺艾芊芝芬花芳芸苏苑英茉茗茜茹荔荷\
莉莎莲莳莹莺菁菲萌萍萝萦萨落蒂蓉蓓蓝蔷蕊蕴蕾薇薰蝶融血裳语贞迷邪铃银锦阳陌雁雅雨雪霄霜霞霭露青静音韵颖颜风飘香馥馨魂魅魑鸢黎黛",1];
ExEncrypt[Str_,"MarySue"]:=Module[{ans,ins},
  ans=IntegerDigits[FromDigits[CodeToCipher@Str,256],Length@CharSet["MarySue"]]+1;
  ins=Select[Accumulate[{RandomInteger[{2,8}]}~Join~RandomInteger[{1,9},Length@ans]],#<Length@ans&];
  StringInsert[StringJoin[CharAss["MarySue"]/@ans],"\[CenterDot]",ins]];
ExDecrypt[Str_String,"MarySue",Safe_:False]:=Module[{input,res},
  input=CharAnti["MarySue"]/@StringPartition[StringDelete[Str,"\[CenterDot]"],1];
  res=IntegerDigits[FromDigits[input-1,Length@CharSet["MarySue"]],256];
  CipherToCode[res,Safe]];
CharSet["Binary"]:=StringPartition["01",1];
End[];

EndPackage[];
