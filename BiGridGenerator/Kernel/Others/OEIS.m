(* ::Package:: *)

(* ::Section:: *)
(*(*OEIS Package: Title and comments*)*)


(* :Title: Package OEIS *)

(* :Context: Utilities`OEIS`         *)

(* :Author:  Enrique P\[EAcute]rez Herrero   *)

(* :Summary: 
	This package provides various utilities for 
    searching and working with the information in 
    The On-Line Encyclopedia of Integer Sequences.
    http://oeis.org/
*)

(* :Package Version: 2.02 *)

(* :Mathematica Version: 9.0.0.0 *)

(* :Links:
The OEIS Foundation Inc:            http://oeisf.org/
OEIS Wiki:                          http://oeis.org/wiki/
Psychedelic Geometry Blogspot:      http://psychedelic-geometry.blogspot.com/
OEIS Page:                          http://oeis.org/wiki/User:Enrique_P\[EAcute]rez _Herrero/
*)

(* :History:
	V. 1.0 26 August 2010, by E.P\[EAcute]rez Herrero.
    V. 2.0 21 November 2010, by EPH.
*)

(* :Keywords:
	packages, sequence, OEIS
*)

(* :Licence:
    OEIS.m:  Wolfram Mathematica Package for working with OEIS data.
    Copyright (C) 2015  Enrique P\[EAcute]rez Herrero

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
    
	Contact: psychgeometry@gmail.com
*)

(* :Limitations:  *)

(* :Discussion:  *)

(* :Changes:  
29/Aug/2010 - Added OEISbFile function and exporting to Html
28/Nov/2010 - Code rebuilt due to changes in OEIS, old links are no longer valid.
04/Jun/2011 - Fixed Unicode LF, instead of Window's CRLF for bFiles.
*)


(* ::Section:: *)
(*(*Begin Package and Help*)*)


BeginPackage["OEIS`"];


(*Help on Package functions *)


(*
Old links are no more valid
OEISServerURL::usage="http://oeis.org/classic/";
OEISWikiURL::usage="http://oeis.org/wiki/";
*)

OEISServerURL::usage="http://oeis.org/";


OEISTotalNumberOfSequences::usage="OEISTotalNumberOfSequences[]: Reads the actual number of Sequences that OEIS contais from http://oeis.org/
or returns a default value if it cannot connect.";

OEISValidateIDQ::usage="OEISValidateIDQ[ID]: Checks that the Sequence ID is valid.";
OEISImport::usage="OEISImport[ID,element]: Retrieves Data, Description, Image, Offset or bFile from OEIS Server"; 
OEISURL::usage="OEISURL[ID]: Gives the OEIS sequence URL when the ID is entered";
OEISFunction::usage="OEISFunction[ID]:  Creates a function named with the OEIS ID and preloads data from the sequence and its bFile, if found";

OEISExport::usage="OEISExport[ID,filename]: Export required data to file";


(*Help on other Functions *)

OEISbFile::usage="OEISGeneratebFile[ID,Vmax,filena,a]: Save the bFile up to Vmax values.";



(*Help on Options *)

bFile::usage="bFile";
URL::usage="URL";
URLType::usage="URLType";
AddHelp::usage="AddHelp";
Output::usage="Output";


(* Error Messages *)

OEIS::conopen="Cannot connect to OEIS Server: `1`"; 
OEIS::bFile ="Cannot open bFile from OEIS Server: `1`";
OEIS::ID="`1` is not a valid OEIS ID";


Unprotect["`*"];


Begin["`Private`"];


(* ::Section:: *)
(*(* URLs *)*)


(*
Old OEIS links are no longer valid
OEISServerURL="http://oeis.org/classic/";
OEISWikiURL="http://oeis.org/wiki/"
*)

OEISServerURL="http://oeis.org/";


(* ::Section:: *)
(*(* Importing Data  *) *)


(*OEISTotalNumberOfSequences[]:   *)

Options[OEISTotalNumberOfSequences]={URL->True};

OEISTotalNumberOfSequences[OptionsPattern[OEISTotalNumberOfSequences]]:=OEISTotalNumberOfSequences[OptionsPattern[OEISTotalNumberOfSequences]]=
	Block[{dataloaded,first,last, defaultvalue=20000,urlQ},
		dataloaded=Quiet[Import[OEISServerURL,"Plaintext"]];
		urlQ=OptionValue[URL];
		If[Head[Element[urlQ,Booleans]]===Symbol,
      If[!urlQ,Return[ defaultvalue]];
      If[dataloaded===$Failed,
      (*No internet connection to OEIS Server *)
      	Message[OEIS::conopen,OEISServerURL];
                Return[defaultvalue];,
                (*Read OEIS Data from: http://oeis.org/*)
	             first=2+Last[Flatten[StringPosition[dataloaded,"Contains"],1]];
                 last=-2+First[Flatten[StringPosition[dataloaded,"sequences."],1]];
	             Return[ToExpression[StringTake[dataloaded,{first,last}]]];
           ],
                 (* URL option is not True or False*)
                 Message[General::opttf,"URL",1];
		         Return[$Failed];
                 ];
        ];

(*Execute Function and connect to OEIS Server *)
OEISTotalNumberOfSequences[];


(* OEISValidateIDQ *)

OEISValidateIDQ[ID_]:=Block[{result,IDNumber},
	Which[
		(*ID is a String*)
		StringQ[ID],
		IDNumber=ToExpression[StringDrop[ID,1]];
	    result=((StringLength[ID]==7)&&
		(StringTake[ID,1]=="A")&&
		NumberQ[IDNumber](*&&
		(IDNumber<=OEISTotalNumberOfSequences[])*)),
		(*ID is a List*)
		ListQ[ID],
		result=And@@OEISValidateIDQ/@ID,
		(*Error *)
		True,
		Message[General::string,"ID",1];
		result=$Failed;
		];
		If[result===False ,Message[OEIS::ID,ID]];
	Return[result];
];


(* OEISImport *)

OEISImport[ID_?OEISValidateIDQ,element_:"Data"]:=Block[{result,myurl,dataloaded},
Which[
	(* Data  *)
	element=="Data",
	myurl=OEISServerURL<>ID<>"/list";
    dataloaded=ToExpression[Drop[Import[myurl,"Data"][[2]][[2]][[1]],1]];
	If[dataloaded===$Failed, result=$Failed,result=dataloaded];,
	
	(* Description  *)
	element=="Description",
	myurl=OEISServerURL<>ID<>"/internal";
	dataloaded=Quiet[Import[myurl,"Plaintext"]];
	If[dataloaded===$Failed, result=$Failed,
		result=Block[{first,last},
		first=2+Flatten[StringPosition[dataloaded,"%N"],1][[2]];
		last=-3+Select[StringPosition[dataloaded,"%"][[All,1]],#>first&][[1]];
		Return[StringReplace[StringTake[dataloaded,{first,last}],"  "~~_->""]]
      ]
		];,

	(* Author *)
	element=="Author",
	myurl=OEISServerURL<>ID<>"/internal";
	dataloaded=Quiet[Import[myurl,"Plaintext"]];
	If[dataloaded===$Failed, result=$Failed,
		result=Block[{first,last,authorfound,resultstring,anotherentryfound},
		authorfound=StringPosition[dataloaded,"%A"];
        If[authorfound!={},
      first=2+Flatten[authorfound,1][[2]];, 
      (*No Author Found*)
            Return[{}];
		];
		last=-2+Select[StringPosition[dataloaded,"Lookup"][[All,1]],#>first&][[1]];
		anotherentryfound=Select[StringPosition[dataloaded,"%"][[All,1]],#>first&];
		If[anotherentryfound!={},last=Min[last,-2+anotherentryfound[[1]]] ];
		resultstring=StringReplace[StringTake[dataloaded,{first,last}],"  "~~_->""];
		(* Remove emails *)
		resultstring=StringReplace[resultstring,"(AT)"->""];
		resultstring=StringReplace[resultstring,"("~~Shortest[___]~~")"->""];
		(* Remove ending dot*)
		resultstring=StringReplace[resultstring," ."->""];
		(*Remove Date*)
		resultstring=StringReplace[resultstring,DatePattern[{" , ","MonthName"," ","Day"," ","Year"}]->""];
		(* Remove Spaces and newlines *)
		resultstring=StringTrim[resultstring];
		Return[{resultstring}]
      ]
		];,
	
    (* Date    *)
	element=="Date",
	myurl=OEISServerURL<>ID<>"/internal";
	dataloaded=Quiet[Import[myurl,"Plaintext"]];
	If[dataloaded===$Failed, result=$Failed,
		result=Block[{first,last,authorfound,resultstring,anotherentryfound},
		authorfound=StringPosition[dataloaded,"%A"];
        If[authorfound!={},
      first=10+Flatten[authorfound,1][[2]];,
      (*No Author Entry Found*)
            Return[{""}];
		];
		last=-2+Select[StringPosition[dataloaded,"page"][[All,1]],#>first&][[1]];
		anotherentryfound=Select[StringPosition[dataloaded,"%"][[All,1]],#>first&];
		If[anotherentryfound!={},last=Min[last,-2+anotherentryfound[[1]]] ];
		resultstring=StringReplace[StringTake[dataloaded,{first,last}],"  "~~_->""];
		resultstring=StringCases[resultstring,DatePattern[{"MonthName"," ","Day"," ","Year"}]];
		If[Length[resultstring]==0,Return[{""}],resultstring=Flatten[StringSplit[resultstring],1]];
		Return[resultstring]
      ]
		];,

	(* Image  *)
	element=="Image",
	myurl=OEISServerURL<>ID<>"/graph?png=1";
	dataloaded=Quiet[Import[myurl,"Image"]];
	If[dataloaded===$Failed, result=$Failed,result=dataloaded];,

	(* bFile  *)
	element=="bFile",
	myurl=OEISServerURL<>ID<>"/b"<>StringDrop[ID,1]<>".txt";
	result=Block[{dataloaded2=Quiet[Import[myurl,"Data"]]},
	If[dataloaded2===$Failed,
		(* No bFile Found *)
		Message[OEIS::bFile,myurl];
		Return[$Failed];,
		Select[dataloaded2,#!={}&]]];,

	(* Offset or MinData*)
	(element=="Offset")||(element=="MinData"),
	myurl=OEISServerURL<>ID<>"/list";
    dataloaded=ToExpression[Drop[Import[myurl,"Data"][[2]][[2]][[1]],1]];
	If[dataloaded===$Failed, result=$Failed,result=dataloaded[[1]][[1]]];,	

	(* MaxData *)
	element=="MaxData",
	Block[{mymaxdata},
		mymaxdata=Quiet[OEISImport[ID,"bFile"]];
		If[mymaxdata===$Failed,
		mymaxdata=First[Last[OEISImport[ID,"Data"]]],
		mymaxdata=First[Last[mymaxdata]];
		];
		Return[mymaxdata]
		],	

	(* Erroneous Input *)
		True,
		Message[General::optx,element,2];
		Return[$Failed];
	];
If[result===$Failed&&(element!="bFile"),Message[OEIS::conopen,myurl]];
Return[result];
];
(* Attributes *)
SetAttributes[OEISImport,{Listable}];


(* OEISURL: *)

Options[OEISURL]={URL->True,bFile->False};

OEISURL[ID_?OEISValidateIDQ,OptionsPattern[OEISURL]]:=
 Block[{urlQ,urltype,bfileQ,url="",result="",myid=""},
	urlQ=OptionValue[URL];
	bfileQ=OptionValue[bFile];
    If[!(Head[Element[bfileQ,Booleans]]===Symbol),
      (* bFile option is not True or False*)
      Message[General::opttf,"bFile",bfileQ];
		    Return[$Failed];
	];
	If[!(Head[Element[urlQ,Booleans]]===Symbol),
      (* URL option is not True or False*)
      Message[General::opttf,"URL",urlQ];
		    Return[$Failed];
	];
	If[bfileQ,myid=("b"<>StringDrop[ID,1]<>".txt"),myid=ID];
	If[urlQ,url=OEISServerURL];
result=url<>myid;
Return[result];
];


(* OEISFunction *)

Options[OEISFunction]={AddHelp->True,bFile->True,Output->True};

OEISFunction[ID_?OEISValidateIDQ,OptionsPattern[OEISFunction]]:=Block[{descriptionloaded,dataloaded,bfileloaded,bfileQ,addhelpQ,outputQ},
	addhelpQ=OptionValue[AddHelp];
	bfileQ=OptionValue[bFile];
	outputQ=OptionValue[Output];
	(* Invalid Values for Valid Options *)
	If[!(Head[Element[addhelpQ,Booleans]]===Symbol),
      (* bFile option is not True or False*)
      Message[General::opttf,"AddHelp",addhelpQ];
		    Return[$Failed];
	];
	If[!(Head[Element[bfileQ,Booleans]]===Symbol),
      (* bFile option is not True or False*)
      Message[General::opttf,"bFile",bfileQ];
		    Return[$Failed];
	];
	If[!(Head[Element[outputQ,Booleans]]===Symbol),
      (* bFile option is not True or False*)
      Message[General::opttf,"Output",outputQ];
		    Return[$Failed];
	];
	descriptionloaded=Quiet[OEISImport[ID,"Description"]];
	If[descriptionloaded===$Failed,
		Return[$Failed];
		Message[OEIS::conopen,OEISURL[ID]],
		(*Adding Help *)
		If[addhelpQ,
		ToExpression[StringJoin[ID,"::usage=\"",ID,"[n]: ",descriptionloaded,"\""]]];
		(*Loading data from bFile if exists*)
		If[bfileQ,bfileloaded=Quiet[OEISImport[ID,"bFile"]],bfileloaded={}];
		If[bfileloaded===$Failed||!bfileQ,dataloaded=OEISImport[ID,"Data"],dataloaded=bfileloaded];
		(ToExpression[StringJoin["("<>ID<>"[#[[1]]]=#[[2]])&"]]/@dataloaded);
		If[outputQ,
      (* Prints information about function created *)
      Print[ID,"(n): ",descriptionloaded];
      Print["Link-1: ",Hyperlink[OEISURL[ID]]];
      (* Old code, wiki link removed
      Print["Link-2: ",Hyperlink[OEISURL[ID,URLType->"Wiki"]]];*)
      If[bfileQ&&!bfileloaded===$Failed,
      	Print["Link-2: ",Hyperlink[OEISURL[ID,bFile->True]]]];
      Print["Help Added: ",addhelpQ||descriptionloaded===$Failed];
      Print["Loaded data from bFile: ",bfileQ&&!bfileloaded===$Failed];
      Print["Loaded data from Sequence: ",!bfileQ||bfileloaded===$Failed];
      Print["Data available: Table of n, ",ID,"(n) for n=",First[First[dataloaded]],"..",First[Last[dataloaded]]];
		]
	]
];



(* ::Section:: *)
(*(* Exporting Data *)*)


(*  OEISExport  *)

OEISExport[ID_?OEISValidateIDQ,filename_]:=Block[{myfileextension=ToLowerCase[FileExtension[filename]]},
Which[
(*AddHelp *)
MemberQ[{"gp","m"},myfileextension],
Block[{myhelptype,myhelpstring,myID},
If[ListQ[ID],myID=ID,myID={ID}];
Which[
	(*Help for PARI/GP *)
	myfileextension==="gp",myhelpstring=StringJoin["addhelp(",#,",\"",#,": ",OEISImport[#,"Description"],"\");"]&/@myID,
          (*Help for Mathematica *)
	myfileextension==="m",myhelpstring=StringJoin[#,"::usage=\"",#,"[n]: ",OEISImport[#,"Description"],"\";"]&/@myID,
          (*Wrong HelpType option value *)
         True,
	 Message[General::optx,myhelptype,2];
         Return[$Failed];
   ];
Return[Export[filename,myhelpstring,"Text"]];
],

(*Data *)
MemberQ[{"txt","xls","csv","tsv","dat"},myfileextension],
Block[{myfilename,myID,myresult},
If[ListQ[ID],myID=ID,myID={ID}];
If[Length[myID]>1,myfilename=Function[#<>"_"<>filename],myfilename=Function[filename]];
Block[{dataloaded},
dataloaded=Quiet[OEISImport[#,"bFile"]];
	If[dataloaded===$Failed,dataloaded=OEISImport[#,"Data"]];
Export[myfilename[#],dataloaded]]&/@myID
],

(*Image *)
(MemberQ[{"jpg","jpeg","jp2", "j2k","bmp","pgn","gif","tiff", "tif"},myfileextension]),
If[ListQ[ID],
Return[Export[#<>"_"<>filename,OEISImport[#,"Image"],"Image"]&/@ID];,
Export[filename,OEISImport[ID,"Image"],"Image"];
],

(*BibTeX *)
(MemberQ[{"bib"},myfileextension]),
	Block[{mybibtex,myID},
		If[ListQ[ID],myID=ID,myID={ID}];
		mybibtex=(Block[{mydate,mymonth={},myyear={},myauthor,mydescription,mycitation={}},
	(*Reading Online Data*)
	mydate=OEISImport[#,"Date"];
	If[Length[mydate]!=1,
	mymonth=mydate[[1]];
	myyear=mydate[[3]];
	];
	myauthor=Flatten[OEISImport[#,"Author"]];
	mydescription=OEISImport[#,"Description"];
	(*Function Output*)
	mycitation="@MISC{oeis"<>#<>","<>"\n"<>
		"AUTHOR={"<>myauthor<>"},"<>"\n"<>
		"TITLE={The {O}n-{L}ine {E}ncyclopedia of {I}nteger {S}equences},"<>"\n"<>
		"HOWPUBLISHED={\\href{"<>OEISServerURL<>#<>"}{"<>#<>"}},"<>"\n"<>
		"MONTH={"<>mymonth<>"},"<>"\n"<>
		"YEAR={"<>myyear<>"},"<>"\n"<>
		"NOTE={"<>mydescription<>"}"<>"\n"<>
		"}"<>"\n";mycitation]&/@myID);
Return[Export[filename,mybibtex,"Text"]];
],

(*HTML *)
(MemberQ[{"htm","html"},myfileextension]),
	Block[{myhtmlcode,myID},
		If[ListQ[ID],myID=ID,myID={ID}];
		myID=({#,Part[myID,#]}&/@Range[Length[myID]]);
		myhtmlcode=(Block[{separator,myauthor,mydescription,mycitation={}},
	(*Reading Online Data*)
	
	myauthor=Flatten[OEISImport[#[[2]],"Author"]];
    If[Length[myauthor]==1,separator=", ",separator=""];
	mydescription=OEISImport[#[[2]],"Description"];
	(*Function Output*)
	mycitation="<a name=\"oeis"<>ToString[#[[2]]]<>"\">["<>ToString[#[[1]]]<>"]-</a> "<>
		myauthor<>separator<>
		"The On-Line Encyclopedia of Integer Sequences. "<>
		"<a href=\""<>OEISServerURL<>ToString[#[[2]]]<>"\">"<>ToString[#[[2]]]<>"</a>: "<>
		mydescription<>"<br/>";
		mycitation])&/@myID;
Return[Export[filename,myhtmlcode,"Text"]];
],

(*Wiki *)
(MemberQ[{"wiki"},myfileextension]),
	Block[{mywikicode,myID},
		If[ListQ[ID],myID=ID,myID={ID}];
		myID=({#,Part[myID,#]}&/@Range[Length[myID]]);
		mywikicode=(Block[{mydescription,mycitation={}},
	(*Reading Online Data*)
	mydescription=OEISImport[#[[2]],"Description"];
	(*Function Output*)
	mycitation="* {{oeis|"<>ToString[#[[2]]]<>"}}: "<>mydescription;
		mycitation])&/@myID;
Return[Export[filename,mywikicode,"Text"]];
],

(*Wrong element*)
True,
 Message[General::optx,filename,2];
	   Return[$Failed];
	]
];


(* ::Section:: *)
(*(* Exporting b-Files *)*)


OEISbFile[ID_?OEISValidateIDQ,VMax_Integer,filename___]:=Block[{mybfiledata,mymindata,mymaxdata,mybfilename,mybfile},
mymindata=OEISImport[ID,"MinData"];
(*mymaxdata=OEISImport[ID,"MaxData"];*)
(*If no input filename use a default filename*) 
If[filename=="Null",mybfilename=OEISURL[ID,URL->False,bFile->True],mybfilename=filename];
(* If the function is not defined load all available data from OEIS*)
If[NameQ[ID]==False,OEISFunction[ID,Output->False]];
mybfiledata=({#,(ToExpression[ID<>"["<>ToString[#]<>"]"])}&/@Range[mymindata,VMax]);
(*Delete non numerical data*)
mybfiledata=Select[mybfiledata,NumberQ[#[[2]]]&];
(* Unicode LF for newlines *)
mybfile=OpenWrite[mybfilename, BinaryFormat->True, CharacterEncoding->"Unicode"]; 
WriteString[mybfile, ToString[#[[1]]]<>" "<>ToString[#[[2]]]<>"\n"]&/@mybfiledata;
Return[Close[mybfile]]
];



(* ::Section:: *)
(*Project Euler 1-500 欧拉计划 全部答案*)
PE$Version = "2016-12-21";
PE$Answers =
     <|1->233168,2->4613732,3->6857,4->906609,5->232792560,6->25164150,7->104743,8->23514624000,9->31875000,10->142913828922,
      11->70600674,12->76576500,13->5537376230,14->837799,15->137846528820,16->1366,17->21124,18->1074,19->171,20->648,
      21->31626,22->871198282,23->4179871,24->2783915460,25->4782,26->983,27->-59231,28->669171001,29->9183,30->443839,
      31->73682,32->45228,33->100,34->40730,35->55,36->872187,37->748317,38->932718654,39->840,40->210,
      41->7652413,42->162,43->16695334890,44->5482660,45->1533776805,46->5777,47->134043,48->9110846700,49->296962999629,50->997651,
      51->121313,52->142857,53->4075,54->376,55->249,56->972,57->153,58->26241,59->107359,
      60->26033,61->28684,62->127035954683,63->49,64->1322,65->272,66->661,67->7273,68->6531031914842725,69->510510,70->8319823,
      71->428570,72->303963552391,73->7295372,74->402,75->161667,76->190569291,77->71,78->55374,79->73162890,80->40886,
      81->427337,82->260324,83->425185,84->101524,85->2772,86->1818,87->1097343,88->7587457,89->743,90->1217,
      91->14234,92->8581146,93->1258,94->518408346,95->14316,96->24702,97->8739992577,98->18769,99->709,100->756872327473,
      101->37076114526,102->228,103->20313839404245,104->329468,105->73702,106->21384,107->259679,108->180180,109->38182,110->9350130049860600,
      111->612407567715,112->1587000,113->51161058134250,114->16475640049,115->168,116->20492570929,117->100808458960497,118->44680,119->248155780267521,120->333082500,
      121->2269,122->1582,123->21035,124->21417,125->2906969179,126->18522,127->18407904,128->14516824220,129->1000023,130->149253,
      131->173,132->843296,133->453647705,134->18613426663617118,135->4989,136->2544559,137->1120149658760,138->1118049290473932,139->10057761,140->5673835352990,
      141->878454337159,142->1006193,143->30758397,144->354,145->608720,146->676333270,147->846910284,148->2129970655314432,149->52852124,150->-271248680,
      151->0.464399,152->301,153->17971254122360635,154->479742450,155->3857447,156->21295121502550,157->53490,158->409511334375,159->14489159,160->16576,
      161->20574308184277971,162->3D58725572C62302,163->343047,164->378158756814587,165->2868868,166->7130034,167->3916160068885,168->59206,169->178653872807,170->9857164023,
      171->142989277,172->227485267000992000,173->1572729,174->209566,175->1,13717420,8,176->96818198400000,177->129325,178->126461847755,179->986262,180->285196020571078987,
      181->83735848679360680,182->399788195976,183->48861552,184->1725323624056,185->4640261571849533,186->2325629,187->17427258,188->95962097,189->10834893628237824,190->371048281,
      191->1918080160,192->57060635927998347,193->684465067343069,194->61190912,195->75085391,196->322303240771079935,197->1.71064,198->52374425,199->0.00396087,200->229161792008,
      201->115039000,202->1209002624,203->34029210557338,204->2944730,205->0.573144,206->1389019170,207->44043947822,208->331951449665644800,209->15964587728784,210->1598174770174689458,
      211->1922364685,212->328968937309,213->330.721,214->1677366278943,215->806844323190414,216->5437849,217->6273134,218->0,219->64564225042,220->139776,963904,
      221->1884161251122450,222->1590933,223->61614848,224->4137330,225->2009,226->0.11316,227->3780.62,228->86226,229->11325263,230->850481152593119296,
      231->7526965179680,232->0.836486,233->271204031455541309,234->1259187438574927161,235->1.00232,236->123/59,237->15836928,238->9922545104535661,239->0.00188785,240->7448717393364181966,
      241->482316491800641154,242->997104142249036713,243->892371480,244->96356848,245->288084712410001,246->810834388,247->782252,248->23507044290,249->9275262564250418,250->1425480602091519,
      251->18946051,252->104924.,253->11.4928,254->8184523820510,255->4.4474,256->85765680,257->139012411,258->12747994,259->20101196798,260->167542057,
      261->238890850232021,262->2531.21,263->2039506520,264->"2816417.1055",265->209110240768,266->1096883702440585,267->0.999993,268->785478606870985,269->1311109198529286,270->82282080,
      271->4617456485273129588,272->8495585919506151122,273->2032447591196869022,274->1601912348822,275->15030564,276->5777137137739632912,277->1125977393124310,278->1228215747273908452,279->416577688,280->430.088,
      281->1485776387445623,282->1098988351,283->28038042525570324,284->5a411d7b,285->157056.,286->52.6495,287->313135496,288->605857431263981935,289->6567944538,290->20444710234716473,
      291->4037526,292->3600060866,293->2209,294->789184709,295->4884650818,296->1137208419,297->2252639041804718029,298->1.76882,299->549936643,300->8.05408,
      301->2178309,302->1170060,303->1111981904675169,304->283988410192,305->18174995535140,306->852938,307->0.731172,308->1539669807660924,309->210139,310->2586528661783,
      311->2466018557,312->324681947,313->2057774861813004,314->132.528,315->13625242,316->542934735751917735,317->"1856532.8455",318->709313889,319->268457129,320->278157919195482643,
      321->2470433131948040,322->999998760323313995,323->6.35518,324->96972774,325->54672965,326->1966666166408794329,327->34315549139516,328->260511850222,329->199740353/29386561536000,330->15955822,
      331->467178235146843549,332->2717.75,333->3053105,334->150320021261690835,335->5032316,336->"CAGBIHEFJDK",337->85068035,338->15614292,339->19823.5,340->291504964,
      341->56098610614277014,342->5943040885644,343->269533451410884183,344->65579304332,345->13938,346->336108797689259276,347->11109800204052,348->1004195061,349->115384615384614952,350->84664213,
      351->11762187201804552,352->378563.,353->1.27599,354->58065134,355->1726545007,356->28010159,357->1739023853137,358->3284144505,359->40632119,360->878825614395267072,
      361->178476944,362->457895958010,363->0.0000372091,364->44855254,365->162619462356610313,366->88351299,367->48271207,368->253.614,369->862400558448,370->41791929448408,
      371->40.6637,372->301450082318807027,373->727227472448913,374->334420941,375->7435327983715286168,376->973059630185670,377->732385277,378->147534623725724718,379->132314136838185,380->"6.3202e25093",
      381->139602943319822,382->697003956,383->22173624649806,384->3354706415856332783,385->3776957309612153700,386->528755790,387->696067597313468,388->831907372805129931,389->"2406376.3623",390->2919133642971,
      391->61029882288,392->3.14867,393->112398351350823112,394->3.23703,395->28.2454,396->173214653,397->141630459461893728,398->2010.59,399->{1508395636674243,"6.5e27330467"},400->438505383468410633,
      401->281632621,402->356019862,403->18224771,404->1199215615081353,405->237696125,406->36813.1,407->39782849136421,408->299742733,409->253223948,410->799999783589946560,
      411->9936352,412->38788800,413->3079418648040719,414->552506775824935461,415->55859742,416->898082747,417->446572970925740,418->1177163565297340320,419->998567458,1046245404,43363922,420->145159332,
      421->2304215802083466198,422->92060460,423->653972374,424->1059760019628,425->46479497324,426->31591886008,427->97138867,428->747215561862,429->98792821,430->"5000624921.38",
      431->23.386,432->754862080,433->326624372659664,434->863253606,435->252541322550,436->0.527666,437->74204709657207,438->2046409616809,439->968697378,440->970746056,
      441->"5000088.8395",442->1295552661530920149,443->2744233049300770,444->"1.200856722e263",445->659104042,446->907803852,447->530553372,448->106467648,449->103.379,450->583333163984220940,
      451->153651073760956,452->345558983,453->104354107,454->5435004633092,455->450186511399999,456->333333208685971546,457->2647787126797397063,458->423341841,459->3996390106631,460->18.4207,
      461->159820276,462->"5.5350769703e1512",463->808981553,464->198775297232878,465->585965659,466->258381958195474745,467->775181359,468->852950321,469->0.567668,470->147668794,
      471->"1.895093981e31",472->73811586,473->35856681704365,474->9690646731515010,475->75780067,476->110243.,477->25044905874565165,478->59510340,479->191541795,480->turnthestarson,
      485->51281274340,486->11408450515,487->106650212746,491->194505988824000,493->6.81874,496->2042473533769142717,497->684901360,498->472294837,499->0.866031,500->35407281,
      501->197912312715,503->3.86946,504->694687,506->18934502,509->151725678,510->315306518862563689,
      511->935247012,512->50660591862310323,515->2422639000800,516->939087315,517->581468882,518->100315739184392,519->804739330,520->238413705,521->44389811|>;


End[];


Protect["`*"];


EndPackage[];
