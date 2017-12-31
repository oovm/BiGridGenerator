
BeginPackage["TeXTableForm`","Global`"];

(* TeXTableForm.m -- *)

(* Mathematica Version 2.0.1 *)

(* Version 1.0 from November 4th, 1992 *)

(* author: Matthias Schunter, Teichstr. 1, D-29 Oldenburg *)

(* Bugs and suggestions: schunter@informatik.uni-hildesheim.de *)

(* The Package is public-domain. *)

(* The distribution of changed versions is not allowed without permission *)

TeXTableForm::usage="TeXTableForm[list,n] generates a body of a LaTeX-table
with n columns. list may be nested up to level 2.
TeXTableForm[list,n,\"filename\"] writes the output to the according file";

TableBegin::usage="TableBegin->\"string\" sets the string in the
table definition. columns may be used to insert the number of big columns;
width may be used to insert the width of each column. Default is 
TableBegin -> \"\\begin{tabular}{|*{columns}{*{width}{|l}|}|}\\n\" "

TableEnd::usage="TableEnd -> \"string\" sets the end of tabular string.
Default is TableEnd -> \"\\end{tabular}\\n\""

`contents={TeXTableForm}

(Attributes[#]={})& /@ `contents

Options[TeXTableForm]={
  TableBegin->"\\begin{tabular}{|*{columns}{*{width}{|l}|}|}\n",
  TableEnd-> "\\end{tabular}\n"};

(* Abbreviation: *)
TTF = TeXTableForm

Begin["`Private`"]

TeXTableForm[inlist_List,columns_Integer:1,file_String:"stdout",opts___]:=
  Module[{width,length,outstream="",stringpad,elems,i,j},
    elems=inlist;
    width=Max[Length /@ elems,1];
    length=Ceiling[Length[elems]/columns];
    stringpad=Table[" ",{width}];
    elems=
      Take[
	Join[
	  ("$"<>#<>"$")& /@ ToString /@ TeXForm /@ Flatten[{#}]
	,
	  stringpad
	]
      ,
	width]& /@ elems;
    elems=Join[elems,Table[stringpad,{length}]];
    outstream=
      If[file=="stdout"
      , "stdout"
      , OpenWrite[file]
      ];
    WriteString[outstream, 
      StringReplace[TableBegin /. {opts} /. Options[TeXTableForm],
	{"columns"->ToString[columns],"width"->ToString[width]}
      ]
    ];
    For[i=1,i<=length,i++,
      For[j=0,j<columns,j++,
        WriteString[outstream,#," & "]& /@ Drop[elems[[i+length j]],{-1}];
	If[j==columns-1,
	  WriteString[outstream,elems[[i+length j]][[-1]]],
	  WriteString[outstream,elems[[i+length j]][[-1]]," & "]
        ]
      ];
      WriteString[outstream," \\\\\n"];
    ];
    WriteString[outstream, 
      StringReplace[TableEnd /. {opts} /. Options[TeXTableForm],
	{"columns"->ToString[columns],"width"->ToString[width]}
      ]
    ];
    If[file!="stdout",Close[outstream],]
  ];

End[] (* Private *);

(Attributes[#]={Protected,ReadProtected})& /@ `contents;
EndPackage[];
Null



