(* Mathematica Package *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)

(* :Title: ExWords *)
(* :Context: ExWords` *)
(* :Author: 28059 *)
(* :Date: 2016-11-16 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 11.0+ *)
(* :Copyright:该软件包遵从CC协议:BY+NA+NC(署名、非商业性使用、相同方式共享） *)
(* :Keywords: *)
(* :Discussion: *)

BeginPackage["ExWords`"];


Begin["`Private`"];
Options[WordSquare]={Language->"English",Dimensions->2};
WordSquare[len_,pattern_:All,OptionsPattern[]]:=Module[{alln,goal},
  alln=Characters/@Select[DictionaryLookup[{OptionValue[Language],All}],StringLength[#1]==len&];
  nmatch[patt_]:=nmatch[Verbatim[patt]]=Length[Cases[alln,patt]];
  findCompletions[m_]:=Module[{nn,ur},
    {ur,nn}=NestWhile[{fixone[#1[[1]],First[#1[[2]]]],Rest[#1[[2]]]}&,
      {m,Ordering[nmatch/@m]},Length[#1[[2]]]>0&&nmatch[#1[[1,#1[[2,1]]]]]==1&];If[Length[nn]==0,{ur},With[{n=First[nn]},
      (ReplacePart[ur,n->#1]&)/@Cases[alln,ur[[n]]]]]];
  findCompletionsOriented[m_]:=Module[{osc},
    osc=findCompletions/@Union[{m,Transpose[m]}];
    osc[[First[Ordering[Length/@osc,1]]]]];
  fixone[ml_,nl_]:=If[FreeQ[ml[[nl]],Verbatim[_]],ml,ReplacePart[ml,nl->First[Cases[alln,ml[[nl]]]]]];
  goal=If[pattern===All,{Nest[ConstantArray[#1,len]&,_,OptionValue[Dimensions]]},pattern];
  FixedPoint[Union[Join@@findCompletionsOriented/@#1]&,goal]];
Options[WordCube]={Random->True,Language->"English",Dimensions->3};
WordCube[len_Integer,op:OptionsPattern[]]:=WordCube[ConstantArray[_,ConstantArray[len,OptionValue[Dimensions]]],op];
WordCube[mat_,OptionsPattern[]]:=Block[{$random=If[OptionValue[Random],RandomSample,Identity],
  $RecursionLimit=Infinity,$IterationLimit=Infinity,dictCache},Catch[inspect[mat]=!=Null,tag]];
dict[numLetters_]:=Replace[dictCache[numLetters],_dictCache:>
    (dictCache[numLetters]=DictionaryLookup[{OptionValue[Language],Repeated[_,{numLetters}]}])];
inspect[mat_]:=inspect[mat,nextIndices[mat]];
nextIndices[mat_]/;FreeQ[mat,Verbatim[_]]:=True;
nextIndices[mat_]:=nextIndices[mat,indices[mat]];
i:indices[wordLen_,dims_]:=i=(Permutations[Append[#1,All]]&)/@DeleteDuplicates[Sort/@Tuples[Range[wordLen],{dims-1}]];
indices[mat_]:=indices[Length[mat],Length[Dimensions[mat]]];
nextIndices[mat_,indices_]:=nextIndices[mat,indices,1+LengthWhile[
  indices[[All,1]],FreeQ[mat[[Sequence@@#1]],Verbatim[_]]&]];
nextIndices[mat_,indices_,nextIndex_]:=Extract[indices,nextIndex];
inspect[mat_,True]:=Throw[mat,tag];
inspect[mat_,indices_]:=Scan[Function[word,inspect[change[mat,word,indices]]],
  (Characters[$random[Pick[#1,StringMatchQ[#1,StringExpression@@mat[[Sequence@@First[indices]]]]]]]&)[dict[Length[mat]]]];
change[mat_,word_,indices_]:=Module[{newMat=mat},Scan[Function[wordPos,newMat[[Sequence@@wordPos]]=word],indices];newMat];
makeTree[wrds:{__String}]:=makeTree[Characters[wrds]];
makeTree[wrds_/;MemberQ[wrds,{}]]:=Prepend[makeTree[DeleteCases[wrds,{}]],{}->{}];
makeTree[wrds_]:=Reap[(If[#1=!={},Sow[Rest[#1],First[#1]]]&)/@wrds,_,#1->makeTree[#2]&][[2]];
getLetterAndAdjacencyRules[(letterMatrix_)?(MatrixQ[#1,StringQ]&)]:=
    Module[{a,lrules,p,adjRules},lrules=(Thread[Range[Length[#1]]->#1]&)[
      Flatten[letterMatrix]];p=ArrayPad[Partition[Array[a,Length[lrules]],
      Last[Dimensions[letterMatrix]]],1];
    adjRules=Flatten[ListConvolve[{{1,1,1},{1,2,1},{1,1,1}},p]/.Plus->List/.
        {left___,2*(v_),right___}:>{v->{left,right}}/.a[x_]:>x];
    Dispatch/@{lrules,adjRules}];
getVertexSequences[adjrules_,letterRules_,allTree_,n_]:=
    Block[{subF,f,getWordsForStartingVertex},
      subF[v_,tree_]:=With[{letter=v/.letterRules},With[{res=letter/.tree},
        res/;res=!=letter]];subF[_,_]:={};
      f[vvlist_,{{}->{},rest___}]:=f[Sow[vvlist],{rest}];f[_,{}]:=Null;
      f[vvlist:{last_,prev_List},subTree_]:=
          Scan[f[{#1,vvlist},subF[#1,subTree]]&,Complement[last/.adjrules,Flatten[vvlist]]];
      getWordsForStartingVertex[v_]:=(If[#1==={},#1,Reverse[Flatten/@First[#1],2]]&)[
        Reap[f[{v,{}},subF[v,allTree]]][[2]]];Flatten[getWordsForStartingVertex/@Range[n],1]];
wordsFromVertexSequences[vseqs_List,letterRules_]:=StringJoin/@(vseqs/.letterRules);
GetWordTree[minLen_Integer:1,maxLen:_Integer|Infinity:Infinity]:=
    makeTree[Select[ToLowerCase[DictionaryLookup["*"]],minLen<=StringLength[#1]<=maxLen&]];
BoggleSolver[board_String,wordTree_]:=BoggleSolver[ToLowerCase[ImportString[board]],wordTree];
BoggleSolver[lboard_,wordTree_]:=Module[{lrules,adjrules},
  {lrules,adjrules}=getLetterAndAdjacencyRules[lboard];
  wordsFromVertexSequences[getVertexSequences[adjrules,lrules,wordTree,Times@@Dimensions[lboard]],lrules]];


End[];

EndPackage[];