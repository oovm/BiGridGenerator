(* Mathematica Package *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)

(* :Title: QuinticEquation *)
(* :Context: QuinticEquation` *)
(* :Author: 28059 *)
(* :Date: 2016-11-14 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: *)
(* :Copyright: (c) 2016 28059 *)
(* :Keywords: *)
(* :Discussion: *)

BeginPackage["QuinticEquation`"];
GaloisGroup::usage="GaloisGroup[eqn]尝试对方程进行可解性判定.";
QuinticSolver::usage="QuinticSolver[eqn]尝试求解一个高次方程的代数解.";



Begin["`Private`"];
GaloisGroup::red="这个方程可以因式分解!";
GaloisGroup[expr_]:=Module[{var},var=Union[Select[Level[expr,{-1}],Head[#1]===Symbol&]];
If[Length[var]==1&&Exponent[expr,var[[1]]]==5,(galois[expr,var[[1]]])[[1]],Return[$Failed]]];
galois[expr_,var_]:=If[Exponent[expr,var]==5&&And@@(#1===Integer||#1===Rational&)/@Head/@CoefficientList[expr,var],
  Module[{factor},If[Head[factor=Factor[expr]]===Times&&Length[Complement[(Exponent[#1,var]&)/@List@@factor,{0,5}]]>0,
    Message[GaloisGroup::red];
    Select[(Irreducible[#1,var,Exponent[#1,var]]&)/@List@@factor,#1=!=NULL&],
    If[Head[factor=Factor[expr]]===Power,Message[GaloisGroup::red];
    Table[SymmetricGroup[1],{i,Exponent[expr,var]}],{Irreducible[expr,var,Exponent[expr,var]]}]]],$Failed]
Irreducible[expr_,var_,n_]:=Module[{isfactorable,introot,roots},{isfactorable,introot,roots}=
    Resolvent[expr/Coefficient[expr,var,n],var];If[isfactorable,SolvableGroup[Sqrt[Discriminant[expr,var]],expr,var,introot,roots],
  If[IntegerQ[Sqrt[Discriminant[expr,var]]],AlternatingGroup[n],SymmetricGroup[n]],$Failed]]/;n==5;
Irreducible[__]:=NULL;
Resolvent[expr_,var_]:=Module[{roots,integralroot,temp},roots=List@@NRoots[expr==0,var,20]/._==(p_):>p;
temp=Stabilizer/@Representatives[roots];integralroot=Position[Chop/@(Round[#1]-#1&)/@temp,_Integer];
If[Length[integralroot]>=1,{True,Round[temp[[integralroot[[1,1]]]]],roots},{False,$Failed,$Failed}]];
Representatives[{x1_,x2_,x3_,x4_,x5_}]:={{x1,x2,x3,x4,x5},{x2,x1,x3,x4,x5},{x3,x2,x1,x4,x5},{x4,x2,x3,x1,x5},{x5,x2,x3,x4,x1},{x1,x5,x3,x4,x2}};
Stabilizer[{x1_,x2_,x3_,x4_,x5_}]:=x1^2*(x2*x5+x3*x4)+x2^2*(x1*x3+x4*x5)+x3^2*(x1*x5+x2*x4)+x4^2*(x1*x2+x3*x5)+x5^2*(x1*x4+x2*x3);
SolvableGroup[disc_,r__]:=If[!IntegerQ[disc],MetacyclicGroup[20],DihedralOrCyclic[r]];
DihedralOrCyclic[expr_,var_,introot_,roots_List]:=Module[{disc,p},
  Fnew[x1_,x2_,x3_,x4_,x5_]:=x1^2*x5+x1*x2^2+x2*x3^2+x3*x4^2+x4*x5^2;
  NewResolvent[{x1_,x2_,x3_,x4_,x5_}]:=(var-Fnew[x1,x2,x3,x4,x5])*(var-Fnew[x2,x1,x5,x4,x3]);
  p=Position[Round/@Stabilizer/@Representatives[roots],introot][[1,1]];
  disc=Discriminant[NewResolvent[Representatives[roots][[p]]],var];
  disc=disc/.{e_Real:>Round[e],e_Complex:>Round[e]};
  If[disc==0,Return[SingularCase[expr,var,roots,5]]];
  If[IntegerQ[Sqrt[disc]],CyclicGroup[5],DihedralGroup[10]]];
SingularCase[expr_,var_,roots_,n_]:=Module[{newp,newroots,stabs,realr,p,disc},
  newp=Tschirnhaus[expr,var,n];
  newroots=newp/.({var->#1}&)/@roots;
  stabs=Stabilizer/@N[Representatives[newroots],32];
  realr=Select[Round[stabs],IntegerQ];
  If[Length[realr]==0,Return[$Failed]];
  p=Position[Round[stabs],First[realr]][[1,1]];
  disc=Discriminant[NewResolvent[Representatives[newroots][[p]]],var];
  If[IntegerQ[Sqrt[disc]],CyclicGroup[5],DihedralGroup[10]]];
Tschirnhaus[expr_,var_,n_]:=Module[{z,randp,newp},
  randp=Sum[z^(-k+n-2)*Random[Integer,{-1,0}],{k,0,n-2}];
  newp=Resultant[expr/.var->z,var-randp,z];
  If[FreeQ[PolynomialGCD[newp,D[newp,{var}]],var],newp,
    Tschirnhaus[expr,var,n]]];
FindN=Switch[Exponent[#,Variables[#]],
  {1},Print["这是一个小学生都会解的一次方程"];Solve[#==0,Variables[#]],
  {2},Print["这是一个中学生都应该会解的二次方程"];Solve[#==0,Variables[#]],
  {3},Print["这是一个大多数大学生都不会解的三次方程"];QSolver3[#],
  {4},Print["这是一个吃饱了撑着没事干才会去手解的四次方程"];QSolver4[#],
  {5},Print["这是一个不知道解不解的出的五次方程"];QSolver5[#],
  _,"这是一个高次方程,我没研究过就不解了"]&;
QuinticSolver[eqn_]:=Module[{},
  If[PolynomialQ[eqn,x],Nothing,Return["你在逗我,这玩意儿根本不是多项式!"]];
  If[Length@Variables[eqn]===1,Nothing,Return["你别逗我,这根本不止一个变量!\n而且我这也不解含参方程."]];
  Print["好的,我们先试着因式分解:"];
  list=(Power@@@FactorList[eqn])[[2;;-1]];
  If[IrreduciblePolynomialQ[eqn],Print["好吧,看来没法因式分解\n我们来看看这个方程:"<>ToString[eqn,TraditionalForm]];
  Print@ToString[FindN[eqn],TraditionalForm],Print["OK,式子可以因式分解成"<>ToString[Factor@eqn,TraditionalForm]];
  Table[Print["我们来看看第"<>ToString[i]<>"部分"<>ToString[list[[i]],TraditionalForm]];
  Print@ToString[FindN[list[[i]],TraditionalForm]],{i,1,Length@list}];];
Print["好了,完事"]];
End[];

EndPackage[];