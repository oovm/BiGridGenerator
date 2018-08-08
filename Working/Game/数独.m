problemStr="800 000 000 003 600 000 070 090 200 050 007 000 000 045 700 000 100 030 001 000 068 008 500 010 090 000 400";
problem=Partition[ToExpression@Characters@StringDelete[problemStr," "],9]
blocks=Table[3 Quotient[i,3]+Quotient[j,3],{i,0,8},{j,0,8}];
rows=Transpose[cols=ConstantArray[Range@9,9]-1];
constrain[blk_]:=Join@@Table[Outer[Plus,Range[9],(Position[blk,k]-1).{81,9}],{k,Min[blk],Max[blk]}];
numberConstraints=Partition[Range@729,9];
basicConstraints=Flatten[constrain/@{blocks,rows,cols},1];
problemConstraints=List/@Flatten@Table[(Position[problem,k]-1).{81,9}+k,{k,1,9}];
allConstraints=Total[UnitVector[729,#]&/@#]&/@Join[basicConstraints,numberConstraints,problemConstraints];
lpResult=Quiet@LinearProgramming[ConstantArray[0,729],allConstraints,ConstantArray[{1,0},Length@allConstraints],ConstantArray[{0,1},729],Integers];
Partition[FirstPosition[#,1][[1]]&/@Partition[lpResult,9],9]