(* ::Package:: *)

BeginPackage["PararealMethod`"]
PararealNDSolve::usage=""
Begin["Private`"]
PararealNDSolve[equation_,initial_,var_,{invar_,range_,frac_,subfrac_}]:=

Module[
{},
sol=
Prepend[{initial[[1,1,1]],initial[[1,2]]}][
Reap[
NDSolve[{equation,initial},var,{invar,initial[[1,1,1]],initial[[1,1,1]]+range},
Method->{"ExplicitEuler",Method->"FixedStep"},
StartingStepSize->range/frac,
StepMonitor:> Sow[{invar,var[invar]}]
]][[2,1]]];
sol1=
ParallelTable[
Prepend[{initial[[1,1,1]]+i range/frac-range/frac,sol[[i,2]]}][
Reap[
NDSolve[{equation,var[initial[[1,1,1]]+i range/frac-range/frac]== sol[[i,2]]},var,{invar,initial[[1,1,1]]+i range/frac-range/frac,initial[[1,1,1]]+i range/frac},
StartingStepSize->range/frac/subfrac,
Method->{"ExplicitEuler"},
StepMonitor:>  Sow[{invar,var[invar]}]
]][[2,1]]],
{i,1,frac}];
convergedifference=
Table[
Total[
Prepend[0][Table[sol1[[i+1]][[1,2]]-sol1[[i]][[-1,2]],{i,1,frac-1}]][[1;;j]]
],{j,1,frac}];
convergesol=
Table[
sol1[[i]]-Table[{0,convergedifference[[i]]},{j,1,Length[sol1[[i]]]}],
{i,1,frac}]

]

End[]
EndPackage[]
