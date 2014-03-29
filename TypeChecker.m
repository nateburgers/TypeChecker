(* ::Package:: *)

(* Nathan Burgers 2014 *)
Needs["Notation`"];
InfixNotation[ParsedBoxWrapper["\[Element]"],DeclareType];
SetAttributes[DeclareType, HoldAll];
DeclareType[symbol_, type_] := Element[symbol,type];

UniqueType[] := typeVariable[RandomInteger[2^32]];

SetAttributes[Type, HoldAll];
Type[c_,expr_] := IfJust[Lookup[c,expr],Function[Element[expr,#]],Type[expr]];
Type[c_,_Element[symbol_,type_]] := Element[symbol,type];
Type[c_,a_String] := Element[a,string];
Type[c_,a_Integer] := Element[a,integer];
Type[c_,a_Real] := Element[a,real];
Type[c_,a_Rational] := Element[a,rational];
Type[c_,a_Complex] := Element[a,complex];
Type[c_,a_Symbol] := Element[a,UniqueType[]];
Type[c_,a_Function[x_,body_]] := Element[a,typeFunction[UniqueType[], Type[c.body]]];
Type[c_,f_[x_]] := Element[{f,x},typeApplication[Type[c,f], Type[c,x]]];

Type[Function[x,x][foo]]

IfJust[just[x_],f_,_] := f[x];
IfJust[none,_,g_] := g;
Lookup[{},_] := none;
Lookup[{{key_,value_},tail___},term_] := If[term === key, just[value], Lookup[{tail},term]];
Disassoc[{},_] := {};
Disassoc[{{key_,value_},tail___},term_] := If[term === key, Disassoc[{tail},term], Prepend[Disassoc[{tail},term],{key,value}]];

(* Operations on types *)
FreeTypeVariables[string] := {};
FreeTypeVariables[integer] := {};
FreeTypeVariables[real] := {};
FreeTypeVariables[rational] := {};
FreeTypeVariables[complex] := {};
FreeTypeVariables[typeVariable[x_]] := {x};
FreeTypeVariables[typeFunction[x_,y_]] := Join[FreeTypeVariables[x],FreeTypeVariables[y]];

ApplyType[types_, typeVariable[n_]] := IfJust[Lookup[types,n], Function[#],typeVariable[n]]; 
ApplyType[types_, typeFunction[x_,y_]] := typeFunction[ApplyType[types,x],ApplyType[types,y]];
ApplyType[types_, t_] := t;

(* Operations on Type Schemes *)
FreeTypeVariables[scheme[vars_,t_]] := Complement[FreeTypeVariables[t],vars]
(*ApplyType[types_, scheme[vars_,t_]] := scheme[vars, *)

NullSubstitution := {};
composeSubstitution[f_,g_] := Map[Function[],g]
