(* ::Package:: *)

(* Nathan Burgers 2014 *)
Needs["Notation`"];
InfixNotation[ParsedBoxWrapper["\[Element]"],DeclareType];
SetAttributes[DeclareType, HoldAll];
DeclareType[symbol_, type_] := Element[symbol,type];

BuildContext[c_,a_String] := Prepend[c,{a,string}];
BuildContext[c_,a_Integer] := Prepend[c,{a,integer}];
BuildContext[c_,a_Real] := Prepend[c,{a,real}];
BuildContext[c_,a_Rational] := Prepend[c,{a,rational}];
BuildContext[c_,a_Complex] := Prepend[c,{a,complex}];
BuildContext[c_,_Element[symbol_,type_]] := Prepend[c,{symbol,type}];

SetAttributes[Type, HoldAll];
Type[a_String] := string;
Type[a_Integer] := integer;
Type[a_Real] := real;
Type[a_Rational] := rational;
Type[a_Complex] := complex;
Type[a_Symbol] := typeVariable[a];
Type[a_Function] := typeFunction[a,b];
(*
Type[a_Symbol] := Type[Evaluate[a]] /; DownValues[a] == {};
Type[a_Symbol] := If[ValueQ[a], none, Function[#]];*)

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


"












