with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with Ada.Strings.Bounded;
package solutionpkp is
	package Integer_Hashed_Maps is new
	Ada.Containers.Indefinite_Hashed_Maps
		(Key_Type        => String,
		Element_Type    => Integer,
		Hash            => Ada.Strings.Hash,
		Equivalent_Keys => "=");
	use Integer_Hashed_Maps;

	type StateMachine is (InitialString, InitialNumber, Number, Color);

	procedure statemachine_proc(state: in out StateMachine; capture: in String);

	procedure Main;
private
	M : Map;
	MCount : Map;
	MMin : Map;
end solutionpkp;

