with Ada.Strings.Unbounded; use  Ada.Strings.Unbounded;
package solutionpk is
	type StateMachine is (name, number);
	procedure statemachine_proc(capture: in Unbounded_String);
	procedure line_proc (capture: in Unbounded_String);
	procedure Main;
end solutionpk;

