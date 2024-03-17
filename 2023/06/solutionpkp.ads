with Ada.Strings.Unbounded; use  Ada.Strings.Unbounded;
package solutionpkp is
	type StateMachine is (time, time_value, distance, distance_value);
	procedure statemachine_proc(capture: in Unbounded_String);
	procedure line_proc (capture: in Unbounded_String);
	procedure Main;
end solutionpkp;

