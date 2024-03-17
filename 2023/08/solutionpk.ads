with Ada.Strings.Unbounded; use  Ada.Strings.Unbounded;
package solutionpk is
	type StateMachine is (command, node_name, node_destination_left, node_destination_right);
	procedure statemachine_proc(capture: in Unbounded_String);
	procedure line_proc (capture: in Unbounded_String);
	procedure Main;
end solutionpk;

