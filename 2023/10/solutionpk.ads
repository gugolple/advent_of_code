with Ada.Strings.Unbounded; use  Ada.Strings.Unbounded;
package solutionpk is
	type StateMachine is (name);
	type Pipe is (Vertical, Horizontal, BNE, BNW, BSW, BSE, Ground, Start);
	type Direction is (North, South, East, West, Invalid);
	procedure statemachine_proc(capture: in Unbounded_String);
	procedure line_proc (capture: in Unbounded_String);
	procedure Main;
end solutionpk;

