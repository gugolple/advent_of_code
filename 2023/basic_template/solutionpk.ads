package solutionpk is
	type StateMachine is (name, name_value);
	procedure statemachine_proc(capture: in Unbounded_String);
	procedure line_proc (capture: in Unbounded_String);
	procedure Main;
end solutionpk;

