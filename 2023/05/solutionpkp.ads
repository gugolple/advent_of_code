with Ada.Strings.Unbounded; use  Ada.Strings.Unbounded;
package solutionpkp is
	type seed_range_record is record
		start : Long_Integer;
		range_end : Long_Integer;
	end record;
	Function "<" (Left, Right: In seed_range_record) return boolean;
	Function "=" (Left, Right: In seed_range_record) return boolean;
	type StateMachine is (seeds, seed_start, seed_range, map_destination, map_source, map_range_length);
	procedure statemachine_proc(capture: in Unbounded_String);
	procedure line_proc (capture: in Unbounded_String);
	procedure Main;
end solutionpkp;

