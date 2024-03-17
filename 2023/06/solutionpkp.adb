with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use  Ada.Strings.Unbounded;
with Ada.Characters.Handling; use  Ada.Characters.Handling;
with Ada.Strings.Maps; use Ada.Strings.Maps;

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;

with solutionpkp; use solutionpkp;

package body solutionpkp is
	time_vector : Long_Integer := 0;
	distance_vector: Long_Integer := 0;
	state: StateMachine;

	procedure statemachine_proc(
		capture: in Unbounded_String
		)
	is
		current_number : Long_Integer := 0;
		trimmed_string : Unbounded_String;
	begin
		trimmed_string := Trim(capture, To_Set(' '), To_Set(":;, "));
		--Put_Line("Token: " & To_String(trimmed_string) & " State: " & state'Image);
		if trimmed_string = "" then
			return;
		end if;
		case state is
			when time =>
				state := time_value;
			when time_value =>
				current_number := Long_Integer'Value(To_String(trimmed_string));
				time_vector := time_vector * (10 ** length(trimmed_string)) + current_number;
			when distance =>
				state := distance_value;
			when distance_value =>
				current_number := Long_Integer'Value(To_String(trimmed_string));
				distance_vector := distance_vector * (10 ** length(trimmed_string)) + current_number;
		end case;
	end statemachine_proc;


	procedure line_proc
		(capture: in Unbounded_String)
	is
		idx,idxl  : Natural := 1;
	begin
		-- Capture destination data
		idx := Index(
			Source => capture,
			Pattern => " ",
			From => idx
			);
		while idx /= 0 loop
			-- Process current match
			--Put_Line("Indexs: " & idxl'image & ", " & idx'image);
			-- Next loop preparation
			statemachine_proc(To_Unbounded_String(Slice(capture,idxl,idx)));

			idxl := idx + 1;
			idx := Index(
				Source => capture,
				Pattern => " ",
				From => idx+1
				);
		end loop;
		statemachine_proc(To_Unbounded_String(Slice(capture,idxl,Length(capture))));
	end line_proc;


	procedure Main is
		F         : File_Type;
		File_Name : constant String := "input.txt";
		str       : Unbounded_String;
		total     : Long_Integer;
		my_distance : Long_Integer;
		sub_total : Long_Integer;
	begin
		Open (F, In_File, File_Name);
		state := time;
		while not End_Of_File (F) loop
			-- Capture the line
			str := To_Unbounded_String(Get_Line (F));
			Put_Line(To_String(str));
			line_proc(str);
			state := distance;
		end loop;

		total := 0;
		Put_Line("time: " & Long_Integer'image(time_vector) & " distance: " & Long_Integer'image(distance_vector));
		sub_total := 0;
		for J in 0 .. time_vector loop
			my_distance := J * (time_vector - J);
			if my_distance > distance_vector then
				sub_total := sub_total + 1;
			end if;
		end loop;
		if total = 0 then
			total := sub_total;
		elsif sub_total > 0 then
			total := total * sub_total;
		end if;

		Put_Line("Total: " & total'image);
		Close (F);
	end Main;
end solutionpkp;
