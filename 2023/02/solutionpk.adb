with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Characters.Handling; use  Ada.Characters.Handling;
with Ada.Strings.Unbounded; use  Ada.Strings.Unbounded;

with solutionpk; use solutionpk;

package body solutionpk is
	current_game_id : Integer := 0;
	current_number : Integer := 0;
	valid_game : Boolean := True;

	procedure statemachine_proc(
		state: in out StateMachine;
		capture: in String
		)
	is
		package B_Str is new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 15);
		use B_Str;
		trimmed_string : Bounded_String;
	begin
		trimmed_string := Trim(To_Bounded_String(capture), To_Set(' '), To_Set(":;, "));
		--Put_Line("State: " & To_String(trimmed_string));
		case state is
			when InitialString =>
				--Put_Line("Game, skip");
				state := InitialNumber;
			when InitialNumber =>
				current_game_id := Integer'Value(To_String(trimmed_string));
				state := Number;
				--Put_Line("Game id: " & To_String(trimmed_string));
			when Number =>
				current_number := Integer'Value(To_String(trimmed_string));
				state := Color;
			when Color =>
				--Put_Line("Color: " & To_String(trimmed_string) & " : " & current_number'Image);
				current_number := MCount(To_String(trimmed_string)) + current_number;
				MCount(To_String(trimmed_string)) :=  current_number;
				Put_Line("Color: " & To_String(trimmed_string) & " : " & current_number'Image);
				if current_number > M(To_String(trimmed_string)) then
					Put_Line("Invalid!");
					valid_game := False;
				end if;
				state := Number;
		end case;
		if Index(Source => To_Bounded_String(capture), Pattern => ";", From => 1) > 0 then
			MCount("red") := 0;
			MCount("blue") := 0;
			MCount("green") := 0;
		end if;
	end statemachine_proc;

	procedure Main is
		F         : File_Type;
		File_Name : constant String := "input.txt";
		str       : Unbounded_String;
		idx,idxl  : Natural := 1;
		total     : Natural := 0;

		current_st: StateMachine := InitialString;
	begin
		M.Include("red", 12);
		M.Include("blue", 14);
		M.Include("green", 13);
		MCount.Include("red", 0);
		MCount.Include("blue", 0);
		MCount.Include("green", 0);
		Open (F, In_File, File_Name);
		while not End_Of_File (F) loop
			-- Capture the line
			str := To_Unbounded_String(Get_Line (F));
			Put_Line("--------------------------------------------------------------------------------");
			Put_Line(To_String(str));

			-- Initialize variables
			current_st := InitialString;
			valid_game := True;
			idx := 1;
			idxl := 1;
			MCount("red") := 0;
			MCount("blue") := 0;
			MCount("green") := 0;

			-- Capture first data
			idx := Index(
				Source => str,
				Pattern => " ",
				From => idx
				);
			while idx /= 0 loop
				-- Print current match
				--Put_Line("Indexs: " & idxl'image & ", " & idx'image);

				statemachine_proc(current_st, Slice(str,idxl,idx));

				-- Next loop preparation
				idxl := idx + 1;
				idx := Index(
					Source => str,
					Pattern => " ",
					From => idx+1
					);
			end loop;
			statemachine_proc(current_st, Slice(str,idxl,Length(str)));
				
			if valid_game then
				total := total + current_game_id;
				Put_Line("Current score: " & total'Image);
			end if;

			valid_game := True;
		end loop;
		Put_Line("Score: " & total'Image);
		Close (F);
	end Main;
end solutionpk;
