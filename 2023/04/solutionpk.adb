with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use  Ada.Strings.Unbounded;
with Ada.Strings.Maps; use Ada.Strings.Maps;

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Ordered_Sets;

with solutionpk; use solutionpk;

package body solutionpk is
	package IntegerSet is 
		new Ada.Containers.Ordered_Sets(
			Element_Type => Integer
			);
	use IntegerSet;
	package IntegerIntegerMap is 
		new Ada.Containers.Indefinite_Ordered_Maps(
			Key_Type => Integer,
			Element_Type => Integer
			);
	use IntegerIntegerMap;

	
	current_id : Integer := 0;
	line_total : Integer := 0;
	--winners_map : IntegerIntegerMap;
	winners_storage : Set;


	procedure statemachine_proc(
		state: in out StateMachine;
		capture: in Unbounded_String
		)
	is
		current_number : Integer := 0;
		trimmed_string : Unbounded_String;
	begin
		trimmed_string := Trim(capture, To_Set(' '), To_Set(":;, "));
		Put_Line("Token: " & To_String(trimmed_string));
		if trimmed_string = "" then
			return;
		end if;
		case state is
			when InitialString =>
				Put_Line("Card, skip");
				state := InitialNumber;
			when InitialNumber =>
				current_id := Integer'Value(To_String(trimmed_string));
				state := Winners;
				Put_Line("Card id: " & current_id'Image);
			when Winners =>
				if trimmed_string = "|" then
					state := Given;
					Put_Line("Change to Given: ");
				else
					current_number := Integer'Value(To_String(trimmed_string));
					Put_Line("Winners: " & current_number'Image);
					winners_storage.Insert(current_number);
				end if;
			when Given =>
				current_number := Integer'Value(To_String(trimmed_string));
				Put_Line("Given: " & current_number'Image);
				if winners_storage.Contains(current_number) then
					if line_total = 0 then
						line_total := 1;
					else
						line_total := line_total * 2;
					end if;
				end if;
		end case;
	end statemachine_proc;


	procedure line_proc
		(capture: in Unbounded_String)
	is
		state : StateMachine;
		idx,idxl  : Natural := 1;
	begin
		Clear(winners_storage);
		current_id := 0;
		state := InitialString;
		-- Capture first data
		idx := Index(
			Source => capture,
			Pattern => " ",
			From => idx
			);
		while idx /= 0 loop
			-- Process current match
			--Put_Line("Indexs: " & idxl'image & ", " & idx'image);

			statemachine_proc(state, To_Unbounded_String(Slice(capture,idxl,idx)));

			-- Next loop preparation
			idxl := idx + 1;
			idx := Index(
				Source => capture,
				Pattern => " ",
				From => idx+1
				);
		end loop;
		statemachine_proc(state, To_Unbounded_String(Slice(capture,idxl,Length(capture))));
	end line_proc;


	procedure Main is
		F         : File_Type;
		File_Name : constant String := "input.txt";
		str       : Unbounded_String;
		total : Integer := 0;
	begin
		Open (F, In_File, File_Name);
		while not End_Of_File (F) loop
			-- Capture the line
			line_total := 0;
			str := To_Unbounded_String(Get_Line (F));
			line_proc(str);
			total := total + line_total;
			Put_Line(To_String(str));
		end loop;
		Put_Line("Result: " & total'Image);
		Close (F);
	end Main;
end solutionpk;
