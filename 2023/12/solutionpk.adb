with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use  Ada.Strings.Unbounded;
with Ada.Strings.Maps; use Ada.Strings.Maps;

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;

with solutionpk; use solutionpk;

package body solutionpk is
	package IntegerVector is new
		Ada.Containers.Vectors
			(Index_Type => Natural,
			Element_Type => Integer 
			);
	use IntegerVector;

	state: StateMachine;
	
	process_line : Unbounded_String;
	values : IntegerVector.Vector;
	procedure statemachine_proc(
		capture: in Unbounded_String
		)
	is
		current_number : Integer := 0;
		trimmed_string : Unbounded_String;
	begin
		trimmed_string := Trim(capture, To_Set(' '), To_Set(":;, "));
		--Put_Line("Token: " & To_String(trimmed_string) & " State: " & state'Image);
		if trimmed_string = "" then
			return;
		end if;
		case state is
			when name =>
				process_line := trimmed_string;
				state := number;
			when number =>
				current_number := Integer'Value(To_String(trimmed_string));
				values.append(current_number);
		end case;
	end statemachine_proc;


	procedure line_proc
		(capture: in Unbounded_String)
	is
		idx,idxl  : Natural := 1;
		Search_Set : constant Character_Set := To_Set(" ,");
	begin
		-- Capture destination data
		idx := Index(
			Source => capture,
			Set => Search_Set,
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
				Set => Search_Set,
				From => idx+1
				);
		end loop;
		statemachine_proc(To_Unbounded_String(Slice(capture,idxl,Length(capture))));
	end line_proc;


	count_pos : long_integer := 0;
	procedure CheckValid is
		beg, fin, tfin : Integer := 1;
		Broken : constant Character_Set := To_Set("#");
		Working : constant Character_Set := To_Set(".");
		valid : Boolean := True;
	begin
		for L of values loop
			beg := Index(
				Source => process_line,
				Set => Broken,
				From => fin
				);
			if beg = 0 then
				valid := False;
				exit;
			end if;
			fin := Index(
				Source => process_line,
				Set => Working,
				From => beg
				);
			if fin = 0 then
				tfin := fin;
				fin := Length(process_line)+1;
			end if;
			--Put_Line("F: " & Fin'Image & "B: " & Beg'Image);
			if fin-beg /= L then
				valid := False;
				exit;
			end if;
		end loop;
		if valid and tfin /= 0 then
			beg := Index(
				Source => process_line,
				Set => Broken,
				From => fin
				);
			if beg /= 0 then
				valid := false;
			end if;
		end if;
		if valid then
			Put_Line("RecursedLine: " & To_String(process_line) & " Valid: " & Valid'Image);
			count_pos := count_pos + 1;
		end if;
	end CheckValid;

	procedure RecurseSolution is
		idx : Integer;
		Search_Set : constant Character_Set := To_Set("?");
	begin
		idx := Index(
			Source => process_line,
			Set => Search_Set,
			From => 1
			);
		if idx = 0 then
			CheckValid;
		else
			overwrite(process_line, idx, ".");
			RecurseSolution;
			overwrite(process_line, idx, "#");
			RecurseSolution;
			overwrite(process_line, idx, "?");
		end if;
	end RecurseSolution;


	procedure Main is
		F         : File_Type;
		File_Name : constant String := "input.txt";
		str       : Unbounded_String;
	begin
		Open (F, In_File, File_Name);
		while not End_Of_File (F) loop
			-- Capture the line
			state := name;
			values.clear;
			str := To_Unbounded_String(Get_Line (F));
			Put_Line(To_String(str));
			line_proc(str);

			Put_Line("Input: " & To_String(process_line));
			for I of values loop
				Put(I'Image & " ");
			end loop;
			Put_Line("");

			RecurseSolution;

			Put_Line("CountPos: " & Count_Pos'Image);
		end loop;
		Close (F);
	end Main;
end solutionpk;
