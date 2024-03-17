with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use  Ada.Strings.Unbounded;
with Ada.Strings.Maps; use Ada.Strings.Maps;

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;

with solutionpkp; use solutionpkp;

package body solutionpkp is
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
	total : long_integer := 0;
	procedure CheckValid is
		beg, fin, tfin : Integer := 1;
		Broken : constant Character_Set := To_Set("#");
		Working : constant Character_Set := To_Set(".");
		valid : Boolean := True;
	begin
		total := total + 1;
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
			Put_Line("Resul: " & To_String(process_line) & " Valid: " & Valid'Image);
			count_pos := count_pos + 1;
		end if;
	end CheckValid;

	procedure RecurseSolution(position_values: Integer := 0; position_string: Integer := 1) is
		idx : Integer;
		idx_end : Integer;
		length : Integer;
		Broken : constant Character_Set := To_Set("#?");
		Working :  constant Character_Set := To_Set(".?");
		brokenslice : Unbounded_String;
		workingslice : Unbounded_String;
	begin
		if position_values = Integer(IntegerVector.Length(values)) then
			Put_Line("Resul: " & To_String(process_line));
			count_pos := count_pos + 1;
			return;
		end if;
		if position_string > 1 then
			if Element(process_line, position_string-1) = '#' then
				Put_Line("Bad");
				return;
			end if;
		end if;
		Put_Line("Currn: " & To_String(process_line) & " pv: " & position_values'Image);
		Length := values(position_values);
		idx := position_string;
		idx_end := idx;
		-- Search start
		while idx /= 0 loop
			Put_Line("B: " & idx'Image & " F: " & idx_end'Image & " D: " & position_values'Image);
			idx := Index(
				Source => process_line,
				Set => Broken,
				From => idx 
				);
			if idx = 0 then
				return;
			end if;
			-- Search end
			idx_end := idx;
			while idx_end /= 0 and (idx_end - idx) < Length loop
				idx_end := Index(
					Source => process_line,
					Set => Working,
					From => idx_end + 1 
					);
			end loop;
			if idx_end = 0 then
				idx_end := Integer(Ada.Strings.Unbounded.Length(process_line));
			end if;
			if (idx_end - idx) > Length then
				Put_Line("Too long");
			else
				Put_Line("B: " & idx'Image & " F: " & idx_end'Image & " D: " & position_values'Image);
				if idx_end + 1 < Integer(Ada.Strings.Unbounded.Length(process_line)) then
					idx_end := idx_end + 1;
				end if;
				-- All good, then replace
				workingslice := Unbounded_Slice(process_line, position_string, idx);
				for I in idx .. idx_end loop
					Overwrite(process_line, I, ".");
				end loop;
				brokenslice := Unbounded_Slice(process_line, idx, idx_end);
				for I in idx .. idx_end loop
					Overwrite(process_line, I, "#");
				end loop;
				-- Set the last one to a working to break the continuos strinng
				Overwrite(process_line, idx_end-1, ".");
				RecurseSolution(position_values + 1, idx_end);
				Replace_Slice(process_line, position_string, idx, To_String(workingslice));
				Replace_Slice(process_line, idx, idx_end, To_String(brokenslice));
				Put_Line("B: " & idx'Image & " F: " & idx_end'Image & " D: " & position_values'Image);
				Put_Line("Currn: " & To_String(process_line) & " pv: " & position_values'Image);
			end if;
			idx := idx + 1;
		end loop;
	end RecurseSolution;


	procedure Main is
		F         : File_Type;
		File_Name : constant String := "input.txt";
		str       : Unbounded_String;
		tmp       : IntegerVector.Vector;
	begin
		Open (F, In_File, File_Name);
		while not End_Of_File (F) loop
			-- Capture the line
			state := name;
			values.clear;
			str := To_Unbounded_String(Get_Line (F));
			Put_Line(To_String(str));
			line_proc(str);

			str := process_line;
			for I in 1..4 loop
				Append(process_line, To_Unbounded_String("?"));
				Append(process_line, str);
			end loop;

			tmp.clear;
			for V of values loop
				tmp.append(V);
			end loop;
			for I in 1..4 loop
				for V of tmp loop
					values.append(V);
				end loop;
			end loop;

			Put_Line("Input: " & To_String(process_line));
			for I of values loop
				Put(I'Image & " ");
			end loop;
			Put_Line("");

			RecurseSolution;

			Put_Line("CountPos: " & Count_Pos'Image);
			Put_Line("Total: " & Total'Image);
		end loop;
		Put_Line("CountPos: " & Count_Pos'Image);
		Put_Line("Total: " & Total'Image);
		Close (F);
	end Main;
end solutionpkp;
