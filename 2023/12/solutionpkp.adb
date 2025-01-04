with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use  Ada.Strings.Unbounded;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Unbounded.Hash;

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;

with solutionpkp; use solutionpkp;

package body solutionpkp is
	package IntegerVector is new
		Ada.Containers.Vectors
			(Index_Type => Natural,
			Element_Type => Integer 
			);
	use IntegerVector;

	type Unbounded_String_Access is access Unbounded_String;
	package StrToCount is new
		Ada.Containers.Hashed_Maps
			( Key_Type => Unbounded_String,
			Element_Type => long_integer,
			Hash => Hash,
			Equivalent_Keys => "=");
	use StrToCount;

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


	function CheckValid(str: Unbounded_String) return boolean is
		beg, fin, tfin : Integer := 1;
		Broken : constant Character_Set := To_Set("#");
		Working : constant Character_Set := To_Set(".");
		valid : Boolean := True;
	begin
		for L of values loop
			beg := Index(
				Source => str,
				Set => Broken,
				From => fin
				);
			if beg = 0 then
				valid := False;
				exit;
			end if;
			fin := Index(
				Source => str,
				Set => Working,
				From => beg
				);
			if fin = 0 then
				tfin := fin;
				fin := Length(str)+1;
			end if;
			--Put_Line("F: " & Fin'Image & "B: " & Beg'Image);
			if fin-beg /= L then
				valid := False;
				exit;
			end if;
		end loop;
		if valid and tfin /= 0 then
			beg := Index(
				Source => str,
				Set => Broken,
				From => fin
				);
			if beg /= 0 then
				valid := false;
			end if;
		end if;
		Put_Line("RecursedLine: " & To_String(str) & " Valid: " & Valid'Image);
		if valid then
			Put_Line("RecursedLine: " & To_String(str) & " Valid: " & Valid'Image);
		end if;
		return valid;
	end CheckValid;

	dp : StrToCount.Map;
	function RecurseSolution(str:in out Unbounded_String; pos : Integer := 1) return long_integer is
		curr : long_integer := 0;
		total : long_integer := 0;
		idx : Integer;
		Search_Set : constant Character_Set := To_Set("?");
		Mslice : Unbounded_String;
		tstr : Unbounded_String_Access;
	begin
		mslice := To_Unbounded_String(Slice(str, pos, Length(str)));
		Put_Line("Pos: " & pos'Image & " slice: " & To_String(mslice) & " str: " & To_string(str));
		if Contains(dp, mslice) then
			return Element(dp, mslice);
		end if;
		idx := Index(
			Source => str,
			Set => Search_Set,
			From => 1
			);
		if idx = 0 then
			if CheckValid(mslice) then
				return 1;
			end if;
		else
			overwrite(str, idx, ".");
			curr := RecurseSolution(str, idx);
			total := total + curr;
			overwrite(str, idx, "#");
			curr := RecurseSolution(str, idx);
			total := total + curr;
			overwrite(str, idx, "?");
			if not Contains(dp, mslice) and total > 0 then
				Insert(dp, mslice, total);
			end if;
		end if;
		Put_Line("R total: " & total'Image);
		return total;
	end RecurseSolution;


	procedure Main is
		F         : File_Type;
		File_Name : constant String := "input.txt";
		str       : Unbounded_String;
		tmp       : IntegerVector.Vector;
		count_pos : long_integer := 0;
		total     : long_integer := 0;
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

			count_pos := 0;
			count_pos := RecurseSolution(process_line);

			Put_Line("CountPos: " & Count_Pos'Image);
			total := total + Count_Pos;
		end loop;
		Put_Line("CountPos: " & Count_Pos'Image);
		Put_Line("Total: " & Total'Image);
		Close (F);
	end Main;
end solutionpkp;
