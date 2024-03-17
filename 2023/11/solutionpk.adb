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
	package Unbounded_StringVector is new
		Ada.Containers.Vectors
			(Index_Type => Natural,
			Element_Type => Unbounded_String 
			);
	use Unbounded_StringVector;

	grid : Unbounded_StringVector.Vector;

	procedure PrintGrid is
	begin
		Put_Line("grid:");
		for s of grid loop
			Put_Line(To_String(s));
		end loop;
	end PrintGrid;

	state: StateMachine;
	procedure statemachine_proc(
		capture: in Unbounded_String
		)
	is
		idx : Integer := 0;
		trimmed_string : Unbounded_String;
	begin
		trimmed_string := Trim(capture, To_Set(' '), To_Set(":;, "));
		--Put_Line("Token: " & To_String(trimmed_string) & " State: " & state'Image);
		if trimmed_string = "" then
			return;
		end if;
		case state is
			when name => 
				grid.append(trimmed_string);
				idx := Index(trimmed_string, "#", 1);
				if idx = 0 then
					Append(grid, trimmed_string);
				end if;
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

	procedure PadGrid is
		idx : Integer;
		COL : Integer;
		empty : Boolean;
	begin
		COL := 1;
		while COL < Length(grid(0)) loop
			empty := True;
			for I in First_Index(grid) .. Last_Index(grid) loop
				if Element(grid(I), COL) = '#' then
					empty := False;
				end if;
			end loop;
			if empty then
				for I in First_Index(grid) .. Last_Index(grid) loop
					Insert(grid(I), COL, ".");
				end loop;
				COL := COL + 1;
			end if;
			COL := COL + 1;
		end loop;
	end PadGrid;

	function CalculateDistances(row, col: Integer) return Integer is
		total : Integer := 0;
		dist: Integer;
		row_str : Unbounded_String;
		cur_row : Integer;
		cur_col : Integer;
	begin
		Replace_Element(grid(row), col, 'X');
		PrintGrid;
		Replace_Element(grid(row), col, '.');
		Put_Line("Sum from row: " & row'image & " col: " & col'Image);
		for cur_row in First_Index(grid) .. Last_Index(grid) loop
			row_str := grid(cur_row);
			cur_col := Index(row_str, "#", 1);
			while cur_col /= 0 loop
				dist := abs ( row - cur_row)   + abs (col - cur_col);
				Put_Line("Sum to row: " & cur_row'image & " col: " & cur_col'Image & " dist: " & dist'Image);
				total := total + dist;
				cur_col := Index(row_str, "#", cur_col + 1);
			end loop;
		end loop;
		return total;
	end;


	procedure CountDistances is
		total : Integer := 0;
		idx : Integer;
		row_idx : Integer;
		row : Unbounded_String;
		cur_start_row : Integer;
		cur_start_col : Integer;
	begin
		for row_idx in First_Index(grid) .. Last_Index(grid) loop
			row := grid(row_idx);
			idx := Index(row, "#", 1);
			while idx /= 0 loop
				total := total + CalculateDistances(row_idx, idx);
				idx := Index(row, "#", idx + 1);
			end loop;
		end loop;
		Put_Line("Total Distances: " & total'image);
	end CountDistances;


	procedure Main is
		F         : File_Type;
		File_Name : constant String := "input.txt";
		str       : Unbounded_String;
	begin
		Open (F, In_File, File_Name);
		while not End_Of_File (F) loop
			-- Capture the line
			str := To_Unbounded_String(Get_Line (F));
			Put_Line(To_String(str));
			line_proc(str);
		end loop;
		PadGrid;
		Put_Line("Captured!");
		PrintGrid;
		CountDistances;
		Close (F);
	end Main;
end solutionpk;
