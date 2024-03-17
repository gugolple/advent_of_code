with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use  Ada.Strings.Unbounded;
with Ada.Strings.Maps; use Ada.Strings.Maps;

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;

with solutionpk; use solutionpk;

package body solutionpk is
	process_line : Unbounded_String;

	function ProcessSlice(str: Unbounded_String) return Integer
	is
		total : Integer := 0;
		chr : Character;
	begin
		Put_Line("Slice proc: " & To_String(str));
		for Idx in 1 .. Length(str) loop
			chr := Element(str, Idx);
			Put_Line("Char: " & chr & " val: " & Character'Pos(chr)'Image);
			total := total + Character'Pos(chr);
			total := total * 17;
			total := total REM 256;
		end loop;
		Put_Line("Local total: " & total'Image);
		return total;
	end ProcessSlice;


	procedure line_proc
		(capture: in Unbounded_String)
	is
		idx,idxl  : Natural := 1;
		Search_Set : constant Character_Set := To_Set(",");
		total : Integer := 0;
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
			total := total + ProcessSlice(To_Unbounded_String(Slice(capture,idxl,idx-1)));

			idxl := idx + 1;
			idx := Index(
				Source => capture,
				Set => Search_Set,
				From => idx+1
				);
		end loop;
		total := total + ProcessSlice(To_Unbounded_String(Slice(capture,idxl,Length(capture))));
		Put_Line("Global total: " & total'Image);
	end line_proc;

	procedure Main is
		F         : File_Type;
		File_Name : constant String := "input.txt";
		str       : Unbounded_String;
	begin
		Open (F, In_File, File_Name);
		while not End_Of_File (F) loop
			-- Capture the line
			str := To_Unbounded_String(Get_Line (F));
			line_proc(str);
		end loop;
		Close (F);
	end Main;
end solutionpk;
