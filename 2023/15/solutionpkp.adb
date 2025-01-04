with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use  Ada.Strings.Unbounded;
with Ada.Strings.Maps; use Ada.Strings.Maps;

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;

with solutionpkp; use solutionpkp;

package body solutionpkp is
	process_line : Unbounded_String;
	type Lens is record
		Id : Unbounded_String;
		FocalLength : Integer;
	end record;

	function "="(Left, Right: Lens) return boolean
	is
	begin
		return Left.Id = Right.Id;
	end "=";

	package LensVector is new
		Ada.Containers.Vectors
			(Index_Type => Natural,
			Element_Type => Lens
			);

	subtype HashRange is Natural range 0 .. 255;
	
	boxes: array (HashRange) of LensVector.Vector;
	

	function Hash(str: Unbounded_String) return HashRange
	is
		total : Integer := 0;
		chr : Character;
	begin
		for Idx in 1 .. Length(str) loop
			chr := Element(str, Idx);
			--Put_Line("Char: " & chr & " val: " & Character'Pos(chr)'Image);
			total := total + Character'Pos(chr);
			total := total * 17;
			total := total REM 256;
		end loop;
		--Put_Line("Local total: " & total'Image);
		return total;
	end Hash;

	procedure ProcessSlice(str: Unbounded_String)
	is
		idx : Integer;
		sidx : Integer;
		Search_Set : constant Character_Set := To_Set("=-");
		boxid : HashRange;
		box : LensVector.Vector;
		operation : Character;
		mylens : Lens;
	begin
		sidx := Index(
			Source => str,
			Set => Search_Set,
			From => 1
			);
		mylens.Id := To_Unbounded_String(Slice(str, 1, sidx-1));
		boxid := Hash(mylens.id);
		box := boxes(boxid);
		Put_Line("Slice proc: " & To_String(str) & " Box: " & boxid'Image);
		operation := Element(str, sidx);
		idx := LensVector.Find_Index(box, mylens);
		if operation = '-' then
			if idx >= 0 then
				Put_Line("Delete!");
				LensVector.Delete(box, idx);
			end if;
		elsif operation = '=' then
			mylens.FocalLength := Integer'Value(Slice(str, sidx+1, Length(str)));
			if idx >= 0 then
				Put_Line("Replace!");
				LensVector.Replace_Element(box, idx, mylens);
			else
				Put_Line("Append!");
				LensVector.Append(box, mylens);
			end if;
		end if;
		boxes(boxid) := box;
	end ProcessSlice;


	procedure line_proc
		(capture: in Unbounded_String)
	is
		idx,idxl  : Natural := 1;
		Search_Set : constant Character_Set := To_Set(",");
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
			ProcessSlice(To_Unbounded_String(Slice(capture,idxl,idx-1)));

			idxl := idx + 1;
			idx := Index(
				Source => capture,
				Set => Search_Set,
				From => idx+1
				);
		end loop;
		ProcessSlice(To_Unbounded_String(Slice(capture,idxl,Length(capture))));
	end line_proc;

	procedure Main is
		F         : File_Type;
		File_Name : constant String := "input.txt";
		str       : Unbounded_String;
		box       : LensVector.Vector;
		mylens: Lens;
		total : Integer := 0;
	begin
		Open (F, In_File, File_Name);
		while not End_Of_File (F) loop
			-- Capture the line
			str := To_Unbounded_String(Get_Line (F));
			line_proc(str);
		end loop;
		Close (F);
		for I in boxes'First .. boxes'Last loop
			box := boxes(I);
			if LensVector.Length(box) > 0 then
				Put_Line("Box: " & I'Image);
				for L in 0 .. Integer(LensVector.Length(box))-1 loop
					mylens := box(L);
					Put_Line("Id: " & To_String(mylens.Id) & " FL" & mylens.FocalLength'Image);
					total := total + (1 + I) * (L + 1) * mylens.FocalLength;
				end loop;
			end if;
		end loop;
		Put_Line("Total: " & Total'Image);
	end Main;
end solutionpkp;
