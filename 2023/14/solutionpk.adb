with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use  Ada.Strings.Unbounded;
with Ada.Strings.Maps; use Ada.Strings.Maps;

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;

with solutionpk; use solutionpk;

package body solutionpk is
	package Unbounded_StringVector is new
		Ada.Containers.Vectors
			(Index_Type => Natural,
			Element_Type => Unbounded_String 
			);
	use Unbounded_StringVector;
	
	process_line : Unbounded_String;

	procedure PrintVector(v: Unbounded_StringVector.Vector)
	is
	begin
		for I of v loop
			Put_Line(To_String(I));
		end loop;
		Put_Line("");
	end;

	procedure MoveNorth(v: in out Unbounded_StringVector.Vector)
	is
		idx : Integer;
		space_row : Integer;
		tmp_row : Unbounded_String;
	begin
		for I in 1 .. Integer(Unbounded_StringVector.Length(v) -1) loop
			--Put_Line("Line: " & I'Image);
			idx := Index(v(I), "O", 1);
			while idx /= 0 loop
				--Put_Line("Idx: " & idx'Image);
				space_row := 0;
				for J in reverse 0 .. I-1 loop
					if Element(v(J), idx) /= '.' then
						space_row := J+1;
						exit;
					end if;
				end loop;
				if space_row /= I then
					--Put_Line("Replaced!");
					tmp_row := v(space_row);
					Replace_Element(tmp_row, idx, 'O');
					Unbounded_StringVector.Replace_Element(v, space_row, tmp_row);
					tmp_row := v(I);
					Replace_Element(tmp_row, idx, '.');
					Unbounded_StringVector.Replace_Element(v, I, tmp_row);
					--PrintVector(v);
				end if;
				idx := Index(v(I), "O", idx + 1);
			end loop;
		end loop;
	end;

	function CalcWeight(v: Unbounded_StringVector.Vector) return Integer
	is
		total : Integer := 0;
		line_val : Integer := Integer(Unbounded_StringVector.Length(v));
	begin
		for R of v loop
			total := total + Integer(Ada.Strings.Unbounded.Count(R, "O")) * line_val;
			line_val := line_val -1;
		end loop;
		return total;
	end CalcWeight;

	procedure Main is
		F         : File_Type;
		File_Name : constant String := "input.txt";
		str       : Unbounded_String;
		values : Unbounded_StringVector.Vector;
	begin
		Open (F, In_File, File_Name);
		while not End_Of_File (F) loop
			-- Capture the line
			str := To_Unbounded_String(Get_Line (F));
			values.append(str);

		end loop;
		Close (F);
		Put_Line("Input:");
		PrintVector(values); 
		MoveNorth(values);
		Put_Line("Northed:");
		PrintVector(values); 
		Put_Line("Result: " & CalcWeight(values)'Image);
	end Main;
end solutionpk;
