with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use  Ada.Strings.Unbounded;
with Ada.Strings.Maps; use Ada.Strings.Maps;

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;

with solutionpk; use solutionpk;

package body solutionpk is
	package UnboundedString_Vector is new
		Ada.Containers.Vectors
			(Index_Type => Natural,
			Element_Type => Unbounded_String 
			);
	use UnboundedString_Vector;

	function HorizontalCompare(v: UnboundedString_Vector.Vector; idx: out Integer; wall: out boolean) return Natural
	is
		res : Natural := 0;
		line : Unbounded_String;
		valid : Boolean := False;
		lidx, ridx : Integer;
		lchr, rchr : Character;
	begin
		wall := False;
		line := v(0);
		-- FOr all positions
		for I in 1 .. Length(line)-1 loop
			valid := True;
			-- All sizes that make sense
			for size in 0 .. Integer'Min(I-1, Length(line)-I-1) loop
				-- All rows
				for str of v loop
					Put_Line(To_String(str) & " Idx: " & I'Image & " Size: " & size'Image);
					lidx := I-size;
					ridx := I+1+size;
					Put_Line(lidx'Image & " - " & ridx'Image);
					lchr := Element(str, lidx);
					rchr := Element(str, ridx);
					Put_Line(lchr & " - " & rchr);
					if lchr /= rchr then
						valid := False;
						exit;
					end if;
					if not valid then
						exit;
					end if;
				end loop;
				if not valid then
					exit;
				else
					Put_Line("Saved idx: " & I'Image & " size: " & size'Image);
					if size + 1 > res then
						if I - size = 0 or I + 1 + size = Ada.Strings.Unbounded.Length(v(0)) then
							wall := True;
						end if;
						res := size + 1;
						idx := I;
					end if;
				end if;
			end loop;
		end loop;
		return res;
	end;

	function VerticalCompare(v: UnboundedString_Vector.Vector; idx: out Integer; wall: out boolean) return Natural
	is
		res : Natural := 0;
		curr : Count_Type;
		ileft, iright : Integer;
		left, right : Unbounded_String;
	begin
		wall := False;
		for I in 0 .. UnboundedString_Vector.Length(v) -1 loop
			curr := 0;
			for J in 0 .. Count_Type'Min(I, UnboundedString_Vector.Length(v) - I) loop
				ileft := Integer(I-J);
				iright := Integer(I+1+J);
				--Put_Line(": " & ileft'Image  & " R: " & iright'Image);
				if iright < Integer(UnboundedString_Vector.Length(v)) then
					left := v(ileft) ;
					right := v(iright);
					if left = right then 
						curr := curr + 1;
					else 
						exit;
					end if;
				end if;
			end loop;
			if Integer(curr) > res then
				if I-curr = 0 or I+1+curr = UnboundedString_Vector.Length(v) then
					wall := True;
				end if;
				idx := Integer(I) + 1;
				res := Integer(curr);
			end if;
		end loop;
		return res;
	end;

	function MainLogic(v: UnboundedString_Vector.Vector) return Integer is
		vwall, hwall : Boolean;
		best_vert: Integer;
		idx_vert: Integer;
		best_hor: Integer;
		idx_hor: Integer;
		total : Integer := 0;
	begin
		Put_Line("Input: ");
		for I of v loop
			Put_Line(To_String(I));
		end loop;
		best_vert := VerticalCompare(v, idx_vert, vwall);
		best_hor := horizontalCompare(v, idx_hor, hwall);
		Put_Line("Vertical: " & best_vert'Image & " " & idx_vert'Image);
		Put_Line("Horizontal: " & best_hor'Image & " " & idx_hor'Image);
		Put_Line("End");
		Put_Line("");

		if hwall and best_hor >= best_vert then
			total := idx_hor;
		else
			total := 100 * idx_vert; 
		end if;
		return total;
	end;

	procedure Main is
		F         : File_Type;
		File_Name : constant String := "input.txt";
		str       : Unbounded_String;
		values : UnboundedString_Vector.Vector;
		total : Integer := 0;
	begin
		Open (F, In_File, File_Name);
		while not End_Of_File (F) loop
			-- Capture the line
			str := To_Unbounded_String(Get_Line (F));
			Put_Line(To_String(str));

			if Length(str) > 0 then
				values.append(str);
			else 
				total := total + MainLogic(values);
				values.clear;
			end if;
		end loop;
		total := total + MainLogic(values);
		Put_Line("Total: " & total'Image);
		Close (F);
	end Main;
end solutionpk;
