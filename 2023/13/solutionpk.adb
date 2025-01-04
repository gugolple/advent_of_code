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

	procedure PrintVector(v: UnboundedString_Vector.Vector)
	is
	begin
		Put_Line("Vector: ");
		for I of v loop
			Put_Line(To_String(I));
		end loop;
	end;

	function Transpose(v: UnboundedString_Vector.Vector) return UnboundedString_Vector.Vector 
	is
		transposed : UnboundedString_Vector.Vector;
		my_str : Unbounded_String;
		count : Integer;
	begin
		for I in 1 .. Length(v(0)) loop
			my_str := To_Unbounded_String(Integer(UnboundedString_Vector.Length(v)));
			count := 1;
			for S of v loop
				Replace_Element(my_str, count, Element(S, I));
				count := count + 1;
			end loop;
			transposed.append(my_str);
		end loop;
		return transposed;
	end Transpose;

	function VerticalCompare(v: UnboundedString_Vector.Vector; idx: out Integer; wall: out boolean) return Natural
	is
		res : Natural := 0;
		curr : Count_Type;
		ileft, iright : Integer;
		left, right : Unbounded_String;
		valid : boolean := True;
		limit : Count_Type;
	begin
		idx := 0;
		wall := False;
		for I in 0 .. UnboundedString_Vector.Length(v) -2 loop
			curr := 0;
			valid := True;
			limit := Count_Type'Min(I, UnboundedString_Vector.Length(v) - I -2);
			Put_Line("Start: " & I'Image & " limit: " & limit'Image);
			for J in 0 .. limit loop
				ileft := Integer(I-J);
				iright := Integer(I+1+J);
				Put_Line(": " & ileft'Image  & " R: " & iright'Image);
				if iright < Integer(UnboundedString_Vector.Length(v)) then
					left := v(ileft) ;
					right := v(iright);
					Put_Line(": " & To_String(left)  & " R: " & To_String(right));
					if left /= right then 
						Put_Line("Fail!");
						valid := False;
						exit;
					else
						curr := J;
					end if;
				end if;
			end loop;
			if valid and (I-curr = 0 or I+1+limit = UnboundedString_Vector.Length(v)-1) then
				Put_Line("Hit!");
				wall := True;
				if Integer(limit + 1) > res then
					idx := Integer(I) + 1;
					res := Integer(limit) + 1;
				end if;
			else
				Put_Line("Not good!");
			end if;
		end loop;
		if res = 0 then
			Put_Line("Reset");
			wall := False;
		end if;
		return res;
	end;

	function MainLogic(v: UnboundedString_Vector.Vector) return Integer is
		vwall, hwall : Boolean;
		t : UnboundedString_Vector.Vector := Transpose(v);
		best_vert: Integer;
		idx_vert: Integer;
		best_hor: Integer;
		idx_hor: Integer;
		total : Integer := 0;
	begin
		Put_Line("--------------------------------------------------------------------------------");
		Put_Line("Proc Start");
		Put_Line("Vertical");
		PrintVector(v);
		best_vert := VerticalCompare(v, idx_vert, vwall);
		Put_Line("Horizontal");
		PrintVector(t);
		best_hor := VerticalCompare(t, idx_hor, hwall);
		Put_Line("Vertical best: " & best_vert'Image & " idx: " & idx_vert'Image & " wall: " & vwall'Image);
		Put_Line("Horizontal best: " & best_hor'Image & " idx: " & idx_hor'Image & " wall: " & hwall'Image);

		if hwall then
			total := idx_hor;
		else
			total := 100 * idx_vert; 
		end if;
		Put_Line("Local value: " & total'Image);
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
			--Put_Line(To_String(str));

			if Length(str) > 0 then
				values.append(str);
			else 
				total := total + MainLogic(values);
				values.clear;
				Put_Line("Total: " & total'Image);
			end if;
		end loop;
		total := total + MainLogic(values);
		Put_Line("Total: " & total'Image);
		Close (F);
	end Main;
end solutionpk;
