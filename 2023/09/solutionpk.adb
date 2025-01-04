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

	type IntegerVector_Access is access all IntegerVector.Vector;

	Function "=" (Left, Right: In IntegerVector.Vector) return boolean
	is
		res : boolean := False;
	begin
		if Left.First_Index = Right.First_Index and Left.Last_Index = Right.Last_Index then
			res := True;
			for I in Left.First_Index .. Left.Last_Index loop
				if Left(I) /= Right(I) then
					res := False;
					exit;
				end if;
			end loop;
		end if;
		return res;
	end "=";

	package IntegerVectorVector is new
		Ada.Containers.Vectors
			(Index_Type => Natural,
			Element_Type => IntegerVector.Vector
			);

		capture_vector : IntegerVector_Access := new IntegerVector.Vector;
	state: StateMachine;
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
			when value =>
				current_number := Integer'Value(To_String(trimmed_string));
				capture_vector.append(current_number);
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

	function recurse_add(IV : IntegerVector_Access) return Integer
	is
		temp_vector : IntegerVector_Access := new IntegerVector.Vector;
		difference : Integer;
		result : Integer := 0;
		only_zeros : Boolean := True;
	begin
		Put("Input: " );
		for I of IV.all loop
			Put(I'image & ", ");
		end loop;
		Put_Line("");
		for I in IV.First_Index .. IV.Last_Index-1 loop
			difference := IV(I+1) - IV(I);
			temp_vector.append(difference);
			if difference /= 0 then
				only_zeros := False;
			end if;
		end loop;
		if not only_zeros then
			result := recurse_add(temp_vector) + IV.Last_Element;
		else
			result := IV.Last_Element;
		end if;
		Put("After: " );
		for I of IV.all loop
			Put(I'image & ", ");
		end loop;
		Put_Line(" Result: " & result'Image);
		return result;
	end recurse_add;


	procedure Main is
		F         : File_Type;
		File_Name : constant String := "input.txt";
		str       : Unbounded_String;
		total : Integer := 0;
		current : Integer := 0;
	begin
		Open (F, In_File, File_Name);
		while not End_Of_File (F) loop
			-- Capture the line
			capture_vector.clear;
			str := To_Unbounded_String(Get_Line (F));
			Put_Line(To_String(str));
			line_proc(str);
			--Put("Catptured: " );
			--for I of capture_vector loop
			--	Put(I'image & ", ");
			--end loop;
			--Put_Line("");
			
			current := recurse_add(capture_vector);
			total := total + current;
			Put_Line("Total: " & total'Image & " current: " & current'Image);
		end loop;
		Close (F);
	end Main;
end solutionpk;
