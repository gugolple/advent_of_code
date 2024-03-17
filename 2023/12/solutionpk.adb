with Ada.Text_IO; use Ada.Text_IO;
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

	function RecurseSolution(str: Unbounded_String; nums: IntegerVector.Vector) return Long_Integer is
		total : Long_Integer := 0;
		idx : Integer;
		first_val : Integer;
		next_nums : IntegerVector.Vector;
		nstr : Unbounded_String;
	begin
		--Put("Str: " & To_String(str) & " nums: ");
		--for I of nums loop
		--	Put(I'Image & ",");
		--end loop;
		--Put_Line("");

		-- If no more chars
		if Length(str) = 0 then
			if Length(nums) = 0 then
				return 1;
			end if;
			return 0;
		end if;

		-- If no more nums
		if Length(nums) = 0 then
			-- No more to worl
			if Index(str, "#", 1) = 0 then
				return 1;
			end if;
			return 0;
		end if;

		-- Now recursion
		-- If we think is blank
		if Element(str, 1) = '.' or Element(str, 1) = '?' then
			nstr := To_Unbounded_String("");
			for I in 2 .. Length(str) loop
				append(nstr, Element(str,I));
			end loop;
			total := total + RecurseSolution(nstr, nums);
		end if;
		-- If we think is hit
		if Element(str, 1) = '#' or Element(str, 1) = '?' then
			first_val := First_Element(nums);
			idx := Index(str, ".", 1);
			if idx = 0 then
				idx := Length(str) +1;
			end if;
			if first_val <= Length(str) and idx > first_val then
				if Length(str) = first_val or else Element(str, first_val+1) /= '#' then
					next_nums.clear;
					for I in 1 .. Integer(Length(nums)-1) loop
						append(next_nums, Element(nums,I));
					end loop;
					nstr := To_Unbounded_String("");
					for I in first_val+2 .. Length(str) loop
						append(nstr, Element(str,I));
					end loop;
					total := total + RecurseSolution(nstr, next_nums);
				end if;
			end if;
		end if;

		--Put("Str: " & To_String(str) & " nums: ");
		--for I of nums loop
		--	Put(I'Image & ",");
		--end loop;
		--Put_Line("");
		--Put_Line("Total: " & total'Image);
		return total;
	end RecurseSolution;


	procedure Main is
		F         : File_Type;
		File_Name : constant String := "input.txt";
		str       : Unbounded_String;
		total     : Long_Integer := 0;
		current   : Long_Integer;
		t: Unbounded_String;
	begin
		Open (F, In_File, File_Name);
		while not End_Of_File (F) loop
			-- Capture the line
			state := name;
			values.clear;
			str := To_Unbounded_String(Get_Line (F));
			Put_Line(To_String(str));
			line_proc(str);

			--Put_Line("Input: " & To_String(process_line));
			--for I of values loop
			--	Put(I'Image & " ");
			--end loop;
			--Put_Line("");

			current := RecurseSolution(process_line, values);
			Put_Line(current'Image);
			total := total + current;
		end loop;
		Close (F);
		Put_Line("Total: " & total'Image);
	end Main;
end solutionpk;
