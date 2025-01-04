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
		--for I of nums loop
		--	Put(I'Image & " ");
		--end loop;
		--Put_Line("Str: " & To_String(str));
		if Length(str) = 0 then
			if Length(nums) = 0 then
				return 1;
			else
				return 0;
			end if;
		end if;
		if Length(nums) = 0 then
			if Index(str, "#", 1) = 0 then
				return 1;
			else
				return 0;
			end if;
		end if;

		-- Respect it as a working
		idx := Index(str, To_Set(".?"), 1);
		if idx > 0 then
			nstr := To_Unbounded_String("");
			for I in idx+1 .. Length(str) loop
				append(nstr,Element(str, I));
			end loop;
			--Put_Line(".nstr: " & To_String(nstr));
			total := total + RecurseSolution(nstr, nums);
		end if;
		-- Respect it as a broken
		idx := Index(str, To_Set("#?"), 1);
		if idx > 0 then
			first_val := First_Element(nums);
			if first_val <= Length(str) and (Index(str, To_Set("."), 1) = 0 or Index(str, To_Set("."), 1) > first_val) then
				if Length(str) = first_val or (first_val+1 <= Length(str) and then Element(str, first_val+1) /= '#') then
					for I in 1 .. Integer(Length(nums))-1 loop
						next_nums.append(nums(I));
					end loop;
					nstr := To_Unbounded_String("");
					for I in first_val+2 .. Length(str) loop
						append(nstr,Element(str, I));
					end loop;
					--Put_Line("#nstr: " & To_String(nstr));
					total := total + recursesolution(nstr, next_nums);
				end if;
			end if;
		end if;
		--for I of nums loop
		--	Put(I'Image & " ");
		--end loop;
		--Put_Line("Str: " & To_String(str));
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
