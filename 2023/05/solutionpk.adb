with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Maps; use Ada.Strings.Maps;

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;

with solutionpk; use solutionpk;

package body solutionpk is

    type Transfomration is record
        start_range: Long_Long_Integer;
        end_range: Long_Long_Integer;
        destination_start: Long_Long_Integer;
    end record;

    function transformationFromString(str: Unbounded_String) return Transfomration
    is
		idx,idxl  : Natural := 1;
		Search_Set : constant Character_Set := To_Set(" ");
        res : Transfomration;
	begin
        --Put_Line("Transform from string: " & To_String(str));
		-- Capture destination data
		idx := Index(
			Source => str,
			Set => Search_Set,
			From => idx
			);
        res.destination_start := Long_Long_Integer'Value(Slice(str,idxl,idx));

        idxl := idx + 1;
        idx := Index(
            Source => str,
            Set => Search_Set,
            From => idx+1
            );
        res.start_range := Long_Long_Integer'Value(Slice(str,idxl,idx));

        idxl := idx + 1;
        res.end_range := res.start_range + Long_Long_Integer'Value(Slice(str,idxl,Length(str))) -1;

        return res;
    end;

	package Long_Long_IntegerVector is new
		Ada.Containers.Vectors
			(Index_Type => Natural,
			Element_Type => Long_Long_Integer 
			);
	use Long_Long_IntegerVector;

	state: StateMachine;
	
	values : Long_Long_IntegerVector.Vector;

	procedure statemachine_proc(
		capture: in Unbounded_String
		)
	is
		current_number : Long_Long_Integer := 0;
		trimmed_string : Unbounded_String;
	begin
		trimmed_string := Trim(capture, To_Set(' '), To_Set(":;, "));
		Put_Line("Token: " & To_String(trimmed_string) & " State: " & state'Image);
		if trimmed_string = "" then
			return;
		end if;
		case state is
			when name =>
				state := number;
			when number =>
				current_number := Long_Long_Integer'Value(To_String(trimmed_string));
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

	procedure Main is
		F         : File_Type := Standard_Input;
		str       : Unbounded_String;
        t         : Transfomration;
        t_values  : Long_Long_IntegerVector.Vector;
        idx       : Natural;
        e         : Long_Long_Integer;
	begin
        -- Read first line, the input
        str := To_Unbounded_String(Get_Line (F));
        Put_Line(To_String(str));
        line_proc(str);

        Put("Val: ");
        for V of values loop
            Put(V'Image & " "); 
        end loop;
        Put_Line("");

		while not End_Of_File (F) loop
			-- Capture the line
			str := To_Unbounded_String(Get_Line (F));

            if Index(str, "map", 1) > 0 then
                Put_Line(To_String(str));
                Put_Line("Map line!");
                -- Add all elements in the transformed vector back
                Append(values, t_values);
                -- Clear all values
                Clear(t_values);
            elsif Length(str) > 0 then
                t := transformationFromString(str);
                Put_Line("S: " & t.start_range'Image & " E: " & t.end_range'Image & " D: " & t.destination_start'Image);

                idx := 1;
                while idx < Natural(Length(values)) loop
                    e := Long_Long_IntegerVector.Element(values, idx);
                    if e >= t.start_range and e <= t.end_range then
                        Delete(values, idx);
                        Append(t_values, e - t.start_range + t.destination_start);
                        idx := idx -1;
                    end if;
                    idx := idx + 1;
                end loop;
            end if;
		end loop;
        Append(values, t_values);
        e := First_Element(values);
        for V of values loop
            Put_Line("Val: " & V'Image);
            if v < e then
                e := v;
            end if;
        end loop;
        Put_Line("");
        Put_Line("Sol: " & e'Image);
		Close (F);
	end Main;
end solutionpk;
