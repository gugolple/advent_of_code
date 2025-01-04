with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Maps; use Ada.Strings.Maps;

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Sets;

with solutionpkp; use solutionpkp;

package body solutionpkp is
    type SeedRange is record
        start_range: Long_Long_Integer;
        end_range: Long_Long_Integer;
    end record;

    function "<"(Left, Right: SeedRange) return boolean
    is
    begin
        return Left.start_range < Right.start_range;
    end;

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

	package NaturalSet is new
		Ada.Containers.Ordered_Sets
			(
			Element_Type => Natural 
			);
	use NaturalSet;

	package SeedRangeVector is new
		Ada.Containers.Vectors
			(Index_Type => Natural,
			Element_Type => SeedRange 
			);
	use SeedRangeVector;

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

    function LLI_to_SR(v: Long_Long_IntegerVector.Vector) return SeedRangeVector.Vector
    is
        idx: Natural := 0;
        sr: SeedRange;
        srv: SeedRangeVector.Vector;
    begin
        while idx < Natural(Length(v)) loop
            sr.start_range := Element(v, idx);
            sr.end_range := Element(v, idx+1) + sr.start_range -1;
            idx := idx + 2;
            Append(srv, sr);
        end loop;
        return srv;
    end;

    package SRSorter is new SeedRangeVector.Generic_Sorting;
    procedure compactVector(v: in out SeedRangeVector.Vector)
    is
        idx : Natural;
        e : SeedRange;
        pe : SeedRange;
    begin
        SRSorter.Sort(v);
        idx := 1;
        e := Element(v, 0);
        while idx < Natural(Length(v)) loop
            pe := e;
            e := Element(v, idx);

            if pe.start_range <= e.start_range and pe.end_range >= e.start_range then
                pe.end_range := Long_Long_Integer'Max(pe.end_range, e.end_range);
                Delete(v, idx);
                idx := idx -1;
                e := pe;
            end if;

            idx := idx + 1;
        end loop;
    end compactVector;

	procedure Main is
		F         : File_Type := Standard_Input;
		str       : Unbounded_String;
        t         : Transfomration;
        t_values  : SeedRangeVector.Vector;
        idx       : Natural;
        e         : SeedRange;
        te        : SeedRange;
        srv       : SeedRangeVector.Vector;
        hit_v     : NaturalSet.Set;
	begin
        -- Read first line, the input
        str := To_Unbounded_String(Get_Line (F));
        Put_Line(To_String(str));
        line_proc(str);

        srv := LLI_to_SR(values);

        compactVector(srv);
        Put_Line("");
        Put_Line("Vals:");
        for V of srv loop
            Put_Line("s: " & V.start_range'Image & " e: " & V.end_range'Image); 
        end loop;
        Put_Line("");

		while not End_Of_File (F) loop
			-- Capture the line
			str := To_Unbounded_String(Get_Line (F));

            if Index(str, "map", 1) > 0 then
                Put_Line("");
                Put_Line("");
                Put_Line(To_String(str));
                Put_Line("Map line!");
                -- Clear all processed lines
                for V of reverse hit_v loop
                    --Put_Line("Delete: " & V'Image);
                    Delete(srv, V);
                end loop;
                Clear(hit_v);
                -- Add all elements in the transformed vector back
                Append(srv, t_values);
                -- Clear all values
                Clear(t_values);
                compactVector(srv);
                for V of srv loop
                    Put_Line("Vals s: " & V.start_range'Image & " e: " & V.end_range'Image); 
                end loop;
                Put_Line("");
                Put_Line("Transforms:");
            elsif Length(str) > 0 then
                t := transformationFromString(str);
                Put_Line("S: " & t.start_range'Image & " E: " & t.end_range'Image & " D: " & t.destination_start'Image);

                idx := 0;
                while idx < Natural(Length(srv)) loop
                    e := Element(srv, idx);
                    -- If e is in some way contained by t
                    if Long_Long_Integer'Max(e.start_range, t.start_range) <= Long_Long_Integer'Min(e.end_range, t.end_range) then
                        Put_Line("Hit! idx: " & idx'Image & " s: " & e.start_range'Image & " e: " & e.end_range'Image); 
                        -- If e has range before the transform
                        if e.start_range < t.start_range then
                            --Put_Line("Start before");
                            te.start_range := e.start_range;
                            te.end_range := t.start_range-1;
                            Append(t_values, te);
                        end if;

                        -- e IS contained
                        -- start is from e only if bigger than from t
                        te.start_range := (if e.start_range > t.start_range then e.start_range else t.start_range) -t.start_range +t.destination_start;
                        -- end is minimun of the two ends
                        te.end_range := Long_Long_Integer'Min(e.end_range, t.end_range) -t.start_range +t.destination_start;
                        Append(t_values, te);

                        -- If e has range after the transform
                        if e.end_range > t.end_range then
                            --Put_Line("End after");
                            te.start_range := t.end_range+1;
                            te.end_range := e.end_range;
                            Append(t_values, te);
                        end if;

                        -- Remove element
                        --Include(hit_v, idx);
                        Delete(srv, idx);
                        if idx > 0 then
                            idx := idx -1;
                        end if;
                    else
                        NULL;
                        --Put_Line("NOT! s: " & e.start_range'Image & " e: " & e.end_range'Image); 
                    end if;
                    idx := idx + 1;
                end loop;
            end if;
		end loop;

        Put_Line("");
        Put_Line("");
        Put_Line("Closing");
        Append(srv, t_values);
        compactVector(srv);
        e := First_Element(srv);
        for V of srv loop
            Put_Line("s: " & V.start_range'Image & " e: " & V.end_range'Image); 
            if v.start_range < e.start_range then
                e := v;
            end if;
        end loop;
        Put_Line("");
        Put_Line("Result s: " & e.start_range'Image & " e: " & e.end_range'Image); 
		Close (F);
	end Main;
end solutionpkp;
