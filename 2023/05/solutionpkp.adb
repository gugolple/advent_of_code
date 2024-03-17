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
        processed: boolean;
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
        res.end_range := Long_Long_Integer'Value(Slice(str,idxl,Length(str)));

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

    -- Single block
    package TransfomrationVector is new
    Ada.Containers.Vectors
        (Index_Type => Natural,
        Element_Type => Transfomration 
        );
    use TransfomrationVector;

    package TransfomrationVectorVector is new
    Ada.Containers.Vectors
        (Index_Type => Natural,
        Element_Type => TransfomrationVector.Vector
        );
    use TransfomrationVectorVector;

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
            sr.processed := False;
            sr.start_range := Element(v, idx);
            sr.end_range := sr.start_range + Element(v, idx+1);
            idx := idx + 2;
            Append(srv, sr);
        end loop;
        return srv;
    end;

    package SRSorter is new SeedRangeVector.Generic_Sorting;
    procedure Main is
        F         : File_Type := Standard_Input;
        str       : Unbounded_String;
        t         : Transfomration;
        idx       : Integer;
        range_del : Long_Long_Integer;
        e         : SeedRange;
        te        : SeedRange;
        srv       : SeedRangeVector.Vector;
        srv_nc    : SeedRangeVector.Vector;
        blocks    : TransfomrationVectorVector.Vector;
        block     : TransfomrationVector.Vector;
        hitted    : boolean;
        os, oe    : Long_Long_Integer;
    begin
        -- Read first line, the input
        str := To_Unbounded_String(Get_Line (F));
        Put_Line(To_String(str));
        line_proc(str);

        srv := LLI_to_SR(values);

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
                Put_Line("Map line!");
                if Length(block) > 0 then
                    Append(blocks, block);
                end if;
                block.clear;
            elsif Length(str) > 0 then
                t := transformationFromString(str);
                range_del := t.destination_start - t.start_range;
                put_line("s: " & t.start_range'image & " e: " & t.end_range'image & " d: " & t.destination_start'image & " delta: " & range_del'image);
                Append(block, t);
            end if;
        end loop;
        Append(blocks, block);

        Put_Line("Actual:!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!");
        -- Now with all the input in a single place!
        for blk of blocks loop
            srv.append(srv_nc);
            srv_nc.clear;
            SRSorter.Sort(srv);
            Put_Line("Seeds");
            for e of srv loop
                Put_Line("(" & e.start_range'Image & "," & e.end_range'Image & ")");
            end loop;
            Put_Line("");
            idx := 0;
            while idx < Integer(Length(srv)) loop
                e := Element(srv, idx);
                hitted := False;
                --Put_Line("Row! s: " & e.start_range'Image & " e: " & e.end_range'Image);
                for transf of blk loop
                    os := Long_Long_Integer'Max(e.start_range, transf.start_range);
                    oe := Long_Long_Integer'Min(e.end_range, transf.start_range + transf.end_range);
                    --Put_Line(os'Image & "," & oe'Image);
                    if os < oe then
                        te.start_range := os - transf.start_range + transf.destination_start;
                        te.end_range := oe - transf.start_range + transf.destination_start;
                        --Put_Line("Hit C! s: " & te.start_range'Image & " e: " & te.end_range'Image);
                        srv_nc.append(te);

                        -- If split KEEP PROCESSING FOR CURRENT LOOP!
                        if os > e.start_range then
                            te.start_range := e.start_range;
                            te.end_range := os;
                            --Put_Line("Hit L! s: " & te.start_range'Image & " e: " & te.end_range'Image);
                            srv.append(te);
                        end if;

                        if e.end_range > oe then
                            te.start_range := oe;
                            te.end_range := e.end_range;
                            --Put_Line("Hit R! s: " & te.start_range'Image & " e: " & te.end_range'Image);
                            srv.append(te);
                        end if;
                        hitted := True;
                        exit;
                    end if;
                end loop;
                if not hitted then
                    --Put_Line("NHit!");
                    srv_nc.append(e);
                end if;
                idx := idx + 1;
            end loop;
            srv.clear;
        end loop;
        srv.append(srv_nc);
        srv_nc.clear;
        SRSorter.Sort(srv);

        Put_Line("Seeds");
        e := First_Element(srv);
        for V of srv loop
            Put_Line("(" & V.start_range'Image & "," & V.end_range'Image & ")");
            if v.start_range < e.start_range then
                e := v;
            end if;
        end loop;
        Put_Line("");
        Put_Line("Result s: " & e.start_range'Image & " e: " & e.end_range'Image); 
        Close (F);
    end Main;
end solutionpkp;
