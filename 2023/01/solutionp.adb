with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Bounded; use Ada.Strings.Bounded;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Maps;  use Ada.Strings.Maps;
with Ada.Strings.Unbounded; use  Ada.Strings.Unbounded;
with Ada.Characters.Handling; use  Ada.Characters.Handling;

procedure solutionp is
        F         : File_Type;
        File_Name : constant String := "input.txt";
        package BStr is new Generic_Bounded_Length(Max => 10);
        use BStr;
        type StrAr is array (1 .. 9) of Bounded_String;
        MatchesD  : constant StrAr := (
                To_Bounded_String("1"),
                To_Bounded_String("2"),
                To_Bounded_String("3"),
                To_Bounded_String("4"),
                To_Bounded_String("5"),
                To_Bounded_String("6"),
                To_Bounded_String("7"),
                To_Bounded_String("8"),
                To_Bounded_String("9")
		);
        MatchesN  : constant StrAr := (
                To_Bounded_String("one"),
                To_Bounded_String("two"),
                To_Bounded_String("three"),
                To_Bounded_String("four"),
                To_Bounded_String("five"),
                To_Bounded_String("six"),
                To_Bounded_String("seven"),
                To_Bounded_String("eight"),
                To_Bounded_String("nine")
                );
        str_b     : Bounded_String;
        str       : Unbounded_String;
        c         : Character;
        first,last: Integer;
        total     : Integer := 0;
        idx       : natural := 1;
        idxf,idxl : natural := 1;
	loopidx   : Integer;

        Whitespace : constant Character_Set :=
                To_Set (' ');
        begin
                Open (F, In_File, File_Name);
                while not End_Of_File (F) loop
                        str := To_Unbounded_String(Get_Line (F));
			idxf := Length(str)+1;
                        first := 0;
			idxl := 0;
                        last := 0;
                        --Put_Line(To_String(str));
                        for loopidx in MatchesD'range loop
				str_b := MatchesD(loopidx);
                                --Put_Line(To_String(str_b));
				idx := Index(
                                        Source => str,
					Pattern => To_String(str_b),
					From => 1,
					Going => Forward
                                        );
				if idx > 0 and idx < idxf then
					idxf := idx;
					first := loopidx;
				end if;
				idx := Index(
                                        Source => str,
					Pattern => To_String(str_b),
					From => Length(str),
					Going => Backward
                                        );
				if idx > 0 and idx > idxl then
					idxl := idx;
					last := loopidx;
				end if;
				Put_Line("Matched: " & idxf'image & ", " & idxl'image);
                        end loop;
                        for loopidx in MatchesN'range loop
				str_b := MatchesN(loopidx);
                                --Put_Line(To_String(str_b));
				idx := Index(
                                        Source => str,
					Pattern => To_String(str_b),
					From => 1,
					Going => Forward
                                        );
				if idx > 0 and idx < idxf then
					idxf := idx;
					first := loopidx;
				end if;
				idx := Index(
                                        Source => str,
					Pattern => To_String(str_b),
					From => Length(str),
					Going => Backward
                                        );
				if idx > 0 and idx > idxl then
					idxl := idx;
					last := loopidx;
				end if;
				Put_Line("Matched: " & idxf'image & ", " & idxl'image);
                        end loop;
                        Put_Line("");
                        total := total + first * 10 + last;
			first := first * 10 + last;
                        Put_Line("Line: " & To_string(str));
                        Put_Line("Matched value:" & first'Image);
                        Put_Line("Current total:" & total'Image);
			Put_Line("");
                        -- Put_Line (To_String(str));
                end loop;
                Close (F);
end solutionp;
