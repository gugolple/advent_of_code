with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use  Ada.Strings.Unbounded;
with Ada.Strings.Maps; use Ada.Strings.Maps;

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;

with solutionpkp; use solutionpkp;

package body solutionpkp is
	type MapMarks is (H, C, X, L); -- Hole, Current, Marked, Land
	package MapMarksVector is new
		Ada.Containers.Vectors
			(Index_Type => Natural,
			Element_Type => MapMarks 
			);
	use MapMarksVector;
	package MapMarksVectorVector is new
		Ada.Containers.Vectors
			(Index_Type => Natural,
			Element_Type => MapMarksVector.Vector
			);
	use MapMarksVectorVector;
	type Direction is (Up, Left, Down, Right);
	type StateMachine is (dir, steps, color);
	state: StateMachine;

	type Location is record
                Row: Long_Integer;
                Col: Long_Integer;
	end record;
	package LocationVector is new
		Ada.Containers.Vectors
			(Index_Type => Natural,
			Element_Type => Location 
			);
	use LocationVector;

	type Instruction is record
		D : Direction;
		Steps : Natural;
                Color: Unbounded_String;
	end record;

	package InstructionVector is new
		Ada.Containers.Vectors
			(Index_Type => Natural,
			Element_Type => Instruction 
			);
	use InstructionVector;

	process_line : Unbounded_String;
        temp_val : Instruction;
	procedure statemachine_proc(
		capture: in Unbounded_String
		)
	is
		current_number : Integer := 0;
		trimmed_string : Unbounded_String;
	begin
		trimmed_string := Trim(capture, To_Set(" #("), To_Set("):;, "));
		--Put_Line("Token: " & To_String(trimmed_string) & " State: " & state'Image);
		if trimmed_string = "" then
			return;
		end if;
		case state is
			when dir =>
                                temp_val.d := 
                                (case Element(trimmed_string,1) is
                                    when 'U' => Up,
                                    when 'L' => Left,
                                    when 'D' => Down,
                                    when 'R' => Right,
                                    when others => raise Constraint_Error);
				state := steps;
			when steps =>
				temp_val.steps := Integer'Value(To_String(trimmed_string));
				state := color;
			when color =>
				temp_val.steps := 0;
                                for C of Slice(trimmed_string,1,5) loop
                                    temp_val.steps := temp_val.steps * 16 + (
                                        case C is
                                            when '0' => 0,
                                            when '1' => 1,
                                            when '2' => 2,
                                            when '3' => 3,
                                            when '4' => 4,
                                            when '5' => 5,
                                            when '6' => 6,
                                            when '7' => 7,
                                            when '8' => 8,
                                            when '9' => 9,
                                            when 'a' => 10,
                                            when 'b' => 11,
                                            when 'c' => 12,
                                            when 'd' => 13,
                                            when 'e' => 14,
                                            when 'f' => 15,
                                            when others => raise Constraint_Error
                                        );
                                end loop;
                                temp_val.d := 
                                (case Element(trimmed_string,6) is
                                    when '3' => Up,
                                    when '2' => Left,
                                    when '1' => Down,
                                    when '0' => Right,
                                    when others => raise Constraint_Error);
		end case;
	end statemachine_proc;


	procedure line_proc
		(capture: in Unbounded_String)
	is
		idx,idxl  : Natural := 1;
		Search_Set : constant Character_Set := To_Set(" ");
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
				From => idxl
				);
		end loop;
		statemachine_proc(To_Unbounded_String(Slice(capture,idxl,Length(capture))));
	end line_proc;

        procedure ShoelaceAndPicks(v: in out InstructionVector.Vector) is
            total: Long_Integer := 0;
            area: Long_Integer := 0;
            boundries : Long_Integer := 0;
            interior : Long_Integer := 0;
            cl : Location;
            lv : LocationVector.Vector;
            lvl : Integer;
        begin
            cl.row := 0;
            cl.col := 0;
            lv.append(cl);
            for Ins of v loop
                case Ins.d is
                    when Up => cl.row := cl.row - Long_Integer(Ins.steps);
                    when Down => cl.row := cl.row + Long_Integer(Ins.steps);
                    when Left => cl.col := cl.col - Long_Integer(Ins.steps);
                    when Right => cl.col := cl.col + Long_Integer(Ins.steps);
                end case;
                boundries := boundries + Long_Integer(Ins.steps);
                lv.append(cl);
            end loop;
            Put_Line("Boundries: " & boundries'Image);

            lvl := Integer(Length(lv)-1);
            for I in 0 .. lvl loop
                area := area + lv(I).row * (lv((I-1) mod lvl).col - lv((I+1) mod lvl).col);
            end loop;
            area := abs(area)/2;
            Put_Line("Area: " & area'Image);

            interior := area - boundries / 2 + 1;
            Put_Line("Interior: " & interior'Image);
            total := interior + boundries;
            Put_Line("Total: " & total'Image);
        end;


	procedure Main is
		F         : File_Type;
		File_Name : constant String := "input.txt";
		str       : Unbounded_String;
                values : InstructionVector.Vector;
	begin
		Open (F, In_File, File_Name);
		while not End_Of_File (F) loop
			-- Capture the line
			state := dir;
			str := To_Unbounded_String(Get_Line (F));
			Put_Line(To_String(str));
			line_proc(str);
                        values.append(temp_val);
		end loop;
		Close (F);

                Put_Line("");
                Put_Line("Input: ");
                for I of values loop
                        Put_Line("Dir: " & I.d'Image & " Steps: " & I.steps'Image & " color: " & To_String(I.color));
                end loop;
                Put_Line("");
                ShoelaceAndPicks(values);
	end Main;
end solutionpkp;
