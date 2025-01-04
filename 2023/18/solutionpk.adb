with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use  Ada.Strings.Unbounded;
with Ada.Strings.Maps; use Ada.Strings.Maps;

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;

with solutionpk; use solutionpk;

package body solutionpk is
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

	type Instruction is record
		D : Direction;
		Steps : Positive;
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
				temp_val.color := trimmed_string;
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

        function PropagateFillMatrix(mat: in out MapMarksVectorVector.Vector) return Integer is
            total : Integer := 0;
            valid : Boolean := True;
            change : Boolean := True;
            idx : Integer;
            mrow : MapMarksVector.Vector;
            max_row : Integer := Integer(Length(mat)-1);
            max_col : Integer := Integer(Length(mat(0))-1);
        begin
            while change loop
                change := False;
                for row in 0 .. Integer(max_row) loop
                    idx := 0;
                    idx := Integer(Find_Index(mat(row), C, idx));
                    while idx >= 0 loop
                        --Put_Line("Row: " & row'Image & " col: " & idx'Image);
                        mat(row)(idx) := X;
                        total := total + 1;
                        change := True;
                        if row > 0 and then mat(row-1)(idx) = L then
                            mat(row-1)(idx) := C;
                        end if;
                        if row < max_row and then mat(row+1)(idx) = L then
                            mat(row+1)(idx) := C;
                        end if;
                        if idx > 0 and then mat(row)(idx-1) = L then
                            mat(row)(idx-1) := C;
                        end if;
                        if idx < max_col and then mat(row)(idx+1) = L then
                            mat(row)(idx+1) := C;
                        end if;
                        if row = 0 or row = max_row then
                            valid := False;
                        end if;
                        if idx = 0 or idx = max_col then
                            valid := False;
                        end if;
                        idx := Integer(Find_Index(mat(row), C, idx+1));
                    end loop;
                end loop;
            end loop;
            if not valid then
                total := 0;
            end if;
            return total;
        end;

        function FillMatrix(mat: in out MapMarksVectorVector.Vector) return Integer is
            idx : Integer;
            total : Integer := 0;
            mrow : MapMarksVector.Vector;
        begin
            for row in 0 .. Integer(Length(mat)-1) loop
                idx := 0;
                idx := Integer(Find_Index(mat(row), L, idx));
                while idx >= 0 loop
                    mat(row)(idx) := C;
                    total := total + PropagateFillMatrix(mat);
                    idx := Integer(Find_Index(mat(row), L, idx+1));
                end loop;
            end loop;
            return total;
        end;

        procedure CreateMarkMap(v: InstructionVector.Vector; l,r,u,d : Integer) is
            cr: Integer := Abs(u);
            cc: Integer := Abs(l);
            matrix: MapMarksVectorVector.Vector;
            matrix_row : MapMarksVector.Vector;
            total: Integer := 0;
        begin
            for row In u..d loop
                matrix_row.clear;
                for col in l..r loop
                    matrix_row.append(MapMarks'Last);
                end loop;
                matrix.append(matrix_row);
            end loop;

            Put_Line("Initial!");
            for mrow of matrix loop
                for mcol of mrow loop
                    Put(mcol'Image & " ");
                end loop;
                Put_Line("");
            end loop;

            matrix(cc)(cr) := MapMarks'First;
            for Ins of v loop
                total := total + Ins.steps;
                case Ins.d is
                    when Up => 
                        for I in 1 .. Ins.steps loop
                            cr := cr -1;
                            matrix(cr)(cc) := MapMarks'First;
                        end loop;
                    when Down => 
                        for I in 1 .. Ins.steps loop
                            cr := cr +1;
                            matrix(cr)(cc) := MapMarks'First;
                        end loop;
                    when Left => 
                        for I in 1 .. Ins.steps loop
                            cc := cc -1;
                            matrix(cr)(cc) := MapMarks'First;
                        end loop;
                    when Right => 
                        for I in 1 .. Ins.steps loop
                            cc := cc +1;
                            matrix(cr)(cc) := MapMarks'First;
                        end loop;
                end case;
            end loop;

            Put_Line("Outside!");
            for mrow of matrix loop
                for mcol of mrow loop
                    Put(mcol'Image & " ");
                end loop;
                Put_Line("");
            end loop;

            total := total + FillMatrix(matrix);

            Put_Line("Final!");
            for mrow of matrix loop
                for mcol of mrow loop
                    Put(mcol'Image & " ");
                end loop;
                Put_Line("");
            end loop;

            Put_Line("Total: " & total'Image);
        end;


        procedure GetRanges(v: InstructionVector.Vector; l,r,u,d : out Integer) is
            cr, cc : Integer := 0;
        begin
            l := 0;
            u := 0;
            r := 0;
            d := 0;
            for Ins of v loop
                case Ins.d is
                    when Up => cr := cr - Ins.steps;
                    when Down => cr := cr + Ins.steps;
                    when Left => cc := cc - Ins.steps;
                    when Right => cc := cc + Ins.steps;
                end case;
                if cc < l then
                    l := cc;
                elsif cc > r then
                    r := cc;
                end if;
                if cr < u then
                    u := cr;
                elsif cr > d then
                    d := cr;
                end if;
            end loop;
        end;


	procedure Main is
		F         : File_Type;
		File_Name : constant String := "input.txt";
		str       : Unbounded_String;
                values : InstructionVector.Vector;
                l,r,u,d : Integer;
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
                GetRanges(values, l,r,u,d);
                Put_Line("Left: " & l'Image & " Right: " & r'Image & " Up: " & u'Image & " Down: " & d'Image);
                CreateMarkMap(values, l,r,u,d);
	end Main;
end solutionpk;
