with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use  Ada.Strings.Unbounded;
with Ada.Strings.Maps; use Ada.Strings.Maps;

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Maps;

with solutionpkp; use solutionpkp;

package body solutionpkp is
	type StateMachine is (id, instr);
        type MyRange is record
            b: Integer;
            e: Integer;
        end record;
        type AllRanges is record
            x: MyRange;
            m: MyRange;
            a: MyRange;
            s: MyRange;
        end record;
        type Instruction is record
            direct: Boolean;
            variable: Character;
            amount: Integer;
            operator: Character;
            destination: Unbounded_String;
        end record;
	package InstructionVector is new
		Ada.Containers.Vectors
			(Index_Type => Natural,
			Element_Type => Instruction 
			);
	use InstructionVector;

	package InstructionMap is 
		new Ada.Containers.Ordered_Maps(
			Key_Type => Unbounded_String,
			Element_Type => InstructionVector.Vector
			);
	use InstructionMap;

	state: StateMachine;
	
        current_identifier: Unbounded_String;
	current_values : InstructionVector.Vector;
	procedure statemachine_proc(
		capture: in Unbounded_String
		)
	is
		lidx, idx : Integer := 0;
		trimmed_string : Unbounded_String;
                cur_instruction : Instruction;
	begin
		trimmed_string := Trim(capture, To_Set(" {,"), To_Set("{}:;, "));
		--Put_Line("Token: " & To_String(trimmed_string) & " State: " & state'Image);
		if trimmed_string = "" then
			return;
		end if;
		case state is
			when id =>
				current_identifier := trimmed_string;
				state := instr;
			when instr =>
                                idx := Index(trimmed_string, To_Set("<>"), 1);
                                if idx > 0 then
                                    cur_instruction.direct := False;
                                    cur_instruction.variable := Element(trimmed_string, idx-1);
                                    cur_instruction.operator := Element(trimmed_string, idx);
                                    lidx := idx+1;
                                    idx := Index(trimmed_string, To_Set(":"), lidx);
                                    cur_instruction.amount := Integer'Value(Slice(trimmed_string, lidx, idx-1));
                                    cur_instruction.destination := To_Unbounded_String(Slice(trimmed_string, idx+1, Length(trimmed_string)));
                                else
                                    cur_instruction.direct := True;
                                    cur_instruction.destination := trimmed_string;
                                end if;
                                Put_Line("Dir: " & cur_instruction.direct'Image & " Destination: " & To_String(cur_instruction.Destination) & " Var: " & cur_instruction.variable'Image & " Amo: " & cur_instruction.amount'Image & " Oper: " & cur_instruction.operator'Image);
                                Append(current_values, cur_instruction);
		end case;
	end statemachine_proc;


	procedure line_proc
		(capture: in Unbounded_String)
	is
		idx,idxl  : Natural := 1;
		Search_Set : constant Character_Set := To_Set(" ,{}");
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



        function countOfRanges(cmds: InstructionMap.Map; iran: AllRanges; identifier: Unbounded_String := To_Unbounded_String("in")) return Long_Integer is
            total : Long_Integer := 0;
            high, low : Integer;
            ran : AllRanges;
            false_range, true_range : MyRange;
            temp_ranges: AllRanges;
        begin
            ran.x.b := iran.x.b;
            ran.x.e := iran.x.e;
            ran.m.b := iran.m.b;
            ran.m.e := iran.m.e;
            ran.a.b := iran.a.b;
            ran.a.e := iran.a.e;
            ran.s.b := iran.s.b;
            ran.s.e := iran.s.e;
            if identifier = "R" then
                Put_Line("Returned!");
                return 0;
            end if;
            if identifier = "A" then
                Put_Line("Returned!");
                return Long_Integer(Long_Integer(ran.x.e-ran.x.b+1) * Long_Integer(ran.m.e-ran.m.b+1) * Long_Integer(ran.a.e-ran.a.b+1) * Long_Integer(ran.s.e-ran.s.b+1));
            end if;

            for Ins of Element(cmds, identifier) loop
                Put_Line("Dir: " & ins.direct'Image & " Destination: " & To_String(ins.Destination) & " Var: " & ins.variable'Image & " Amo: " & ins.amount'Image & " Oper: " & ins.operator'Image);
                if ins.direct then
                    total := total + countOfRanges(cmds, ran, ins.destination);
                else
                    case ins.variable is
                        when 'x' =>
                            low := ran.x.b;
                            high := ran.x.e;
                        when 'm' =>
                            low := ran.m.b;
                            high := ran.m.e;
                        when 'a' =>
                            low := ran.a.b;
                            high := ran.a.e;
                        when 's' =>
                            low := ran.s.b;
                            high := ran.s.e;
                        when others => raise Constraint_Error;
                    end case;
                    case ins.operator is
                        when '<' =>
                            true_range.b := low;
                            true_range.e := ins.amount-1;
                            false_range.b := ins.amount;
                            false_range.e := high;
                        when '>' =>
                            true_range.b := ins.amount + 1;
                            true_range.e := high;
                            false_range.b := low;
                            false_range.e := ins.amount;
                        when others => raise Constraint_Error;
                    end case;
                    if true_range.b <= true_range.e then
                        temp_ranges.x.b := ran.x.b;
                        temp_ranges.x.e := ran.x.e;
                        temp_ranges.m.b := ran.m.b;
                        temp_ranges.m.e := ran.m.e;
                        temp_ranges.a.b := ran.a.b;
                        temp_ranges.a.e := ran.a.e;
                        temp_ranges.s.b := ran.s.b;
                        temp_ranges.s.e := ran.s.e;
                        case ins.variable is
                            when 'x' =>
                                temp_ranges.x.b := true_range.b;
                                temp_ranges.x.e := true_range.e;
                            when 'm' =>
                                temp_ranges.m.b := true_range.b;
                                temp_ranges.m.e := true_range.e;
                            when 'a' =>
                                temp_ranges.a.b := true_range.b;
                                temp_ranges.a.e := true_range.e;
                            when 's' =>
                                temp_ranges.s.b := true_range.b;
                                temp_ranges.s.e := true_range.e;
                            when others => raise Constraint_Error;
                        end case;
                        total := total + countOfRanges(cmds, temp_ranges, ins.destination);
                    end if;
                    if false_range.b <= false_range.e then
                        case ins.variable is
                            when 'x' =>
                                ran.x.b := false_range.b;
                                ran.x.e := false_range.e;
                            when 'm' =>
                                ran.m.b := false_range.b;
                                ran.m.e := false_range.e;
                            when 'a' =>
                                ran.a.b := false_range.b;
                                ran.a.e := false_range.e;
                            when 's' =>
                                ran.s.b := false_range.b;
                                ran.s.e := false_range.e;
                            when others => raise Constraint_Error;
                        end case;
                    else
                        exit;
                    end if;
                end if;
            end loop;
            return total;
        end;


	procedure Main is
		F         : File_Type;
		File_id : constant String := "input.txt";
		str       : Unbounded_String;
                cmds      : InstructionMap.Map;
                total     : Long_Integer := 0;
                ranges    : AllRanges;
	begin
		Open (F, In_File, File_id);
		while not End_Of_File (F) loop
			-- Capture the line
                        current_values.clear;
                        state := id;
			str := To_Unbounded_String(Get_Line (F));
                        if str = "" then 
                            Put_Line("Exit!");
                            exit;
                        end if;
			Put_Line(To_String(str));
			line_proc(str);
                        Put_Line("Key: " & To_String(current_identifier));
                        cmds.Insert(current_identifier, current_values);
		end loop;
		Close (F);
                ranges.x.b := 1;
                ranges.m.b := 1;
                ranges.a.b := 1;
                ranges.s.b := 1;
                ranges.x.e := 4000;
                ranges.m.e := 4000;
                ranges.a.e := 4000;
                ranges.s.e := 4000;
                total := countOfRanges(cmds, ranges);
                Put_Line("Total: " & total'image);
        end Main;
end solutionpkp;
