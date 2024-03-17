with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use  Ada.Strings.Unbounded;
with Ada.Strings.Maps; use Ada.Strings.Maps;

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Maps;

with solutionpk; use solutionpk;

package body solutionpk is
	type StateMachine is (id, instr, values);
        type Part is record
            val_x: Integer;
            val_m: Integer;
            val_a: Integer;
            val_s: Integer;
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
        current_part : Part;
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
                        when values =>
                                case Element(trimmed_string,1) is
                                    when 'x' => current_part.val_x := Integer'Value(Slice(trimmed_string,3,Length(trimmed_string)));
                                    when 'm' => current_part.val_m := Integer'Value(Slice(trimmed_string,3,Length(trimmed_string)));
                                    when 'a' => current_part.val_a := Integer'Value(Slice(trimmed_string,3,Length(trimmed_string)));
                                    when 's' => current_part.val_s := Integer'Value(Slice(trimmed_string,3,Length(trimmed_string)));
                                    when others => raise Constraint_Error;
                                end case;
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
                if idx = 0 then
                    state := values;
                end if;
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

        function CalculateAcceptance(im: InstructionMap.Map; p: Part) return Integer is
            result : Integer := 0;
            op_res : Boolean;
            curr_val : Integer;
            current_step: Unbounded_String;
            current_ins_vec: InstructionVector.Vector;
        begin
            current_step := To_Unbounded_String("in");
            while current_step /= "R" and current_step /= "A" loop 
                Put_Line("Step: " & To_String(current_step));
                current_ins_vec := Element(im, current_step);
                for Ins of current_ins_vec loop
                    if Ins.direct then
                        current_step := Ins.destination;
                        exit;
                    else
                        curr_val := (case Ins.variable is
                            when 'x' => p.val_x,
                            when 'm' => p.val_m,
                            when 'a' => p.val_a,
                            when 's' => p.val_s,
                            when others => raise Constraint_Error);
                        case Ins.operator is
                            when '>' => op_res := curr_val > Ins.amount;
                            when '<' => op_res := curr_val < Ins.amount;
                            when others => raise Constraint_Error;
                        end case;
                        if op_res then
                            current_step := Ins.destination;
                            exit;
                        end if;
                    end if;
                end loop;
            end loop;
            if current_step = "A" then
                result := p.val_x + p.val_m + p.val_a + p.val_s;
            end if;
            return result;
        end;

	procedure Main is
		F         : File_Type;
		File_id : constant String := "input.txt";
		str       : Unbounded_String;
                cmds      : InstructionMap.Map;
                total     : Integer := 0;
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
                state := values;
		while not End_Of_File (F) loop
			-- Capture the line
			str := To_Unbounded_String(Get_Line (F));
			Put_Line(To_String(str));
			line_proc(str);
                        total := total + CalculateAcceptance(cmds, current_part);
		end loop;
                Put_Line("Total: " & total'image);
		Close (F);
        end Main;
end solutionpk;
