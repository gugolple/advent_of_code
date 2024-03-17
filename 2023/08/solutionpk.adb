with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use  Ada.Strings.Unbounded;
with Ada.Strings.Maps; use Ada.Strings.Maps;

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Maps;

with solutionpk; use solutionpk;

package body solutionpk is
	type Node is record
		Self: Unbounded_String;
		Left: Unbounded_String;
		Right: Unbounded_String;
	end record;

	Function "<" (Left, Right: In Node) return boolean
	is
		res : boolean := false;
	begin
		if Left.Self < Right.Self then
			res := true;
		end if;
		return res;
	end "<";

	Function "=" (Left, Right: In Node) return boolean
	is
		res : boolean := false;
	begin
		if Left.Self = Right.Self then
			res := true;
		end if;
		return res;
	end "=";

	package NodeMap is 
		new Ada.Containers.Ordered_Maps(
			Key_Type => Unbounded_String,
			Element_Type => Node 
			);

	state: StateMachine;
	
	command_string : Unbounded_String;
	all_nodes : NodeMap.Map;
	current_node : Node;

	procedure statemachine_proc(
		capture: in Unbounded_String
		)
	is
		current_number : Long_Integer := 0;
		trimmed_string : Unbounded_String;
	begin
		trimmed_string := Trim(capture, To_Set(" ("), To_Set("():;, "));
		--Put_Line("Token: " & To_String(trimmed_string) & " State: " & state'Image);
		if trimmed_string = "" or trimmed_string = "=" then
			state := node_destination_left;
			return;
		end if;
		case state is
			when command =>
				command_string := trimmed_string;
				state := node_name;
			when node_name =>
				current_node.Self := trimmed_string;
			when node_destination_left =>
				current_node.Left := trimmed_string;
				state := node_destination_right;
			when node_destination_right =>
				current_node.Right := trimmed_string;
				all_nodes.insert(current_node.self, current_node);
		end case;
	end statemachine_proc;


	procedure line_proc
		(capture: in Unbounded_String)
	is
		idx,idxl  : Natural := 1;
	begin
		-- Capture destination data
		idx := Index(
			Source => capture,
			Pattern => " ",
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
				Pattern => " ",
				From => idx+1
				);
		end loop;
		statemachine_proc(To_Unbounded_String(Slice(capture,idxl,Length(capture))));
	end line_proc;
	procedure Main is
		F         : File_Type;
		File_Name : constant String := "input.txt";
		str       : Unbounded_String;
		current_node : Unbounded_String := To_Unbounded_String("AAA");
		total : Integer := 0;
	begin
		Open (F, In_File, File_Name);
		state := command;
		while not End_Of_File (F) loop
			-- Capture the line
			str := To_Unbounded_String(Get_Line (F));
			Put_Line(To_String(str));
			line_proc(str);
			state := node_name;
		end loop;

		Put_Line("");
		Put_Line("Our data");
		Put_Line(To_String(command_string));
		for N of all_nodes loop
			Put_Line("Self: " & To_String(N.self) & " Left: " & To_String(N.Left) & " Right " & To_String(N.Right));
		end loop;
		
		while current_node /= "ZZZ" loop
			Put_Line("Current_node: " & To_String(current_node));
			for I in 1 .. Length(command_string) loop
				case Element(command_string, I) is 
					when 'L' => current_node := NodeMap.Element(all_nodes, current_node).Left;
					when 'R' => current_node := NodeMap.Element(all_nodes, current_node).Right;
					when others => Put_Line("Failuroso");
				end case;
				total := total + 1;
				Put_Line("Current_node: " & To_String(current_node));
				if current_node = "ZZZ" then
					Put_Line("Found!");
					exit;
				end if;
			end loop;
		end loop;

		Put_Line("Current_node: " & To_String(current_node));
		Put_Line("Total: " & total'Image);

		Close (F);
	end Main;
end solutionpk;
