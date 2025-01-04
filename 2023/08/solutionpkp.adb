with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use  Ada.Strings.Unbounded;
with Ada.Strings.Maps; use Ada.Strings.Maps;

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;

with solutionpkp; use solutionpkp;

package body solutionpkp is
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

	package LocationVector is new
		Ada.Containers.Vectors
			(Index_Type => Natural,
			Element_Type => Unbounded_String
			);

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
		total : Integer := 0;
		all_locations : LocationVector.Vector;
		continue : Boolean := True;
		current : Unbounded_String;
		next_step : Unbounded_String;
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
			if Element(N.Self, Length(N.Self)) = 'A' then
				Put_Line("Start!");
				LocationVector.append(all_locations, N.Self);
			end if;
		end loop;
		
		while continue loop
			for I in 1 .. Length(command_string) loop
				Put_Line("new loop!");
				continue := False;
				for J in all_locations.first_index .. all_locations.last_index loop
					current := all_locations(J);
					case Element(command_string, I) is 
						when 'L' => next_step := NodeMap.Element(all_nodes, current).Left;
						when 'R' => next_step := NodeMap.Element(all_nodes, current).Right;
						when others => Put_Line("Failuroso");
					end case;
					Put_Line("Origin: " & To_String(current) & " Destination: " & To_String(next_step));
					all_locations(J) := next_step;
					if Element(next_step, Length(next_step)) /= 'Z' then
						continue := True;
					end if;
				end loop;
				total := total + 1;
				if not continue then
					Put_Line("All Found!");
					exit;
				end if;
			end loop;
		end loop;

		Put_Line("Total: " & total'Image);
		Close (F);
	end Main;
end solutionpkp;
