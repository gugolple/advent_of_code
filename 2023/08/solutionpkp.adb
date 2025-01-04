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


	type IterIndex is record
		Iterations: Integer;
		Index: Integer;
	end record;

	Function "=" (Left, Right: In IterIndex) return boolean
	is
	begin
		return Left.Iterations = Right.Iterations and Left.Index = Right.Index;
	end "=";
		

	package IterIndexVector is new
		Ada.Containers.Vectors
			(Index_Type => Natural,
			Element_Type => IterIndex
			);

	Function "=" (Left, Right: In IterIndexVector.Vector) return boolean
	is
		res : boolean := False;
	begin
		if Left.First_Index = Right.First_Index and Left.Last_Index = Right.Last_Index then
			res := True;
			for I in Left.First_Index .. Left.Last_Index loop
				if Left(I) /= Right(I) then
					res := False;
				end if;
			end loop;
		end if;
		return res;
	end "=";

	package IterIndexVectorVector is new
		Ada.Containers.Vectors
			(Index_Type => Natural,
			Element_Type => IterIndexVector.Vector
			);

	function NextStep(
		start: Unbounded_String;
		operation: Character)
		return Unbounded_String
	is
		next: Unbounded_String := To_Unbounded_String("");
	begin
		--Put_Line("Operation: " & operation);
		case operation is 
			when 'L' => next := NodeMap.Element(all_nodes, start).Left;
			when 'R' => next := NodeMap.Element(all_nodes, start).Right;
			when others => Put_Line("Failuroso");
		end case;
		return next;
	end NextStep;

	function IIV_Logic(
		iiv : in out IterIndexVector.Vector;
		iterations : Integer;
		current_idx : Integer
		) return Boolean
	is
		ii : IterIndex := (iterations, current_idx);
		res : boolean := False;
	begin
		Put_Line("Iterations: " & Iterations'Image & " idx: " & current_idx'Image);
		if not IterIndexVector.Contains(iiv, ii) then
			Put_Line("Appended!");
			iiv.append(ii);
			res := True;
		end if;
		return res;
	end IIV_Logic;


	function Vectorize(start: Unbounded_String) return IterIndexVector.Vector
	is
		iterAcum : IterIndexVector.Vector;
		continue : Boolean := True;
		current_node : Unbounded_String := start;
		total : Integer;
		current_idx : Natural;
	begin
		current_idx := 1;
		while continue loop
			total := 0;
			while Element(current_node, Length(current_node)) /= 'Z' loop
				--Put_Line("Current_node: " & To_String(current_node));
				while current_idx <= Length(command_string) loop
					current_node := NextStep(current_node, Element(command_string, current_idx));
					total := total + 1;
					--Put_Line("Current_node: " & To_String(current_node));
					if Element(current_node, Length(current_node)) = 'Z' then
						if not IIV_Logic(iterAcum, total, current_idx) then
							continue := False;
							exit;
						else
							current_idx := current_idx + 1;
							if current_idx > Length(command_string) then
								current_idx := 1;
							end if;
							total := 1;
							current_node := NextStep(current_node, Element(command_string, current_idx));
						end if;
					end if;
					current_idx := current_idx + 1;
				end loop;
				current_idx := 1;
			end loop;
			if continue then
				if not IIV_Logic(iterAcum, total, current_idx) then
					continue := False;
					exit;
				else
					current_idx := current_idx + 1;
					if current_idx > Length(command_string) then
						current_idx := 1;
					end if;
					total := 1;
					current_node := NextStep(current_node, Element(command_string, current_idx));
				end if;
			end if;
		end loop;
		return iterAcum;
	end Vectorize;


	-- Euclidean algorithm
	function gcd(l,x: Long_Integer) return Long_Integer
	is
		a : Long_Integer := l;
		b : Long_Integer := x;
		q, r : Long_Integer := 1;
	begin
		while r > 0 loop
			q := a/b;
			r := (a rem b);
			a := b;
			b := r;
		end loop;
		return q;
	end gcd;

	function lcm(a,b: Long_Integer) return Long_Integer
	is
		temp : Long_Integer;
	begin
		temp := a * b;
		return temp / gcd(a, b);
	end lcm;



	procedure Main is
		F         : File_Type;
		File_Name : constant String := "input.txt";
		str       : Unbounded_String;
		total : Integer := 0;
		continue : Boolean := True;
		current : Unbounded_String;
		next_step : Unbounded_String;
		all_locations : LocationVector.Vector;
		all_steps : IterIndexVectorVector.Vector;
		lcmv : Long_Integer := 0;
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

		Put_Line("Post procesing");
		for S of all_locations loop
			all_steps.append(Vectorize(S));
			Put_Line("");
			Put_Line("Iterations of: " & To_String(S));
			for II of IterIndexVectorVector.Last_element(all_steps) loop
				Put_Line("Iterations: " & II.Iterations'Image & " Idx: " & II.Index'Image);
			end loop;
		end loop;

		Put_Line("Final procesing");
		for AS of all_steps loop
			for II of AS loop
				Put_Line("Iterations: " & II.Iterations'Image & " Idx: " & II.Index'Image);
			end loop;
		end loop;
		
		Close (F);
	end Main;
end solutionpkp;
