with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use  Ada.Strings.Unbounded;
with Ada.Strings.Maps; use Ada.Strings.Maps;

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;

with solutionpk; use solutionpk;

package body solutionpk is
	package Unbounded_StringVector is new
		Ada.Containers.Vectors
			(Index_Type => Natural,
			Element_Type => Unbounded_String 
			);

	type DirectionLocation is record
		Row : Integer;
		Column : Integer;
		Dir : Direction;
		Count : Integer;
	end record;
	type DirectionLocation_Access is access DirectionLocation;

	Function "=" (Left, Right: In DirectionLocation) return boolean
	is
		res : boolean := False;
	begin
		return Left.row = Right.row and Left.Count = Right.Column;
	end "=";


	package DirectionLocationVector is new
		Ada.Containers.Vectors
			(Index_Type => Natural,
			Element_Type => DirectionLocation
			);

	grid : Unbounded_StringVector.Vector;

	state: StateMachine;

	procedure statemachine_proc(
		capture: in Unbounded_String
		)
	is
		current_number : Long_Integer := 0;
		trimmed_string : Unbounded_String;
	begin
		trimmed_string := Trim(capture, To_Set(' '), To_Set(":;, "));
		--Put_Line("Token: " & To_String(trimmed_string) & " State: " & state'Image);
		if trimmed_string = "" then
			return;
		end if;
		case state is
			when name =>
				grid.append(trimmed_string);
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


	function NextMovement(dl: DirectionLocation) return DirectionLocation
	is
		ndl : DirectionLocation;
		crow : Unbounded_String;
	begin
		-- Calculate next position
		Put_Line("Orig Row: " & dl.row'Image & " Column: " & dl.column'Image & " dir: " & dl.dir'Image);
		ndl.dir := Invalid;
		ndl.row := dl.row;
		ndl.column := dl.column;
		ndl.count := dl.count;
		case dl.dir is
			when North => 
				if dl.row > 0 then
					ndl.row := dl.row -1;
				end if;
			when South => 
				if dl.row < Integer(Unbounded_StringVector.Length(grid)) then
					ndl.row := dl.row +1;
				end if;
			when West => 
				if dl.column > 1 then
					ndl.column := dl.column -1;
				end if;
			when East => 
				if dl.column < Length(grid(0)) then
					ndl.column := dl.column +1;
				end if;
			when others =>
				ndl.dir := Invalid;
				return dl;
		end case;
		-- Set next movement
		crow := grid(ndl.row);
		ndl.count := dl.count + 1;
		case Element(crow, ndl.column) is
			when '|' => 
				if dl.dir = North then
					ndl.dir := North;
				elsif dl.dir = South then
					ndl.dir := South;
				end if;
			when '-' => 
				if dl.dir = West then
					ndl.dir := West;
				elsif dl.dir = East then
					ndl.dir := East;
				end if;
			when 'L' => 
				if dl.dir = South then
					ndl.dir := East;
				elsif dl.dir = West then
					ndl.dir := North;
				end if;
			when 'J' => 
				if dl.dir = South then
					ndl.dir := West;
				elsif dl.dir = East then
					ndl.dir := North;
				end if;
			when '7' => 
				if dl.dir = North then
					ndl.dir := West;
				elsif dl.dir = East then
					ndl.dir := South;
				end if;
			when 'F' => 
				if dl.dir = North then
					ndl.dir := East;
				elsif dl.dir = West then
					ndl.dir := South;
				end if;
			when '.' => ndl.dir := Invalid;
			when 'S' => Put_Line("Looped!");
			when others => Put_Line("Invalid value");
		end case;
		Put_Line("Dest Row: " & ndl.row'Image & " Column: " & ndl.column'Image & " dir: " & ndl.dir'Image);
		return ndl;
	end NextMovement;

	
	function StartVector(current_grid: Unbounded_StringVector.Vector) return DirectionLocationVector.Vector
	is
		next_steps : DirectionLocationVector.Vector;
		current_dl : DirectionLocation_Access;
		ndl : DirectionLocation;
		row : Integer;
		idx : Integer;
	begin
		row := 0;
		for line of current_grid loop
			idx := Index(Source => line, Pattern => "S", From => 1);
			if idx > 0 then
				Put_Line("Row: " & row'Image & " Column: " & idx'Image);
				exit;
			end if;
			row := row + 1;
		end loop;
		for dir in North .. West loop
			current_dl := new DirectionLocation;
			current_dl.dir := dir;
			current_dl.row := row;
			current_dl.column := idx;
			current_dl.count := 0;
			ndl := NextMovement(current_dl.all);
			if ndl.dir /= Invalid then
				current_dl.all := ndl;
				next_steps.append(current_dl.all);
			end if;
		end loop;
		return next_steps;
	end;

	procedure PrintGrid(dlv : DirectionLocationVector.Vector)
	is
		current_line : Unbounded_String;
		dl : DirectionLocation;
		nums : Unbounded_String;
		c : Character;
	begin
		Put_Line("Grid:");
		for I in Unbounded_StringVector.First_Index(grid) .. Unbounded_StringVector.Last_Index(grid) loop
			current_line := grid(I);
			for J in DirectionLocationVector.First_Index(dlv) .. DirectionLocationVector.Last_Index(dlv) loop
				dl := dlv(J);
				if dl.row = I then
					nums := To_Unbounded_String(J'image);
					c := Element(nums, Length(nums));
					Replace_Element(current_line, dl.column, c);
				end if;
			end loop;
			Unbounded_StringVector.Replace_Element(grid, I, current_line);
			Put_Line(To_String(current_line));
		end loop;
		Put_Line("");
	end PrintGrid;


	procedure WalkGrid is
		next_steps : DirectionLocationVector.Vector;
		next_dl : DirectionLocation;
		step : DirectionLocation;
		loop_dl : DirectionLocation;
		steps : Integer;
		idx : Integer;
		finish: Boolean := False;
	begin
		next_steps := StartVector(grid);
		Put_Line("");
		Put_Line("Input");
		while not finish loop
			idx := 0;
			for J in DirectionLocationVector.First_Index(next_steps) .. DirectionLocationVector.Last_Index(next_steps) loop
				step := next_steps(J);
				Put_Line("");
				Put_Line("Idx " & idx'Image);
				Put_Line("Step is Dir: " & step.dir'Image & " Row: " & step.row'Image & " Column: " & step.column'Image);
				next_dl := NextMovement(step);
				for K in DirectionLocationVector.First_Index(next_steps) .. DirectionLocationVector.Last_Index(next_steps) loop 
					if K /= J then
						loop_dl := next_steps(K);
						if loop_dl.row = next_dl.row and loop_dl.column = next_dl.column then
							if next_dl.count = loop_dl.count then
								steps := next_dl.count;
							else
								steps := loop_dl.count;
							end if;
							finish := True;
							exit;
						end if;
					end if;
				end loop;
				next_steps(J) := next_dl;
				if finish then
					exit;
				end if;
				idx := idx + 1;
			end loop;
			--PrintGrid(next_steps);
		end loop;
		Put_Line("Solution!");
		PrintGrid(next_steps);
		Put_Line("Result: " & steps'Image);
	end WalkGrid;


	procedure Main is
		F         : File_Type;
		File_Name : constant String := "input.txt";
		str       : Unbounded_String;
	begin
		Open (F, In_File, File_Name);
		while not End_Of_File (F) loop
			-- Capture the line
			str := To_Unbounded_String(Get_Line (F));
			Put_Line(To_String(str));
			line_proc(str);
		end loop;
		WalkGrid;
		Close (F);
	end Main;
end solutionpk;
