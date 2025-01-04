with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use  Ada.Strings.Unbounded;
with Ada.Strings.Maps; use Ada.Strings.Maps;

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Sets;

with solutionpkp; use solutionpkp;

package body solutionpkp is
	valid_positions : Character_Set := To_Set("|-LJ7F. ");
	count_positions : Character_Set := To_Set("|-LJ7F.");
	MOV_STEP : constant Integer := 2;
	
	package Unbounded_StringVector is new
		Ada.Containers.Vectors
			(Index_Type => Natural,
			Element_Type => Unbounded_String 
			);

	type Location is record
		Row : Integer;
		Col : Integer;
	end record;
	type Location_Access is access Location;

	package LocationVector is new
		Ada.Containers.Vectors
			(Index_Type => Natural,
			Element_Type => Location
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
		new_string : Unbounded_String;
	begin
		trimmed_string := Trim(capture, To_Set(' '), To_Set(":;, "));
		--Put_Line("Token: " & To_String(trimmed_string) & " State: " & state'Image);
		if trimmed_string = "" then
			return;
		end if;
		case state is
			when name =>
				for I in reverse 1 .. Length(trimmed_string) loop
					Insert(trimmed_string, I, " ");
				end loop;	
				if Unbounded_StringVector.Length(grid) > 0 then
					for I in 1 .. Length(trimmed_string) loop
						Insert(new_string, I, " ");
					end loop;
					grid.append(new_string);
				end if;
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

	procedure MarkLocation(dl: Location; char: Character)
	is
		current_line : Unbounded_String;
	begin
		if not Is_In(Element(grid(dl.row), dl.col), To_set('B')) then
			Replace_Element(grid(dl.row), dl.col, char);
		end if;
	end;

	procedure MarkPosition(dl: DirectionLocation; char: Character)
	is
		current_line : Unbounded_String;
	begin
		Replace_Element(grid(dl.row), dl.column, char);
	end;

	procedure MarkPositionDLV(dlv: DirectionLocationVector.Vector)
	is
		current_line : Unbounded_String;
		dl : DirectionLocation;
		nums : Unbounded_String;
		c : Character;
	begin
		for J in DirectionLocationVector.First_Index(dlv) .. DirectionLocationVector.Last_Index(dlv) loop
			nums := To_Unbounded_String(J'image);
			c := Element(nums, Length(nums));
			MarkPosition(dl, c);
		end loop;
	end;


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
				if dl.row >= MOV_STEP then
					for I in 0..MOV_STEP-1 loop
						ndl.row := dl.row -I;
						MarkPosition(ndl, 'B');
					end loop;
					ndl.row := dl.row - MOV_STEP;
				end if;
			when South => 
				if dl.row <= Integer(Unbounded_StringVector.Length(grid)) -MOV_STEP then
					for I in 0..MOV_STEP-1 loop
						ndl.row := dl.row +I;
						MarkPosition(ndl, 'B');
					end loop;
					ndl.row := dl.row + MOV_STEP;
				end if;
			when West => 
				-- Idx starts at 1
				if dl.column > MOV_STEP then
					for I in 0..MOV_STEP-1 loop
						ndl.column := dl.column -I;
						MarkPosition(ndl, 'B');
					end loop;
					ndl.column := dl.column - MOV_STEP;
				end if;
			when East => 
				if dl.column <= (Length(grid(0)) -MOV_STEP) then
					for I in 0..MOV_STEP-1 loop
						ndl.column := dl.column +I;
						MarkPosition(ndl, 'B');
					end loop;
					ndl.column := dl.column + MOV_STEP;
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

	procedure PrintGrid
	is
		current_line : Unbounded_String;
	begin
		Put_Line("Grid:");
		for I in Unbounded_StringVector.First_Index(grid) .. Unbounded_StringVector.Last_Index(grid) loop
			current_line := grid(I);
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
			Put_Line("Loop?");
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
							MarkPosition(next_dl, 'B');
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
		PrintGrid;
		Put_Line("Result: " & steps'Image);
	end WalkGrid;

	function BordersWall(l: location) return boolean
	is
		res: boolean := False;
	begin
		if l.row = 0 then
			res := True;
		end if;
		if l.row = Integer(Unbounded_StringVector.Length(grid))-1 then
			res := True;
		end if;
		if l.col = 1 then
			res := True;
		end if;
		if l.col = Length(grid(0)) then
			res := True;
		end if;
		return res;
	end BordersWall;


	function CanMarkAround(l: location) return Boolean 
	is
		ml : location;
		res : Boolean := False;
	begin
		ml.col := l.col;
		if l.row > 0 then
			ml.row := l.row -1;
			if Is_In(Element(grid(ml.row), ml.col), valid_positions) then
				res := True;
			end if;
		end if;
		if l.row < Integer(Unbounded_StringVector.Length(grid))-1 then
			ml.row := l.row +1;
			if Is_In(Element(grid(ml.row), ml.col), valid_positions) then
				res := True;
			end if;
		end if;
		ml.row := l.row;
		if l.col > 1 then
			ml.col := l.col -1;
			if Is_In(Element(grid(ml.row), ml.col), valid_positions) then
				res := True;
			end if;
		end if;
		if l.col < Length(grid(0)) then
			ml.col := l.col +1;
			if Is_In(Element(grid(ml.row), ml.col), valid_positions) then
				res := True;
			end if;
		end if;
		return res;
	end CanMarkAround;


	function CountMarkAround(l: location) return Integer
	is
		ml : location;
		total : Integer := 0;
	begin
		ml.col := l.col;
		if l.row > 0 then
			ml.row := l.row -1;
			if Is_In(Element(grid(ml.row), ml.col), count_positions) then
				total := total + 1;
			end if;
			MarkLocation(ml,'X');
		end if;
		if l.row < Integer(Unbounded_StringVector.Length(grid))-1 then
			ml.row := l.row +1;
			if Is_In(Element(grid(ml.row), ml.col), count_positions) then
				total := total + 1;
			end if;
			MarkLocation(ml,'X');
		end if;
		ml.row := l.row;
		if l.col > 1 then
			ml.col := l.col -1;
			if Is_In(Element(grid(ml.row), ml.col), count_positions) then
				total := total + 1;
			end if;
			MarkLocation(ml,'X');
		end if;
		if l.col < Length(grid(0)) then
			ml.col := l.col +1;
			if Is_In(Element(grid(ml.row), ml.col), count_positions) then
				total := total + 1;
			end if;
			MarkLocation(ml,'X');
		end if;
		return total;
	end CountMarkAround;


	function FillGridAux(l: Location) return Integer
	is
		total : Integer := 0;
		idx : Integer;
		char_idx : Integer;
		change: boolean := True;
		border : boolean := false;
		cl : Location;
		nl : Location;
	begin
		if Is_In(Element(grid(l.row), l.col), count_positions) then
			total := 1;
		end if;
		MarkLocation(l, 'X');
		while change loop
			change := False;
			for row in Unbounded_StringVector.First_Index(grid) .. Unbounded_StringVector.Last_Index(grid) loop 
				for char_idx in 1 .. Integer(Length(grid(row))) loop
					if Element(grid(row), char_idx) = 'X' then
						cl.row := row;
						cl.col := char_idx;
						if CanMarkAround(cl) then
							change := True;
							if BordersWall(cl) then
								--Put_Line("WALL row: " & cl.row'Image & " col: " & cl.col'Image);
								border := true;
							end if;
							total := total + CountMarkAround(cl);
						end if;
					end if;
				end loop;	
			end loop;
			--Put_Line("Can mark " & change'Image);
			--PrintGrid;
		end loop;
		--Put_Line("Border: " & Border'Image & " total: " & total'Image);
		if border then
			total := 0;
		end if;
		return total;
	end;


	procedure FillGrid
	is
		total : Integer := 0;
		add : Integer;
		l : Location;
		col : Integer;
		available : boolean := True;
	begin
		Put_Line("FillGrid");
		while available loop
			available := False;
			for row in Unbounded_StringVector.First_Index(grid) .. Unbounded_StringVector.Last_Index(grid) loop 
				col := Index(
					source => grid(row), 
					set => valid_positions,
					from => 1
					);
				if col /= 0 then
					available := True;
					l.row := row;
					l.col := col;
					exit;
				end if;
			end loop;
			add := FillGridAux(l);
			total := total + add;
			Put_Line("result and addition: "  & add'Image & " total: " & total'Image);
			PrintGrid;
		end loop;
		Put_Line("Elements within: " & total'Image);
	end FillGrid;


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

		PrintGrid;
		WalkGrid;
		FillGrid;
		Close (F);
	end Main;
end solutionpkp;
