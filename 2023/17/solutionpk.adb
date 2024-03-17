with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use  Ada.Strings.Unbounded;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Unchecked_Deallocation;

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Sets;

with solutionpk; use solutionpk;

package body solutionpk is
	type Direction is (Down, Right, Up, Left, None);
	subtype DirectionValid is Direction range Down .. Left;

	function "+"(d: DirectionValid; a: Integer) return DirectionValid is
		del : Integer;
	begin
		del := (DirectionValid'Pos(d) + a) mod (DirectionValid'Size+1);
		return DirectionValid'Val(del);
	end;

	function Opposite(d: DirectionValid) return DirectionValid
	is
	begin
		return (case d is
			when Up => Down,
			when Left => Right,
			when Down => Up,
			When Right => Left);
	end;

	type Location is record
		Col : Integer;
		Row : Integer;
	end record;
	type Location_Access is access Location;
	procedure Free is new Ada.Unchecked_Deallocation
		(Object => Location, Name => Location_Access);
	type LocationDirection is record
		loc : Location;
		dir : Direction;
        end record;

	function "="(Left, Right: Location) return Boolean is
	begin
		return Left.row = Right.Row and Left.col = Right.Col;
	end;

	function "<"(Left, Right: Location) return Boolean is
	begin
		return Left.row < Right.Row or (Left.row = Right.row and (Left.col < Right.Col));
	end;

	function "="(Left, Right: Location_Access) return Boolean is
	begin
		return Left.all = Right.all;
	end;

	function "<"(Left, Right: Location_Access) return Boolean is
	begin
		return Left.all < Right.all;
	end;

	package LocationSet is 
		new Ada.Containers.Ordered_Sets(
			Element_Type => Location_Access
			);
	use LocationSet;

	package IntegerVector is new
		Ada.Containers.Vectors
			(Index_Type => Natural,
			Element_Type => Integer 
			);
	type IntegerVector_Access is Access IntegerVector.Vector;
	use IntegerVector;

	package IntegerVectorVector is new
		Ada.Containers.Vectors
			(Index_Type => Natural,
			Element_Type => IntegerVector_Access
			);
	use IntegerVectorVector;

	-- Dijkstra 
	type DijkstraStorage is record
		cost: Integer;
		l: Location;
		d: Direction;
		dirSteps: Natural;
	end record;
	type DijkstraStorage_Access is Access DijkstraStorage;

	function "<"(Left, Right: DijkstraStorage) return Boolean is
		res : Boolean := False;
	begin
		if Left.cost < Right.cost then
			res := True;
		elsif Left.cost = Right.cost then
			if Left.l < Right.l then
				res := True;
			elsif Left.l = Right.l then
				if Left.dirSteps < Right.dirSteps then
					res := True;
				elsif Left.dirSteps = Right.dirSteps then
					res := Left.d < Right.d;
				end if;
			end if;
		end if;
		return res;
	end;

	procedure Free is new Ada.Unchecked_Deallocation
	      (Object => DijkstraStorage, Name => DijkstraStorage_Access);

	function "<"(Left, Right: DijkstraStorage_Access) return Boolean is
	begin
		return Left.all < Right.all;
	end;

	package DijkstraStorageSet is 
		new Ada.Containers.Ordered_Sets(
			Element_Type => DijkstraStorage_Access
			);
	use DijkstraStorageSet;

	-- Functions
	function Step(l : Location; d: Direction) return Location 
	is
		nl : Location;
	begin
		nl.row := l.row;
		nl.col := l.col;
		case d is
			when Up => nl.row := l.row-1;
			when Left => nl.col := l.col -1;
			when Down => nl.row := l.row +1;
			when Right => nl.col := l.col +1;
			when others => null;
		end case;
		return nl;
	end;

	function LocationValid(minr, minc, maxr, maxc: Integer; l: location) return Boolean
	is
	begin
		return l.row >= minr and l.row <= maxr and l.col >= minc and l.col <= maxc; 
	end;

	procedure PrintGrid(ivv : IntegerVectorVector.Vector) is
	begin
		Put_Line("Grid");
		for iv of ivv loop
			for I of iv.all loop
				Put(I'Image & " ");
			end loop;
			Put_Line("");
		end loop;
		Put_Line("");
	end;

	INVALID_RANGE_START : constant Integer := 100000000;
	function CloneGrid(ivv : IntegerVectorVector.Vector; val : Integer := INVALID_RANGE_START) return IntegerVectorVector.Vector
       	is
		nivv : IntegerVectorVector.Vector;
		tiv : IntegerVector_Access;
	begin
		nivv.clear;
		for iv of ivv loop
			tiv := new IntegerVector.Vector;
			for I in 0 .. IntegerVector.Length(iv.all)-1 loop
				tiv.append(val);
			end loop;
			nivv.append(tiv);
		end loop;
		return nivv;
	end;

	function process_line(str: Unbounded_String) return IntegerVector_Access
	is
		iv : IntegerVector_Access := new IntegerVector.Vector;
	begin
		for I in 1 .. Length(str) loop
			iv.append(Integer'Value(Slice(str, I, I)));
		end loop;
		return iv;
	end;

	function BestRoute(grid: IntegerVectorVector.Vector ) return Integer
	is
		LIMIT_STRAIGHT : constant Integer := 2;
		grid_totals : IntegerVectorVector.Vector;
		nl : Location;
		td : Direction;
		best_val : Integer := Integer'Last; 
		current_dij, next_dij: DijkstraStorage_Access;
		traveled : DijkstraStorageSet.Set;
		pendig_queue : DijkstraStorageSet.Set;
		res : Integer := 0;
	begin
		grid_totals := CloneGrid(grid,0);

		current_dij := new DijkstraStorage;
		current_dij.l.row := 0;
		current_dij.l.col := 0;
		current_dij.cost := 0;
		current_dij.dirSteps := 0;
		current_dij.d := None;

		Put_Line("daf");

		pendig_queue.Insert(current_dij);

		Put_Line("Main loop");
		-- Main loop
		while Length(pendig_queue) > 0 loop
			current_dij := First_Element(pendig_queue);
			Delete_First(pendig_queue);
			if current_dij.l.row = Integer(Length(grid)-1) and current_dij.l.col = Integer(Length(grid(0).all)-1) then
				res := current_dij.cost;
				Free(current_dij);
				exit;
			end if;
			-- Only if not processed
			if not Contains(traveled, current_dij) and (grid_totals(current_dij.l.row)(current_dij.l.col) > current_dij.cost or grid_totals(current_dij.l.row)(current_dij.l.col)=0) then
				traveled.Insert(current_dij);
				grid_totals(current_dij.l.row)(current_dij.l.col) := current_dij.cost;
				Put_Line("Loc row: " & current_dij.l.row'Image & " col: " & current_dij.l.col'Image & " cost: " & current_dij.cost'Image);
				-- All other locations
				for d in DirectionValid'First .. DirectionValid'Last loop
					td := Opposite(d);
					--Put_Line("Dir: " & d'Image & " Opodir: " & td'Image & " currDir: " & current_dij.d'Image);
					if td /= current_dij.d then
						nl := Step(current_dij.l, d);
						if LocationValid(0,0,Integer(Length(grid)-1), Integer(Length(grid(0).all)-1), nl) then
							if d /= current_dij.d then
								--Put_Line("Steps valid");
								next_dij := new DijkstraStorage;
								next_dij.cost := current_dij.cost + grid(nl.row)(nl.col);
								next_dij.d := d;
								next_dij.l := nl;
								next_dij.dirSteps := 1;
								if not Contains(pendig_queue, next_dij) then
									Put_Line("Nextdij dir: " & d'Image & " row: " & nl.row'Image & " col: " & nl.col'Image & " cost: " & next_dij.cost'Image);
									pendig_queue.Insert(next_dij);
								else
									Free(next_dij);
								end if;
							elsif d = current_dij.d and current_dij.dirSteps < LIMIT_STRAIGHT then
								--Put_Line("Steps valid");
								next_dij := new DijkstraStorage;
								next_dij.cost := current_dij.cost + grid(nl.row)(nl.col);
								next_dij.d := d;
								next_dij.l := nl;
								next_dij.dirSteps := current_dij.dirSteps + 1;
								if not Contains(pendig_queue, next_dij) then
									Put_Line("Nextdij dir: " & d'Image & " row: " & nl.row'Image & " col: " & nl.col'Image & " cost: " & next_dij.cost'Image);
									pendig_queue.Insert(next_dij);
								else
									Free(next_dij);
								end if;

							end if;
						end if;
					end if;
				end loop;
				Put_Line("ResultPath");
				PrintGrid(grid_totals);
			else
				Free(current_dij);
			end if;
		end loop;

		while Length(traveled) > 0 loop
			current_dij := First_Element(traveled);
			Delete_First(traveled);
			Free(current_dij);
		end loop;

		Put_Line("ResultPath");
		PrintGrid(grid_totals);
		return res;
	end;


	procedure Main is
		F         : File_Type;
		File_Name : constant String := "input.txt";
		str       : Unbounded_String;
		grid      : IntegerVectorVector.Vector;
		total     : Integer;
	begin
		Open (F, In_File, File_Name);
		while not End_Of_File (F) loop
			-- Capture the line
			str := To_Unbounded_String(Get_Line (F));
			Put_Line(To_String(str));
			grid.append(process_line(str));
		end loop;
		Close (F);

		Put_Line("Input: ");
		PrintGrid(grid);
		total := BestRoute(grid);
		Put_Line("Input: ");
		PrintGrid(grid);
		Put_Line("Total: " & total'Image);
	end Main;
end solutionpk;
