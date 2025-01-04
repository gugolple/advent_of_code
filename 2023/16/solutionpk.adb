with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use  Ada.Strings.Unbounded;
with Ada.Strings.Maps; use Ada.Strings.Maps;

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Sets;


package body solutionpk is
	type Direction is (Up, Down, Left, Right, UpDown, LeftRight, Bad);
	type Location is record
		Col : Integer;
		Row : Integer;
        end record;

	type LocationDirection is record
		loc : Location;
		dir : Direction;
        end record;

	type LocationDirection_Access is access LocationDirection;

	function "<"(Left, Right: LocationDirection) return Boolean is
	begin
		return Left.loc.row < Right.loc.Row or (Left.loc.row = Right.loc.row and (Left.loc.col < Right.loc.Col or (Left.loc.col = Right.loc.col and Left.dir < Right.dir)));
	end;

	package LocationDirectionSet is 
		new Ada.Containers.Ordered_Sets(
			Element_Type => LocationDirection
			);
	use LocationDirectionSet;

	package LocationDirectionVector is new
		Ada.Containers.Vectors
			(Index_Type => Natural,
			Element_Type => LocationDirection 
			);
	use LocationDirectionVector;


	function NextDir(d: Direction; c: Character) return Direction
	is
		nd : Direction;
	begin
		nd := (case c is
			when '/' => 
				(case d is
					when Up => Right,
					when Left => Down,
					when Down => Left,
					when Right => Up,
					when others => Bad),
			when '\' => 
				(case d is
					when Up => Left,
					when Left => Up,
					when Down => Right,
					when Right => Down,
					when others => Bad),
			when '|' => 
				(case d is
					when Right | Left => UpDown,
					when others => d),
			when '-' => 
				(case d is
					when Up | Down => LeftRight,
					when others => d),
			when others => d);
		return nd;
	end;

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

	package Unbounded_StringVector is new
		Ada.Containers.Vectors
			(Index_Type => Natural,
			Element_Type => Unbounded_String 
			);
	use Unbounded_StringVector;

	package IntegerVector is new
		Ada.Containers.Vectors
			(Index_Type => Natural,
			Element_Type => Integer 
			);
	use IntegerVector;

	package IntegerVectorVector is new
		Ada.Containers.Vectors
			(Index_Type => Natural,
			Element_Type => IntegerVector.Vector
			);
	use IntegerVectorVector;


	function WalkGrid (grid: Unbounded_StringVector.Vector ; walked_tiles : in out IntegerVectorVector.Vector) return Natural
	is
		total : Natural := 0;
		row_tiles : IntegerVector.Vector;
		current_tile : Integer;
		pending : LocationDirectionVector.Vector;
		current : LocationDirection;
		temporal : LocationDirection_Access;
		current_grid : Character;
		nloc : Location;
		ndir : Direction;
		started_tiles : LocationDirectionSet.Set;
	begin
		current.Loc.row := 0;
		current.Loc.col := 1;
		current.Dir := Right;
		pending.append(current);
		while Length(pending) > 0 loop
			--Put_Line("Map: ");
			--for V of walked_tiles loop
			--	for n of V loop
			--		Put(n'Image & " ");
			--	end loop;
			--	Put_Line("");
			--end loop;
			--Put_Line("");

			-- Pop from stack
			current := First_Element(pending);
			Delete_First(pending);
			Put_Line("Start row: " & current.loc.row'Image & " col: " & current.loc.col'Image & " dir: " & current.dir'Image);
			if not Contains(started_tiles, current) then
				temporal := new LocationDirection;
				temporal.dir := current.dir;
				temporal.loc.row := current.loc.row;
				temporal.loc.col := current.loc.col;
				started_tiles.Insert(current);
				while current.loc.row >= 0 and current.loc.col >= 1 and current.loc.row < Integer(Length(grid)) and current.loc.col <= Length(grid(0)) loop
					Put_Line("row: " & current.loc.row'Image & " col: " & current.loc.col'Image);
					-- Add to the walked
					row_tiles := walked_tiles(current.loc.row);
					-- String to vector differences
					current_tile := row_tiles(current.loc.col -1);
					if current_tile = 0 then
						total := total + 1;
					end if;
					row_tiles(current.loc.col -1) := current_tile + 1;
					walked_tiles(current.loc.row) := row_tiles;

					-- Logic
					current_grid := Element(grid(current.loc.row), current.loc.col);
					ndir := NextDir(current.dir, current_grid);
					if ndir = LeftRight then
						current.dir := Left;
						temporal := new LocationDirection;
						temporal.dir := Right;
						nloc := Step(current.loc, temporal.dir);
						temporal.loc.row := nloc.row;
						temporal.loc.col := nloc.col;
						if not Contains(started_tiles, temporal.all) then
							Put_Line("Append row: " & temporal.loc.row'Image & " col: " & temporal.loc.col'Image & " dir: " & temporal.dir'Image);
							pending.append(temporal.all);
						end if;
					elsif ndir = UpDown then
						current.dir := Up;
						temporal := new LocationDirection;
						temporal.dir := Down;
						nloc := Step(current.loc, temporal.dir);
						temporal.loc.row := nloc.row;
						temporal.loc.col := nloc.col;
						if not Contains(started_tiles, temporal.all) then
							Put_Line("Append row: " & temporal.loc.row'Image & " col: " & temporal.loc.col'Image & " dir: " & temporal.dir'Image);
							pending.append(temporal.all);
						end if;
					else
						current.dir := ndir;
					end if;	

					nloc := Step(current.loc, current.dir);
					current.loc.col := nloc.col;
					current.loc.row := nloc.row;
				end loop;
			end if;
		end loop;
		return total;
	end;


	procedure Main is
		F         : File_Type;
		File_Name : constant String := "input.txt";
		str       : Unbounded_String;
		values : Unbounded_StringVector.Vector;
		walked_tiles: IntegerVectorVector.Vector;
		row_tiles : IntegerVector.Vector;
		total : Integer;
	begin
		Open (F, In_File, File_Name);
		while not End_Of_File (F) loop
			-- Capture the line
			str := To_Unbounded_String(Get_Line (F));
			Put_Line(To_String(str));
			values.append(str);
			row_tiles := IntegerVector.To_Vector(Count_Type(Ada.Strings.Unbounded.Length(str)));
			for I in First_Index(row_tiles) .. Last_Index(row_tiles) loop
				row_tiles.Replace_Element(I, 0);
			end loop;
			IntegerVectorVector.append(walked_tiles, row_tiles);
		end loop;
		Close (F);

		Put_Line("Input: ");
		for I of values loop
			Put_Line(To_String(I));
		end loop;
		Put_Line("");

		Put_Line("Map: ");
		for V of walked_tiles loop
			for n of V loop
				Put(n'Image & " ");
			end loop;
			Put_Line("");
		end loop;
		Put_Line("");

		total := WalkGrid(values, walked_tiles);

		Put_Line("Map: ");
		for V of walked_tiles loop
			for n of V loop
				Put(n'Image & " ");
			end loop;
			Put_Line("");
		end loop;
		Put_Line("");

		Put_Line("Input: ");
		for idx in 0 .. Integer(Length(values))-1 loop
			str := values(idx);
			for jdx in 0 .. Length(values(0))-1 loop
				if Element(str, jdx+1) = '.' and walked_tiles(idx)(jdx) > 0 then
					Replace_Element(str, jdx+1, 'X');
				end if;
			end loop;
			Put_Line(To_String(str));
		end loop;
		Put_Line("");

		Put_Line("Total: " & total'Image);
	end Main;
end solutionpk;
