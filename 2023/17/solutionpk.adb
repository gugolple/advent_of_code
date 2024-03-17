with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use  Ada.Strings.Unbounded;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Unchecked_Deallocation;

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Sets;

with solutionpk; use solutionpk;

package body solutionpk is
	type Direction is (Down, Right, Up, Left);
	type Location is record
		Col : Integer;
		Row : Integer;
	end record;
	type Location_Access is access Location;
	procedure FreeLocation is new Ada.Unchecked_Deallocation
		(Object => Location, Name => Location_Access);
	type LocationDirection is record
		loc : Location;
		dir : Direction;
        end record;

	function "="(Left, Right: Location_Access) return Boolean is
	begin
		return Left.row = Right.Row and Left.col = Right.Col;
	end;

	function "<"(Left, Right: Location_Access) return Boolean is
	begin
		return Left.row < Right.Row or (Left.row = Right.row and (Left.col < Right.Col));
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

	function process_line(str: Unbounded_String) return IntegerVector_Access
	is
		iv : IntegerVector_Access := new IntegerVector.Vector;
	begin
		for I in 1 .. Length(str) loop
			iv.append(Integer'Value(Slice(str, I, I)));
		end loop;
		return iv;
	end;


	best_val : Integer := Integer'Last; 
	traveled : LocationSet.Set;
	function BestRoute(grid: IntegerVectorVector.Vector ; l: Location; acum: Integer := 0) return Integer
	is
		best : Integer := Integer'Last;
		current : Integer;
		maxr : Integer := Integer(Length(grid)-1);
		maxc : Integer := Integer(Length(grid(0).all)-1);
		nl : Location_Access;
	begin
		nl := new Location;
		--Put_Line("Row: " & l.row'Image & " col: " & l.col'Image);
		if Length(traveled) > 144 then
			Put_Line("Shit!");
		end if;
		if acum > best_val then
			return best;
		end if;
		if l.row = maxr and l.col = maxc then
			Put_Line("End: " & acum'Image);
			best := acum;
			if best < best_val then
				best_val := best;
			end if;
		else
			for d in Direction loop
				nl.all := Step(l, d);
				if LocationValid(0,0,maxr,maxc,nl.all) and not Contains(traveled, nl) then
					traveled.Insert(nl);
					current := BestRoute(grid, nl.all, acum + grid(l.row).all(l.col));
					if current < best then
						best := current;
					end if;
					traveled.Delete(nl);
				end if;
			end loop;
		end if;
		FreeLocation(nl);
		return best;
	end;


	procedure Main is
		F         : File_Type;
		File_Name : constant String := "input.txt";
		str       : Unbounded_String;
		grid      : IntegerVectorVector.Vector;
		total     : Integer;
		l : Location;
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
		l.col := 0;
		l.row := 0;
		total := BestRoute(grid, l);
		Put_Line("Total: " & total'Image);
	end Main;
end solutionpk;
