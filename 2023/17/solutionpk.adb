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

	procedure calculateVal(grid_val: IntegerVectorVector.Vector ; grid_totals : in out IntegerVectorVector.Vector ; l: Location)
	is
		LIMIT_STRAIGHT : constant Integer := 3;
		line_sum : Integer;
		vrow, vcol : Boolean := True;
		col_val, row_val : Integer;
		next_val : Integer;
	begin
		if l.row = 0 then
			vrow := False;
		end if;
		if l.col = 0 then
			vcol := False;
		end if;
		if l.row >= LIMIT_STRAIGHT then
			line_sum := grid_totals(l.row-LIMIT_STRAIGHT)(l.col);
			for I in l.row -LIMIT_STRAIGHT+1 .. l.row -1 loop
				line_sum := line_sum + grid_val(I)(l.col);
			end loop;
			if line_sum = grid_totals(l.row-1)(l.col) then
				vrow := False;
				Put_Line("Straight rows");
			end if;
		end if;
		if l.col >= LIMIT_STRAIGHT then
			line_sum := grid_totals(l.row)(l.col-LIMIT_STRAIGHT);
			for I in l.col -LIMIT_STRAIGHT+1 .. l.col -1 loop
				line_sum := line_sum + grid_val(l.row)(I);
			end loop;
			if line_sum = grid_totals(l.row)(l.col-1) then
				vcol := False;
				Put_Line("Straight cols");
			end if;
		end if;
		if vcol and vrow then
			col_val := grid_totals(l.row)(l.col-1);
			row_val := grid_totals(l.row-1)(l.col);
			if col_val < row_val then
				vrow := False;
			else
				vcol := False;
			end if;
		end if;
		if vcol then
			col_val := grid_totals(l.row)(l.col-1);
			next_val := col_val + grid_val(l.row)(l.col);
		elsif vrow then
			row_val := grid_totals(l.row-1)(l.col);
			next_val := row_val + grid_val(l.row)(l.col);
		else
			next_val := INVALID_RANGE_START;
			Put_Line("Absolute Fuck!");
		end if;
		Put_Line("Row: " & l.row'Image & " col: " & l.col'Image & " nv: " & next_val'Image & " cv: " & col_val'Image & " rv: " & row_val'Image);
		grid_totals(l.row)(l.col) := next_val;
	end;


	best_val : Integer := Integer'Last; 
	traveled : LocationSet.Set;
	function BestRoute(grid: IntegerVectorVector.Vector ) return Integer
	is
		calc_grid : IntegerVectorVector.Vector;
		row_limit : Integer;
		l : Location;
	begin
		calc_grid := CloneGrid(grid);
		calc_grid(0)(0) := grid(0)(0);
		Put_Line("Initial grid");
		PrintGrid(calc_grid);
		row_limit := Integer(Length(calc_grid)-1);
		for I in 1 .. row_limit loop
			for idx in 0 .. I loop
				l.row := idx;
				l.col := I-idx;
				--calc_grid(l.row)(l.col) := I;
				calculateVal(grid, calc_grid, l);
			end loop;
		end loop;
		Put_Line("Inter grid");
		PrintGrid(calc_grid);
		for I in 1 .. Integer(Length(calc_grid(0).all)-1) loop
			for idx in 0 .. row_limit - I loop
				l.row := row_limit - idx;
				l.col := I+idx;
				--calc_grid(l.row)(l.col) := row_limit + I;
				calculateVal(grid, calc_grid, l);
			end loop;
		end loop;
		Put_Line("Final grid");
		PrintGrid(calc_grid);
		return calc_grid(Integer(Length(calc_grid)-1))(Integer(Length(calc_grid(0).all)-1));
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
