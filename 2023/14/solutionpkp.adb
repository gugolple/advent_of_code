with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use  Ada.Strings.Unbounded;
with Ada.Strings.Maps; use Ada.Strings.Maps;

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;

with solutionpkp; use solutionpkp;

package body solutionpkp is
	TOTAL_CICLES : constant Integer := 1000000000;
	type StateMachine is (UP, DOWN, LEFT, RIGHT);
	package Unbounded_StringVector is new
		Ada.Containers.Vectors
			(Index_Type => Natural,
			Element_Type => Unbounded_String 
			);
	use Unbounded_StringVector;
	type Unbounded_StringVector_Access is access Unbounded_StringVector.Vector;

	package Unbounded_StringVectorVector is new
		Ada.Containers.Vectors
			(Index_Type => Natural,
			Element_Type => Unbounded_StringVector.Vector 
			);
	use Unbounded_StringVectorVector;

	process_line : Unbounded_String;

	function Transpose(v: Unbounded_StringVector.Vector) return Unbounded_StringVector.Vector 
	is
		transposed : Unbounded_StringVector.Vector;
		my_str : Unbounded_String;
		count : Integer;
	begin
		for I in 1 .. Length(v(0)) loop
			my_str := To_Unbounded_String(Integer(Unbounded_StringVector.Length(v)));
			count := 1;
			for S of reverse v loop
				Replace_Element(my_str, count, Element(S, I));
				count := count + 1;
			end loop;
			transposed.append(my_str);
		end loop;
		return transposed;
	end Transpose;

	function "="(Left, Right: Unbounded_StringVector.Vector) return Boolean
	is
		lrow, rrow : Unbounded_String;
	begin
		if Length(Left) /= Length(Right) then
			return False;
		else
			for I in 0 .. Integer(Length(Left))-1 loop
				lrow := Left(I);
				rrow := Right(I);
				if lrow /= rrow then
					return False;
				end if;
			end loop;
		end if;
		return True;
	end "=";

	function CloneVector(v: Unbounded_StringVector.Vector) return Unbounded_StringVector.Vector
	is
		myv: Unbounded_StringVector_Access;
		myr: Unbounded_String;
		rlen : Integer;
	begin
		myv := new Unbounded_StringVector.Vector;
		for R of v loop
			rlen := Integer(Ada.Strings.Unbounded.Length(R));
			myr := To_Unbounded_String(rlen);
			for I in 1 .. rlen loop
				Replace_Element(myr, I, Element(R, I));
			end loop;
			myv.append(myr);
		end loop;
		return myv.all;
	end;

	procedure PrintVector(v: Unbounded_StringVector.Vector)
	is
	begin
		for I of v loop
			Put_Line(To_String(I));
		end loop;
		Put_Line("");
	end;

	procedure MoveNorth(v: in out Unbounded_StringVector.Vector)
	is
		idx : Integer;
		space_row : Integer;
		tmp_row : Unbounded_String;
	begin
		for I in 1 .. Integer(Unbounded_StringVector.Length(v) -1) loop
			--Put_Line("Line: " & I'Image);
			idx := Index(v(I), "O", 1);
			while idx /= 0 loop
				--Put_Line("Idx: " & idx'Image);
				space_row := 0;
				for J in reverse 0 .. I-1 loop
					if Element(v(J), idx) /= '.' then
						space_row := J+1;
						exit;
					end if;
				end loop;
				if space_row /= I then
					--Put_Line("Replaced!");
					tmp_row := v(space_row);
					Replace_Element(tmp_row, idx, 'O');
					Unbounded_StringVector.Replace_Element(v, space_row, tmp_row);
					tmp_row := v(I);
					Replace_Element(tmp_row, idx, '.');
					Unbounded_StringVector.Replace_Element(v, I, tmp_row);
					--PrintVector(v);
				end if;
				idx := Index(v(I), "O", idx + 1);
			end loop;
		end loop;
	end;

	function SingleCicle(v: Unbounded_StringVector.Vector) return Unbounded_StringVector.Vector
	is
		myv : Unbounded_StringVector.Vector := v;
	begin
		--Put_Line("North");
		MoveNorth(myv);
		--PrintVector(myv);
		--Put_Line("Left");
		myv := Transpose(myv);
		MoveNorth(myv);
		--PrintVector(myv);
		--Put_Line("South");
		myv := Transpose(myv);
		MoveNorth(myv);
		--PrintVector(myv);
		--Put_Line("Right");
		myv := Transpose(myv);
		MoveNorth(myv);
		--PrintVector(myv);
		--Put_Line("Original dir");
		myv := Transpose(myv);
		--PrintVector(myv);
		return myv;
	end;

	function Cicle(v: Unbounded_StringVector.Vector) return Unbounded_StringVector.Vector
	is
		myv : Unbounded_StringVector.Vector := v;
		tv : Unbounded_StringVector.Vector;
		old_myv : Unbounded_StringVector.Vector;
		all_vecs : Unbounded_StringVectorVector.Vector;
		found_cycle : Boolean := False;
		iterations : Integer := 0;
		cicle_length : Integer;
		cicle_start : Integer;
		cicle_result : Integer;
	begin
		while not found_cycle loop
			iterations := iterations + 1;
			-- Proces next
			old_myv := CloneVector(myv);
			all_vecs.append(old_myv);
			myv := SingleCicle(myv);

			for vidx in 0 .. Integer(Length(all_vecs))-1 loop
				tv := all_vecs(vidx);
				if tv = myv then
					cicle_start := vidx;
					cicle_length := Integer(Length(all_vecs)) - vidx;
					found_cycle := True;
				end if;
			end loop;
		end loop;
		Put_Line("Cicle!: " & cicle_length'Image);
		cicle_result := TOTAL_CICLES - iterations;
		Put_Line("Pendings!: " & cicle_result'Image);
		cicle_result := cicle_result REM cicle_length;
		Put_Line("IdxRes!: " & cicle_result'Image);
		myv := all_vecs(cicle_result + cicle_start);
		return myv;
	end;

	function CalcWeight(v: Unbounded_StringVector.Vector) return Integer
	is
		total : Integer := 0;
		line_val : Integer := Integer(Unbounded_StringVector.Length(v));
	begin
		for R of v loop
			total := total + Integer(Ada.Strings.Unbounded.Count(R, "O")) * line_val;
			line_val := line_val -1;
		end loop;
		return total;
	end CalcWeight;

	procedure Main is
		F         : File_Type;
		File_Name : constant String := "input.txt";
		str       : Unbounded_String;
		values : Unbounded_StringVector.Vector;
		values_clone : Unbounded_StringVector.Vector;
	begin
		Open (F, In_File, File_Name);
		while not End_Of_File (F) loop
			-- Capture the line
			str := To_Unbounded_String(Get_Line (F));
			values.append(str);

		end loop;
		Close (F);
		Put_Line("Input:");
		PrintVector(values); 
		values_clone := CloneVector(values);
		values := Cicle(values);
		Put_Line("Result: " & CalcWeight(values)'Image);
	end Main;
end solutionpkp;
