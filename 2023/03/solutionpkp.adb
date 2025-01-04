with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings; use  Ada.Strings; -- Adds Backward
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Unbounded; use  Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Sets;
with Ada.Strings.Maps.Constants;

with solutionpkp; use solutionpkp;

package body solutionpkp is
	function replace_string(
		source : in Unbounded_String;
		value  : in String
		) return Unbounded_String
	is
		local : Unbounded_String := source;
		I : Integer;
	begin
		I := 1;
		while I < Length(local) loop
			Overwrite(local, I, value);
			I := I + 1;
		end loop;
		return local;
	end replace_string;

	procedure Main is
		F         : File_Type;
		File_Name : constant String := "input.txt";
		str       : Unbounded_String;
		tstr      : Unbounded_String;
		package C_Vectors is new
		Ada.Containers.Vectors
			(Index_Type => Natural,
			Element_Type => Unbounded_String);
		V : C_Vectors.Vector;
		SearchSet : constant Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.To_Set("*");
		SearchESet : constant Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.To_Set("#$%&*+-/=@.");
		idx : Integer;
		idxl, idxr : Integer;
		idxtl, idxtr : Integer;
		idxt : Integer;
		value : Integer;
		check_line : Integer;
		total : Integer := 0;
		ttotal : Integer := 0;
		countmatch : Integer := 0;
	begin
		Open (F, In_File, File_Name);
		str := To_Unbounded_String(Get_Line (F));
		V.append(str);
		V.prepend(replace_string(str,"."));
		Put_Line(To_String(str));
		while not End_Of_File (F) loop
			-- Capture the line
			str := To_Unbounded_String(Get_Line (F));
			V.append(str);
			Put_Line(To_String(str));
		end loop;
		V.append(replace_string(str,"."));
		-- For each line
		for I in V.first_index+1 .. V.last_index-1 loop
			str := V(I);
			Put_Line(To_String(str));
			idx := Index( Source => str,
				Set => SearchSet,
				From => 1 );
			-- For each SYMBOL localized
			while idx > 0 loop
				Put_Line("################################################################################");
				Put_Line("Line: " & I'Image & " column: " & idx'image);

				idxl := idx;
				-- Set the correct limits left, right
				if idx > 1 then
					idxl := idxl-1;
				end if;
				idxr := idx;
				if idx < Length(str) then
					idxr := idxr+1;
				end if;
				Put_Line("Idxl: " & idxl'Image & " idx: " & idx'Image & " idxr: " & idxr'Image);
				Put_Line("################################################################################");

				countmatch := 0;
				ttotal := 0;
				-- Check the higher, current and lower line
				for si in -1 .. 1 loop
					idxtl := idxl;
					idxtr := idxr;
					check_line := I+si;
					tstr := V(check_line);
					Put_Line("$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$");
					Put_Line("Checking line: " & check_line'image & " around " & idx'Image);
					Put_Line(To_String(tstr));
					if Is_Digit(Element(tstr,idxtl)) and Is_Digit(Element(tstr,idxtr)) and idxtr-idxtl = 2 and not Is_Digit(Element(tstr,idxtr-1)) then
						Put_Line("DUAL " & To_string(tstr));
						idxt := Index( Source => tstr,
							Set => SearchESet,
							From => idxtl,
							Going => Backward);
						if idxt = 0 then
							idxt := 1;
						else
							-- One more to remove non digit
							idxt := idxt + 1;
						end if;
						Put_Line("Idxt: " & idxt'Image & " idxtl: " & idxtl'Image);
						value := Integer'Value(Slice(tstr, idxt, idxtl));
						Put_Line("Digit! " & Slice(tstr, idxt, idxtl) & " value: " & value'Image);
						if ttotal = 0 then
							ttotal := value;
						else
							ttotal := ttotal * value;
						end if;
						countmatch := countmatch + 1;

						---- Remove already counted number
						--for ri in idxt .. idxtl loop
						--	Replace_Element(
						--		source => tstr,
						--		Index => ri,
						--		By => ';');
						--end loop;

						idxt := Index( Source => tstr,
							Set => SearchESet,
							From => idxtr);
						-- If not match at limit of string
						if idxt = 0 then
							idxt := Length(tstr);
						else
							-- One less to remove non digit
							idxt := idxt - 1;
						end if;
						Put_Line("idxtr: " & idxtr'Image & "Idxt: " & idxt'Image );
						value := Integer'Value(Slice(tstr, idxtr, idxt));
						Put_Line("Digit! " & Slice(tstr, idxtr, idxt) & " value: " & value'Image);
						if ttotal = 0 then
							ttotal := value;
						else
							ttotal := ttotal * value;
						end if;
						countmatch := countmatch + 1;

						---- Remove already counted number
						--for ri in idxtr .. idxt loop
						--	Replace_Element(
						--		source => tstr,
						--		Index => ri,
						--		By => ';');
						--end loop;
					else
						-- If only one number
						-- Check left most non number from current position 
						idxt := Index( Source => tstr,
							Set => Ada.Strings.Maps.Constants.Decimal_Digit_Set,
							From => idxtl );
						-- If 0 then no number in this line
						if idxt /= 0 and idxt <= idxtr then
							-- If lest most, could start more left
							if idxt = idxtl then
								-- Search for left most non number
								idxtl := Index( Source => tstr,
									Set => SearchESet,
									From => idxt,
									Going => Backward);
								-- If no match, start of line
								if idxtl = 0 then
									idxtl := 1;
								else
									-- One more to remove non digit
									idxtl := idxtl + 1;
								end if;
							else
								idxtl := idxt;
							end if;
							-- The right limit now it will be the first non match
							idxtr := Index( Source => tstr,
								Set => SearchESet,
								From => idxt);
							-- If not match at limit of string
							if idxtr = 0 then
								idxtr := Length(tstr);
							else
								-- One less to remove non digit
								idxtr := idxtr - 1;
							end if;
							Put_Line("idxtl: " & idxtl'Image & " idxt: " & idxt'Image & " idxtr: " & idxtr'Image);
							value := Integer'Value(Slice(tstr, idxtl, idxtr));
							Put_Line("Digit! " & Slice(tstr, idxtl, idxtr) & " value: " & value'Image);
							if ttotal = 0 then
								ttotal := value;
							else
								ttotal := ttotal * value;
							end if;
							countmatch := countmatch + 1;

							---- Remove already counted number
							--for ri in idxtl .. idxtr loop
							--	Replace_Element(
							--		source => tstr,
							--		Index => ri,
							--		By => ';');
							--end loop;
						else
							Put_Line("No match");
							Put_Line("idxtl: " & idxtl'Image & " idxt: " & idxt'Image & " idxtr: " & idxtr'Image);
						end if;
					end if;
					Put_Line("Checked line: " & check_line'image);
					Put_Line(To_String(tstr));
					V(check_line) := tstr;
				end loop;
				if countmatch = 2 then
					total := total + ttotal;
				end if;

				idx := Index( Source => str,
					Set => SearchSet,
					From => idx+1 );
			end loop;
		end loop;
		Put_Line("Total: " & total'Image);
		Close (F);
	end Main;
end solutionpkp;
