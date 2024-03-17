with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use  Ada.Strings.Unbounded;
with Ada.Characters.Handling; use  Ada.Characters.Handling;
with Ada.Strings.Maps; use Ada.Strings.Maps;

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Ordered_Sets;

with solutionpk; use solutionpk;

package body solutionpk is
	package Long_IntegerSet is 
		new Ada.Containers.Ordered_Sets(
			Element_Type => Long_Integer
			);
	use Long_IntegerSet;
	seeds_storage : Long_IntegerSet.Set;
	seeds_current : Long_IntegerSet.Set;
	seeds_mapped  : Long_IntegerSet.Set;


	destination   : Long_Integer;
	source        : Long_Integer;
	source_end    : Long_Integer;
		
	state: StateMachine;

	procedure statemachine_proc(
		capture: in Unbounded_String
		)
	is
		current_number : Long_Integer := 0;
		trimmed_string : Unbounded_String;
	begin
		trimmed_string := Trim(capture, To_Set(' '), To_Set(":;, "));
		--Put_Line("Token: " & To_String(trimmed_string));
		if trimmed_string = "" then
			return;
		end if;
		case state is
			when seeds =>
				--Put_Line("seeds");
				state := seed_number;
			when seed_number =>
				current_number := Long_Integer'Value(To_String(trimmed_string));
				--Put_Line("seed id: " & current_number'Image);
				seeds_current.insert(current_number);
			when map_destination =>
				current_number := Long_Integer'Value(To_String(trimmed_string));
				destination := current_number;
				--Put_Line("destination: " & current_number'Image);
				state := map_source;
			when map_source =>
				current_number := Long_Integer'Value(To_String(trimmed_string));
				source := current_number;
				--Put_Line("source: " & current_number'Image);
				state := map_range_length;
			when map_range_length =>
				current_number := Long_Integer'Value(To_String(trimmed_string));
				source_end := source + current_number;
				--Put_Line("range_length: " & current_number'Image);
		end case;
	end statemachine_proc;


	procedure line_proc
		(capture: in Unbounded_String)
	is
		idx,idxl  : Natural := 1;
	begin
		destination := 0;
		source := 0;
		source_end := 0;
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
	begin
		Open (F, In_File, File_Name);
		seeds_current.clear;
		seeds_storage.clear;
		state := seeds;
		while not End_Of_File (F) loop
			-- Capture the line
			str := To_Unbounded_String(Get_Line (F));
			Put_Line(To_String(str));

			if Index(str, "map", 1) > 0 then
				Put_Line("New map " & To_String(str));
				--seeds_storage.clear;
				for s of seeds_mapped loop
					Delete(seeds_storage,s);
				end loop;
				for s of seeds_current loop
					seeds_storage.insert(s);
				end loop;
				seeds_current.clear;
				seeds_mapped.clear;
				state := map_destination;
			else
				line_proc(str);
			end if;


			if state = map_range_length then
				Put_Line("Processing seeds through map");
				Put_Line("D: " & destination'image & " S: " & source'image & " SE: " & source_end'image);
				for s of seeds_storage loop
					if s >= source and s < source_end then
						Put_Line("s in range: " & s'image);
						seeds_current.insert(destination + s - source);
						seeds_mapped.insert(s);
					end if;
				end loop;
				state := map_destination;
			end if;
		end loop;
		Put_Line("Process last map " & To_String(str));
		--seeds_storage.clear;
		for s of seeds_mapped loop
			Delete(seeds_storage,s);
		end loop;
		for s of seeds_current loop
			seeds_storage.insert(s);
		end loop;
		seeds_current.clear;
		seeds_mapped.clear;

		Put_Line("Best seed: " & First_Element(seeds_storage)'Image);
		Close (F);
	end Main;
end solutionpk;
