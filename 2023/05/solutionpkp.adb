with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use  Ada.Strings.Unbounded;
with Ada.Characters.Handling; use  Ada.Characters.Handling;
with Ada.Strings.Maps; use Ada.Strings.Maps;

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Ordered_Sets;

with solutionpkp; use solutionpkp;

package body solutionpkp is
	Function "<" (Left, Right: In seed_range_record) return boolean
	is
		res : boolean := false;
	begin
		if Left.start < Right.start then
			res := true;
		elsif Left.start = Right.start then
			if Left.range_end < Right.range_end then
				res := true;
			end if;
		end if;
		return res;
	end "<";

	Function "=" (Left, Right: In seed_range_record) return boolean
	is
		res : boolean := false;
	begin
		if Left.start = Right.start then
			if Left.range_end = Right.range_end then
				res := true;
			end if;
		end if;
		return res;
	end "=";

	package SeedRangeRecordSet is 
		new Ada.Containers.Ordered_Sets(
			Element_Type => seed_range_record
			);

	use SeedRangeRecordSet;
	seeds_storage : SeedRangeRecordSet.Set;
	seeds_current : SeedRangeRecordSet.Set;
	seeds_mapped  : SeedRangeRecordSet.Set;


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
		seed_rec : seed_range_record;
	begin
		trimmed_string := Trim(capture, To_Set(' '), To_Set(":;, "));
		--Put_Line("Token: " & To_String(trimmed_string));
		if trimmed_string = "" then
			return;
		end if;
		case state is
			when seeds =>
				--Put_Line("seeds");
				state := seed_start;
			when seed_start =>
				current_number := Long_Integer'Value(To_String(trimmed_string));
				source := current_number;
				--Put_Line("seed id: " & current_number'Image);
				state := seed_range;
			when seed_range =>
				current_number := Long_Integer'Value(To_String(trimmed_string));
				seed_rec := (
						start => source,
						range_end => source + current_number -1
						);
				insert(
					container => seeds_current,
					new_item => seed_rec
					);
				--Put_Line("seed id: " & current_number'Image);
				state := seed_start;
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
				source_end := source + current_number -1;
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
		seed_l   : seed_range_record;
		seed_m   : seed_range_record;
		seed_r   : seed_range_record;
	begin
		Open (F, In_File, File_Name);
		seeds_current.clear;
		seeds_storage.clear;
		state := seeds;
		while not End_Of_File (F) loop
			-- Capture the line
			str := To_Unbounded_String(Get_Line (F));
			--Put_Line(To_String(str));

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
				Put_Line("Seed state");
				for s of seeds_storage loop
					Put_Line("seed: " & s.start'Image & " end: " & s.range_end'Image);
				end loop;
			else
				line_proc(str);
			end if;


			if state = map_range_length then
				--Put_Line("");
				--Put_Line("Processing seeds through map");
				Put_Line("D: " & destination'image & " S: " & source'image & " SE: " & source_end'image);
				for s of seeds_storage loop
					seed_m.start := s.start;
					seed_m.range_end := s.range_end;
					if not seeds_mapped.contains(s) and s.start < source_end and s.range_end > source then
					--if s.start < source_end and s.range_end > source then
						--Put_Line("Seed range start: " & s.start'Image & " end: " & s.range_end'Image);
						-- If any kind of match
						if s.start < source then
							seed_l := (
								start => s.start,
								range_end => source -1
								);
							seed_m.start := source;
							--Put_Line("Left map start: " & seed_l.start'Image & " end: " & seed_l.range_end'Image);
							seeds_current.include(seed_l);
						end if;
						if s.range_end > source_end then
							seed_m.range_end := source_end;
							seed_r := (
								start => source_end +1,
								range_end => s.range_end
								);
							--Put_Line("Right map start: " & seed_r.start'Image & " end: " & seed_r.range_end'Image);
							seeds_current.include(seed_r);
						end if;
						seed_m.start := seed_m.start - source + destination;
						seed_m.range_end := seed_m.range_end - source + destination;
						--Put_Line("Middle map start: " & seed_m.start'Image & " end: " & seed_m.range_end'Image);
						--if seed_m.start = 0 then
						--	Put_Line("FUCK!");
						--end if;
						-- Add mapping
						seeds_current.include(seed_m);
						-- Mark for deletion from sources
						seeds_mapped.include(s);
						--Put_Line("deleted seed: " & s.start'Image & " end: " & s.range_end'Image);
					end if;

				end loop;
				state := map_destination;
			end if;
		end loop;
		--seeds_storage.clear;
		for s of seeds_mapped loop
			Delete(seeds_storage,s);
		end loop;
		for s of seeds_current loop
			seeds_storage.include(s);
		end loop;
		seeds_current.clear;
		seeds_mapped.clear;

		Put_Line("After map:");
		for s of seeds_storage loop
			Put_Line("seed: " & s.start'Image & " end: " & s.range_end'Image);
		end loop;
		Close (F);
	end Main;
end solutionpkp;
