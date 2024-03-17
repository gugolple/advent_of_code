with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use  Ada.Strings.Unbounded;
with Ada.Strings.Maps; use Ada.Strings.Maps;

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Vectors;

-- Priority queue to manage pulses efficiently
with Ada.Containers.Unbounded_Priority_Queues;
with Ada.Containers.Indefinite_Holders;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Unchecked_Deallocation;

with solutionpk; use solutionpk;

package body solutionpk is
        type PulseQueue is record
            id: Long_Integer;
            origin: Unbounded_String;
            destination: Unbounded_String;
            hi_low : Boolean;
        end record;

	type PulseQueue_Access is Access PulseQueue;

	procedure Free is new Ada.Unchecked_Deallocation
	      (Object => PulseQueue, Name => PulseQueue_Access);

        package PulseQueue_Holder is
            new Ada.Containers.Indefinite_Holders (
                Element_Type => PulseQueue_Access
                );
	use PulseQueue_Holder;

        package PulseQueue_Interface is
            new Ada.Containers.Synchronized_Queue_Interfaces (
                Element_Type => PulseQueue_Holder.Holder
                );
	use PulseQueue_Interface;

	function GetPriority(E: PulseQueue_Holder.Holder) return Long_Integer is
            result : Long_Integer;
	begin
		return E.Element.id;
	end;

        function DS_Before(L, R: Long_Integer) return boolean
        is
        begin
            return L<R;
        end;
        package PulseQueueQueue is 
		new Ada.Containers.Unbounded_Priority_Queues(
			Queue_Interfaces => PulseQueue_Interface,
                        Queue_Priority => Long_Integer,
                        Get_Priority => GetPriority,
                        Before => DS_Before
			);
	use PulseQueueQueue;

	package Unbounded_StringVector is new
		Ada.Containers.Vectors
			(Index_Type => Natural,
			Element_Type => Unbounded_String 
			);
	use Unbounded_StringVector;

	package NameMemMap is 
		new Ada.Containers.Indefinite_Ordered_Maps(
			Key_Type => Unbounded_String,
			Element_Type => Boolean
			);
	use NameMemMap;

        type ModuleType is (Broadcaster, FlipFlop, Conjuction);
        type Module is record
            mod_type: ModuleType;
            destinations: Unbounded_StringVector.Vector;
            inputMemory: NameMemMap.Map;
            memory : Boolean := False;
        end record;

	package NameModMap is 
		new Ada.Containers.Indefinite_Ordered_Maps(
			Key_Type => Unbounded_String,
			Element_Type => Module 
			);
	use NameModMap;
	
	type StateMachine is (name, destinations);

	state: StateMachine;

        my_mod : Module;
	mod_name : Unbounded_String;
	procedure statemachine_proc(
		capture: in Unbounded_String
		)
	is
		current_number : Integer := 0;
		trimmed_string : Unbounded_String;
	begin
		trimmed_string := Trim(capture, To_Set(' '), To_Set(":;, "));
		--Put_Line("Token: " & To_String(trimmed_string) & " State: " & state'Image);
		if trimmed_string = ""  or trimmed_string = "->" then
			return;
		end if;
		case state is
			when name =>
                                if Element(trimmed_string,1) = '%' then
                                    my_mod.mod_type := FlipFlop;
                                    mod_name := To_Unbounded_String(Slice(trimmed_string,2,Length(trimmed_string)));
                                elsif Element(trimmed_string,1) = '&' then
                                    my_mod.mod_type := Conjuction;
                                    mod_name := To_Unbounded_String(Slice(trimmed_string,2,Length(trimmed_string)));
                                else
                                    my_mod.mod_type := Broadcaster;
                                    mod_name := trimmed_string;
                                end if;
				state := destinations;
			when destinations =>
                            my_mod.destinations.append(trimmed_string);
		end case;
	end statemachine_proc;


	procedure line_proc
		(capture: in Unbounded_String)
	is
		idx,idxl  : Natural := 1;
		Search_Set : constant Character_Set := To_Set(" ,");
	begin
		-- Capture destinations data
		idx := Index(
			Source => capture,
			Set => Search_Set,
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
				Set => Search_Set,
				From => idxl
				);
		end loop;
		statemachine_proc(To_Unbounded_String(Slice(capture,idxl,Length(capture))));
	end line_proc;

        procedure initializeNameMod( nm : in out NameModMap.Map) is
        begin
            for it in nm.Iterate loop
                for I of nm(it).destinations loop
                    if nm.Contains(I) then
                        if nm(I).mod_type = Conjuction then
                            nm(I).inputMemory.insert(Key(it), False);
                        end if;
                    end if;
                end loop;
            end loop;
        end initializeNameMod;

        procedure iteratePulse( nm : in out NameModMap.Map; total_low,total_high : in out Long_Integer) is
            id : Long_Integer := Long_Integer'First;
	    pendig_queue : PulseQueueQueue.Queue;
            hold_val : PulseQueue_Holder.Holder;
	    current_pulse: PulseQueue_Access;
	    temp_pulse: PulseQueue_Access;
            npv: Boolean;
        begin
            current_pulse := new PulseQueue;
            current_pulse.id := id;
            id := id + 1;
            current_pulse.origin := To_Unbounded_String("broadcaster");
            current_pulse.destination := To_Unbounded_String("broadcaster");
            current_pulse.hi_low := False;
	    pendig_queue.Enqueue(To_Holder(current_pulse));
            while pendig_queue.Current_Use > 0 loop
                pendig_queue.Dequeue(hold_val);
                current_pulse := Element(hold_val);
                Clear(hold_val);
                if current_pulse.hi_low = True then
                    total_high := total_high + 1;
                else
                    total_low := total_low + 1;
                end if;
                if nm.Contains(current_pulse.destination) then
                    case nm(current_pulse.destination).mod_type is
                        when Broadcaster =>
                            for I of nm(current_pulse.destination).destinations loop
                                temp_pulse := new PulseQueue;
                                temp_pulse.id := id;
                                id := id + 1;
                                temp_pulse.origin := current_pulse.destination;
                                temp_pulse.destination := I;
                                temp_pulse.hi_low := False;
                                pendig_queue.Enqueue(To_Holder(temp_pulse));
                            end loop;
                        when FlipFlop =>
                            if current_pulse.hi_low = False then
                                npv := not nm(current_pulse.destination).memory;
                                nm(current_pulse.destination).memory := npv;
                                for I of nm(current_pulse.destination).destinations loop
                                    temp_pulse := new PulseQueue;
                                    temp_pulse.id := id;
                                    id := id + 1;
                                    temp_pulse.origin := current_pulse.destination;
                                    temp_pulse.destination := I;
                                    temp_pulse.hi_low := npv;
                                    pendig_queue.Enqueue(To_Holder(temp_pulse));
                                end loop;
                            end if;
                        when Conjuction =>
                            nm(current_pulse.destination).inputMemory(current_pulse.origin) := current_pulse.hi_low;
                            npv := False;
                            for it in nm(current_pulse.destination).inputMemory.Iterate loop
                                if nm(current_pulse.destination).inputMemory(it) = False then
                                    npv := True;
                                    exit;
                                end if;
                            end loop;
                            for I of nm(current_pulse.destination).destinations loop
                                temp_pulse := new PulseQueue;
                                temp_pulse.id := id;
                                id := id + 1;
                                temp_pulse.origin := current_pulse.destination;
                                temp_pulse.destination := I;
                                temp_pulse.hi_low := npv;
                                pendig_queue.Enqueue(To_Holder(temp_pulse));
                            end loop;
                    end case;
                end if;
                Free(current_pulse);
            end loop;
        end iteratePulse;

        function countPulses( nm : in out NameModMap.Map) return Long_Integer is
            low, high : Long_Integer := 0;
        begin
            for I in 1..1000 loop
                iteratePulse(nm, low, high);
            end loop;
            return low * high;
        end countPulses;


	procedure Main is
		F         : File_Type;
		File_Name : constant String := "input.txt";
		str       : Unbounded_String;
                nm : NameModMap.Map;
	begin
		Open (F, In_File, File_Name);
		while not End_Of_File (F) loop
			-- Capture the line
			state := name;
                        my_mod.destinations.clear;
			str := To_Unbounded_String(Get_Line (F));
			line_proc(str);

                        nm.Insert(mod_name, my_mod);
		end loop;
		Close (F);

                initializeNameMod(nm);

                Put_Line("Memory!");
                for it in nm.Iterate loop
                    Put_Line("Module: " & To_String(Key(it)));
                    Put_Line("Destinations!");
                    for I of nm(it).destinations loop
                            Put(To_String(I) & ", ");
                    end loop;
                    Put_Line("");
                    Put_Line("Input memory!");
                    for sit in nm(it).inputMemory.Iterate loop
                            Put(To_String(Key(sit)) & ", ");
                    end loop;
                    Put_Line("");
                    Put_Line("End");
                    Put_Line("");
                end loop;

                Put_Line("Pulses: " & countPulses(nm)'Image);
	end Main;
end solutionpk;
