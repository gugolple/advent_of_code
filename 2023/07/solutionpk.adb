with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use  Ada.Strings.Unbounded;
with Ada.Strings.Maps; use Ada.Strings.Maps;

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Ordered_Maps;

with solutionpk; use solutionpk;

package body solutionpk is
	type HandBet is record
		Hand: Cards;
		HandTypeC: HandType;
		Bet: Integer;
	end record;


	function "<"(Left,Right: HandBet) return Boolean is
		res: Boolean := false;
	begin
		if Right.HandTypeC > Left.HandTypeC then
			res := true;
		elsif Right.HandTypeC = Left.HandTypeC then
			for I in Right.Hand'Range loop
				if Right.Hand(I) > Left.Hand(I) then
					res := True;
					exit;
				elsif Right.Hand(I) < Left.Hand(I) then
					exit;
				end if;
			end loop;
		end if;
		return res;
	end "<";

	package HandBetVector is new
		Ada.Containers.Vectors(
			Index_Type => Natural,
			Element_Type => HandBet
			);
	package HandBetVectorSorter is new HandBetVector.Generic_Sorting;


	package HandRepresentation is 
		new Ada.Containers.Indefinite_Ordered_Maps(
			Key_Type => Card,
			Element_Type => Integer
			);
	
	handbet_vector : HandBetVector.Vector;
	last_hand : Cards;
	last_handtype : HandType;

	state: StateMachine;
	procedure statemachine_proc(
		capture: in Unbounded_String
		)
	is
		current_number : Integer := 0;
		trimmed_string : Unbounded_String;
		current_bet : HandBet;
		current_card : Card;
		repr : HandRepresentation.Map;
	begin
		trimmed_string := Trim(capture, To_Set(' '), To_Set(":;, "));
		--Put_Line("Token: " & To_String(trimmed_string) & " State: " & state'Image);
		if trimmed_string = "" then
			return;
		end if;
		case state is
			when hand =>
				for I in 1 .. Length(trimmed_string) loop
					case element(trimmed_string, I)	is
						when '2' => current_card := N2;
						when '3' => current_card := N3;
						when '4' => current_card := N4;
						when '5' => current_card := N5;
						when '6' => current_card := N6;
						when '7' => current_card := N7;
						when '8' => current_card := N8;
						when '9' => current_card := N9;
						when 'T' => current_card := T;
						when 'J' => current_card := J;
						when 'Q' => current_card := Q;
						when 'K' => current_card := K;
						when 'A' => current_card := A;
						when others => Put_Line("fock");
					end case;
					if not repr.contains(current_card) then
						repr.insert(current_card, 1);
					else
						repr(current_card) := repr(current_card) + 1;
					end if;
					last_hand(I) := current_card;
				end loop;

				case HandRepresentation.Length(repr) is
					when 1 => last_handtype := FiveKind;
					when 2 =>
						if HandRepresentation.first_element(repr) = 1 
							or HandRepresentation.first_element(repr) = 4 then
							last_handtype := FourKind;
						else
							last_handtype := FullHouse;
						end if;
					when 3 =>
						last_handtype := TwoPair;
						for K in repr.Iterate loop
							if repr(K) > 2 then
								last_handtype := ThreeKind;
							end if;
						end loop;
					when 4 => last_handtype := TwoKind;
					when 5 => last_handtype := HighCard;
					when others => Put_Line("DAFOK");
				end case;
				state := bet;
			when bet =>
				current_number := Integer'Value(To_String(trimmed_string));
				
				current_bet.hand := last_hand;
				current_bet.bet := current_number;
				current_bet.HandTypeC := last_handtype;
				
				handbet_vector.append(current_bet);
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
	procedure Main is
		F         : File_Type;
		File_Name : constant String := "input.txt";
		str       : Unbounded_String;
		total     : Integer := 0;
		HB        : HandBet;
	begin
		Open (F, In_File, File_Name);
		while not End_Of_File (F) loop
			-- Capture the line
			state := hand;
			str := To_Unbounded_String(Get_Line (F));
			Put_Line(To_String(str));
			line_proc(str);
		end loop;
		HandBetVectorSorter.Sort(handbet_vector);
		for I in handbet_vector.first_index .. handbet_vector.last_index loop
			HB := handbet_vector(I);
			for H of HB.hand loop
				Put(H'Image);
			end loop;
			Put_Line(" Bet: " & HB.bet'Image & " HandType: " & HB.HandTypeC'Image);
			total := total + (I+1) * HB.bet;
		end loop;
		Put_Line("Total: " & Total'Image);
		Close (F);
	end Main;
end solutionpk;
