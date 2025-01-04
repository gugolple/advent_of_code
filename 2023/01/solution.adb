with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Handling;
use  Ada.Characters.Handling;
with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;

procedure solution is
	F         : File_Type;
	File_Name : constant String := "input.txt";
	str       : Unbounded_String;
	c         : Character;
	first,last: Integer;
	total     : Integer := 0;
begin
	Open (F, In_File, File_Name);
	while not End_Of_File (F) loop
		str := To_Unbounded_String(Get_Line (F));
		first := 0;
		for J in 1 .. Length (str) loop
			c := Element (str, J);
			if Is_Digit(c) then
				if first = 0 then
					first := Integer'Value ((1 => c));
					last := integer'value ((1 => c));
				else
					last := integer'value ((1 => c));
				end if;
				Put(c);
			end if;
		end loop;
		Put_Line("");
		total := total + first * 10 + last;
		Put_Line("Current total:" & total'Image);
		-- Put_Line (To_String(str));
	end loop;
	Close (F);
end solution;
