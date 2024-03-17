with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Handling; use  Ada.Characters.Handling;
with Ada.Strings.Unbounded; use  Ada.Strings.Unbounded;

procedure solution is
	F         : File_Type;
	File_Name : constant String := "input.txt";
	str       : Unbounded_String;
begin
	Open (F, In_File, File_Name);
	while not End_Of_File (F) loop
		str := To_Unbounded_String(Get_Line (F));
		Put_Line (To_String(str));
	end loop;
	Close (F);
end solution;
