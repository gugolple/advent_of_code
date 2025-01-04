with Ada.Strings.Unbounded; use  Ada.Strings.Unbounded;
with Ada.Containers.Ordered_Sets;
package solutionpkp is
	function replace_string(
		source : in Unbounded_String;
		value  : in String
		) return Unbounded_String;
	procedure Main;
end solutionpkp;

