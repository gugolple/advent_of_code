with Ada.Strings.Unbounded; use  Ada.Strings.Unbounded;
package solutionpkp is
	type StateMachine is (hand, bet);
	type Card is (J, N2, N3, N4, N5, N6, N7, N8, N9, T, Q, K, A);
	type HandType is (HighCard, TwoKind, TwoPair, ThreeKind, FullHouse, FourKind, FiveKind);
	type Cards is array (1 .. 5) of Card;
	procedure statemachine_proc(capture: in Unbounded_String);
	procedure line_proc (capture: in Unbounded_String);
	procedure Main;
end solutionpkp;

