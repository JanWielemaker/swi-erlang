:- module(io, []).

write(Term) :-
	distribution:echo(Term).
	
writeln(Term) :-
	distribution:echo(Term).