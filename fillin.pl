%	File       : fillin.pl
%	Author     : Student Name: Xiong1	StudentId: 722890
%	Purpose    : Program for filling in puzzle. A Corssword puzzle and a list
%                of words are given. This program is supposed to fill words in         
%                wordList to the puzzle. Puzzle consists of three kinds of 
%                squares which are to be written, filled in solid and not to 
%                be written in. After filling in the puzzle, every word will 
%                be placed exactly once and words corss one another. 

%	The SWI Prolog Libaray provides two different, imcompatible transpose/2 
%	predicate. To use the suitable one for the program, the following line 
%	need to be added. This defines the predicate transpose(Matrix, Matrix0)
%	that holds when Matrix0 and Matrix are lists of lists, and the "columns"
%	of each are the "rows" of the other.
:- ensure_loaded(library(clpfd)). 

%	PuzzleFile and WordlistFile are the puzzle and words files need to be read
%	in. SolutionFile is the output file name.
main(PuzzleFile, WordlistFile, SolutionFile) :-
	read_file(PuzzleFile, Puzzle),
	read_file(WordlistFile, Wordlist),
	valid_puzzle(Puzzle),
	solve_puzzle(Puzzle, Wordlist, Solved),
	print_puzzle(SolutionFile, Solved).

%	Filename is the name of file need to be read in, Content is a list of 
%	string, which is the content of the file. 
read_file(Filename, Content) :-
	open(Filename, read, Stream),
	read_lines(Stream, Content),
	close(Stream).

%	Read all lines in the file.
read_lines(Stream, Content) :-
	read_line(Stream, Line, Last),
	(   Last = true
	->  (   Line = []
	    ->  Content = []
	    ;   Content = [Line]
	    )
	;  Content = [Line|Content1],
	    read_lines(Stream, Content1)
	).

%	Read a line in the file.
read_line(Stream, Line, Last) :-
	get_char(Stream, Char),
	(   Char = end_of_file
	->  Line = [],
	    Last = true
	; Char = '\n'
	->  Line = [],
	    Last = false
	;   Line = [Char|Line1],
	    read_line(Stream, Line1, Last)
	).

%	Print the puzzle to the SolutionFile.
print_puzzle(SolutionFile, Puzzle) :-
	open(SolutionFile, write, Stream),
	maplist(print_row(Stream), Puzzle),
	close(Stream).

% 	Print a row into the stream.
print_row(Stream, Row) :-
	maplist(put_puzzle_char(Stream), Row),
	nl(Stream).

%	Print a character to the the stream. 
put_puzzle_char(Stream, Char) :-
	(   var(Char)
	->  put_char(Stream, '_')
	;   put_char(Stream, Char)
	).

%	Ensure the puzzle is valid, which means all rows of puzzle are the same
%	length. 
valid_puzzle([]).
valid_puzzle([Row|Rows]) :-
	maplist(samelength(Row), Rows).

%	Ensure two lists have the same length.
samelength([], []).
samelength([_|L1], [_|L2]) :-
	same_length(L1, L2).

%	The idea of sloving the puzzle is replacing all undersocre to logical
%	variable of the puzzle, and finding out all slots. Then we just need to 
%   fill the words in the wordlist to the slots.
solve_puzzle(Puzzle, Wordlist, Puzzle_Replaced):-
	undersocre_to_var(Puzzle, Puzzle_Replaced),
	slots_of_puzzle(Puzzle_Replaced, Slots),
	fillin_words_to_slots(Slots, Wordlist).

%	Replace undersocre character to logical variable in the puzzle. 
%	A puzzle consisted of a number of rows. 
undersocre_to_var([],[]).
undersocre_to_var(Puzzle, Puzzle_Replaced):-
	maplist(replace_row, Puzzle, Puzzle_Replaced).

%	Replace undersocre character to logical variable in a row of puzzle.
%	A row of puzzle consists of a list of characters.
replace_row([],[]).
replace_row(Puzzle_Row, Puzzle_Row_Replaced):-
	maplist(replace_underscore, Puzzle_Row, Puzzle_Row_Replaced).

%	Replace undersocre character to logical variable.
replace_underscore(X, Y):-
	( X == '_' -> 
		Y = _
	    ; Y = X
	).

%	To find the all slots of a puzzle. First, we can use slots_of_puzzle_row/3 
%	to get all horiztontal slots. Then we just transpose the puzzle, and use 
% 	the same predicate to get veritcal slots. Besides, we need append those 
%	two kinds of slots into one. After filtering the slot with length smaller 
%	than 1, we could get all slots of the puzzle.  
slots_of_puzzle(Puzzle_Replaced, Slots):-
	slots_of_puzzle_row(Puzzle_Replaced, Horiztontal_Slots),
	transpose(Puzzle_Replaced, Transpose_Puzzle_Replaced),
	slots_of_puzzle_row(Transpose_Puzzle_Replaced, Vertical_Slots),
	append(Horiztontal_Slots, Vertical_Slots, AllSlots),
	qualify_slot_of_allslots(AllSlots, Slots).

%	Get the slots of puzzle row by row. 
slots_of_puzzle_row([],[]).
slots_of_puzzle_row([HeadRow|TailRows], Slots):-
	slot_of_row(HeadRow, [], RowSlot),
	append(RowSlot, Slots1, Slots),
	slots_of_puzzle_row(TailRows, Slots1).

%	Get the slots of one row. If character X is not #, just add this character 
%	to the slot accumulator called ThisSlot. Otherwise, reaching the end of a 
%	slot,we need add ThisSlot to Slots and set the slot accumulator to []. 
slot_of_row([],[],[]).
slot_of_row([], ThisSlot, [ThisSlot]):-
	ThisSlot \= []. 
slot_of_row([X|XS], ThisSlot, Slots):- 
	(X \== '#' ->
		append(ThisSlot, [X], ThisSlot1),
		slot_of_row(XS,ThisSlot1, Slots)
		;Slots = [ThisSlot|Slots1],
		slot_of_row(XS, [], Slots1)
	).

%	The length of a slot need to be greater than 1. 
qualify_length(Slot):-
	length(Slot, Y),
	Y > 1.

%	Get the slots with length greater than 1 from all slots.
qualify_slot_of_allslots([X|XS], Slots):-
	filter(qualify_length, [X|XS], Slots).

%	Define a filter/3 predicate in Prolog, which has similar effect as the 
%   filter function in Haskell. This predicate could be used to filter a 
%	list according to the goal. 
filter(_, [], []).
filter(Pred, [X|XS], YS):-
	( call(Pred, X) ->
		YS = [X|YS1]
		;YS = YS1
	),filter(Pred, XS, YS1).

%	Inverse version of predicate filter/3, which has opposite effect.  
inverse_filter(_, [], []).
inverse_filter(Pred, [X|XS], YS):-
	( call(Pred, X) ->
		YS = YS1
		;YS = [X|YS1]
	),inverse_filter(Pred, XS, YS1).

%	After getting all slots from puzzle, we need to fill the words in wordlist 
%	into slots. The idea of the process is choosing the best slot from all 
%	slots, which matches the least number of words. Then select one word from 
% 	all matching words, and fill it into the best slot. Using the rest of 
%	wordlist and slots do this procedure again until there is no word in the 
%	wordlist. 
fillin_words_to_slots([],[]).
fillin_words_to_slots(Slots, WordList):-	
	choose_slot_from_slots(Slots, WordList, ChosenSlot),
	matching_words_of_wordlist(ChosenSlot, WordList, MatchingWords),
	member(ChosenWord, MatchingWords),
	ChosenSlot = ChosenWord,
	rest_words_of_wordlist(ChosenWord, WordList, RestWords),
	rest_slots_of_slots(ChosenSlot, Slots, RestSlots),
	fillin_words_to_slots(RestSlots, RestWords).

% 	To Choose the slot matching the least number of words. At first, we set 
%	the first slot as the slot with fewest match words and matching words 
%	number of first slot as the fewest match number. 
choose_slot_from_slots([HeadSlot |TailSlots], WordList, ChosenSlot):-
	setof(HeadSlot, member(HeadSlot, WordList), MatchingWords),
	length(MatchingWords, MatchingNumber),
	choose_slot_from_slots(TailSlots, MatchingNumber, WordList, HeadSlot, 
	ChosenSlot).

%	Porcess all slots to get the slot matching the least number of words. 
%	CurrentChosenSlot is the slot with fewest matching words so far.
choose_slot_from_slots([], _, _, ChosenSlot, ChosenSlot).
choose_slot_from_slots([HeadSlot|TailSlots], FewestMatch, WordList, 
	CurrentChosenSlot, ChosenSlot):-	
	%	To get the matching words list of a slot, we combine predicate setof/3
	%	and member/2. Then we use length/2 to get length of the matching words 
	%	list, which is the number of matching words.  
	setof(HeadSlot, member(HeadSlot, WordList), MatchingWords),
	length(MatchingWords, MatchingNumber),
	(MatchingNumber < FewestMatch -> 
		CurrentChosenSlot1 = HeadSlot,
		FewestMatch1 = MatchingNumber
		;CurrentChosenSlot1 = CurrentChosenSlot,
		FewestMatch1 = FewestMatch
		), choose_slot_from_slots(TailSlots, FewestMatch1, WordList, 
		CurrentChosenSlot1, ChosenSlot).

% 	Can not use filter( =(ChosenSlot), WordList, MatchingWords) here, we are 
%	using the inverse version of filter, because unbound variables is easy to 
%	accidentally unify with something we don't mean to. \= is used instead of 
%	= .
matching_words_of_wordlist(ChosenSlot, WordList, MatchingWords):-
	inverse_filter( \=(ChosenSlot), WordList, MatchingWords).

%	Get the rest of words after filling one word of matching words into the 
%	chosen slot. 
rest_words_of_wordlist(ChosenWord, WordList, RestWords):-
	filter(\==(ChosenWord), WordList, RestWords).

%	Get the rest of slots after filling one word of matching words into the 
%	chosen slot. 
rest_slots_of_slots(ChosenSlot, Slots, RestSlots):-
	filter(\==(ChosenSlot), Slots, RestSlots).


























