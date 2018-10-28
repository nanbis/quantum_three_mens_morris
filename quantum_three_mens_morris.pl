% Neculae Vlad-Cristian
% 321CA

% Quantum Three Men's Morris

% Se propune o variantă cuantică a jocului "Three Men's Morris", joc
% în care doi adversari plasează și apoi mută câte trei piese pe o
% tablă cu 3x3 celule. Un jucător va avea trei piese albe, iar cel
% de-al doilea trei piese negre. Scopul fiecăruia este de a-și aranja
% piesele pe aceeași linie, pe aceeași coloană sau pe aceeași
% diagonală.
%
%  (1,1) -- (1,2) -- (1,3)
%    |    \   |    /   |
%    |     \  |   /    |
%  (2,1) -- (2,2) -- (2,3)
%    |     /  |   \    |
%    |    /   |    \   |
%  (3,1) -- (3,2) -- (3,3)
%
% Pe tablă sunt 9 celule.
%
% Jocul are două etape:
%
%  i. Plasarea pieselor
%
%     Alternativ, fiecare jucător va plasa câte o piesă în stare
%     cuantică pe tablă. Asta presupune alegerea a două celule în care
%     NU se află o piesă în stare clasică. Cele două celule vor deveni
%     legate la nivel cuantic (eng. entangled).
%
%     Atunci când se construiește un ciclu de celule legate cuantic
%     (entangled) jucătorul următor (nu cel care a creat ciclul) va
%     "măsura" ("observa") poziția ultimei piese plasate pe tablă (cea
%     care închis ciclul) și va alege în care dintre cele două celule
%     va rămâne aceasta. Observarea unei poziții duce la colapsarea
%     întregii componente a grafului din care face parte ciclul.
%
%     Etapa de plasare a pieselor se va termina atunci când fiecare
%     dintre cei doi jucători are câte trei piese indiferent în ce
%     stare.  (Se poate produce un ciclu în această etapă sau nu.)
%
% ii. Mutarea pieselor
%
%     Alternativ, fiecare jucător alege o piesă pe care să o mute
%     într-o celulă liberă (în care nu se află o piesă în stare
%     clasică). Dacă piesa se află în stare cuantică, atunci ambele
%     celule posibile se vor schimba. Dacă piesa se alfă în stare
%     clasică, atunci se va indica o pereche de celule vecine, iar
%     piesa va ajunge într-o stare cuantică. Efectul unei mutări lasă
%     piesa mutată în stare cuantică, iar cele două celule posibile
%     sunt, desigur, legate la nivel cuantic.
%
%     Atunci când se construiește un ciclu de celule legate cuantic
%     jucătorul următor (nu cel care a creat ciclul) va "măsura"
%     poziția ultimei piese mutate (cea care a închis ciclul) și va
%     alege în care dintre cele două celule posibile va rămâne
%     aceasta. Observarea unei poziții poate duce la observarea
%     pozițiilor mai multor piese.
%
%     Jocul se încheie atunci când cel puțin unul dintre jucători are
%     trei piese în stare clasică pe aceeași linie, coloană sau
%     diagonală.
%
% Reprezentări folosite:
%
%  - O celulă va fi reprezentată printr-un tuplu pos(X,Y) unde X,Y
%    sunt 1, 2 sau 3.
%
%  - O piesă va fi reprezentată diferit în funcție de starea ei:
%     classic/2   e.g.  classic(pos(2,2), white)
%     quantum/3   e.g.  quantum(pos(1,2), pos(3,1), black)
%
%  - O stare va fi o listă de piese (maximum șase)
%     e.g.: [classic(pos(2,2), white), classic(pos(1,5), black),
%            quantum(pos(1,3), pos(2,3), white)]

% ----------------------------------------------------------------------

% Rezolvați pe rând cerințele de mai jos!

% [Cerința 1] Să se scrie predicatul next_player/2 care descrie
% alternanța culorilor jucătorilor. black îi urmează lui white, iar
% white îi urmează lui black.

% next_player/2
% next_player(?Color1, ?Color2)

next_player(white, black).
next_player(black, white).

% ----------------------------------------------------------------------

% [Cerința 2] Scrieți un predicat cell(?Cell) care să fie adevărat
% pentru atunci când Cell este o structură pos(X,Y) reprezentând o
% celulă de pe tablă.

% cell/1
% cell(?Cell)

cell(pos(X, Y)):- between(1, 3, X), between(1, 3, Y).

% ----------------------------------------------------------------------

% [Cerința 3] Scrieți un predicat valid_pairs(+State, +Color, -Pairs)
% care să descrie legătura dintre starea State și toate perechile de
% celule în care jucătorul Color ar putea așeza o piesă în stare
% cuantică. Celulele ocupate de piese în stare clasică nu reprezintă
% soluții valide. De asemenea, nici perechile de celule deja legate
% cuantic de o piesă a aceluiași jucător nu reprezintă perchi valide.
% Lista Pairs trebuie să NU conțină și o pereche și inversa ei.


% valid_pairs/3
% valid_pairs(+State, +Color, -Pairs)

valid_pairs(S, C, P):-  findall((pos(X1, Y1), pos(X2, Y2)),
                            (
                                cell(pos(X1, Y1)),
                                cell(pos(X2, Y2)),
                                pos(X1, Y1) @< pos(X2, Y2),
                                \+ member(classic(pos(X1, Y1), _), S),
                                \+ member(classic(pos(X2, Y2), _), S),
                                \+ member(quantum(pos(X1, Y1), pos(X2, Y2), C), S)
                            ),
                            P).

%generez toate perechile de celule pe care nu exista o piesa clasica, si pe care
%nu exista o piesa quantum de culoarea ceruta

% ----------------------------------------------------------------------

% Cerința 4. Scrieți un predicat valid_moves(+State, +Color, -Moves)
% care leagă variabila Moves la lista tuturor mutărilor pe care le
% poate face jucătorul Color. O mutare poate fi de două feluri:
%
%  move(classic(pos(1,2), white), quantum(pos(1,3), pos(2,1), white))
%     sau
%  move(quantum(pos(3,3), pos(1,1), white),
%       quantum(pos(1,3), pos(2,1), white))


% valid_moves/3
valid_moves(S, C, M):-
    findall(move(classic(pos(X1,Y1), C), quantum(pos(X2,Y2), pos(X3,Y3), C)),
        (
            valid_pairs(S, C, P), member((pos(X2, Y2), pos(X3, Y3)) ,P), member(classic(pos(X1, Y1), C), S)
        ),
        M1),
    findall(move(quantum(pos(X1,Y1), pos(X2,Y2), C), quantum(pos(X3,Y3), pos(X4,Y4), C)),
        (
            valid_pairs(S, C, P), member((pos(X3, Y3), pos(X4, Y4)) ,P), member(quantum(pos(X1,Y1), pos(X2,Y2), C), S),
            pos(X1,Y1)\=pos(X3,Y3),
            pos(X1,Y1)\=pos(X4,Y4),
            pos(X2,Y2)\=pos(X3,Y3),
            pos(X2,Y2)\=pos(X4,Y4)
        ),
        M2),
    append(M1, M2, M).

% generez mai intai toate perechile de tipul(calssic, quantum), cu classic "apartinand" lui S
% si quantum "apartinand" lui valid_pairs de culoarea respectiva
% apoi generez toate perechile de tipul (quantum1, quantum2), cu qunatum1 "apartinand" lui S
% si quantum2 "apartinand" lui valid_pairs, verificand sa nu se suprapuna partial cele 2 quantum


% ----------------------------------------------------------------------

% Cerința 5. Scrieți un predicat winner(+State, -Winner) care produce
% legarea variabilei Winner la lista jucătorilor care au cele trei
% piese în stare clasică aliniate. Dacă nimeni nu a câștigat, winner
% eșuează (nu leagă Winner la lista vidă).


% winner/2
% winner(+State, -Colors)

winner(S, Col):- 
    findall(C,
        (
            member(classic(pos(X1, Y1), C), S),
            member(classic(pos(X2, Y2), C), S),
            member(classic(pos(X3, Y3), C), S),
            pos(X1, Y1) \= pos(X2, Y2),
            pos(X2, Y2) \= pos(X3, Y3),
            pos(X1, Y1) @< pos(X2, Y2),
            pos(X2, Y2) @< pos(X3, Y3),
            (
                (X1==X2, X2==X3);
                (Y1==Y2, Y2==Y3);
                (X1==Y1, X2==Y2, X3==Y3);
                (S1 is X1 + Y1, S2 is X2 + Y2, S3 is X3 + Y3, S1==S2, S2==S3)
            )
        ),
        Col),
    Col\=[].

% generez cate 3 piese de tipul classic din S si verific fie sa aiba ordonatele egale,
% fie sa aiba abscisele egale, fie suma dintre abscisa si ordonata, pentru fiecare, sa fie
% egale intre ele(adica sa fie pe diagonala secundara a tablei), fie sa aiba, fiecare, abscisa
% egala cu ordonata, adica sa fie pe diagonala principala

% ----------------------------------------------------------------------

% Cerința 6. Se cere scrierea unui predicat has_cycle(+State) care să
% fie adevărat dacă starea repsectivă conține un ciclu de celule
% legate cuantic.
%
% has_cycle([quantum(pos(1,1), pos(3,2), white),
%            quantum((2,1), (3,2), black),
%            quantum(pos(1,1), pos(2,1), white)])
%   => true.
%
% has_cycle([quantum(pos(1,1), pos(3,2), black),
%            quantum(pos(3,1), pos(3,2), white)])
%   => false.

% has_cycle/1
% has_cycle(+State)

% generam mai intai in Potentials toate piesele in stare quantum,
% si apelam o functie ajutatoare

has_cycle(S):- findall((X,Y,Z),
    member(quantum(X,Y,Z), S),
    Potentials), has_cycle_helper(Potentials, Potentials).

% iteram prin toate piesele de tip quantum si vedeam daca
% pot incepe un ciclu din prima casuta a piesei, folosind
% functia check_cycle

has_cycle_helper([], _):- fail.
has_cycle_helper([(S, F, C)|Ps], Pots):-
   check_cycle(S, S, Pots, [])
    -> true
    ; has_cycle_helper(Ps, Pots).

% check_cycle returneaza true daca avem un ciclu incepand
% din Start, folosind miscarile Moves. La fiecare pas, vream
% sa continui din Start, mergand spre End. Generez toate miscarile
% quantum nevizitate care au o casuta in pozitia Start. continui
% pe acea miscare, schimband Start cu celalalta casuta a piesei quantum
% curente, si incerc sa ajung spre End.

check_cycle(Start, End, Moves, Visited):- 

    member((Start, X, C), Moves), \+ member((Start, X, C), Visited),
    (
        X == End
        ->  true
        ;   delete(Moves, (Start, X, C), Moves1), check_cycle(X, End, Moves1, [(Start, X, C)|Visited])
    );
    member((Y, Start, C1), Moves), \+ member((Y, Start, C1), Visited),
    (
          Y == End
         ->  true
         ;   delete(Moves, (Y, Start, C1), Moves2), check_cycle(Y, End, Moves2, [(Y, Start, C1)|Visited])
    );
    fail.  


% ----------------------------------------------------------------------

% Cerința 7. Se cere scrierea unui predicat collapse(+State, +Piece,
% +Cell, -NewState) care să producă starea obținută prin "măsurarea"
% piesei Piece în celula Cell. Starea NewState este rezulatul
% colapsării stării State. Piece este garantat un membru al lui State.

% collapse/4
% collapse(+State, +Piece, +Cell, -NewState)

% collapse_helper(+State, +MoveList, -NewState)
% State reprezinta starea actuala, NewState este starea finala iar MoveList
% este lista de piese quantum care trebuie transformate in piese de tip classic.
% la fiecare pas, luam Head de MoveList, adica o piesa quantum si o inlocuiam
% in State cu o piesa de tip classic. Pe urma, generam toate celelalte piese
% quantum din State care aveau o casuta comuna cu noua piesa classic introdusa.
% Luam lista de piese nou generata si o apenduiam la MoveList. Apelam recursiv
% pana epuizam MoveList

collapse(S, quantum(X, Y, C), Z, NewS):- collapse_helper(S, [(quantum(X, Y, C), Z)], NewS).

collapse_helper(S, [(quantum(X, Y, C), Z)|T], NewS):- delete(S, quantum(X, Y, C), S1),
													S2=[classic(Z, C)|S1],
													findall((quantum(Z, Next1, C1), Next1),
														    (
                                                             member(quantum(Z, Next1, C1), S1),
															 \+ member((quantum(Z, Next1, C1), Next1), T)
                                                            ),
														  SRight),
													findall((quantum(Next2, Z, C2), Next2),
														  (
                                                            member(quantum(Next2, Z, C2), S1)
															,\+ member((quantum(Next2, Z, C2), Next2), T)
                                                          ),
														  SLeft),
													append(SRight, SLeft, NewT),
													append(T, NewT, FInalT),
													(
                                                        collapse_helper(S2, FInalT, NewS);NewS=S2
                                                    ). 



% ----------------------------------------------------------------------
% ----------------------------------------------------------------------


% Un jucător trebuie să definească trei strategii:
%
%   - alegerea unei perechi de celule neocupate în care să plaseze
%     următoarea piesă (în etapa de plasare a pieselor)
%
%        place(+State, +Color, +Step, +ValidPairs, -Pair)
%
%   - alegerea unei mutări
%
%        move(+State, +Color, +Step, +ValidMoves, -Move)
%
%   - observarea unei piese într-una din cele două poziții posibile
%
%        measure(+State, +Color, +Step, +Piece, -Cell)
%
%   În toate cele trei cazuri, State reprezintă starea curentă a
%   jocului, Color culoarea jucătorului curent, iar Step numărul
%   mutării (important deoarece jocul se termină după maximum 50 de
%   mutări).
%
%
% Mai jos este descris un jucător cu strategii aleatoare.

rand_place(_State, _Color, _Step, ValidPairs, (Cell1, Cell2)):-
    random_member((Cell1, Cell2), ValidPairs), !.

rand_measure(_State, _Color, _Step, Piece, Cell):-
    Piece = quantum(Cell1, Cell2, _LastColor),
    random_member(Cell, [Cell1, Cell2]), !.

rand_move(_State, _Color, _Step, ValidMoves, Move):-
    random_member(Move, ValidMoves), !.

% ----------------------------------------------------------------------

% [Cerința 8] Definiți strategiile pentru un jucător care să câștige în
% medie mai mult de 50% dintre jocur împotriva jucătorul random.

% generam o pereche de celule in care as putea sa pun o piesa quantum. verificam
% ca daca introduc respectiva piesa, nu creez un ciclu sau, daca creez un ciclu,
% oricum ar observa inamicul respectivul ciclu, nu ar genera o remiza sau o infrangere.
% pe urma, incercam ca cel putin o casuta a miscarii quantum de introdus sa fie pozitionata
% intre alte 2 casute ale altor miscari de tip quantum.

smart_place(State, Color, Step, ValidPairs, Pair):-
	(
			member((Cell1, Cell2), ValidPairs),
			Pair=(Cell1, Cell2),
			(
               \+ has_cycle([quantum(Cell1, Cell2, Color)|State]); enemy_does_not_win([quantum(Cell1, Cell2, Color)|State], quantum(Cell1, Cell2, Color), Color)
            ),
            extract_owned(Color, State, Owned),
            (
                (
                    Cell1 = pos(X, Y),
                    (
                        member(pos(X, Y1), Owned),
                        member(pos(X, Y2), Owned),
                        \+ Y1 == Y2
                    );
                    (
                        member(pos(X1, Y), Owned),
                        member(pos(X2, Y), Owned),
                        \+ X1 == X2
                    )
                    
                );
                (
                    Cell2 = pos(Xa, Ya),
                    (
                        member(pos(Xa, Ya1), Owned),
                        member(pos(Xa, Ya2), Owned),
                        \+ Ya1 == Ya2
                    );
                    (
                        member(pos(Xa1, Ya), Owned),
                        member(pos(Xa2, Ya), Owned),
                        \+ Xa1 == Xa2
                    )
                    
                );true
            )
	).

% enemy_does_not_win:
% State primit ca parametru este o stare cu un ciclu. Acest predicat
% verifica ca oricum ar fi observat acel ciclu, acesta nu va duce
% la o remiza sau o infrangere pentru Color.

enemy_does_not_win(State, quantum(Cell1, Cell2, Color), Color):- 
        Piece = quantum(Cell1, Cell2, Color),
        (
            collapse(State, Piece, Cell1, NoCycle1),
            (
                winner(NoCycle1, Winner1),
                Winner1==[Color];
                \+ winner(NoCycle1, Winner1)
            )
        );
        (
            collapse(State, Piece, Cell2, NoCycle2),            
            (
                winner(NoCycle2, Winner2),
                Winner2==[Color];
                \+ winner(NoCycle2, Winner2)
            )
        ).

% extract_owned(+Color, +State, -Owned) extrage din State
% toate celulele pe care se afla o piesa, quantum sau classic,
% apartinand lui Color

extract_owned(Color ,[], []).
extract_owned(Color, [quantum(C1, C2, Color)|S],[C1, C2|Owned]):- extract_owned(Color, S, Owned).
extract_owned(Color, [quantum(C1, C2, ColorF)|S],Owned):- extract_owned(Color, S, Owned).

extract_owned(Color, [classic(C, Color)|S], [C|Owned]):- extract_owned(Color, S, Owned).
extract_owned(Color, [classic(C, ColorF)|S], Owned):- extract_owned(Color, S, Owned).

% in smart_measure, alegeam mai intai o celula in care daca as fi "observat" ultima
% piesa adaugata in State(cea care a creat ciclul), aceasta nu ar fi dus la o infrangere
% sau o rezmiza. pe urma, incercam sa plasez respectiva celula astfel incat sa fie pe
% aceeasi linie sau coloana cu inca 2 alte celule controlate de Color.

smart_measure(State, Color, Step, Piece, Cell):-
	Piece = quantum(Cell1, Cell2, _LastColor),
	(
        (
            collapse(State, Piece, Cell1, NoCycle1),

            winner(NoCycle1, Winner1),
            (
                Winner1==[Color]
            ),

            extract_owned(Color, NoCycle1, Owned1),
            (
                (
                    member(pos(X, Y1), Owned1),
                    member(pos(X, Y2), Owned1),
                    \+ Y1 == Y2
                );
                (
                    member(pos(X1, Y), Owned1),
                    member(pos(X2, Y), Owned1),
                    \+ X1 == X2
                );
                true
            ),
            Cell=Cell1
        );
		(
			collapse(State, Piece, Cell2, NoCycle2),

			winner(NoCycle2, Winner2),
			(
				member(Color, Winner2),
				\+ member(_LastColor, Winner2)
			),

            extract_owned(Color, NoCycle2, Owned2),
            (
                (
                    member(pos(X, Y1), Owned2),
                    member(pos(X, Y2), Owned2),
                    \+ Y1 == Y2
                );
                (
                    member(pos(X1, Y), Owned2),
                    member(pos(X2, Y), Owned2),
                    \+ X1 == X2
                );
                true
            ),
			Cell=Cell2
		);
        (
            collapse(State, Piece, Cell1, NoCycle1),
            \+ winner(NoCycle1, Winner1),
            Cell = Cell1
        );
        (
            collapse(State, Piece, Cell2, NoCycle2),
            \+ winner(NoCycle2, Winner2),

            Cell = Cell2
        )
	).

% smart_move reprezinta un amestec intre smart_place si smart_measure.
% selectez o miscare care fie nu formeaza un ciclu, fie formeaza un ciclu
% care nu duce la o infrangere sau o remiza. Incerc pe urma sa plasez ambele
% dintre casutele miscarii quantum de introdus pe aceeasi linie si pe aceeasi
% coloana cu alte casute controlate(minim una pe aceeasi coloana si minim una
% pe aceeasi linie)

smart_move(State, Color, Step, ValidMoves, Move):-
    (
    	member(Move, ValidMoves),
    	Move=move(Last, Next),
    	delete(State, Last, NewState),
        Move = move(_, quantum(Cell1, Cell2, Color)),
        Pair=(Cell1, Cell2),
        (
           \+ has_cycle([quantum(Cell1, Cell2, Color)|State]); enemy_does_not_win([quantum(Cell1, Cell2, Color)|State], quantum(Cell1, Cell2, Color), Color)
        ),
        extract_owned(Color, State, Owned),
        (
            (
                (
                    Cell1 = pos(X, Y),
                    (
                        member(pos(X, Y1), Owned),
                        member(pos(X, Y2), Owned),
                        \+ Y1 == Y2
                    );
                    (
                        member(pos(X1, Y), Owned),
                        member(pos(X2, Y), Owned),
                        \+ X1 == X2
                    )
                    
                ),
                (
                    Cell2 = pos(Xa, Ya),
                    (
                        member(pos(Xa, Ya1), Owned),
                        member(pos(Xa, Ya2), Owned),
                        \+ Ya1 == Ya2
                    );
                    (
                        member(pos(Xa1, Ya), Owned),
                        member(pos(Xa2, Ya), Owned),
                        \+ Xa1 == Xa2
                    )
                    
                )
            );
            (
                Cell1 = pos(X, Y),
                Cell2 = pos(Xa, Ya),
                (
                    member(pos(X, Y1), Owned),
                    member(pos(Xa, Ya1), Owned),
                    member(pos(X1, Y), Owned),
                    member(pos(Xa, Ya2), Owned)
                )
            );true
        )
    ).


% ----------------------------------------------------------------------

% [Bonus]. Definiți strategiile pentru un jucător care să câștige în
% medie mai mult de 95% dintre jocuri împotriva jucătorul random.

% la predicatul bonus_X apelez smart_X

bonus_place(State, Color, Step, ValidPairs, Pair):-
    smart_place(State, Color, Step, ValidPairs, Pair).

bonus_measure(State, Color, Step, Piece, Cell):-
    smart_measure(State, Color, Step, Piece, Cell).

bonus_move(State, Color, Step, ValidMoves, Move):-
    smart_move(State, Color, Step, ValidMoves, Move).

% ----------------------------------------------------------------------
% ----------------------------------------------------------------------

% verbose.  %% Comentați linia aceasta pentru a vedea evoluția jocurilor.
verbose:- fail.

play(Player1, Player2, State, Color, Step, LastPiece, Winner):-
    Player1 = (PPlace, PMeasure, PMove),
    ( verbose -> format('-------------------- Pas [~w]~n', [Step]),
      format('Apel has_cycle(~w)...~n', [State]); true ),
    ( has_cycle(State) ->
      ( verbose -> format('Apel ~w(~w, ~w, ~w, ~w, Cell)...~n',
           [PMeasure, State, Color, Step, LastPiece]) ; true ),
      ( call(PMeasure, State, Color, Step, LastPiece, Cell) ->
    ( verbose -> format('Apel collapse(~w, ~w, ~w, NoCycle)...~n',
         [State, LastPiece, Cell]); true ),
        ( collapse(State, LastPiece, Cell, NoCycle) ->
      true
    ; format('collapse(~w, ~w, ~w, NoCycle) a eșuat.~n',
         [State, LastPiece, Cell]),
      !, fail)
      ; format('~w(~w, ~w, ~w, ~w, Cell) a eșuat.~n',
           [PMeasure, State, Color, Step, LastPiece]),
    !, fail)
    ; NoCycle = State),
    ( winner(NoCycle, Winner), !
    ; Step =:= 50, !, Winner = [white, black],
      (   verbose -> format('Am ajuns la pasul 50.~n'); true )
    ; ( length(NoCycle, 6) ->
    ( verbose -> format('Apel valid_moves(~w, ~w, ValidMoves).~n',
         [NoCycle, Color]); true ),
        ( valid_moves(NoCycle, Color, ValidMoves)->
      ( verbose -> format('Apel ~w(~w, ~w, ~w, ~w, Move)...~n',
           [PMove, NoCycle, Color, Step, ValidMoves]); true ),
          ( call(PMove, NoCycle, Color, Step, ValidMoves, Move) ->
        Move = move(OldPiece, NewPiece),
        select(OldPiece, NoCycle, NewPiece, NextState), !
      ; format('~w(~w, ~w, ~w, ~w, Move) a eșuat.~n',
           [PMove, NoCycle, Color, Step, ValidMoves]),
        !, fail)
    ; format('valid_moves(~w, ~w, ValidMoves) a eșuat.~n',
         [NoCycle, Color]),
      !, fail)
      ; (verbose -> format('Apel valid_pairs(~w, ~w, ValidPairs)...~n',
                               [NoCycle, Color]); true),
        ( valid_pairs(NoCycle, Color, ValidPairs) ->
          ( verbose -> format('Apel ~w(~w, ~w, ~w, ~w, (Cell1, Cell2)).~n',
           [PPlace, NoCycle, Color, Step, ValidPairs]); true),
      ( call(PPlace, NoCycle, Color, Step, ValidPairs, (Cell1, Cell2)) ->
        NewPiece = quantum(Cell1, Cell2, Color),
        NextState = [NewPiece | NoCycle], !
      ; format('~w(~w, ~w, ~w, ~w, (Cell1, Cell2)) a eșuat.~n',
           [PPlace, NoCycle, Color, Step]),
        !, fail)
    ; format('valid_pairs(~w, ~w, ValidPairs) a eșuat.~n', [NoCycle, Color]),
      !, fail) ),
      next_player(Color, NextColor), Step1 is Step + 1, !,
      play(Player2, Player1, NextState, NextColor, Step1, NewPiece, Winner) ).


play_against_random(Strategy, Winner):-
    %% Player is black, Rand is white
    Player = (Strategy, black),
    Random = ((rand_place, rand_measure, rand_move), white),
    random_permutation([Player, Random], [(Player1, Color1),(Player2, _)]),
    play(Player1, Player2, [], Color1, 0, none, Winner).


score_against_random(Strategy, Score):-
    score_against_random(Strategy, 1000, 0, 0, 0, WinsNo, DrawsNo, LosesNo),
    format(' Black: ~d, Draws: ~d, White: ~d. ', [WinsNo, DrawsNo, LosesNo]),
    Score is WinsNo / 1000.0.

score_against_random(_, 0, B, D, W, B, D, W):- !.
score_against_random(Strategy, N1, B1, D1, W1, B, D, W):-
    play_against_random(Strategy, Winner),
    (Winner = [black] -> B2 is B1 + 1 ; B2 = B1),
    (Winner = [white] -> W2 is W1 + 1 ; W2 = W1),
    (Winner = [_, _] -> D2 is D1 + 1 ; D2 = D1),
    N2 is N1 - 1,
    ( verbose ->
      format('>>>>>>>>>>> Mai sunt de jucat ~w jocuri. Strategia a câștigat ~w jocuri, Random ~w jocuri, ~w remize. ~n',
         [N2, B2, W2, D2])
    ; true ),
    score_against_random(Strategy, N2, B2, D2, W2, B, D, W).
