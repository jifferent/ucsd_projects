%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helpers

%isin(X,L) is true if X appears in L
isin(X,[X|_]).
isin(X,[_|T]) :- isin(X,T).

%hint: use append, reverse, bagof judiciously.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Problem 1: Rules

zip(L1,L2,L3) :- helper_zip(L1, L2, L3).
helper_zip([], L, L) :- !.
helper_zip([ X | Xs ], L, [ X | Zs ]) :- zip(Xs, L, Zs).
helper_zip(L, [ Y | Ys ], [ Y | Zs ]) :- zip(L, Ys, Zs).

assoc(L,X,Y) :- helper_assoc(L, X, Y).
helper_assoc([ [X, Y] | _ ], X, Y).
helper_assoc([ _ | Ts ], X, Y) :- assoc(Ts, X, Y).

remove_duplicates(L1,L2) :- reverse(L1, Rl1), reverse(L2, Rl2), helper_duplicates(Rl1, Rl2), !.
helper_duplicates([], []).
helper_duplicates([ H | T ], [ H | R ]) :- not(isin(H, T)), helper_duplicates(T, R).
helper_duplicates([ H | T ], L2) :- isin(H, T), helper_duplicates(T, L2).

union(L1,L2,L3) :- append(L1, L2, L), remove_duplicates(L, L3).

intersection(L1,L2,L3) :- helper_intersection(L1, L2, L3).
helper_intersection([], _, []).
helper_intersection([ X | Xs ], L2, [ X | Ys ]) :- isin(X, L2), intersection(Xs, L2, Ys).
helper_intersection([ X | Xs ], L2, L3) :- not(isin(X, L2)), intersection(Xs, L2, L3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Problem 2: Facts

cost(carne_asada,3).
cost(lengua,2).
cost(birria,2).
cost(carnitas,2).
cost(adobado,2).
cost(al_pastor,2).
cost(guacamole,1).
cost(rice,1).
cost(beans,1).
cost(salsa,1).
cost(cheese,1).
cost(sour_cream,1).
cost(taco,1).
cost(tortilla,1).


ingredients(carnitas_taco, [taco,carnitas, salsa, guacamole]).
ingredients(birria_taco, [taco,birria, salsa, guacamole]).
ingredients(al_pastor_taco, [taco,al_pastor, salsa, guacamole, cheese]).
ingredients(guacamole_taco, [taco,guacamole, salsa,sour_cream]).
ingredients(al_pastor_burrito, [tortilla,al_pastor, salsa]).
ingredients(carne_asada_burrito, [tortilla,carne_asada, guacamole, rice, beans]).
ingredients(adobado_burrito, [tortilla,adobado, guacamole, rice, beans]).
ingredients(carnitas_sopa, [sopa,carnitas, guacamole, salsa,sour_cream]).
ingredients(lengua_sopa, [sopa,lengua,beans,sour_cream]).
ingredients(combo_plate, [al_pastor, carne_asada,rice, tortilla, beans, salsa, guacamole, cheese]).
ingredients(adobado_plate, [adobado, guacamole, rice, tortilla, beans, cheese]).

taqueria(el_cuervo, [ana,juan,maria],
        [carnitas_taco, combo_plate, al_pastor_taco, carne_asada_burrito]).

taqueria(la_posta,
        [victor,maria,carla], [birria_taco, adobado_burrito, carnitas_sopa, combo_plate, adobado_plate]).

taqueria(robertos, [hector,carlos,miguel],
        [adobado_plate, guacamole_taco, al_pastor_burrito, carnitas_taco, carne_asada_burrito]).

taqueria(la_milpas_quatros, [jiminez, martin, antonio, miguel],
        [lengua_sopa, adobado_plate, combo_plate]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Problem 2: Rules

available_at(X,L) :- taqueria(L, _, Xs), isin(X, Xs).

multi_available(X) :- bagof(L, available_at(X, L), Ls), length(Ls, K), K > 1.

work_at(X, L) :- taqueria(L, Xs, _), isin(X, Xs).

overworked(X) :- bagof(L, work_at(X, L), Ls), length(Ls, K), K > 1.

total_cost(X,K) :- ingredients(X, L), helper_total_cost(L, K).
helper_total_cost([], 0).
helper_total_cost([H | T], K) :- cost(H, V), helper_total_cost(T, K1), K is K1 + V.

has_ingredients(X,Is) :- ingredients(X, L), helper_has_ingredients(Is, L).
helper_has_ingredients([], _).
helper_has_ingredients([I | Is], L) :- isin(I, L), helper_has_ingredients(Is, L).

avoids_ingredients(X,Is) :- ingredients(X, L), helper_avoids_ingredients(Is, L).
helper_avoids_ingredients([], _).
helper_avoids_ingredients([I | Is], L) :- not(isin(I, L)), helper_avoids_ingredients(Is, L).

p1(L, X) :- bagof(L, has_ingredients(L, X), L).

p2(L,Y) :- bagof(L, avoids_ingredients(L, Y), L).

find_items(L,X,Y) :- p1(L1,X), p2(L2,Y), intersection(L1,L2,L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
