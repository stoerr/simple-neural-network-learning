:- op(50,fx,'@'). /* Vector marker */
:- discontiguous rule/2.

isName(Atom) :- atom(Atom), atom_chars(Atom,[C|_]), char_type(C, alpha), Atom \= pd.

rule(0+X,X). 
rule(X+0,X).
rule(1*X,X).
rule(X*1,X).
rule(0*_,0).
rule(_*0,0).
rule(sum(_,0),0).
rule(X-X,0).

rule((X+Y)*Z, (X*Z)+(Y*Z)).
rule(X*(Y+Z), X*Y + X*Z).

rule(X+(Y+Z), X+Y+Z).
rule(X*(Y*Z), X*Y*Z).

rule(X+Y, Y+X) :- X @< Y.
rule(X*Y, Y*X) :- X @< Y.
rule(X+Y+Z, X+Z+Y) :- Y @< Z.
rule(X*Y*Z, X*Z*Y) :- Y @< Z.

rule(X+X, X*2).
rule(X+X*Y, X*Z) :- number(Y), Z is Y + 1.
rule(X*Y+X, X*Z) :- number(Y), Z is Y + 1.
rule(X*A+X*B, X*C) :- number(A), number(B), C is A + B.

rule(d(V,X + Y), d(V,X) + d(V,Y)).
rule(d(V,X - Y), d(V,X) - d(V,Y)).
rule(d(V,-X), -d(V,X)).
rule(d(V,X*Y), d(V,X) * Y + X * d(V,Y)).
rule(d(V,V), 1).
rule(d(V,N), 0) :- number(N); atom(N), V \= N.
rule(d(V,sum(I,T)), sum(I,d(V,T))).
rule(d(V,X^N),N*d(V,X)*X^N1) :- number(N), N1 is N-1.

rule(d(V,ind(I,T)), ind(I,d(V,T))).
rule(ind(V,X+Y), ind(V,X)+ind(V,Y)).
rule(ind(V,@X * Y), ind(V,X) * Y) :- Y \= @_.
rule(ind(V,X * @Y), X * ind(V,Y)) :- X \= @_.
rule(@X * @Y, sum(I,ind(I,X)*ind(I,Y)) ,[I|R],R).

rule(d(V,X),D * d(V,A)) :- X =.. [F,A], isName(F), atomic_concat(F,'`',FD), D =.. [FD,d(V,A)].

/*
rule(d(V,FT),S) :- compound(FT), FT=..[F|A], isName(F), funcderiv(V,F,A,A,1,S).
funcderiv(_,_,_,[],_,0).
funcderiv(V,F,A,[A1|AR],N,S) :- N1 is N+1, funcderiv(V,F,A,AR,N1,S1), PD=..[pd,F,[N]|A], S = PD*d(V,A1) + S1.
*/

rule(d(V,FT),S) :- compound(FT), FT=..[F,A1,A2], isName(F), PD1=..[pd,F,[1],A1,A2], PD2=..[pd,F,[2],A1,A2], S=PD1*d(V,A1)+PD2*d(V,A2).
rule(d(V,FT),S) :- compound(FT), FT=..[pd,F,V,A1,A2], PD1=..[pd,F,[1|V],A1,A2], PD2=..[pd,F,[2|V],A1,A2], S=PD1*d(V,A1)+PD2*d(V,A2).

applyrules(P,X,TX,IV,IVR) :- call(P,X,T), ground(T), !, applyrules(P,T,TX,IV,IVR), !.
applyrules(P,X,TX,IV,IVR) :- call(P,X,T,IV,IV1), ground(T), !, applyrules(P,T,TX,IV1,IVR), !.
applyrules(_,X,X,IV,IV).

treplacelist(_,[],[],IV,IV) :- !.
treplacelist(P,[H|T], [HR|TR],IV,IVR) :- treplace(P,H,HR,IV,IV1), treplacelist(P,T,TR,IV1,IVR), !.

treplace(X,TX) :- treplace(rule,X,TX), !.
treplace(P,X,TX) :- treplace(P,X,TX,[i,j,k,l,i1,i2,i3,i4,i5,i6,i7,i8], _), !.

treplace(P,X,TX,IV,IVR) :- applyrules(P, X, XR,IV,IV1), XR =.. [F|L], treplacelist(P, L,LR,IV1,IV2), XRR =.. [F|LR], 
	applyrules(P, XRR, XRRR,IV2,IV3), !, (XRR \= XRRR, !, treplace(P,XRRR,TX,IV3,IVR); XRRR = TX, IVR=IV3), !.

subst(V,S,V,S) :- !.
subst(_,_,[],[]) :- !.
subst(V,S,[H|T],[HS|TS]) :- !, subst(V,S,H,HS), subst(V,S,T,TS), !.
subst(V,S,T,TS) :- !, T =.. [TF|Args], subst(V,S,Args,Sargs), TS =.. [TF|Sargs], !.

printLtx(T) :- treplace(printrule,T,T1), !, print('$$'), write_term(T1,[]), print('$$'), nl.

printrule(d(V,T), R) :- atomic_list_concat(['\\frac{d}{d', V, '}'], P), R =.. [P,T].
printrule(sum(V,T), R) :- atomic_list_concat(['\\sum_{', V, '}'], P), R =.. [P,T].
printrule(ind(V,T), R) :- T=.. [F|A], atomic_list_concat(['{', F, '}_{', V, '}'], P), R =.. [P|A].
printrule(@T, R) :- T=..[F|A], atomic_list_concat(['\\overline{', F, '}'], FO), R =.. [FO|A].
printrule(PD, R) :- PD=..[pd,F,V|A], swritef(VS,'%p',[V]), atomic_list_concat([F, '^{', VS, '}'], FD), R=..[FD|A].
:- dynamic(printrule/4).

printEval(T) :- print(T), nl, treplace(T,TR), print(TR), nl, printLtx(T=TR), nl, nl.

:- printEval(d(x, f(x,x*x))).
:- printEval(d(x, d(x, f(x,x*x)))).
/* :- printEval(d(x, f(x * @g, @b(@v(x * @g),x * @g)))).
:- printEval(d(x, d(x, f(x * @g, @b(@v(x * @g),x * @g))))). */

:- printEval(d(x, f(x, b(v(x),x)))).
:- printEval(d(x, d(x, f(x, b(v(x),x))))).

/* 
f(I,P+x*G,B(V(I,P+x*G),P+x*G)) ; ignore I, shift by @p: f(x * @g, @b(@v(x * @g),x * @g))
:- d(x,2*x+x*x,Z), print(Z), nl, s(Z,SZ), s(SZ,SSZ), s(SSZ,SSSZ), print(SSSZ), nl, nl.

['/Users/hps/personal/learning2/nnDerivation.pl']. 
*/
