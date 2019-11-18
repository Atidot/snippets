% -*- Mode: Prolog -*-
:- discontiguous type/2.
:- discontiguous reference/2.

type(insurance,      contract).

type(life,           insurance).
type(health,         insurance).
type(disability,     insurance).
type(long_term_care, insurance).
type(home,           insurance).
type(auto,           insurance).
reference(type(_, insurance), uri("https://www.investopedia.com/terms/i/insurance.asp")).

type(term,      life).
type(universal, life).
type(variable,  life).
reference(type(_, life), uri("https://www.investopedia.com/terms/l/lifeinsurance.asp")).

type(level_term,            term).
type(yearly_renewable_term, term).
type(decreasing_term,       term).
reference(type(_, term), uri("https://www.investopedia.com/terms/t/termlife.asp")).

sub_type(X,Y) :- type(X,Y).
sub_type(X,Y) :- type(X,Z), sub_type(Z,Y).
