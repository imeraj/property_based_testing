-module(bday_calendar).

%% API exports
-export([is_leap_year/1]).

-spec is_leap_year(non_neg_integer()) -> boolean().
is_leap_year(Year) when Year rem 4 =:= 0, Year rem 100 > 0 -> true;
is_leap_year(Year) when Year rem 400 =:= 0 -> true;
is_leap_year(_Year) -> false.