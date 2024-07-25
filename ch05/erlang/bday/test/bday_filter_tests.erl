-module(bday_filter_tests).
-include_lib("eunit/include/eunit.hrl").

%% Property
bday_filter_test() ->
  Years = generate_years_data(2018, 2038),
  People = generate_people_for_year(3),
  lists:foreach(fun(YearData) ->
      BirthDays = find_birthdays_for_year(People, YearData),
      every_birthday_once(People, BirthDays),
      on_right_date(People, BirthDays) end,
    Years).

%% Assertions
every_birthday_once(People, Birthdays) ->
  Found = lists:sort(lists:append([Found || {_,Found} <- Birthdays])),
  NotFound = People -- Found,
  FoundManyTimes = Found -- lists:usort(Found), % usort drops dupes
  ?assertEqual([], NotFound),
  ?assertEqual([], FoundManyTimes).

on_right_date(_People, Birthdays) ->
  [calendar:valid_date({Y, PM, PD}) andalso
    ?assertEqual({M,D}, {PM,PD}) || {{Y,M,D}, Found} <- Birthdays, #{"date_of_birth" := {_, PM, PD}} <- Found].

generate_years_data(End, End) ->
  [];
generate_years_data(Start, End) ->
  [generate_year_data(Start) | generate_years_data(Start + 1, End)].

generate_year_data(Year) ->
  DaysInFeb = case calendar:is_leap_year(Year) of
                true -> 29;
                false -> 28
              end,
  month(Year,1,31) ++ month(Year, 2, DaysInFeb) ++ month(Year,3,31) ++
    month(Year,4,30) ++ month(Year,5,31) ++ month(Year,6,30) ++ month(Year,7,31) ++
    month(Year,8,31) ++ month(Year,9,30) ++ month(Year,10,31) ++ month(Year,11,30) ++
    month(Year,12,31).

month(Year, Month, 1) -> [{Year,Month,1}];
month(Year, Month, Day) ->
  [{Year,Month,Day} | month(Year, Month, Day-1)].

generate_people_for_year(N) ->
  YearSeed = generate_year_data(2016), % leap year
  lists:append([people_for_year(YearSeed) || _ <- lists:seq(1,N)]).

people_for_year(Year) ->
  [person_for_date(Date) || Date <- Year].

person_for_date({_, M, D}) ->
  #{"name" => make_ref(),
    "date_of_birth" => {rand:uniform(1)+1900,M,D}}.

find_birthdays_for_year(_, []) -> [];
find_birthdays_for_year(People, [Day|Year]) ->
  Found = bday_filter:birthday(People, Day),
  [{Day, Found} | find_birthdays_for_year(People, Year)].
