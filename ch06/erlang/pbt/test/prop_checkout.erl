-module(prop_checkout).
-include_lib("proper/include/proper.hrl").

%% Properties
prop_no_special1() ->
  ?FORALL({ItemList, ExpectedPrice, PriceList}, item_price_list(),
    ExpectedPrice =:= checkout:total(ItemList, PriceList, [])).

prop_no_special2() ->
  ?FORALL({ItemList, ExpectedPrice, PriceList}, item_price_list(),
    collect(
      bucket(length(ItemList), 10),
      ExpectedPrice =:= checkout:total(ItemList, PriceList, []))).

%% Generators
item_price_list() ->
  ?LET(PriceList, price_list(),
      ?LET({ItemList, ExpectedPrice}, item_list(PriceList),
        {ItemList, ExpectedPrice, PriceList})).

price_list() ->
  ?LET(PriceList, non_empty(list({non_empty(string()), integer()})),
    lists:ukeysort(1, PriceList)).

item_list(PriceList) ->
  ?SIZED(Size, item_list(Size, PriceList, {[], 0})).

item_list(0, _PriceList, Acc) -> Acc;
item_list(N, PriceList, {ItemAcc, ExpectedPrice}) ->
    ?LET({Item, Price}, elements(PriceList),
      item_list(N-1, PriceList, {[Item|ItemAcc], Price + ExpectedPrice})).

%% Helpers
bucket(N, Unit) ->
  (N div Unit) * Unit.
