-module(bday_mail_tpl).
-export([full/1, body/1]).

-spec full(bday_employee:employee()) -> {[string()], string(), string()}.
full(Employee) ->
  {[bday_employee:email(Employee)], "Happy birthday!", body(Employee)}.

-spec body(bday_employee:employee()) -> string().
body(Employee) ->
    lists:flatten(io_lib:format("Happy birthday, dear ~s!",
                        [bday_employee:first_name(Employee)])).