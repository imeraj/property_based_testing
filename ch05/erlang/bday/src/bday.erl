-module(bday).
-export([main/1]).

main([Path]) ->
  {ok, Data} = file:read_file(Path),
  Handle = bday_employee:from_csv(binary_to_list(Data)),
  Query = bday_employee:filter_birthday(Handle, date()),  % update the date on CSV
  BdaySet = bday_employee:fetch(Query),
  Mails = [bday_mail_tpl:full(Employee) || Employee <- BdaySet],
  [send_email(To, Topic, Body) || {To, Topic, Body} <- Mails].

send_email(To, _, _) ->
  io:format("sent birthday email to ~p~n", [To]).
