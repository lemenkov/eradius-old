-ifndef(LOGGER_HRL).
%% Use Internal logger

-define(DBG(F,A), io:format("(~w:~b): " ++ F ++ "~n", [?MODULE, ?LINE] ++ A)).
-define(ERR(F,A), io:format("ERROR (~w:~b): " ++ F ++ "~n", [?MODULE, ?LINE] ++ A)).

-else.
%% Use logging macroses defined in LOGGER_HRL
-include(??LOGGER_HRL).
-endif.
