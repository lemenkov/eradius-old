-module(eradius_app).

-behaviour(application).

%% Application and Supervisor callbacks
-export([start/2, stop/1, init/1]).

-define(MAX_RESTART,    5).
-define(MAX_TIME,      60).

start(_Application, _Type) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_S) ->
    ok.

init([]) ->
    {ok,
     {_SupFlags = {one_for_one, ?MAX_RESTART, ?MAX_TIME},
      [
       %% Megaco messaging server
       { eradius_sup, {eradius, start_link, []},
         permanent, 2000, worker, [eradius] },
       { eradius_acc_sup, {eradius_acc, start_link, []},
         permanent, 2000, worker, [eradius_acc] },
       { eradius_dict_sup, {eradius_dict, start_link, []},
         permanent, 2000, worker, [eradius_dict] }
      ]
     }
    }.
