-module(dspr_sup).

-behaviour(supervisor).

-export([init/1]).
-export([start_link/0]).

-define(SERVER, ?MODULE).
-define(CHILD(I, Type), {I, {I, start_link, []}, transient, 5000, Type, [I]}).


start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


init([]) ->
    SupFlags = {simple_one_for_one, 5, 10},
    ChildSpec = ?CHILD(dspr, worker),
    {ok, {SupFlags, [ChildSpec]}}.
