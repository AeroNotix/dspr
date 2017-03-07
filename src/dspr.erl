-module(dspr).

-behaviour(gen_server).

-export([register/2]).
-export([register/3]).
-export([register/4]).
-export([unregister/2]).
-export([unregister/3]).
-export([whereis/2]).

-export([code_change/3]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([init/1]).
-export([start/0]).
-export([start_link/0]).
-export([terminate/2]).


-define(SERVER, ?MODULE).

-record(state, {
          registrations = [] :: list(),
          notify :: pid(),
          notify_monitor :: reference()
         }).


start() ->
    supervisor:start_child(dspr_sup, []).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    {ok, #state{}};
init([Notify]) ->
    process_flag(trap_exit, true),
    Ref = erlang:monitor(process, Notify),
    {ok, #state{notify=Notify, notify_monitor=Ref}}.

handle_call({register, Term, Pid}, _From, #state{registrations=R}=State)
  when is_pid(Pid) ->
    case already_registered(Term, R) of
        true ->
            {reply, {error, already_registered}, State};
        false ->
            Ref = erlang:monitor(process, Pid),
            {reply, ok, #state{registrations=[{Term, Ref, Pid}|R]}}
    end;
handle_call({unregister, Term}, _From, #state{registrations=R}) ->
    {reply, ok, #state{registrations=lists:keydelete(Term, 1, R)}};
handle_call({whereis, Term}, _From, #state{registrations=R}=State) ->
    {reply, lists:keyfind(Term, 1, R), State}.

handle_cast(_Cast, State) ->
    {noreply, State}.

handle_info({'DOWN', Ref, _, _, _}, #state{notify_monitor=Ref}=State) ->
    {stop, normal, State};
handle_info({'DOWN', Ref, _, _, _}=Msg, #state{registrations=R, notify=Notify}) ->
    case {lists:keyfind(Ref, 2, R), erlang:is_pid(Notify)} of
        {_, false} ->
            ok;
        {{_, _, _}, true} ->
            Notify ! Msg
    end,
    {noreply, #state{registrations=lists:keydelete(Ref, 2, R)}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

already_registered(Term, R) ->
    lists:keyfind(Term, 1, R) /= false.

register(Where, Term) ->
    dspr:register(Where, Term, self()).

register(Where, Term, Pid) ->
    dspr:register(Where, Term, Pid, 5000).

register(Where, Term, Pid, Timeout) ->
    gen_server:call(Where, {register, Term, Pid}, Timeout).

unregister(Where, Term) ->
    dspr:register(Where, Term, 5000).

unregister(Where, Term, Timeout) ->
    gen_server:call(Where, {unregister, Term}, Timeout).

whereis(Where, Term) ->
    whereis(Where, Term, 5000).

whereis(Where, Term, Timeout) ->
    gen_server:call(Where, {whereis, Term}, Timeout).
