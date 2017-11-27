%%%-------------------------------------------------------------------
%% @doc find_peers gen_server.
%% @end
%%%-------------------------------------------------------------------
-module(find_peers_server).

-behaviour(gen_server).

-define(DEFAULT_SYNC_TIMEOUT, 500).

%% API
-export([start_link/0]).

%% Behaviour callbacks
-export([init/1, handle_info/2]).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    Args = [],
    Options = [],
    gen_server:start_link(?MODULE, Args, Options).

%%====================================================================
%% Behaviour callbacks
%%====================================================================

init(_Args) ->
    Time = application:get_env(find_peers, sync_timeout, ?DEFAULT_SYNC_TIMEOUT),
    erlang:send_after(Time, self(), poll),
    {ok, #{}}.

handle_info(poll, State) ->
    discover(),
    Time = application:get_env(find_peers, sync_timeout, ?DEFAULT_SYNC_TIMEOUT),
    erlang:send_after(Time, self(), poll),
    {noreply, State}.

%%====================================================================
%% Internal functions
%%====================================================================

discover() ->
    Provider = find_peers_udp,
    Nodes0 = apply(Provider, poll, []),
    Nodes1 = fresh_nodes(Nodes0),
    connect_nodes(Nodes1).

fresh_nodes(Nodes) ->
    Nodes0 = sets:from_list(Nodes),
    Nodes1 = sets:from_list(erlang:nodes()),
    Nodes2 = sets:subtract(Nodes0, Nodes1),
    sets:to_list(Nodes2).

connect_nodes(Nodes) ->
    Fun = fun (Node) -> net_kernel:connect_node(Node) end,
    lists:foreach(Fun, Nodes).
