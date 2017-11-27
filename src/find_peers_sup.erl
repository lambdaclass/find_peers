%%%-------------------------------------------------------------------
%% @doc find_peers top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(find_peers_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    error_logger:info_msg("[~p:~p/~p]", [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY]),
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    error_logger:info_msg("[~p:~p/~p]", [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY]),
    Children =
        [
            #{
                id => find_peers_server,
                start => {find_peers_server, start_link, []}
            },
            #{
                id => find_peers_udp,
                start => {find_peers_udp, start_link, []}
            }
        ],
    {ok, { {one_for_one, 0, 1}, Children} }.

%%====================================================================
%% Internal functions
%%====================================================================
