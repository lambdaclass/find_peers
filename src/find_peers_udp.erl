%%%-------------------------------------------------------------------
%% @doc find_peers udp polling gen_server.
%% @end
%%%-------------------------------------------------------------------
-module(find_peers_udp).

-behaviour(gen_server).

-define(DEFAULT_PORT, 45900).
-define(DEFAULT_IP, {0,0,0,0}).
-define(DEFAULT_MADDR, {230,0,0,1}).
-define(DEFAULT_TTL, 1).

%% API
-export([start_link/0, poll/0]).

%% Behaviour callbacks
-export([init/1, handle_call/3, handle_info/2, terminate/2]).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    Args = [],
    Options = [],
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, Options).

poll() ->
    gen_server:call(?MODULE, poll).

%%====================================================================
%% Behaviour callbacks
%%====================================================================

init(_Args) ->
    Port = port(),
    Maddr = maddr(),
    Opts =
        [
            binary,
            {reuseaddr, true},
            {broadcast, true},
            {multicast_loop, true},
            {active, 10},
            {multicast_ttl, ttl()},
            {ip, ip()},
            {add_membership, {Maddr, {0,0,0,0}}}
        ],
    {ok, Socket} = gen_udp:open(Port, Opts),

    State =
        #{
            seen => sets:new(),
            socket => Socket,
            conn => {Maddr, Port, Socket}
        },
    {ok, State, 0}.

handle_call(poll, _From, State = #{seen := Nodes}) ->
    {reply, sets:to_list(Nodes), State}.

handle_info(broadcast, State = #{conn := {Maddr, Port, Socket}}) ->
    NodeBin = erlang:atom_to_binary(node(), utf8),
    Msg = <<"Peer:", NodeBin/binary>>,
    gen_udp:send(Socket, Maddr, Port, [Msg]),
    Time = rand:uniform(4000) + 3000,
    erlang:send_after(Time, self(), broadcast),
    {noreply, State};

handle_info(timeout, State) ->
    handle_info(broadcast, State),
    {noreply, State};

handle_info({udp, Socket, _, _, <<"Peer:", Name/binary>>}, State = #{seen := Nodes}) ->
    error_logger:info_msg("[~p:~p/~p] Peer ~p sees ~s", [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, node(), Name]),
    Nodes1 = sets:add_element(erlang:binary_to_atom(Name, utf8), Nodes),
    inet:setopts(Socket, [{active, 1}]),
    {noreply, State#{seen := Nodes1}};

handle_info({udp, Socket, _, _, _}, State) ->
    % Bad packet, but don't die.
    inet:setopts(Socket, [{active, 1}]),
    {noreply, State}.

terminate(_Reason, #{socket := Socket}) ->
    gen_udp:close(Socket).

%%====================================================================
%% Internal functions
%%====================================================================

port() -> application:get_env(find_peers, port, ?DEFAULT_PORT).
ip() -> application:get_env(find_peers, ip, ?DEFAULT_IP).
maddr() -> application:get_env(find_peers, multicast_addr, ?DEFAULT_MADDR).
ttl() -> application:get_env(find_peers, ttl, ?DEFAULT_TTL).
