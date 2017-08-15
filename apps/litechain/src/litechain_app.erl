%%%-------------------------------------------------------------------
%% @doc litechain public API
%% @end
%%%-------------------------------------------------------------------

-module(litechain_app).

-behaviour(application).

-include_lib("block.hrl").

%% Application callbacks
-export([start_link/0, start/2, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([calcHash/4, nextBlock/1]).

-define(SERVER, ?MODULE).
-record(state, {name}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start(_StartType, _StartArgs) ->
    %%gen_server:start_link({local, ?SERVER}, ?MODULE, [], []),
    litechain_sup:start_link().

init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%-------------------------------------------------------------------------------
% Hash Block
%-------------------------------------------------------------------------------
calcHash(Index, Prehash, Timestamp, Data) ->
    <<X:256/big-unsigned-integer>> = crypto:hash(sha256, Index ++ Prehash ++ Timestamp ++ Data),
    integer_to_list(X, 16).

%-------------------------------------------------------------------------------
% Next Block
%-------------------------------------------------------------------------------
nextBlock(Data) ->
    NewIndex,
    PreHash,
    NewTimestamp,
    NewtHash = calcHash(NewIndex, PreHash, NewTimestamp, Data),
    rec_block(NewIndex, Prehash, NewTimestamp, Data, NewtHash).


%-------------------------------------------------------------------------------
% Record from Clause for Block
%-------------------------------------------------------------------------------
rec_block(Index, Prehash, Timestamp, Data, Hash) ->
	#block{ index       = Index,
			prehash     = Prehash,
            timestamp   = Timestamp,
            data        = Data,
            hash        = Hash
		    }.
