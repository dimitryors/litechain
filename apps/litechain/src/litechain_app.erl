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
-export([new_block/1]).

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
    litechain = ets:new(litechain,  [set, named_table]),
    % Dev
    add_block( genesisBlock() ),
    {ok, #state{}}.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
new_block(Data) -> gen_server:cast(litechain_app, {new_block, Data}).


handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({new_block, Data}, State) ->
    Rec = nextBlock(Data),
    add_block(Rec),
    {noreply, State};
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
    IndexList       = integer_to_list(Index),
    TimestampList   = integer_to_list(Timestamp),
    Doc             = IndexList ++ Prehash ++ TimestampList ++ Data,
    <<X:256/big-unsigned-integer>> = crypto:hash(sha256, Doc),
    integer_to_list(X, 16).

%-------------------------------------------------------------------------------
% Next Block
%-------------------------------------------------------------------------------
nextBlock(Data) ->
    LastIndex           = ets:last(litechain),
    [{_, LastBlock}]    = ets:lookup(litechain, LastIndex),
    NewIndex            = LastBlock#block.index + 1,
    PreHash             = LastBlock#block.hash,
    NewTimestamp        = gregorian_seconds(),
    NewHash             = calcHash(NewIndex, PreHash, NewTimestamp, Data),
    rec_block(NewIndex, PreHash, NewTimestamp, Data, NewHash).

%-------------------------------------------------------------------------------
% Genesis Block
%-------------------------------------------------------------------------------
genesisBlock() ->
    Index     = 1,
    Prehash   = integer_to_list( gregorian_seconds() ),
    Timestamp = gregorian_seconds(),
    Data      = "genesis block",
    Hash      = calcHash(Index, Prehash, Timestamp, Data),
    rec_block(Index, Prehash, Timestamp, Data, Hash).

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

%-------------------------------------------------------------------------------
% Get current timestamp in Gregorian seconds format
%-------------------------------------------------------------------------------
gregorian_seconds() ->
    TS = os:timestamp(),
    %calendar:gregorian_seconds_to_datetime(63670024974).
    calendar:datetime_to_gregorian_seconds( calendar:now_to_universal_time(TS) ).

%-------------------------------------------------------------------------------
% Add block to litechain
%-------------------------------------------------------------------------------
add_block(Rec) when is_record(Rec, block) =:= true ->
    ets:insert( litechain, { Rec#block.index, Rec#block{} } );
add_block(Any) -> io:format("Any: ~n", Any).

