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
-export([new_block/1, nextBlock/1, add_block/1, select_all/0, getLastBlock/0]).

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
    litechain = ets:new(litechain,  [set, {keypos, #block.index}, named_table]),
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
calcHash(Rec) when is_record(Rec, block) ->
    IndexList       = integer_to_list(Rec#block.index),
    TimestampList   = integer_to_list(Rec#block.timestamp),
    Doc             = IndexList ++ Rec#block.prehash ++ TimestampList ++ Rec#block.data,
    <<X:256/big-unsigned-integer>> = crypto:hash(sha256, Doc),
    integer_to_list(X, 16).

%-------------------------------------------------------------------------------
% Next Block
%-------------------------------------------------------------------------------
nextBlock(Data) ->
    LastBlock           = getLastBlock(),
    NewIndex            = LastBlock#block.index + 1,
    PreHash             = LastBlock#block.hash,
    NewTimestamp        = gregorian_seconds(),
    NewHash             = calcHash( 
                                    rec_block({NewIndex, PreHash, NewTimestamp, Data}) 
                                ),
    rec_block({NewIndex, PreHash, NewTimestamp, Data, NewHash}).

%-------------------------------------------------------------------------------
% Genesis Block
%-------------------------------------------------------------------------------
genesisBlock() ->
    Index     = 0,
    Prehash   = integer_to_list( gregorian_seconds() ),
    Timestamp = gregorian_seconds(),
    Data      = "genesis block",
    Hash      = calcHash(  
                            rec_block({Index, Prehash, Timestamp, Data}) 
                        ),
    rec_block({Index, Prehash, Timestamp, Data, Hash}).

%-------------------------------------------------------------------------------
% Add block to litechain
%-------------------------------------------------------------------------------
add_block(Rec) when is_record(Rec, block) =:= true, Rec#block.index =:= 0 ->
    ets:insert( litechain, Rec#block{} );
add_block(Rec) when is_record(Rec, block) =:= true, Rec#block.index > 0 ->
    ets:insert( litechain, Rec#block{} );
add_block(Any) -> io:format("Any: ~n", Any).


%-------------------------------------------------------------------------------
% Get Last Block
%-------------------------------------------------------------------------------
getLastBlock() ->
    %QueryGenrate       = ets:fun2ms(fun(#block{index=X}) -> X end),
    Query               = [{#block{index = '$1',prehash = '_',timestamp = '_', data = '_',hash = '_'}, [], ['$1']}],
    ListIndex           = ets:select(litechain, Query),
    LastIndex           = lists:max(ListIndex),
    [LastBlock]         = ets:lookup(litechain, LastIndex),
    LastBlock.

%-------------------------------------------------------------------------------
% Verify New Block
%-------------------------------------------------------------------------------
% verifyBlock({NewBlock}) ->
%     PrevBlock = getLastBlock(),
%     verifyBlock({NewBlock, PrevBlock});
% verifyBlock({NewBlock, PrevBlock}) when NewBlock#block.index - 1 =/= PrevBlock#block.index ->
%     {ok, invalid_index};
% verifyBlock({NewBlock, PrevBlock}) when NewBlock#block.prehash =/= PrevBlock#block.hash ->
%     {ok, invalid_previous_hash};
% verifyBlock({NewBlock, _PrevBlock}) when calcHash(NewBlock) =/= NewBlock#block.hash ->
%     {ok, invalid_hash};
% verifyBlock({_NewBlock, _PrevBlock}) ->
%     {ok, valid}.

%-------------------------------------------------------------------------------
% Record from Clause for Block
%-------------------------------------------------------------------------------
rec_block({Index, Prehash, Timestamp, Data}) ->
	#block{ index       = Index,
			prehash     = Prehash,
            timestamp   = Timestamp,
            data        = Data
		    };
rec_block({Index, Prehash, Timestamp, Data, Hash}) ->
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
% Show All Blocks
%-------------------------------------------------------------------------------
select_all() ->
    % ets:fun2ms(fun(N=#block{index=X}) -> N end),
    Query = [{#block{index = '$1',prehash = '_',timestamp = '_', data = '_',hash = '_'}, [], ['$_']}],
    ets:select(litechain, Query ).
