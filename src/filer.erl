%%%-------------------------------------------------------------------
%%% @doc File worker
%%%-------------------------------------------------------------------
-module(filer).

-behaviour(gen_server).

-export([start_link/0, set_source_dir/1, unset_source_dir/0, print_state/0, get_file/0, get_file/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SKIP_FILES, ".skip_files").

-record(state, {
  source_dir :: string(),
  sent_files = [],
  files :: list(),
  ignore :: list()
}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% API
%%% ==================================================================

-spec unset_source_dir() -> ok.
unset_source_dir() ->
  F = fun(Node) ->
    Resp = gen_server:call({?MODULE, Node}, unset_source_dir),
    io:format("Resp: ~p~n", [Resp])
      end,
  [F(X) || X <- nodes()].

-spec set_source_dir(Source :: string()) -> ok.
set_source_dir(Source) ->
  F = fun(Node) ->
    Resp = gen_server:call({?MODULE, Node}, {set_source_dir, Source}),
    io:format("Resp: ~p~n", [Resp])
      end,
  [F(X) || X <- nodes()].

-spec print_state() -> ok.
print_state() ->
  F = fun(Node) ->
    Resp = gen_server:call({?MODULE, Node}, print_state),
    io:format("Resp: ~p~n", [Resp])
      end,
  [F(X) || X <- nodes()].

-spec get_file() -> ok.
get_file() ->
  get_file(1).

-spec get_file(Num :: integer()) -> ok.
get_file(Num) ->
  case gen_tcp:listen(5678, [binary, {packet, 0}, {active, false}]) of
    {ok, LSock} ->
      [Node] = nodes(),
      case gen_server:call({?MODULE, Node}, {get_file, Num}) of
        {ok, File} ->
          {ok, Sock} = gen_tcp:accept(LSock),
          {ok, Bin} = do_recv(Sock, []),
          ok = gen_tcp:close(Sock),
          ok = gen_tcp:close(LSock),
          file_utils:save_file(File, Bin)
      end;
    {error, Reason} ->
      io:format("error, reason: ~p~n", [Reason])
  end.

%%%===================================================================
%%% GenServer
%%% ==================================================================

init([]) ->
  {ok, #state{}}.

handle_call({set_source_dir, Source}, _From, State = #state{}) ->
  % check if source dir was set up before
  case Source =:= State#state.source_dir of
    true -> {reply, {error, source_dir_set_before}, State};
    false ->
      case file_utils:lookup_files(Source, ?SKIP_FILES) of
        {error, Msg} -> {reply, {error, Msg}, State};
        {ok, Result} ->
          NewState = State#state{source_dir = Source, files = maps:get(files, Result), ignore = maps:get(ignore, Result)},
          {reply, ok, NewState}
      end
  end;

handle_call(print_state, _From, State = #state{}) ->
  {reply, {ok, State}, State};

handle_call(unset_source_dir, _From, State = #state{}) ->
  NewState = State#state{source_dir = undefined, files = [], ignore = []},
  {reply, {ok, State}, NewState};

handle_call({get_file, _Num}, _From, State = #state{files = undefined}) ->
  {reply, {ok, files_not_found}, State};

handle_call({get_file, _Num}, _From, State = #state{files = []}) ->
  {reply, {ok, files_not_found}, State};

handle_call({get_file, _Num}, _From, State = #state{}) ->
  [File | _Rest] = State#state.files,
  exchanger:send_file(File),
  {reply, {ok, File}, State}.

handle_cast(_Request, State = #state{}) ->
  {noreply, State}.

handle_info(Info, State = #state{}) ->
  io:format("Get info: ~p~n", [Info]),
  {noreply, State}.

terminate(_Reason, _State = #state{}) ->
  ok.

code_change(_OldVsn, State = #state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_recv(Sock, Bs) ->
  case gen_tcp:recv(Sock, 0) of
    {ok, B} ->
      do_recv(Sock, [Bs, B]);
    {error, closed} ->
      {ok, list_to_binary(Bs)}
  end.