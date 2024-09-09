%%%-------------------------------------------------------------------
%%% @doc Exchange worker
%%%-------------------------------------------------------------------
-module(exchanger).

-behaviour(gen_server).

-export([start_link/0, send_file/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% API
%%% ==================================================================

send_file(File) ->
  io:format("Sending file: ~p~n", [File]),
  gen_server:cast(?MODULE, {send_file, File}),
  ok.


%%%===================================================================
%%% GenServer
%%% ==================================================================

init([]) ->
  {ok, #state{}}.

handle_call(_Request, _From, State = #state{}) ->
  {reply, ok, State}.

handle_cast({send_file, File}, State = #state{}) ->
  SomeHostInNet = "localhost",
  case file:open(File, [raw]) of
    {ok, IoDevice} ->
      case gen_tcp:connect(SomeHostInNet, 5678, [binary, {packet, 0}]) of
        {ok, Sock} ->
          case file:sendfile(IoDevice, Sock, 0, 0, []) of
            {ok, BytesSent} ->
              io:format("Sent ~p bytes~n", [BytesSent]),
              gen_tcp:close(Sock);
            {error, Reason} ->
              io:format("Send error: ~p~n", [Reason])
          end;
        {error, econnrefused} ->
          io:format("Connection error~n")
      end,
      file:close(File);
    {error, Reason} ->
      io:format("File open error: ~p~n", [Reason])
  end,
  {noreply, State}.

handle_info(Info, State = #state{}) ->
  io:format("Get info: ~p~n", [Info]),
  {noreply, State}.

terminate(_Reason, _State = #state{}) ->
  ok.

code_change(_OldVsn, State = #state{}, _Extra) ->
  {ok, State}.
