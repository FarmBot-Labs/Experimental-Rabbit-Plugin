%%% Fetch an encryption key from the FarmBot server.

-module(key_fetcher).

-export([start_link/0])

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  % HTTP get
  {ok, Url } = application:get_env(rabbitmq_auth_backend_jwt, farmbot_api_key_url).

  case httpc:request(get, {Url, []}, [], []) of
    {ok, {{_, 200, _}, _, Resp }} -> {ok, Resp};
    Error                         -> {stop, Error}
  end.

handle_call(fetch, _Arg, Key) ->
  {reply, Key, Key}.

handle_cast(_Cast, Key) ->
  {noreply, Key}.
