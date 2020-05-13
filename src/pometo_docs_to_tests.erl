-module(pometo_docs_to_tests).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = pometo_docs_to_tests_prv:init(State),
    {ok, State1}.
