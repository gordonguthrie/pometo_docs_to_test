-module(pometo_docs_to_tests_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, pometo_docs_to_tests).
-define(DEPS, [app_discovery]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},                        % The 'user friendly' name of the task
            {module, ?MODULE},                        % The module implementation of the task
            {bare, true},                             % The task can be run by the user, always true
            {deps, ?DEPS},                            % The list of dependencies
            {example, "rebar3 pometo_docs_to_tests"}, % How to use the plugin
            {opts, []},                               % list of options understood by the plugin
            {short_desc, "builds eunit tests from pometo markdown documentation"},
            {desc, "builds eunit tests from pometo markdown documentation"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    lists:foreach(fun make_tests/1, rebar_state:project_apps(State)),
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

make_tests(App) ->
    Root = rebar_app_info:dir(App),
    DocsFiles = get_files(Root ++ "/docs/*"),
    io:format("DocsFiles are ~p~n", [DocsFiles]),
    [generate_tests(X) || X <- DocsFiles],
    ok.

get_files(Root) ->
    RawFiles = filelib:wildcard(Root),
    Files     = [X            || X <- RawFiles, filename:extension(X) == ".md"],
    Dirs      = [X ++ "/*"    || X <- RawFiles, filelib:is_dir(X)],
    DeepFiles = [get_files(X) || X <- Dirs],
    Files ++ DeepFiles.

generate_tests([]) -> ok;
generate_tests(File) ->
    io:format("generating tests from ~p~n", [File]),
    {ok, Lines} = read_lines(File),
    io:format("Lines are ~p~n", [Lines]),
    ok.

read_lines(File) ->
    case file:open(File, read) of
        {error, Err} -> {error, Err};
        {ok, Id}     -> read_l2(Id, [])
    end.

read_l2(Id, Acc) ->
    case file:read_line(Id) of
        {ok, Data}   -> read_l2(Id, [Data | Acc]);
        {error, Err} -> {error, Err};
        eof          -> {ok, lists:reverse(Acc)}
    end.



