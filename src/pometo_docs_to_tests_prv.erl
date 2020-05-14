-module(pometo_docs_to_tests_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, pometo_docs_to_tests).
-define(DEPS, [app_discovery]).

-define(IN_TEXT,        1).
-define(GETTING_TEST,   2).
-define(GETTING_RESULT, 3).

-record(test, {
               seq        = 1,
               title      = "",
               codeacc    = [],
               resultsacc = []
    }).

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
    Hash = binary:bin_to_list(base16:encode(crypto:hash(sha, Lines))),
    Basename = filename:basename(File, ".md"),
    FileName = Basename ++ "_" ++ Hash ++ "_tests",
    gen_test2(FileName, Lines),
    ok.

gen_test2(Filename, Lines) ->
    Header  = "-module(" ++ Filename ++ ").\n\n",
    Include = "-include_lib(\"eunit/include/eunit.hrl\").\n\n",
    Export  = "-compile([export_all]).\n\n",
    io:format("about to gen tests~n"),
    Body = gen_test3(Lines, ?IN_TEXT, #test{}, []),
    io:format("Body is ~p~n", [Body]),
    Module = Header ++ Include ++ Export ++ Body,
    io:format("tests are ~p~n", [Module]),
    ok.

gen_test3([], _, _, Acc) -> lists:reverse(Acc);
gen_test3(["## " ++ Title | T], ?IN_TEXT, Test, Acc) ->
    io:format("in 1 ~p~n", [Title]),
    gen_test3(T, ?IN_TEXT, Test#test{title = Title}, Acc);
gen_test3([_H | T], ?IN_TEXT, Test, Acc) ->
    gen_test3(T, ?IN_TEXT, Test, Acc);
gen_test3(["## " ++ Line | T], ?GETTING_RESULT, Test, Acc) ->
    io:format("in 3 ~p~n", [Line]),
    #test{resultsacc = R} = Test,
    gen_test3(T, ?GETTING_RESULT, Test#test{resultsacc = [Line | R]}, Acc);
gen_test3(["## " ++ Line | T], ?GETTING_TEST, Test, Acc) ->
    io:format("in 4 ~p~n", [Line]),
    #test{codeacc = C} = Test,
    gen_test3(T, ?GETTING_TEST, Test#test{codeacc = [Line | C]}, Acc);
gen_test3(["```pometo_results" ++ _Rest | T], ?IN_TEXT, Test, Acc) ->
    io:format("in 5~n"),
    gen_test3(T, ?GETTING_RESULT, Test, Acc);
gen_test3(["```" ++ _Rest | T], ?GETTING_RESULT, Test, Acc) ->
    io:format("in 6~n"),
    #test{seq        = N,
          title      = T,
          codeacc    = C,
          resultsacc = R} = Test,
    NewTest = make_test(T, integer_to_list(N), lists:reverse(C), lists:reverse(R)),
    gen_test3(T, ?IN_TEXT, #test{seq = N + 1}, [NewTest| Acc]);
gen_test3(["```pometo" ++ _Rest | T], ?IN_TEXT, Test, Acc) ->
    io:format("in 7~n"),
    gen_test3(T, ?GETTING_TEST, Test, Acc);
gen_test3(["```" ++ _Rest | T], ?GETTING_TEST, Test, Acc) ->
    io:format("in 8~n"),
    gen_test3(T, ?IN_TEXT, Test, Acc).

make_test(Title, Seq, Code, Results) ->
Title ++ "_" ++ Seq ++ "_test_() ->\n" ++
    "Code = \"" ++ string:join(Code, "\n") ++ "\"," ++
    "Expected = \"" ++ string:join(Results, "\n") ++ "\"," ++
    "run(Code, Expected).".

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



