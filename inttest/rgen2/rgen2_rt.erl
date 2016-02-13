%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
-module(rgen2_rt).

-compile(export_all).

%% Exercise release generation w/ templating

setup([Target]) ->
  retest_utils:load_module(filename:join(Target, "inttest_utils.erl")),
  ok.

files() ->
    [
     {create_dir, "rel"},
     {copy, "rebar.config"},
     {create, "src/rgen2.app.src", app(rgen2)}
    ] ++ inttest_utils:rebar_setup().

run(_Dir) ->
    {ok, _} = retest_sh:run("./rebar compile", []),
    {ok, _} = retest_sh:run("../rebar -vv create-node nodeid=rgen2",
                            [{dir, filename:join([retest_utils:get_cwd(), "rel"])}]),
    {ok, _} = retest_sh:run("./rebar -vv generate", []),
    true = filelib:is_dir("rel/rgen2"),
    true = filelib:is_file("rel/rgen2/bin/rgen2"),
    ok.

%%
%% Generate the contents of a simple .app.src file
%%
app(Name) ->
    "{application, " ++ atom_to_list(Name) ++ ",
           [{vsn, \"1\"},
            {modules, []},
             {registered, []},
             {applications, [kernel, stdlib]}]}.\n".
