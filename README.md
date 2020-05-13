# Pometo Docs To Tests Rebar3 Plugin


Builds eunit tests from `Pometo` markdown documentation

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        {pometo_docs_to_tests, {git, "https://github.com/gordonguthrie/pometo_docs_to_tests.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 pometo_docs_to_tests
    ===> Fetching pometo_docs_to_tests
    ===> Compiling pometo_docs_to_tests
    <Plugin Output>
