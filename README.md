# Pometo Docs To Test Rebar3 Plugin


Builds eunit tests from `Pometo` markdown documentation

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        {pometo_docs_to_test, {git, "https://host/user/pometo_docs_to_test.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 pometo_docs_to_test
    ===> Fetching pometo_docs_to_test
    ===> Compiling pometo_docs_to_test
    <Plugin Output>
