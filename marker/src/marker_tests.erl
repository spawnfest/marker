-module(marker_tests).
-export([]).
-include_lib("eunit/include/eunit.hrl").

marker_test() ->
    t("<div></div>", "").

t(ExpectedHTML, Markdown) ->
    {ok, Tree} = marker:markdown(Markdown),
    HTML = render:html(Tree),
    ?assertEqual(
        ExpectedHTML,
        HTML).
 
