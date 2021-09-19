%%% render module is responsible for rendering Markdown documents parsed
%%% by marker:marker function.
-module(render).
-include_lib("eunit/include/eunit.hrl").

-export([html/1]).

tags() ->
    #{
      paragraph =>   #{open => "<p>", close => "</p>"},
      block_quote => #{open => "<blockquote>", close => "</blockquote>"},
      bullet_list => #{open => "<ul>", close => "</ul>"},
      list_item =>   #{open => "<li>", close => "</li>"},
      document =>    #{open => "<div>", close => "</div>"}
     }.

html({str, Text}) ->
    Text;
html({italic, Text}) ->
    "<i>" ++ Text ++ "</i>";
html({emph, Text}) ->
    "<b>" ++ Text ++ "</b>";
html({Type, Blocks}) ->
    OpenTag = maps:get(open, maps:get(Type, tags())),
    CloseTag = maps:get(close, maps:get(Type, tags())),
    OpenTag ++ lists:flatten(lists:map(fun html/1, Blocks)) ++ CloseTag.

html_test() ->
    ?assertEqual("<div></div>",
                 html({document, []})),

    ?assertEqual("<div><p>foo</p></div>",
                 html({document, [{paragraph, [{str, "foo"}]}]})),

    ?assertEqual("<div><ul><li></li></ul></div>",
                 html({document, [{bullet_list, [{list_item, []}]}]})).
