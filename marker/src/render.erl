%%% render module is responsible for rendering Markdown documents parsed
%%% by marker:marker function.
-module(render).
-include_lib("eunit/include/eunit.hrl").

-export([html/1]).

tags() ->
    #{
      heading1 =>   #{open => "<h1>", close => "</h1>"},
      heading2 =>   #{open => "<h2>", close => "</h2>"},
      heading3 =>   #{open => "<h3>", close => "</h3>"},
      heading4 =>   #{open => "<h4>", close => "</h4>"},
      heading5 =>   #{open => "<h5>", close => "</h5>"},
      heading6 =>   #{open => "<h6>", close => "</h6>"},
      paragraph =>   #{open => "<p>", close => "</p>"},
      horizontal_line => #{open => "<hr />", close => ""},
      block_quote => #{open => "<blockquote>", close => "</blockquote>"},
      bullet_list => #{open => "<ul>", close => "</ul>"},
      list_item =>   #{open => "<li>", close => "</li>"},
      document =>    #{open => "<div>", close => "</div>"},
      soft_break =>  #{open => "", close => "<br />"}
     }.

html({str, Text}) ->
    Text;
html({italic, Text}) ->
    "<i>" ++ Text ++ "</i>";
html({emph, Text}) ->
    "<b>" ++ Text ++ "</b>";
html({code_fence, Text}) ->
    "<pre>" ++ Text ++ "</pre>";
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
