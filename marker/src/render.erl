%%% render module is responsible for rendering Markdown documents parsed
%%% by marker:marker function.
-module(render).
-include_lib("eunit/include/eunit.hrl").

-export([block_to_html/1]).

tags() ->
    #{
      paragraph =>   #{
                       open => "<p>",
                       close => "</p>"
                      },
      block_quote => #{
                       open => "<blockquote>",
                       close => "</blockquote>"
                      },
      bullet_list => #{
                       open => "<ul>",
                       close => "</ul>"
                      },
      list_item =>   #{
                       open => "<li>",
                       close => "</li>"
                      },
      document =>    #{
                       open => "<div>",
                       close => "</div>"
                      }
     }.

%% Base cases for our block_to_string/1 function
%% are either a paragraph block or a none atom.
block_to_html({paragraph, [], Text}) ->
    "<p>" ++ Text ++ "</p>"; %% Close tags here

block_to_html(none) ->
    "";

block_to_html({Type, ClosedBlocks, OpenBlock}) ->
    OpenTag = maps:get(open, maps:get(Type, tags())),
    CloseTag = maps:get(close, maps:get(Type, tags())),
    OpenTag ++ blocks_to_html(ClosedBlocks) ++ block_to_html(OpenBlock) ++ CloseTag.

%% When there are no blocks in the array
blocks_to_html([]) ->
    "";

blocks_to_html([Block = {Type, _, _}|Rest]) ->
    CloseTag = maps:get(close, maps:get(Type, tags())),
    block_to_html(Block) ++ blocks_to_html(Rest).

block_to_html_test() ->
    ?assertEqual("<div></div>",
                 block_to_html({document, [], none})),

    ?assertEqual("<div><p>foo</p></div>",
                 block_to_html({document,
                                [
                                 {paragraph, [], "foo"}
                                ],
                                none})),

    ?assertEqual("<div><ul><li></li></ul></div>",
                 block_to_html({document,
                                [],
                                {bullet_list, [], {list_item, [], none}}})).
