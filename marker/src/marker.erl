%%% marker module is the main marker lib module responsible for Markdown parsing.
-module(marker).
-include_lib("eunit/include/eunit.hrl").

-export([markdown/1]).

%% markdown/1 parses given string and returns a (possibly nested) list
%% describing the CommonMark (default flavour) document's structure.
%% Each element is either:
%%   * a tuple in form {type, [T]} describing subdocument, e.g. ordered list
%%   * a tuple in form {type, text, alttext}, where type is the type of the element
%%
%% Type returned in tuples described above can be one of:
%%   * normal
%%   * paragraph
%%   * italic
%%   * bold
%%   * link
markdown(T)
    -> {ok, to_paragraphs(T)}.

%% markdown/2 parses given string according to passed flavour.
%% At the moment the only supported flavour for the time being is CommonMark.
markdown(T, _)
    -> markdown(T).

%% to_paragraphs/1 splits given string into paragraph elements.
to_paragraphs(T)
    -> lists:map(fun parse/1,
        lists:map(fun(X) -> {par, X} end,
            lists:filter(fun(X) -> length(X) > 0 end,string:split(T, "\n", all)))).

parse({par, T})
    -> {par, [{normal, T}]}.

markdown_test() ->
    ?assertEqual({ok, [{par, [{normal, "testpar"}]}]}, markdown("testpar")),
    ?assertEqual({ok, [{par, [{normal, "testpar"}]}, {par, [{normal, "nextpar"}]}]}, markdown("testpar\n\nnextpar")).
    % ?assertEqual(
    %     {ok, [
    %         {par, [{bold, "bold text"}, {normal, "normal text"}, {italic, "italic"}]},
    %         {par, "nextpar"}
    %     ]},
    %     markdown(
    %         "**bold text** normal text _italic_\n\n"
    %         "# heading in next par"
    %     )).

partial_test() ->
    ?assertEqual(
        [{par, [{normal, "testpar"}]}],
        to_paragraphs("testpar")),
    ?assertEqual(
        [{par, [{normal, "testpar"}]}, {par, [{normal, "nextpar"}]}, {par, [{normal, "thirdpar"}]}],
        to_paragraphs("testpar\n\nnextpar\n\nthirdpar")).
