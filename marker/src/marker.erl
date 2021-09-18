%%% marker module is the main marker lib module responsible for Markdown parsing.
%%% Parsing follows the parsing strategy described in
%%% https://spec.commonmark.org/0.30/#appendix-a-parsing-strategy
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
%%   * str
%%   * paragraph
%%   * bullet_list
%%   * list_item
%%   * italic
%%   * emph
%%   * link
markdown(T)
    -> {ok, parse_into_blocks(T)}.

%% markdown/2 parses given string according to passed flavour.
%% At the moment the only supported flavour for the time being is CommonMark.
markdown(T, _)
    -> markdown(T).

%% parse_blocks implements the first phase of CommonMark parsing strategy,
%% dividing input text into blocks.
%% Returns a list representing a document tree.
parse_into_blocks(T)
    -> parse_doc({document, [], none}, string:split(T, "\n", all)).

%% parse_doc runs through all the lines and builds the document tree
%% describe in phase 1 of the CommonMark algorithm. After blocks
%% are separated, each can be formatted using inline rules for emphasis,
%% italic or links.
parse_doc(Document, [NextLine|T]) ->
    parse_doc(merge_blocks(Document, line_to_block(NextLine)), T);
parse_doc(Document, []) -> Document.

%% merge_blocks manages open and closed blocks, based on phase 1
%% of the CommonMark algorithm.
merge_blocks(none, X) -> X;

%% if the previous block's open block
%% was none and we've got another empty block
%% then we simply return the previous block
merge_blocks(
  A = {TypeA, ClosedA, none},
  {empty, _, _}) ->
    A;

merge_blocks(
    {TypeA, ClosedA, OpenA},
    {empty, _, _}) ->
        {TypeA, ClosedA ++ [OpenA], none};

merge_blocks(
    {bullet_list, CloseBullets, OpenItem},
    {bullet_list, _, OpenB}) ->
        {bullet_list, CloseBullets ++ [OpenItem], OpenB};

merge_blocks(
    {bullet_list, CloseBullets, OpenItem},
    B={list_item, _, _}) ->
        {bullet_list, CloseBullets ++ [OpenItem], B};

merge_blocks(
    {block_quote, ClosedA, OpenA},
    {block_quote, _, OpenB}) ->
        {block_quote, ClosedA, merge_blocks(OpenA, OpenB)};

merge_blocks(
    {paragraph, ClosedA, OpenA},
    {paragraph, _, OpenB}) ->
        {paragraph, ClosedA, OpenA ++ "\n" ++ OpenB};

merge_blocks(
    {TypeA, ClosedA, OpenA},
    B={paragraph, _, _}) ->
        {TypeA, ClosedA, merge_blocks(OpenA, B)};

merge_blocks(
    {TypeA, ClosedA, none},
    B) ->
        {TypeA, ClosedA, B};

merge_blocks(
    {TypeA, ClosedA, OpenA = {Type, _, _}},
    {Type, _, OpenB}) ->
        {TypeA, ClosedA, merge_blocks(OpenA, OpenB)};

merge_blocks(
    {TypeA, ClosedA, OpenA},
    B) ->
        {TypeA, ClosedA ++ [OpenA], B}.


line_to_block([62|T]) ->
    {block_quote, [], line_to_block(string:strip(T, left))};
line_to_block([45|T]) ->
    {bullet_list, [], {list_item, [], line_to_block(string:strip(T, left))}};
line_to_block([]) -> {empty, [], []};
line_to_block(T) -> {paragraph, [], T}.

markdown_test() ->
    ?assertEqual({ok, {document, [], none}}, markdown("")),
    ?assertEqual({ok, {document, [], {paragraph, [], "testpar"}}}, markdown("testpar")),
    ?assertEqual(
        {ok, {document, [{paragraph, [], "testpar"}], {paragraph, [], "nextpar"}}},
        markdown("testpar\n\nnextpar")),
    ?assertEqual(
        {ok, {document, [],
            {block_quote, [],
                {bullet_list, [{list_item, [], {paragraph, [], "Qui *quodsi iracundia*"}}],
                    {list_item, [], {paragraph, [], "aliquando id"}}}}}},
        markdown(
"> - Qui *quodsi iracundia*\n"
"> - aliquando id")),
    ?assertEqual(
        {ok, {document, [],
            {block_quote, [{paragraph, [], "Lorem ipsum dolor sit amet."}],
                {bullet_list, [{list_item, [], {paragraph, [], "Qui *quodsi iracundia*"}}],
                    {list_item, [], {paragraph, [], "aliquando id"}}}}}},
        markdown(
"> Lorem ipsum dolor\n"
" sit amet.\n"
"> - Qui *quodsi iracundia*\n"
"> - aliquando id")).


parse_doc_test() ->
    ?assertEqual(
        {document, [], {block_quote, [], {paragraph, [], "Lorem ipsum dolor sit amet."}}},
        parse_into_blocks("> Lorem ipsum dolor \nsit amet.")).

line_type_test() ->
    ?assertEqual(
        {block_quote, [], {paragraph, [], "Lorem ipsum dolor \nsit amet."}},
        line_to_block("> Lorem ipsum dolor \nsit amet.")),
    ?assertEqual(
        {block_quote, [],
            {bullet_list, [],
                {list_item, [],
                    {paragraph, [], "Qui *quodsi iracundia*"}}}},
        line_to_block("> - Qui *quodsi iracundia*")).

merge_blocks_test() ->
    ?assertEqual(
        {paragraph, [], "Lorem ipsum dolor sit amet."},
        merge_blocks(
            {paragraph, [], "Lorem ipsum dolor"},
            {paragraph, [], " sit amet."}
            )),
    ?assertEqual(
        {block_quote, [], {paragraph, [], "Lorem ipsum dolor sit amet."}},
        merge_blocks(
            {block_quote, [], {paragraph, [], "Lorem ipsum dolor"}},
            {paragraph, [], " sit amet."}
            )),
    ?assertEqual(
        {document, [], {block_quote, [], {paragraph, [], "Lorem ipsum dolor sit amet."}}},
        merge_blocks(
            {document, [], {block_quote, [], {paragraph, [], "Lorem ipsum dolor"}}},
            {block_quote, [], {paragraph, [], " sit amet."}}
            )),
    ?assertEqual(
        {document, [],
            {block_quote, [{paragraph, [], "Lorem ipsum dolor"}],
                {bullet_list, [],
                    {list_item, [], {paragraph, [], "Qui *quodsi iracundia*"}}}}},
        merge_blocks(
            {document, [], {block_quote, [], {paragraph, [], "Lorem ipsum dolor"}}},
            {block_quote, [], {bullet_list, [], {list_item, [], {paragraph, [], "Qui *quodsi iracundia*"}}}}
            )),
    ?assertEqual(
        {bullet_list, [{list_item, [], {paragraph, [], "Qui *quodsi iracundia*"}}],
            {list_item, [], {paragraph, [], "aliquando id"}}},
        merge_blocks(
            {bullet_list, [], {list_item, [], {paragraph, [], "Qui *quodsi iracundia*"}}},
            {bullet_list, [], {list_item, [], {paragraph, [], "aliquando id"}}}
            )),
    ?assertEqual(
        {block_quote, [],
            {bullet_list, [{list_item, [], {paragraph, [], "Qui *quodsi iracundia*"}}],
            {list_item, [], {paragraph, [], "aliquando id"}}}},
        merge_blocks(
            {block_quote, [],
                {bullet_list, [],
                    {list_item, [], {paragraph, [], "Qui *quodsi iracundia*"}}}},
            {block_quote, [],
                {bullet_list, [],
                    {list_item, [], {paragraph, [], "aliquando id"}}}}
            )).
