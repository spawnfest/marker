%%% marker module is the main marker lib module responsible for Markdown parsing.
%%% Parsing follows the parsing strategy described in
%%% https://spec.commonmark.org/0.30/#appendix-a-parsing-strategy
-module(marker).
-include_lib("eunit/include/eunit.hrl").

-export([markdown/1]).

%% markdown/1 parses given string and returns a (possibly nested) list
%% describing the CommonMark (default flavour) document's structure.
%%
%% Possible types of the elements:
%%   * str
%%   * paragraph
%%   * bullet_list
%%   * horizontal_line
%%   * block_quote
%%   * inline_code
%%   * heading
%%   * soft_break
%%   * list_item
%%   * italic
%%   * emph
%%   * link
markdown(T)
    -> {ok, parse_inlines( % phase 2
        parse_into_blocks(T))}. % phase 1

%% phase1/2 parses given string according to passed flavour.
%% At the moment the only supported flavour for the time being is CommonMark.
phase1(T, _)
    -> phase1(T).

phase1(T) ->
    {ok, parse_into_blocks(T)}.

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

merge_blocks(
  {bullet_list, CloseBullets, OpenItem},
  {bullet_list, _, OpenB}) ->
    {bullet_list, CloseBullets ++ [OpenItem], OpenB};

merge_blocks(
  {bullet_list, CloseBullets, OpenItem},
  B = {list_item, _, _}) ->
    {bullet_list, CloseBullets ++ [OpenItem], B};

merge_blocks(
  {block_quote, ClosedA, OpenA},
  {block_quote, _, OpenB}) ->
    {block_quote, ClosedA, merge_blocks(OpenA, OpenB)};

merge_blocks(
    {TypeA, ClosedA, OpenA = {heading, _, _}},
    B) ->
    {TypeA, ClosedA ++ [OpenA], B};

merge_blocks(
  {paragraph, ClosedA, OpenA},
  {paragraph, _, OpenB}) ->
    {paragraph, ClosedA, OpenA ++ "\n" ++ OpenB};

merge_blocks(
  {TypeA, ClosedA, OpenA},
  B = {paragraph, _, _}) ->
    {TypeA, ClosedA, merge_blocks(OpenA, B)};

merge_blocks({TypeA, ClosedA, none}, B) ->
    {TypeA, ClosedA, B};

merge_blocks(
  {TypeA, ClosedA, OpenA = {Type, _, _}},
  {Type, _, OpenB}) ->
    {TypeA, ClosedA, merge_blocks(OpenA, OpenB)};

merge_blocks({TypeA, ClosedA, OpenA}, B) ->
    {TypeA, ClosedA ++ [OpenA], B}.


line_to_block([62|T]) ->
    {block_quote, [], line_to_block(string:strip(T, left))};
line_to_block(S=[45|T]) ->
    case string:prefix(S, "---") of
        nomatch ->
            {bullet_list, [], {list_item, [], line_to_block(string:strip(T, left))}};
        _ ->
            {horizontal_line, [], none}
    end;
line_to_block([35|T]) -> % TODO: implement different level of headings
    {heading, [line_to_block(string:strip(T, left))], {soft_break, [], ""}};
line_to_block([]) -> none;
line_to_block(T) -> {paragraph, [], T}.

%% Base cases for our block_to_string/1 function
%% are either a paragraph block or a none atom
block_to_string({paragraph, [], Text}) ->
    "paragraph\n" ++ Text;

block_to_string(none) ->
    "";

block_to_string({Type, ClosedBlocks, OpenBlock}) ->
    atom_to_list(Type) ++ "\n" ++ blocks_to_string(ClosedBlocks) ++ block_to_string(OpenBlock).

%% When there are no blocks in the array
blocks_to_string([]) ->
    "";

blocks_to_string([Block|Rest]) ->
    block_to_string(Block) ++ blocks_to_string(Rest).

block_to_string_test() ->
    ?assertEqual(
       "paragraph\nfoo",
       block_to_string({paragraph, [], "foo"})
      ),

    ?assertEqual(
       "document\n"
       "paragraph\n"
       "foo",
       block_to_string({document, [{paragraph,[],"foo"}], none})
      ).

%% close/1 transforms parsing tree from the phase 1 of the CommonMark
%% parsing approach by "closing" all the blocks. It goes through all the parsing
%% tree and moves currently open block to the closed blocks list and drops
%% the third element from the list, i.e.
%%   {<type>, ClosedBlocks, OpenBlock} => {<type>, ClosedBlocks ++ [OpenBlock]}
close(none) ->
    {paragraph, []}; % TODO: treat none as an empty string.
close({paragraph, [], T}) ->
    {paragraph, T};
close({soft_break, [], T}) ->
    {soft_break, T};
close([H|T]) ->
    [close(H)|close(T)];
close({T, ClosedBlocks, OpenBlock}) ->
    ClosedOpenBlock = close(OpenBlock),
    ClosedChildren = lists:map(fun close/1, ClosedBlocks),
    {T, ClosedChildren ++ [ClosedOpenBlock]}.


%% parse_inlines implements the whole process of the phase 2 of CommonMark
%% parsing. It takes as input the tree of blocks from the phase 1
%% (parse_into_blocks function) and:
%%   1. Closes all the created blocks on all levels, starting from the root.
%%   2. Walks the tree top to bottom.
%%   3. Replaces inline elements - paragraphs and (TODO) headings - with their
%%      proper representation, i.e. formatting elements like emph and italic.
%%   4. Returns transformed tree that is ready for rendering by render module.
parse_inlines(T) ->
    Closed = close(T),
    transform_tree_inlines(Closed).

%% transform_tree_inlines traverse the parsing tree of closed blocks
%% and converts inline elements - paragraphs and headings - into style elements
%% like str, emph or italic.
transform_tree_inlines(T={paragraph, _}) ->
    parse_inline(T);
transform_tree_inlines({Type, ClosedBlocks}) ->
    {Type, lists:map(fun transform_tree_inlines/1, ClosedBlocks)}.

parse_inlines_test() ->
    ?assertEqual(
        {document, [{paragraph, [{italic, "test"}, {str, " par "}, {emph, "agraph"}]}]},
        parse_inlines({document, [], {paragraph, [], "*test* par **agraph**"}})).

%% parse_inline implements the phase 2 of CommonMark parsing, i.e.
%% converting the content of paragraphs and headings into
%% a list of str, emph, italic etc. elements, ready to apply formatting
%% in the render part.
parse_inline({paragraph, T}) ->
    {paragraph, parse_inline_text(T, 0, [], [], [])}.

%% parse_inline_text reads the text T, goes through characters and produces
%% the list of {<type>, <subseq>} pair with proper formatting, where
%% <type> is one of:
%%   * emph
%%   * italic
%%   * inline_code
%%   * soft_break
%% TODO: support links and images.
append_result(Rs, _, []) ->
    Rs;
append_result(Rs, Type, R) ->
    Rs ++ [{Type, R}].

%% parse_inline_text goes through text and collects different formattings
%% inside Result list.
%% It accepts:
%%   * text left to be parsed
%%   * index of the current character
%%   * stack of formatting openers like '*'. '**', '`' etc.
%%     (TODO: fix openers removal, currently
%%   * buffer, i.e. text collected to be formatted with the next formatter
%%   * resulting list of pairs {<type>, <text>}
%% TODO: generalize opener/closer cases
parse_inline_text([], _, _, [], Result) -> Result;
parse_inline_text([], _, _, Buffer, Result) -> Result ++ [{str, Buffer}];
%% inline code
parse_inline_text([96|T], N, Stack, Buffer, Result) ->
    case  lists:keyfind(inline_code, 1, Stack) of
        % there's no opener on the stack, add new one
        false ->
            parse_inline_text(T, N+1, [{inline_code, N}|Stack], "", append_result(Result, str, Buffer));
        % there was an opener on Pos position, add new italic element to result
        {inline_code, _} ->
            parse_inline_text(T, N+1, lists:keydelete(inline_code, 1, Stack), "", append_result(Result, inline_code, Buffer))
    end;
%% emphasis
parse_inline_text([42|T], N, Stack, Buffer, Result) ->
    case T of
        [42|S] ->
            case  lists:keyfind(emph, 1, Stack) of
                % there's no opener on the stack, add new one
                false ->
                    parse_inline_text(S, N+2, [{emph, N}|Stack], "", append_result(Result, str, Buffer));
                % there was an opener on Pos position, add new emph element to result
                {emph, _} ->
                    parse_inline_text(S, N+2, lists:keydelete(emph, 1, Stack), "", append_result(Result, emph, Buffer))
            end;
        _ ->
            case  lists:keyfind(italic, 1, Stack) of
                % there's no opener on the stack, add new one
                false ->
                    parse_inline_text(T, N+1, [{italic, N}|Stack], "", append_result(Result, str, Buffer));
                % there was an opener on Pos position, add new italic element to result
                {italic, _} ->
                    parse_inline_text(T, N+1, lists:keydelete(italic, 1, Stack), "", append_result(Result, italic, Buffer))
            end
    end;
parse_inline_text([H|T], N, Stack, Buffer, Result) ->
    parse_inline_text(T, N+1, Stack, Buffer ++ [H], Result).


phase1_test() ->
    ?assertEqual({ok, {document, [], none}}, phase1("")),
    ?assertEqual({ok, {document, [], {paragraph, [], "testpar"}}}, phase1("testpar")),
    ?assertEqual(
        {ok, {document, [{paragraph, [], "testpar"}], {paragraph, [], "nextpar"}}},
        phase1("testpar\n\nnextpar")),
    ?assertEqual(
        {ok, {document, [],
            {block_quote, [],
                {bullet_list, [{list_item, [], {paragraph, [], "Qui *quodsi iracundia*"}}],
                    {list_item, [], {paragraph, [], "aliquando id"}}}}}},
        phase1(
"> - Qui *quodsi iracundia*\n"
"> - aliquando id")),
    ?assertEqual(
        {ok, {document, [],
            {block_quote, [{paragraph, [], "Lorem ipsum dolor\nsit amet."}],
                {bullet_list, [{list_item, [], {paragraph, [], "Qui *quodsi iracundia*"}}],
                    {list_item, [], {paragraph, [], "aliquando id"}}}}}},
        phase1("> Lorem ipsum dolor\n"
                 "sit amet.\n"
                 "> - Qui *quodsi iracundia*\n"
                 "> - aliquando id")),
    ?assertEqual(
        {ok, {document,
            [{paragraph, [], "testpar"}, {horizontal_line, [], none}],
            {heading, [{paragraph, [], "header"}], {soft_break, [], []}}}},
        phase1("testpar\n---\n# header")).


parse_doc_test() ->
    ?assertEqual(
        {document, [], {block_quote, [], {paragraph, [], "Lorem ipsum dolor \nsit amet."}}},
        parse_into_blocks("> Lorem ipsum dolor \nsit amet.")).

line_to_block_test() ->
    ?assertEqual(
        {block_quote, [], {paragraph, [], "Lorem ipsum dolor \nsit amet."}},
        line_to_block("> Lorem ipsum dolor \nsit amet.")),
    ?assertEqual(
        {block_quote, [],
            {bullet_list, [],
                {list_item, [],
                    {paragraph, [], "Qui *quodsi iracundia*"}}}},
        line_to_block("> - Qui *quodsi iracundia*")),
    ?assertEqual(
        {horizontal_line, [], none},
        line_to_block("---")).

merge_blocks_test() ->
    ?assertEqual(
        {paragraph, [], "Lorem ipsum dolor\n sit amet."},
        merge_blocks(
            {paragraph, [], "Lorem ipsum dolor"},
            {paragraph, [], " sit amet."}
            )),
    ?assertEqual(
        {block_quote, [], {paragraph, [], "Lorem ipsum dolor\n sit amet."}},
        merge_blocks(
            {block_quote, [], {paragraph, [], "Lorem ipsum dolor"}},
            {paragraph, [], " sit amet."}
            )),
    ?assertEqual(
        {document, [], {block_quote, [], {paragraph, [], "Lorem ipsum dolor\n sit amet."}}},
        merge_blocks(
            {document, [], {block_quote, [], {paragraph, [], "Lorem ipsum dolor"}}},
            {block_quote, [], {paragraph, [], " sit amet."}}
            )),
     ?assertEqual(
        {document, [{heading, [], {paragraph, [], "Lorem ipsum dolor"}}], {paragraph, [], " sit amet."}},
        merge_blocks(
            {document, [], {heading, [], {paragraph, [], "Lorem ipsum dolor"}}},
            {paragraph, [], " sit amet."}
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


close_test() ->
    ?assertEqual(
        {paragraph, "par"},
        close({paragraph, [], "par"})),
    ?assertEqual(
        {list_item, [{paragraph, "par"}]},
        close({list_item, [], {paragraph, [], "par"}})),
    ?assertEqual(
        {document, [{paragraph, "testpar"}]},
        close({document, [], {paragraph, [], "testpar"}})),
    ?assertEqual(
        {bullet_list,
            [
                {paragraph, "Qui *quodsi iracundia*"},
                {paragraph, "aliquando id"}
            ]},
        close({bullet_list,
                  [{paragraph, [], "Qui *quodsi iracundia*"}],
                   {paragraph, [], "aliquando id"}})),
    ?assertEqual(
        {bullet_list,
            [
                {list_item, [{paragraph, "Qui *quodsi iracundia*"}]},
                {list_item, [{paragraph, "aliquando id"}]}
            ]},
    close({bullet_list,[{list_item, [], {paragraph, [], "Qui *quodsi iracundia*"}}],{list_item, [], {paragraph, [], "aliquando id"}}})).



parse_inline_test() ->
    ?assertEqual(
        {paragraph, [{str, "aliquando id"}]},
        parse_inline(
            {paragraph, "aliquando id"})),
    ?assertEqual(
        {paragraph, [{str, "Qui "}, {italic, "quodsi iracundia"}]},
        parse_inline(
            {paragraph, "Qui *quodsi iracundia*"})),
    ?assertEqual(
        {paragraph, [{str, "Qui "}, {italic, "quodsi"}, {str, " "}, {emph, "iracundia"}]},
        parse_inline(
            {paragraph, "Qui *quodsi* **iracundia**"})),
    ?assertEqual(
        {paragraph, [{italic, "test"}, {str, " par "}, {emph, "agraph"}]},
        parse_inline({paragraph, "*test* par **agraph**"})).
