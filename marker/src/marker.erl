%%% marker module is the main marker lib module responsible for Markdown parsing.
%%% Parsing follows the parsing strategy described in
%%% https://spec.commonmark.org/0.30/#appendix-a-parsing-strategy
-module(marker).
-include_lib("eunit/include/eunit.hrl").

-export([markdown/1]).

%% markdown/1 parses given string and returns a (possibly nested) list
%% describing the CommonMark (default flavour) document's structure.
%%
%% The tree returned by the markdown/1 function has a regular recursive
%% structure with text elements as leaves. Each element may be in either
%% this form for non-leaves:
%%   {<type_atom>, [<child_element>,...]}
%% or this - leaves:
%%   {<type_atom>, <text>}
%%
%% Possible types of the elements:
%%   * paragraph
%%   * bullet_list
%%   * ordered_list
%%   * horizontal_line
%%   * block_quote
%%   * code_fence
%%   * inline_code
%%   * code_fence
%%   * heading
%%   * soft_break
%%   * list_item
%%   * italic
%%   * str
%%   * emph
%%   * link
markdown(T) ->
    Phase1Result = parse_into_blocks(T),
    {ok, parse_inlines(Phase1Result)}. % phase 2

%% phase1/2 parses given string according to passed flavour.
%% At the moment the only supported flavour for the time being is CommonMark,
%% so the second argument is ignored.
phase1(T, _)
    -> phase1(T).

phase1(T) ->
    {ok, parse_into_blocks(T)}.

%% parse_into_blocks/1 implements the first phase of CommonMark parsing strategy,
%% dividing input text into blocks.
%%
%% Returns a tuple representing a document tree, with {document, ...} as root.
parse_into_blocks(T)
  -> parse_doc({normal, {document, [], none}}, [], string:split(T, "\n", all)).

%% parse_doc/3 runs through all the lines and builds the document tree
%% described in phase 1 of the CommonMark algorithm. After blocks
%% are separated, each can be formatted using inline rules for emphasis,
%% italic or links.
%%
%% The first argument defines currently running mode,
%% it's added to simplify code blocks parsing.
parse_doc({normal, Document}, Buffer, [NextLine|T]) ->
    Line = line_to_block(NextLine),
    case Line of
        {code_fence, _, _} -> % begin of code block, switch mode
            parse_doc({codeblock, Document}, [], T);
        _ ->
            parse_doc({normal, merge_blocks(Document, Line)}, Buffer, T)
    end;
parse_doc({codeblock, Document}, Buffer, [NextLine|T]) ->
    Line = line_to_block(NextLine),
    case Line of
        % end of code block, get back to normal mode, store code block
        {code_fence, _, _} ->
            parse_doc({normal, merge_blocks(Document, {code_fence, [], Buffer})}, [], T);
        _ ->
            parse_doc({codeblock, Document}, Buffer ++ NextLine, T)
    end;
parse_doc({_, Document}, _, []) -> Document.

%% merge_blocks manages open and closed blocks, based on phase 1
%% of the CommonMark algorithm. It defines how the block from the read line
%% should be incorportated into the parsing tree.
merge_blocks(none, B) -> B;

merge_blocks(A, none) -> A;

merge_blocks(
  {bullet_list, CloseBullets, OpenItem},
  {bullet_list, _, OpenB}) ->
    {bullet_list, CloseBullets ++ [OpenItem], OpenB};

merge_blocks(
  {bullet_list, CloseBullets, OpenItem},
  B = {list_item, _, _}) ->
    {bullet_list, CloseBullets ++ [OpenItem], B};

merge_blocks(
  {ordered_list, CloseBullets, OpenItem},
  {ordered_list, _, OpenB}) ->
    {ordered_list, CloseBullets ++ [OpenItem], OpenB};

merge_blocks(
  {ordered_list, CloseBullets, OpenItem},
  B = {list_item, _, _}) ->
    {ordered_list, CloseBullets ++ [OpenItem], B};

merge_blocks(
  {block_quote, ClosedA, OpenA},
  {block_quote, _, OpenB}) ->
    {block_quote, ClosedA, merge_blocks(OpenA, OpenB)};

merge_blocks(
  {TypeA, ClosedA, OpenA = {code_fence, _, _}},
  B) ->
    {TypeA, ClosedA ++ [OpenA], B};

merge_blocks(
    {TypeA, ClosedA, OpenA = {heading, _, _}},
    B) ->
    {TypeA, ClosedA ++ [OpenA], B};

merge_blocks(
    {TypeA, ClosedA, OpenA = {horizontal_line, _, _}},
    B) ->
    {TypeA, ClosedA ++ [OpenA], B};

merge_blocks(
  {paragraph, ClosedA, OpenA},
  {paragraph, _, OpenB}) ->
    {paragraph, ClosedA, OpenA ++ "\n" ++ OpenB};

%% If the open block of A is a heading type
%% An B is a type of paragraph
%% Then merging those should close the heading of A
%% Otherwise the blocks are passed to merge_blocks
merge_blocks(
  {TypeA, ClosedA, OpenA = {TypeC, _, _}},
  B = {paragraph, _, _}) ->
    case lists:member(TypeC, heading_tags()) of
      true ->
        {TypeA, ClosedA ++ [OpenA], B};
      false ->
      {TypeA, ClosedA, merge_blocks(OpenA, B)}
    end;

merge_blocks(
  {TypeA, ClosedA, OpenA = {_, _, _}},
  B = {code_fence, _, _}) ->
      {TypeA, ClosedA++[OpenA], B};

merge_blocks({TypeA, ClosedA, none}, B) ->
    {TypeA, ClosedA, B};

merge_blocks(
  {TypeA, ClosedA, OpenA = {heading1, _, _}},
  B = {heading1, _, _}) ->
    {TypeA, ClosedA ++ [OpenA], B};

merge_blocks(
  {TypeA, ClosedA, OpenA = {Type, _, _}},
  {Type, _, OpenB}) ->
    {TypeA, ClosedA, merge_blocks(OpenA, OpenB)};

merge_blocks({TypeA, ClosedA, OpenA}, B) ->
    {TypeA, ClosedA ++ [OpenA], B}.

%% line_to_block/1 defines the way read lines are transformed into blocks.
line_to_block(">" ++ T) ->
    {block_quote, [], line_to_block(string:trim(T, leading))};

%% Thematic breaks
line_to_block(Line = "___" ++ T) ->
  case string:trim(T) of
    [] ->
      {horizontal_line, [], none};
    _ ->
      {paragraph, [], Line}
  end;

line_to_block(Line = "***" ++ T) ->
  case string:trim(T) of
    [] ->
      {horizontal_line, [], none};
    _ ->
      {paragraph, [], Line}
  end;

line_to_block(Line = "---" ++ T) ->
  case string:trim(T) of
    [] ->
      {horizontal_line, [], none};
    _ ->
      {paragraph, [], Line}
  end;

line_to_block(M = "```" ++ T) ->
  case string:trim(T) of
    [] ->
      {code_fence, [], ""};
    _ ->
      {paragraph, [], M ++ T}
  end;


%% Unordered list items
line_to_block("- " ++ T) ->
  {bullet_list, [], {list_item, [], line_to_block(string:trim(T, leading))}};

line_to_block("-" ++ T) ->
  {paragraph, [], "-" ++ T};


%% Ordered list items
line_to_block("1. " ++ T) ->
  {ordered_list, [], {list_item, [], line_to_block(string:trim(T, leading))}};

line_to_block("1." ++ T) ->
  {paragraph, [], "1." ++ T};


%% Headings
line_to_block("###### " ++ T) ->
    {heading6, [], line_to_block(string:trim(T, leading))};

line_to_block("##### " ++ T) ->
    {heading5, [], line_to_block(string:trim(T, leading))};

line_to_block("#### " ++ T) ->
    {heading4, [], line_to_block(string:trim(T, leading))};

line_to_block("### " ++ T) ->
    {heading3, [], line_to_block(string:trim(T, leading))};

line_to_block("## " ++ T) ->
    {heading2, [], line_to_block(string:trim(T, leading))};

line_to_block("# " ++ T) ->
    {heading1, [], line_to_block(string:trim(T, leading))};

line_to_block("#" ++ "") ->
    {heading1, [], none};

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

heading_tags() ->
  [heading1, heading2, heading3, heading4, heading5, heading6].

%% close/1 transforms parsing tree from the phase 1 of the CommonMark
%% parsing approach by "closing" all the blocks. It goes through all the parsing
%% tree and moves currently open block to the closed blocks list and drops
%% the third element from the list, i.e.
%%   {<type>, ClosedBlocks, OpenBlock} => {<type>, ClosedBlocks ++ [OpenBlock]}
close(none) ->
    none; % TODO: treat none as an empty string.
close({paragraph, [], T}) ->
    {paragraph, T};
close({soft_break, [], T}) ->
    {soft_break, T};
close({code_fence, [], T}) ->
    {code_fence, T};
close([H|T]) ->
    [close(H)|close(T)];
close({T, [], none}) ->
  {T, []};
close({T, ClosedBlocks, OpenBlock}) ->
    ClosedOpenBlock = close(OpenBlock),
    ClosedChildren = lists:map(fun close/1, ClosedBlocks),
    {T, ClosedChildren ++ [ClosedOpenBlock]}.


%% parse_inlines/1 implements the whole process of the phase 2 of CommonMark
%% parsing. It takes as input the tree of blocks from the phase 1
%% (parse_into_blocks/1 function) and:
%%   1. Closes all the created blocks on all levels, starting from the root.
%%   2. Walks the tree top to bottom.
%%   3. Replaces inline elements - paragraphs and headings - with their
%%      proper representation, i.e. formatting elements like emph and italic.
%%   4. Returns transformed tree that is ready for rendering by the render module.
parse_inlines(T) ->
    Closed = close(T),
    transform_tree_inlines(Closed).

%% transform_tree_inlines/1 traverse the parsing tree of closed blocks
%% and converts the content of inline elements - paragraphs and headings - into
%% the style elements like str, emph or italic.
transform_tree_inlines(T={paragraph, _}) ->
    parse_inline(T);
transform_tree_inlines({code_fence, T}) ->
    {code_fence, T};
transform_tree_inlines({Type, ClosedBlocks}) ->
    {Type, lists:map(fun transform_tree_inlines/1, ClosedBlocks)}.


%% parse_inline/1 implements the phase 2 of CommonMark parsing, i.e.
%% converting the content of paragraphs and headings into
%% a list of str, emph, italic etc. elements, ready to apply formatting
%% in the render part.
parse_inline({paragraph, T}) ->
    {paragraph, parse_inline_text(T, 0, [], [], [])}.

%% append_result/3 is just a helper function for parse_inline_text/5.
append_result(Rs, _, []) ->
    Rs;
append_result(Rs, Type, R) ->
    Rs ++ [{Type, R}].

%% parse_inline_text/5 goes through text and collects different formattings
%% inside Result list.
%% It accepts:
%%   * text left to be parsed
%%   * index of the current character
%%   * stack of formatting openers like '*'. '**', '`' etc.
%%     (TODO: fix openers removal, currently
%%   * buffer, i.e. text collected to be formatted with the next formatter
%%   * resulting list of pairs {<type>, <text>}
%% TODO: support links and images.
%% TODO: generalize opener/closer cases, as for now they cannot be nested.
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

%% Test functions below only
markdown_test() ->
    ?assertEqual({ok, {document, [{code_fence, "code"}]}}, markdown("```\ncode\n```")),
    ?assertEqual(
        {ok, {document, [{code_fence, "dsada"}, {paragraph, [{str, "d"}]}]}},
        markdown("```\ndsada\n```\nd")),
    ?assertEqual(
        {ok,{document,[{paragraph,[{inline_code,"inline code"}]}]}},
        markdown("`inline code`")).

phase1_test() ->
    ?assertEqual({ok, {document, [], none}}, phase1("")),

    ?assertEqual({ok, {document, [], {paragraph, [], "testpar"}}}, phase1("testpar")),

    % TODO Fix this issue with double none inputs one after another
    % ?assertEqual(
    %     {ok, {document, [{paragraph, [], "testpar"}], {paragraph, [], "nextpar"}}},
    %     phase1("testpar\n\nnextpar")),

    ?assertEqual(
        {ok, {document, [],
            {block_quote, [],
                {bullet_list, [{list_item, [], {paragraph, [], "Qui *quodsi iracundia*"}}],
                    {list_item, [], {paragraph, [], "aliquando id"}}}}}},
        phase1("> - Qui *quodsi iracundia*\n"
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
              {heading1, [], {paragraph, [], "header"}}}},
        phase1("testpar\n---\n# header")),

    ?assertEqual({ok, {document, [], {paragraph, [], "#Foo"}}}, phase1("#Foo\n")),

    ?assertEqual({ok, {document,[],{heading1,[],{paragraph,[],"Foo"}}}}, phase1("# Foo\n")),

    ?assertEqual({ok, {document,[{heading1,[],{paragraph,[],"Foo"}}],{heading1,[],{paragraph,[],"Bar"}}}}, phase1("# Foo\n# Bar\n")).

parse_into_blocks_test() ->
    ?assertEqual(
        {document, [], {block_quote, [], {paragraph, [], "Lorem ipsum dolor \nsit amet."}}},
        parse_into_blocks("> Lorem ipsum dolor \nsit amet.")),
    ?assertEqual(
        {document, [], {code_fence, [], "code"}},
        parse_into_blocks("```\ncode\n```")),
    ?assertEqual(
        {document, [{code_fence, [], "code"}], {paragraph, [], "d"}},
        parse_into_blocks("```\ncode\n```\nd")).

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
        line_to_block("---")),

    ?assertEqual(
       {heading1, [], {paragraph, [], "Foo"}},
       line_to_block("# Foo")),

    ?assertEqual(
       {paragraph, [], "#Foo"},
       line_to_block("#Foo")),

    ?assertEqual(
       {paragraph, [], "-Foo"},
       line_to_block("-Foo")),

    ?assertEqual(
       {code_fence, [], ""},
       line_to_block("```")).

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
            )),

    ?assertEqual(
        {document,[{heading1,[],{paragraph,[],"foo"}}], {heading1,[],{paragraph,[],"Bar"}}},
        merge_blocks({document, [], {heading1, [], {paragraph, [], "foo"}}},
                    {heading1, [], {paragraph, [],  "Bar"}})),
    ?assertEqual(
        {document,[], {code_fence, [], ""}},
        merge_blocks({document, [], none},
                    {code_fence, [], ""})),

    ?assertEqual(
        {document, [{code_fence, [], "code"}], {paragraph, [], "par"}},
        merge_blocks({document, [], {code_fence, [], "code"}},
                    {paragraph, [], "par"})).


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
        close({bullet_list,[{list_item, [], {paragraph, [], "Qui *quodsi iracundia*"}}],{list_item, [], {paragraph, [], "aliquando id"}}})),
    ?assertEqual(
        {document, [{code_fence, ""}]},
        close({document, [], {code_fence, [], ""}})).

parse_inlines_test() ->
    ?assertEqual(
        {document, [{paragraph, [{italic, "test"}, {str, " par "}, {emph, "agraph"}]}]},
        parse_inlines({document, [], {paragraph, [], "*test* par **agraph**"}})),
    ?assertEqual(
        {document, [{code_fence, ""}]},
        parse_inlines({document,[], {code_fence, [], ""}})).

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
