-module(functions).

-export([functions/2]).

-type fun_name() :: string().
-type fun_arity() :: integer().
-type line_num() :: integer().
-type boundaries() :: {line_num(), line_num()}.
-type ast() :: erl_syntax:forms().
-type tokens() :: erl_scan:tokens().

% Finds all the functions in the AST, and pairs them up with their first and last line in the source
-spec functions(ast(), tokens()) -> [{fun_name(), fun_arity(), boundaries()}].
functions(AST, Tokens) ->
    Functions = lists:filter(fun(Tree) -> erl_syntax:type(Tree) =:= function end, AST),
    StartLines = lists:map(fun(Tree) -> erl_syntax:get_pos(leftmost_node(Tree)) end, Functions),
    RightmostLines = lists:map(fun(Tree) -> erl_syntax:get_pos(rightmost_node(Tree)) end, Functions),
    EndLines = lists:map(fun(EndLine) -> offset(EndLine, Tokens) end, RightmostLines),
    Names = lists:map(fun(Tree) -> erl_syntax:atom_literal(erl_syntax:function_name(Tree)) end, Functions),
    Arities = lists:map(fun erl_syntax:function_arity/1, Functions),
    lists:zip3(Names, Arities, lists:zip(StartLines, EndLines)).


-spec rightmost_node(ast()) -> ast().
rightmost_node(Tree) ->
    case erlang:is_list(Tree) of
        true -> rightmost_node(lists:last(Tree));
        false ->
            Children = erl_syntax:subtrees(Tree),
            case lists:reverse(Children) of
                [] -> Tree;
                [RightmostChild | _] ->
                    case RightmostChild of
                        [] -> Tree;
                        _  -> rightmost_node(RightmostChild)
                    end
            end
    end.

-spec leftmost_node(ast()) -> ast().
leftmost_node(Tree) ->
    case erlang:is_list(Tree) of
        true -> leftmost_node(hd(Tree));
        false ->
            Children = erl_syntax:subtrees(Tree),
            case Children of
                [] -> Tree;
                [LeftMostChild | _] -> leftmost_node(LeftMostChild)
            end
    end.

% Finds the number of lines after the last meaningful node in the AST
% This is needed because we want the last line of the function based on the
% source file, not from the AST, which doesn't deal with dots and other "unnecessary"
% syntactic elements
-spec offset(line_num(), tokens()) -> line_num().
offset(LineNum, Tokens) ->
    Rest = lists:dropwhile(fun(T) -> element(2, T) =/= LineNum end, Tokens),
    % We use dot as the last token from the function
    {value, EndToken} = lists:search(fun(T) -> element(1, T) =:= dot end, Rest),
    element(2, EndToken).
