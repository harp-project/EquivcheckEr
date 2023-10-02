-module(pt).

-export([parse_transform/2]).

-include("codegen.hrl").

parse_transform(Forms, _Options) ->
    % io:fwrite("Forms = ~p~n", [Forms]),
    % Forms.
    parse_trans:plain_transform(fun do_transform/1, Forms).

do_transform({'op', L, '!', Lhs, Rhs}) ->
    [NewRhs] = parse_trans:plain_transform(fun transform_send/1, [Rhs]),
    {'op', L, '!', Lhs, NewRhs};
do_transform({'receive', _, _} = T) ->
    hd(parse_trans:plain_transform(fun transform_receive/1, [T]));
do_transform(_) ->
    continue.


% In case of sending a message, what we want is to send a message as is, but also
% to log it somehow (e.g.: we are printing it out in this example)
% Because the RHS of ! doesn't allow sequencing with ',', we have to somehow wrap the
% print statement and the expressin we are sending, so they will be represented by a
% single syntactic unit
% That can be done by just having them as the body of a lambda, which is then applied immediately
% Pid ! {self(), hello}
% Pid ! (fun() ->
%                io:format("~p~n",[{self(), hello}]),
%                {self(), hello}
%        end)()

transform_send(T) ->
    L = erl_syntax:get_pos(T),
    {call,
     L,
     {'fun',
      L,
      {clauses,
       [{clause,
         L,
         [],[],
         [{call,
           L,
           {remote,L,{atom,L,io},{atom,L,format}},
           [{string,L,"Sent: ~p~n"},{cons,L,T,{nil,L}}]}, % This is where we log the message
          T % This is the original message
         ]}]}},
     []}.

% When receiving messages, we want to first populate the mailbox of the process,
% so reading from it won't block
% We can match on the receive node in the AST, and replace it with a lambda that
% uses PropEr to generate a list of random data (I'm using any for now), then
% send it to itself
% fun() -> 
%          {ok, RandomData} = proper_gen:pick(proper_types:list(proper_types:any()), 100),
%          lists:map(fun(X) -> self() ! X end, RandomData),
%          receive
%              {Pid, Msg} ->
%                  io:format("~p~n",[Msg])
%          end
%  end().
%
% Later it would be better to look at the match clasuses inside the receive and generate
% data based on that
transform_receive(T) ->
    L = erl_syntax:get_pos(T),
    {call,
     L,
     {'fun',
      L,
      {clauses,
       [{clause,
         L,
         [],[],
         [{match,
           L,
           {tuple,
            L,
            [{atom,L,ok},{var,L,'RandomData'}]},
           {call,
            L,
            {remote,
             L,
             {atom,L,proper_gen},
             {atom,L,pick}},
            [{call,
              L,
              {remote,
               L,
               {atom,L,proper_types},
               {atom,L,list}},
              [{call,
                L,
                {remote,
                 L,
                 {atom,L,proper_types},
                 {atom,L,any}},
                []}]},
             {integer,L,1000}]}},
          {call,
           L,
           {remote,L,{atom,L,lists},{atom,L,map}},
           [{'fun',
             L,
             {clauses,
              [{clause,
                L,
                [{var,L,'X'}],
                [],
                [{op,
                  L,
                  '!',
                  {call,L,{atom,L,self},[]},
                  {var,L,'X'}}]}]}},
            {var,L,'RandomData'}]},
          T
         ]}]}},
     []}.

% get_types(Clauses) ->
%     erlang:display(erl_syntax_lib:analyze_form(Clauses)).
% lists:map(fun({_,_,[P],_,_}) -> erlang:display(erl_syntax:type(P)) end, Clauses).
