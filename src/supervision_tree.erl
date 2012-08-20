% $legal:1594:
% 
% Copyright (c) 2011, Michael Lowell Roberts.  
% All rights reserved. 
% 
% Redistribution and use in source and binary forms, with or without 
% modification, are permitted provided that the following conditions are 
% met: 
% 
%   - Redistributions of source code must retain the above copyright 
%   notice, this list of conditions and the following disclaimer. 
% 
%   - Redistributions in binary form must reproduce the above copyright 
%   notice, this list of conditions and the following disclaimer in the 
%   documentation and/or other materials provided with the distribution.
%  
%   - Neither the name of the copyright holder nor the names of 
%   contributors may be used to endorse or promote products derived 
%   from this software without specific prior written permission. 
% 
% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS 
% IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED 
% TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A 
% PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER 
% OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED 
% TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR 
% PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF 
% LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING 
% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS 
% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
% 
% ,$

-module(supervision_tree).
-behaviour(supervisor).

-export([
      find_child/2,
      start_link/2, 
      test/0
   ]).

%% supervisor callbacks
-export([init/1]).

% @doc a primitive test harness for the {@module} module.
test() ->
   % [mlr][todo] i should put together a more elaborate test.
   application:start(sasl),
   application:start(supervision_tree),
   supervision_tree:start_link([], 
      [
         {worker, 
             {supervision_tree_test, start_link, 
                 [{local, supervision_tree_test}]}, defaults}]),
   Pid = whereis(supervision_tree_test),
   undefined = gen_server:call(Pid, recall),
   ok = gen_server:call(Pid, {store, 'o hai!'}),
   'o hai!' = gen_server:call(Pid, recall),
   ok.

% @doc creates a supervision tree as part of larger supervision tree.
% <ul>
%  <li>`Options' represents an option list that specifies how the
%  {@module} process should be created. for specific options that are allowed,
%  please see the overview page.</li> 
%  <li>`Children' represents a list of child node specifications. for more
%  details regarding the form of these specifications, please see the overview
%  page.</li> 
% </ul>
% @todo link to the overview page.
start_link(Options, Children) when is_list(Children) ->
   {Module, Function, Args} = start_using_option(Options, Children),
   apply(Module, Function, Args).

%% @private
init([Options, Children]) ->
   supervisor_specification(Options, Children).

supervisor_specification(Options, Children) ->
   {MaxRestarts, PeriodSec} = maximum_restart_frequency_option(Options),
   ChildSpecs = child_specifications(Children),
   {ok, 
      {{restart_strategy_option(Options), MaxRestarts, PeriodSec}, ChildSpecs}}.

child_specifications(Children) ->
   lists:map(fun child_specification/1, Children).
      
child_specification({worker, StartUsing, Options}) ->
   {name_option(StartUsing, Options), StartUsing, restart_option(Options), 
      shutdown_option(worker, Options), worker, 
      [callback_module_option(StartUsing, Options)]};
child_specification({supervisor, Options, Children}) ->
   StartUsing = start_using_option(Options, Children),
   {name_option(StartUsing, Options), StartUsing, restart_option(Options), 
      shutdown_option(supervisor, Options), supervisor, 
      [callback_module_option(StartUsing, Options)]};
child_specification({supervision_tree, Module, Options, Children}) ->
   StartUsing = {Module, start_link, [Options, Children]},
   {name_option(StartUsing, Options), StartUsing, restart_option(Options), 
      shutdown_option(supervisor, Options), supervisor, 
      [callback_module_option(StartUsing, Options)]};
child_specification({gen_event_manager, StartUsing, Options}) ->
   {name_option(StartUsing, Options), StartUsing, restart_option(Options), 
      shutdown_option(worker, Options), worker, dynamic}.

option_default(Name) ->
   {ok, Value} = application:get_env(supervision_tree, Name),
   Value.

specification_option(SpecName, Options, DefaultName) ->
   supervision_tree_misc:find_option(SpecName, Options, option_default(DefaultName)).

restart_strategy_option(Options) ->
   specification_option(restart_strategy, Options, default_restart_strategy).

maximum_restart_frequency_option(Options) ->
   {{restarts, Count}, per, Period} =
      specification_option(maximum_restart_frequency, Options, 
         default_maximum_restart_frequency),
   {seconds, Sec} = supervision_tree_misc:convert(seconds, Period),
   {Count, Sec}.

restart_option(Options) ->
   specification_option(restart, Options, default_restart).

shutdown_option(Type, Options) ->
   Option = case Type of
      worker ->
         specification_option(restart, Options, default_worker_shutdown);
      supervisor ->
         % [mlr] "If the child process is another supervisor, Shutdown should be
         % set to infinity to give the subtree ample time to shutdown. It is
         % also allowed to set it to infinity, if the child process is a worker.
         % see <http://www.erlang.org/doc/man/supervisor.html>.
         supervision_tree_misc:find_option(restart, Options, {wait, forever})
   end,
   case Option of
      {wait, forever} ->
         infinity;
      {wait, Period} ->
         {milliseconds, Msec} = supervision_tree_misc:convert(milliseconds, Period),
         Msec;
      kill ->
         brutal_kill
   end.

registered_supervisor_option(Options) ->
   case supervision_tree_misc:find_option(registered_supervisor, Options, false) of
      false ->
         false;
      {local, Local} when is_atom(Local) ->
         {local, Local};
      {global, Global} when is_atom(Global) ->
         {global, Global};
      {via, Module, Name} when is_atom(Module) andalso is_atom(Name) ->
         {via, Module, Name};
      Other ->
         error(
            {badarg, 
               {option, {registered_supervisor, Other}}, 
               {message, 
                  "the form of the `registered_supervisor` option must match"
                  "the *SupName* argument would be passed into "
                  "`supervisor:start_link()`. please refer to the relevant "
                  "erlang documentation for more details."}})
   end.

start_using_option(Options, Children) ->
   Args = [Options, Children],
   case registered_supervisor_option(Options) of
      false ->
         supervision_tree_misc:find_option(start_using, Options, 
            {supervisor, start_link, [supervision_tree, Args]});
      Other ->
         supervision_tree_misc:find_option(start_using, Options,
            {supervisor, start_link, [Other, supervision_tree, Args]})
   end.

callback_module_option(StartUsing, Options) ->
   Default = case StartUsing of
      {supervisor, start_link, [Module1, _]} ->
         Module1;
      {supervisor, start_link, [_, Module2, _]} ->
         Module2;
      {gen_server, start_link, [Module3, _, _]} ->
         Module3;
      {gen_server, start_link, [_, Module4, _, _]} ->
         Module4;
      {gen_fsm, start_link, [Module5, _, _]} ->
         Module5;
      {gen_fsm, start_link, [_, Module6, _, _]} ->
         Module6;
      {Other, _, _} ->
         Other
   end,
   supervision_tree_misc:find_option(callback_module, Options, Default).

name_option({Module, _, _}, Options) ->
   supervision_tree_misc:find_option(name, Options, Module).

find_child(SupervisorPid, ChildId) ->
   % [mlr][todo] the erlang documentation refers to a child's name as an "id."
   Children = supervisor:which_children(SupervisorPid),
   Result = [Pid || {Id, Pid, _, _} <- Children, Id =:= ChildId],
   case Result of
      [Found] ->
         {ok, Found};
      [] ->
         {not_found, {id, ChildId}}
   end.

% $vim:23: vim:set sts=3 sw=3 et:,$

