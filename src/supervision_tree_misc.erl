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

-module(supervision_tree_misc).

%% API
-export([
      convert/2,
      convert_float/2,
      convert_int/2,
      find_option/2,
      find_option/3
   ]).

convert(To, {_, Scalar} = From) when is_integer(Scalar) ->
   convert_int(To, From);
convert(To, From) ->
   convert_float(To, From).

convert_int(To, From) ->
   {To, Scalar} = convert_float(To, From),
   {To, round(Scalar)}.

convert_float(To, {From, Scalar}) ->
   {Unit, Multiplier} = normalize_unit(From),
   {Unit, Divisor} = normalize_unit(To),
   {To, Scalar * Multiplier / Divisor}.

normalize_unit(hours) ->
   {seconds, 3600};
normalize_unit(minutes) ->
   {seconds, 60};
normalize_unit(seconds) ->
   {seconds, 1};
normalize_unit(milliseconds) ->
   {seconds, 1.0e-3};
normalize_unit(microseconds) ->
   {seconds, 1.0e-6}.

%-spec find(atom(), options()) -> ok(any()) | not_found(atom).
find_option(Name, defaults) ->
   {not_found, Name};
find_option(Name, Options) ->
   case proplists:lookup(Name, Options) of
      none ->
         {not_found, Name};
      {Name, Value} ->
	 {ok, Value}
   end.

%-spec find(atom(), options(), any()) -> ok(any()).
% [mlr][todo] rename this method, since it has a different return signature.
find_option(Name, Options, Default) ->
   case find_option(Name, Options) of
      {not_found, Name} ->
	 Default;
      {ok, Value} ->
	 Value
   end.

% $vim:23: vim:set sts=3 sw=3 et:,$
