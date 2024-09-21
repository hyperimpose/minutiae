%%--------------------------------------------------------------------
%% Copyright (C) 2023-2024 hyperimpose.org
%%
%% This file is part of minutiae.
%%
%% This program is free software: you can redistribute it and/or modify
%% it under the terms of the GNU Affero General Public License as published
%% by the Free Software Foundation, version 3.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU Affero General Public License for more details.
%%
%% You should have received a copy of the GNU Affero General Public License
%% along with this program.  If not, see <https://www.gnu.org/licenses/>.
%%--------------------------------------------------------------------

-module(minutiae_app).

-behaviour(application).

-export([start/2, stop/1]).


%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the application.
%% @end
%%--------------------------------------------------------------------

start(_Type, _Args) ->
    %% Start the top supervisor
    minutiae_sup:start_link().


stop(_State) ->
    ok.
