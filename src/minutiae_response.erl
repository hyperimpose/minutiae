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

%%%-------------------------------------------------------------------
%%% This module provides functions to convert the output of libminutia
%%% to Erlang friendly representations.
%%%
%%% Specifically the following functions will:
%%% - Convert binary keys to atoms
%%% - Remove keys meant for internal use (_link, _lang, _ttl etc.)
%%% - Use the atoms true, false to represent boolean values
%%%-------------------------------------------------------------------

-module(minutiae_response).

-export([http/1]).
-export_types([http/1]).


%%%===================================================================
%%% HTTP
%%%===================================================================

%%% Types ============================================================

-type http() :: http_html()
              | http_fallback()
              | http_lainchan_thread()
              | http_file_audio()
              | http_file_image()
              | http_file_video()
              | http_mupdf()
              | http_nitter()
              | http_reddit_comments()
              | http_twitter_tweet()
              | http_youtube_search()
              | http_youtube_video().

-type http_html() ::
        #{m        => http_html,
          t        => binary(),  % Title generated by minutia
          explicit => float()}.  % Adult content

-type http_fallback() ::
        #{m        => http_fallback,
          t        => binary(),   % Title generated by minutia
          explicit => float(),    % Adult content
          filename => binary(),   % From Content-Disposition
          mimetype => binary(),   % From Content-Type
          size     => binary()}.  % Human readable filesize

-type http_file_audio() ::
        #{m        => http_file_audio,
          t        => binary(),   % Title generated by minutia
          album    => binary(),   % From the tags
          artist   => binary(),   % From the tags
          date     => binary(),   % From the tags
          duration => binary(),   % Human readable: 02:12
          explicit => float(),    % Adult content
          filename => binary(),   % From Content-Disposition
          mimetype => binary(),   % From Content-Type
          size     => binary(),   % Human readable filesize
          title    => binary()}.  % From the tags

-type http_file_image() ::
        #{m        => http_file_image,
          t        => binary(),    % Title generated by minutia
          artist   => binary(),    % From the tags
          explicit => float(),     % Adult content
          filename => binary(),    % From Content-Disposition
          height   => integer(),   % In pixels
          mimetype => binary(),    % From Content-Type
          size     => binary(),    % Human readable filesize
          title    => binary(),    % From the tags
          width    => integer()}.  % In pixels

-type http_file_video() ::
        #{m        => http_file_video,
          t        => binary(),    % Title generated by minutia
          duration => binary(),    % Human readable: 01:12:54
          explicit => float(),     % Adult content
          filename => binary(),    % From Content-Disposition
          height   => integer(),   % In pixels
          mimetype => binary(),    % From Content-Type
          size     => binary(),    % Human readable filesize
          title    => binary(),    % From the tags
          width    => integer()}.  % In pixels

-type http_lainchan_thread() ::
        #{m        => http_lainchan_thread,
          t        => binary(),   % Title generated by minutia
          board    => binary(),   % The board of the post
          files    => integer(),  % Number of files posted
          post     => binary(),   % The post's content
          replies  => integer(),  % Number of replies in thread
          title    => string()}.  % Thread/post title

-type http_mupdf() ::
        #{m        => http_mupdf,
          t        => binary(),   % Title generated by minutia
          explicit => float(),    % Adult content
          mimetype => binary(),   % Content-Type header
          pages    => binary(),   % Number of pages in document
          size     => binary(),   % Human readable filesize
          title    => binary()}.  % Document metadata or snippet

-type http_nitter() ::
        #{m            => http_nitter,
          t            => binary(),    % Usually the username
          description  => binary(),    % Usually the tweet
          explicit     => float()}.  % Adult content

-type http_reddit_comments() ::
        #{m           => http_reddit_comments,
          t           => binary(),   % Title generated by minutia
          author_name => binary(),   % Username that made the post / comment
          subreddit   => binary(),   % The subreddit in which the post was made
          title       => binary()}.  % Post title

-type http_twitter_tweet() ::
        #{m => http_twitter_tweet,
          t => binary()}.  % Title generated by minutia

-type http_youtube_search() ::
        #{m       => http_youtube_search,
          t       => binary(),  % Title generated by minutia
          results => [#{short_url => binary(),     % youtu.be
                        name      => binary(),     % Video title
                        date      => binary(),     % 2 weeks ago
                        views     => binary(),     % 29.404 views
                        channel   => binary(),     % Channel name
                        duration  => binary(),     % 9:02
                        yt_id     => binary()}]}.  % Video id

-type http_youtube_video() ::
        #{m                => http_youtube_video,
          t                => binary(),    % Title by minutia
          author_name      => binary(),    % Channel name
          author_url       => binary(),    % Link to channel
          height           => integer(),   % In pixels
          html             => binary(),    % HTML embed
          thumbnail_height => integer(),   % In pixels
          thumbnail_width  => integer(),   % In pixels
          thumbnail_url    => binary(),    % Link to thumbnail
          title            => binary(),    % Video title
          type             => binary(),    % <<"video">> or ???
          width            => integer()}.  % In pixels

%%% Function =========================================================

-spec http(In :: map()) -> {ok, http()} | false | {error, Error :: binary()}.

http(#{<<"@">> := <<"false">>}) ->
    false;

http(#{<<"@">> := <<"error">>, <<"t">> := T}) ->
    {error, T};

http(#{<<"@">>        := <<"http:html">>,
       <<"t">>        := T,
       <<"explicit">> := Explicit}) ->
    {ok, #{m        => http_html,
           t        => T,
           explicit => binary_to_float(Explicit)}};

http(#{<<"@">>        := <<"http:fallback">>,
       <<"t">>        := T,
       <<"mimetype">> := Mimetype,
       <<"explicit">> := Explicit,
       <<"filename">> := Filename,
       <<"size">>     := Size}) ->
    {ok, #{m        => http_fallback,
           t        => T,
           mimetype => Mimetype,
           explicit => binary_to_float(Explicit),
           filename => Filename,
           size     => Size}};

http(#{<<"@">>        := <<"http:file:audio">>,
       <<"t">>        := T,
       <<"album">>    := Album,
       <<"artist">>   := Artist,
       <<"date">>     := Date,
       <<"duration">> := Duration,
       <<"explicit">> := Explicit,
       <<"filename">> := Filename,
       <<"mimetype">> := Mimetype,
       <<"size">>     := Size,
       <<"title">>    := Title}) ->
    {ok, #{m        => http_file_audio,
           t        => T,
           album    => Album,
           artist   => Artist,
           mimetype => Mimetype,
           date     => Date,
           duration => Duration,
           explicit => binary_to_float(Explicit),
           filename => Filename,
           size     => Size,
           title    => Title}};

http(#{<<"@">>        := <<"http:file:image">>,
       <<"t">>        := T,
       <<"artist">>   := Artist,
       <<"explicit">> := Explicit,
       <<"filename">> := Filename,
       <<"height">>   := Height,
       <<"mimetype">> := Mimetype,
       <<"size">>     := Size,
       <<"title">>    := Title,
       <<"width">>    := Width}) ->
    {ok, #{m        => http_file_image,
           t        => T,
           artist   => Artist,
           explicit => binary_to_float(Explicit),
           filename => Filename,
           height   => Height,
           mimetype => Mimetype,
           size     => Size,
           title    => Title,
           width    => Width}};

http(#{<<"@">>        := <<"http:file:video">>,
       <<"t">>        := T,
       <<"duration">> := Duration,
       <<"explicit">> := Explicit,
       <<"filename">> := Filename,
       <<"height">>   := Height,
       <<"mimetype">> := Mimetype,
       <<"size">>     := Size,
       <<"title">>    := Title,
       <<"width">>    := Width}) ->
    {ok, #{m        => http_file_video,
           t        => T,
           duration => Duration,
           explicit => binary_to_float(Explicit),
           filename => Filename,
           height   => Height,
           mimetype => Mimetype,
           size     => Size,
           title    => Title,
           width    => Width}};

http(#{<<"@">>        := <<"http:lainchan:thread">>,
       <<"t">>        := T,
       <<"board">>    := Board,
       <<"files">>    := Files,
       <<"post">>     := Post,
       <<"replies">>  := Replies,
       <<"title">>    := Title}) ->
    {ok, #{m        => http_lainchan_thread,
           t        => T,
           board    => Board,
           files    => Files,
           post     => Post,
           replies  => Replies,
           title    => Title}};

http(#{<<"@">>        := <<"http:mupdf">>,
       <<"t">>        := T,
       <<"explicit">> := Explicit,
       <<"mimetype">> := Mimetype,
       <<"pages">>    := Pages,
       <<"size">>     := Size,
       <<"title">>    := Title}) ->
    {ok, #{m        => http_mupdf,
           t        => T,
           explicit => binary_to_float(Explicit),
           mimetype => Mimetype,
           pages    => Pages,
           size     => Size,
           title    => Title}};

http(#{<<"@">>           := <<"http:nitter">>,
       <<"t">>           := T,
       <<"description">> := Description,
       <<"explicit">>    := Explicit}) ->
    {ok, #{m            => http_nitter,
           t            => T,
           description  => Description,
           explicit     => binary_to_float(Explicit)}};

http(#{<<"@">>           := <<"http:reddit:comments">>,
       <<"t">>           := T,
       <<"title">>       := Title,
       <<"author_name">> := AuthorName,
       <<"subreddit">>   := Subreddit}) ->
    {ok, #{m           => http_reddit_comments,
           t           => T,
           title       => Title,
           author_name => AuthorName,
           subreddit   => Subreddit}};

http(#{<<"@">> := <<"http:twitter:tweet">>, <<"t">> := T}) ->
    {ok, #{m => http_twitter_tweet, t => T}};

http(#{<<"@">>       := <<"http:youtube:search">>,
       <<"t">>       := T,
       <<"results">> := Results}) ->
    R1 = lists:map(fun (#{<<"short_url">> := ShortUrl,
                          <<"name">> := Name,
                          <<"date">> := Date,
                          <<"views">> := Views,
                          <<"channel">> := Channel,
                          <<"duration">> := Duration,
                          <<"yt_id">> := YtId}) ->
                           #{short_url => ShortUrl,
                             name      => Name,
                             date      => Date,
                             views     => Views,
                             channel   => Channel,
                             duration  => Duration,
                             yt_id     => YtId}
                   end,
                   Results),
    {ok, #{m       => http_youtube_search,
           t       => T,
           results => R1}};

http(#{<<"@">>                := <<"http:youtube:video">>,
       <<"t">>                := T,
       <<"author_name">>      := AuthorName,
       <<"author_url">>       := AuthorUrl,
       <<"height">>           := Height,
       <<"html">>             := Html,
       <<"thumbnail_height">> := ThumbnailHeight,
       <<"thumbnail_url">>    := ThumbnailUrl,
       <<"thumbnail_width">>  := ThumbnailWidth,
       <<"title">>            := Title,
       <<"type">>             := Type,
       <<"width">>            := Width}) ->
    {ok, #{m                => http_youtube_video,
           t                => T,
           author_name      => AuthorName,
           author_url       => AuthorUrl,
           height           => Height,
           html             => Html,
           thumbnail_height => ThumbnailHeight,
           thumbnail_url    => ThumbnailUrl,
           thumbnail_width  => ThumbnailWidth,
           title            => Title,
           type             => Type,
           width            => Width}}.
