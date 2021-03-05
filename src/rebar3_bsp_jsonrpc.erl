%%==============================================================================
%% A JSON-RPC Helper Library
%%==============================================================================
-module(rebar3_bsp_jsonrpc).

%%==============================================================================
%% Exports
%%==============================================================================
-export([ default_opts/0
        , split/1
        , split/2
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("rebar3_bsp.hrl").

%%==============================================================================
%% API
%%==============================================================================
-spec split(binary()) -> {[map()], binary()}.
split(Data) ->
  split(Data, default_opts()).

-spec split(binary(), [any()]) -> {[map()], binary()}.
split(Data, DecodeOpts) ->
  split(Data, DecodeOpts, []).

-spec split(binary(), [any()], [map()]) -> {[map()], binary()}.
split(Data, DecodeOpts, Responses) ->
  try parse_headers(Data) of
    {Headers, Data1} ->
      BinLength     = proplists:get_value(<<"content-length">>, Headers),
      Length        = binary_to_integer(BinLength),
      CurrentLength = byte_size(Data1),
      case CurrentLength < Length of
        true  ->
          {lists:reverse(Responses), Data};
        false ->
          <<Body:Length/binary, Rest/binary>> = Data1,
          Response  = jsx:decode(Body, DecodeOpts),
          split(Rest, DecodeOpts, [Response|Responses])
      end
  catch _:_ ->
      {lists:reverse(Responses), Data}
  end.

-spec default_opts() -> [any()].
default_opts() ->
  [return_maps, {labels, atom}].


%% Copied from cowlib 2.3.0
%% Copyright (c) 2013-2015, Loïc Hoguin <essen@ninenines.eu>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

%% @doc Parse the list of headers.

-define(LOWER(Function, Rest, A0, Acc), case C of
  $A -> Function(Rest, A0, << Acc/binary, $a >>);
  $B -> Function(Rest, A0, << Acc/binary, $b >>);
  $C -> Function(Rest, A0, << Acc/binary, $c >>);
  $D -> Function(Rest, A0, << Acc/binary, $d >>);
  $E -> Function(Rest, A0, << Acc/binary, $e >>);
  $F -> Function(Rest, A0, << Acc/binary, $f >>);
  $G -> Function(Rest, A0, << Acc/binary, $g >>);
  $H -> Function(Rest, A0, << Acc/binary, $h >>);
  $I -> Function(Rest, A0, << Acc/binary, $i >>);
  $J -> Function(Rest, A0, << Acc/binary, $j >>);
  $K -> Function(Rest, A0, << Acc/binary, $k >>);
  $L -> Function(Rest, A0, << Acc/binary, $l >>);
  $M -> Function(Rest, A0, << Acc/binary, $m >>);
  $N -> Function(Rest, A0, << Acc/binary, $n >>);
  $O -> Function(Rest, A0, << Acc/binary, $o >>);
  $P -> Function(Rest, A0, << Acc/binary, $p >>);
  $Q -> Function(Rest, A0, << Acc/binary, $q >>);
  $R -> Function(Rest, A0, << Acc/binary, $r >>);
  $S -> Function(Rest, A0, << Acc/binary, $s >>);
  $T -> Function(Rest, A0, << Acc/binary, $t >>);
  $U -> Function(Rest, A0, << Acc/binary, $u >>);
  $V -> Function(Rest, A0, << Acc/binary, $v >>);
  $W -> Function(Rest, A0, << Acc/binary, $w >>);
  $X -> Function(Rest, A0, << Acc/binary, $x >>);
  $Y -> Function(Rest, A0, << Acc/binary, $y >>);
  $Z -> Function(Rest, A0, << Acc/binary, $z >>);
  C -> Function(Rest, A0, << Acc/binary, C >>)
end).

-spec parse_headers(binary()) -> {[{binary(), binary()}], binary()}.
parse_headers(Data) ->
  parse_header(Data, []).

parse_header(<< $\r, $\n, Rest/bits >>, Acc) ->
  {lists:reverse(Acc), Rest};
parse_header(Data, Acc) ->
  parse_hd_name(Data, Acc, <<>>).

parse_hd_name(<< C, Rest/bits >>, Acc, SoFar) ->
  case C of
    $: -> parse_hd_before_value(Rest, Acc, SoFar);
    $\s -> parse_hd_name_ws(Rest, Acc, SoFar);
    $\t -> parse_hd_name_ws(Rest, Acc, SoFar);
    _ -> ?LOWER(parse_hd_name, Rest, Acc, SoFar)
  end.

parse_hd_name_ws(<< C, Rest/bits >>, Acc, Name) ->
  case C of
    $: -> parse_hd_before_value(Rest, Acc, Name);
    $\s -> parse_hd_name_ws(Rest, Acc, Name);
    $\t -> parse_hd_name_ws(Rest, Acc, Name)
  end.

parse_hd_before_value(<< $\s, Rest/bits >>, Acc, Name) ->
  parse_hd_before_value(Rest, Acc, Name);
parse_hd_before_value(<< $\t, Rest/bits >>, Acc, Name) ->
  parse_hd_before_value(Rest, Acc, Name);
parse_hd_before_value(Data, Acc, Name) ->
  parse_hd_value(Data, Acc, Name, <<>>).

parse_hd_value(<< $\r, Rest/bits >>, Acc, Name, SoFar) ->
  case Rest of
    << $\n, C, Rest2/bits >> when C =:= $\s; C =:= $\t ->
      parse_hd_value(Rest2, Acc, Name, << SoFar/binary, C >>);
    << $\n, Rest2/bits >> ->
      Value = clean_value_ws_end(SoFar, byte_size(SoFar) - 1),
      parse_header(Rest2, [{Name, Value}|Acc])
  end;
parse_hd_value(<< C, Rest/bits >>, Acc, Name, SoFar) ->
  parse_hd_value(Rest, Acc, Name, << SoFar/binary, C >>).

%% This function has been copied from cowboy_http.
clean_value_ws_end(_, -1) ->
  <<>>;
clean_value_ws_end(Value, N) ->
  case binary:at(Value, N) of
    $\s -> clean_value_ws_end(Value, N - 1);
    $\t -> clean_value_ws_end(Value, N - 1);
    _ ->
      S = N + 1,
      << Value2:S/binary, _/bits >> = Value,
      Value2
  end.
