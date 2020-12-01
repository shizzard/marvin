-module(marvin_plugin_huify_transform).

-export([huify/1]).

-define(cl_vowels(),
    [$а, $е, $ё, $и, $о, $у, $ы, $э, $ю, $я]).
-define(cl_consonants(),
    [$б, $в, $г, $д, $ж, $з, $к, $л, $м, $н, $п, $р, $с, $т, $ф, $х, $ц, $ч,
     $ш, $щ]).
-define(cl_etc(),
    [$й, $ь, $ъ]).

huify(Word) when is_binary(Word) ->
    case syllabify(Word) of
        {ok, List} when erlang:length(List) < 4 -> do_huify(List);
        {ok, List} -> do_huify(lists:nthtail(erlang:length(List) - 4, List));
        {error, Reason} -> {error, Reason}
    end.


%% one-syllable word
do_huify([[Grapheme | Syllable]]) ->
    {SubReplace, SubKeep} = case lists:member(Grapheme, ?cl_vowels()) of
        %% first grapheme is a vowel; cut all vowels, replace them, keep the rest
        true ->
            {_, ToReplace, ToKeep} = lists:foldl(fun
                (G, {true, ToReplace, ToKeep}) -> {true, ToReplace, [G | ToKeep]};
                (G, {false, ToReplace, ToKeep}) -> case lists:member(G, ?cl_vowels()) of
                    true -> {false, [G | ToReplace], ToKeep};
                    false -> {true, ToReplace, [G | ToKeep]}
                end
            end, {false, [], []}, [Grapheme | Syllable]),
            {ToReplace, lists:reverse(ToKeep)};
        %% first grapheme is not a vowel; cut everything before first vowel,
        %% replace it, keep the rest
        false ->
            {_, ToReplace, ToKeep} = lists:foldl(fun
                (G, {true, ToReplace, ToKeep}) -> {true, ToReplace, [G | ToKeep]};
                (G, {false, ToReplace, ToKeep}) -> case lists:member(G, ?cl_vowels()) of
                    true -> {true, [G | ToReplace], ToKeep};
                    false -> {false, [G | ToReplace], ToKeep}
                end
            end, {false, [], []}, [Grapheme | Syllable]),
            {ToReplace, lists:reverse(ToKeep)}
    end,
    {ok, unicode:characters_to_binary(lists:flatten([do_replace(SubReplace), SubKeep]))};
%% multi-syllable word
do_huify([[Grapheme1 | Syllable1], [Grapheme2 | Syllable2] | Syllables]) ->
    {SubReplace, SubKeep} = case lists:member(Grapheme2, ?cl_vowels()) of
        %% first grapheme of the second syllable is a vowel; cut all of first
        %% vowels of the second syllable and replace them, also toss the first
        %% syllable; keep the rest, including trailing syllables
        true ->
            {_, ToReplace, ToKeep} = lists:foldl(fun
                (G, {true, ToReplace, ToKeep}) -> {true, ToReplace, [G | ToKeep]};
                (G, {false, ToReplace, ToKeep}) -> case lists:member(G, ?cl_vowels()) of
                    true -> {false, [G | ToReplace], ToKeep};
                    false -> {true, ToReplace, [G | ToKeep]}
                end
            end, {false, [], []}, [Grapheme2 | Syllable2]),
            {lists:reverse(ToReplace), lists:flatten([lists:reverse(ToKeep) | Syllables])};
        %% first grapheme of the second syllable is not a vowel; keep it,
        %% work with the first one
        false ->
            case lists:member(Grapheme1, ?cl_vowels()) of
                %% first grapheme of the first syllable is a vowel; cut all
                %% vowels, replace them, keep the rest, including trailing syllables
                true ->
                    {_, ToReplace, ToKeep} = lists:foldl(fun
                        (G, {true, ToReplace, ToKeep}) -> {true, ToReplace, [G | ToKeep]};
                        (G, {false, ToReplace, ToKeep}) -> case lists:member(G, ?cl_vowels()) of
                            true -> {false, [G | ToReplace], ToKeep};
                            false -> {true, ToReplace, [G | ToKeep]}
                        end
                    end, {false, [], []}, [Grapheme1 | Syllable1]),
                    {ToReplace, lists:flatten([lists:reverse(ToKeep), [Grapheme2 | Syllable2] | Syllables])};
                %% first grapheme of the first syllable is not a vowel; cut all
                %% graphemes before the first vowel and replace it; keep the rest,
                %% including trailing syllables
                false ->
                    {_, ToReplace, ToKeep} = lists:foldl(fun
                        (G, {true, ToReplace, ToKeep}) -> {true, [G | ToReplace], ToKeep};
                        (G, {false, ToReplace, ToKeep}) -> case lists:member(G, ?cl_vowels()) of
                            true -> {true, [G | ToReplace], ToKeep};
                            false -> {false, ToReplace, [G | ToKeep]}
                        end
                    end, {false, [], []}, lists:reverse(Syllable1)),
                    {lists:reverse(ToReplace), lists:flatten([ToKeep, [Grapheme2 | Syllable2] | Syllables])}
            end
    end,
    {ok, unicode:characters_to_binary(lists:flatten([do_replace(SubReplace), SubKeep]))}.

%% replacement map; argument is reversed, ret is not
do_replace([$а | _]) -> "Хуя";
do_replace([$е | _]) -> "Хуе";
do_replace([$ё | _]) -> "Хуе";
do_replace([$и | _]) -> "Хуи";
do_replace([$о | _]) -> "Хуе";
do_replace([$у | _]) -> "Хую";
do_replace([$ы | _]) -> "Хуи";
do_replace([$э | _]) -> "Хуе";
do_replace([$ю | _]) -> "Хую";
do_replace([$я | _]) -> "Хуя";
do_replace(_) -> "Хуй".


syllabify(Word) ->
    syllabify(string:next_grapheme(Word), {_subst = [], _orig = [], _sylls = []}).
%% next grapheme is invalid
syllabify({error, _}, _) ->
    {error, invalid_string};
%% break the word into syllables
syllabify(Word, {[$v, $v, $e | Acc], [G1, G2 | Orig], Sylls}) ->
    syllabify(Word, {[$v, $v, $-, $e | Acc], [G1, G2], [lists:reverse(Orig) | Sylls]});
syllabify(Word, {[$c, $v, $e | Acc], [G1, G2 | Orig], Sylls}) ->
    syllabify(Word, {[$c, $v, $-, $e | Acc], [G1, G2], [lists:reverse(Orig) | Sylls]});
syllabify(Word, {[$v, $c, $e | Acc], [G1, G2 | Orig], Sylls}) ->
    syllabify(Word, {[$v, $c, $-, $e | Acc], [G1, G2], [lists:reverse(Orig) | Sylls]});
syllabify(Word, {[$c, $c, $e | Acc], [G1, G2 | Orig], Sylls}) ->
    syllabify(Word, {[$c, $c, $-, $e | Acc], [G1, G2], [lists:reverse(Orig) | Sylls]});
syllabify(Word, {[$v, $c, $v, $c | Acc], [G1, G2 | Orig], Sylls}) ->
    syllabify(Word, {[$v, $c, $-, $v, $c | Acc], [G1, G2], [lists:reverse(Orig) | Sylls]});
syllabify(Word, {[$v, $c, $c, $v | Acc], [G1, G2 | Orig], Sylls}) ->
    syllabify(Word, {[$v, $c, $-, $c, $v | Acc], [G1, G2], [lists:reverse(Orig) | Sylls]});
syllabify(Word, {[$v, $v, $v, $c | Acc], [G1, G2 | Orig], Sylls}) ->
    syllabify(Word, {[$v, $v, $-, $v, $c | Acc], [G1, G2], [lists:reverse(Orig) | Sylls]});
syllabify(Word, {[$c, $v, $v, $c | Acc], [G1, G2 | Orig], Sylls}) ->
    syllabify(Word, {[$c, $v, $-, $v, $c | Acc], [G1, G2], [lists:reverse(Orig) | Sylls]});
syllabify(Word, {[$v, $c, $c, $c, $v | Acc], [G1, G2 | Orig], Sylls}) ->
    syllabify(Word, {[$v, $c, $c, $-, $c, $v | Acc], [G1, G2], [lists:reverse(Orig) | Sylls]});
syllabify(Word, {[$v, $c, $c, $c, $c, $v | Acc], [G1, G2, G3 | Orig], Sylls}) ->
    syllabify(Word, {[$v, $c, $c, $-, $c, $c, $v | Acc], [G1, G2, G3], [lists:reverse(Orig) | Sylls]});
%% find if next grapheme hits any group
syllabify([Grapheme | Rest], {Acc, Orig, Sylls}) ->
    case {
        lists:member(Grapheme, ?cl_etc()),
        lists:member(Grapheme, ?cl_vowels()),
        lists:member(Grapheme, ?cl_consonants())
    } of
        {true, _, _} -> syllabify(string:next_grapheme(Rest), {[$e | Acc], [Grapheme | Orig], Sylls});
        {_, true, _} -> syllabify(string:next_grapheme(Rest), {[$v | Acc], [Grapheme | Orig], Sylls});
        {_, _, true} -> syllabify(string:next_grapheme(Rest), {[$c | Acc], [Grapheme | Orig], Sylls});
        _ -> {error, invalid_grapheme}
    end;
%% return
syllabify([], {_Acc, Orig, Sylls}) -> {ok, lists:reverse([lists:reverse(Orig) | Sylls])}.


