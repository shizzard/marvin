Definitions.

Digit = 0-9
Space = \s\t\n\r
Punctuation = :,;\.!?
SpecialCharacters = <@!&#:>

Rules.

<@! : {token, {user_mention_start, TokenLine}}.
<@ : {token, {user_mention_start, TokenLine}}.
<@& : {token, {role_mention_start, TokenLine}}.
<# : {token, {channel_mention_start, TokenLine}}.
<: : {token, {emoji_mention_start, TokenLine}}.
> : {token, {mention_end, TokenLine}}.
[{Space}] : skip_token.
[{Punctuation}] : skip_token.
[{SpecialCharacters}] : skip_token.
[{Digit}]+ : {token, {integer, TokenLine, list_to_integer(TokenChars)}}.
[^{Space}{Punctuation}{SpecialCharacters}{Digit}]+ : {token, {word, TokenLine, unicode:characters_to_binary(TokenChars)}}.

Erlang code.
