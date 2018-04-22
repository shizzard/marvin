Definitions.

Digit = [0-9]
Letter = [^0-9<:@&!>,;\.\s\t\n\r]
Separator = [:,;\.\s\t\n\r]

Rules.

<@! : {token, {user_mention_start, TokenLine}}.
<@ : {token, {user_mention_start, TokenLine}}.
<@& : {token, {role_mention_start, TokenLine}}.
<# : {token, {channel_mention_start, TokenLine}}.
<: : {token, {emoji_mention_start, TokenLine}}.
> : {token, {mention_end, TokenLine}}.
{Separator}+ : skip_token.
{Digit}+ : {token, {integer, TokenLine, list_to_integer(TokenChars)}}.
{Letter}+ : {token, {word, TokenLine, unicode:characters_to_binary(TokenChars)}}.

Erlang code.
