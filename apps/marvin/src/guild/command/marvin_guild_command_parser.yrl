Nonterminals command user_mention role_mention channel_mention emoji_mention element.
Terminals integer word user_mention_start role_mention_start channel_mention_start emoji_mention_start mention_end.
Rootsymbol command.


user_mention -> user_mention_start integer mention_end : '$2'.
role_mention -> role_mention_start integer mention_end : '$2'.
channel_mention -> channel_mention_start integer mention_end : '$2'.
emoji_mention -> emoji_mention_start word integer mention_end : '$3'.

element -> user_mention : {user_mention, integer_to_binary(extract_value('$1'))}.
element -> role_mention : {role_mention, integer_to_binary(extract_value('$1'))}.
element -> channel_mention : {channel_mention, integer_to_binary(extract_value('$1'))}.
element -> emoji_mention : {emoji_mention, integer_to_binary(extract_value('$1'))}.
element -> integer : extract_value('$1').
element -> word : extract_value('$1').

command -> element : ['$1'].
command -> element command : ['$1'|'$2'].

Erlang code.

extract_value({_, _, Value}) -> Value.
