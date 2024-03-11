-module(config).

-export([load_config/0,
         save_config/1,
         update_config/3,
         lookup/2]).

-type config() :: [{string(), string()}].

-define(CONF_DEFAULT_LOC, "equivcheckrc").
-define(SEPARATOR, " "). % Separates the config key from the value in the config file

-spec update_config(config(), string(), string()) -> config().
update_config(Config, Key, Value) ->
    lists:keystore(Key, 1, Config, {Key, Value}).

-spec lookup(config(), string()) -> string() | boolean().
lookup(Config, Key) ->
    case lists:keyfind(Key, 1, Config) of
        {_, Value} -> Value;
        _Otherwise -> false
    end.

-spec create_default() -> config().
create_default() ->
    [].
    
-spec deserialize(string()) -> config().
deserialize(ConfigStr) ->
    Lines = string:split(string:trim(ConfigStr), "\n", all),
    lists:map(fun(Line) ->
                      [Key, Value] = string:split(Line, ?SEPARATOR),
                      {Key, Value} end, Lines).

-spec serialize(config()) -> string().
serialize(Config) ->
    lists:foldr(fun({Key, Value}, ConfigStr) ->
                        Key ++ ?SEPARATOR ++ Value ++ "\n" ++ ConfigStr end,
                "",
                Config).

-spec load_config() -> config().
load_config() -> % TODO non-default location as argument
    FileName = filename:basedir(user_config, ?CONF_DEFAULT_LOC),
    case filelib:is_file(FileName) of
        true ->
            {_, File} = file:read_file(FileName),
            deserialize(binary:bin_to_list(File));
        false ->
            create_default()
    end.

-spec save_config(config()) -> atom().
save_config(Config) ->
    FileName = filename:basedir(user_config, ?CONF_DEFAULT_LOC),
    file:write_file(FileName, binary:list_to_bin(serialize(Config))).
