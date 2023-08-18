-define(TEMP_FOLDER, "tmp"). % TODO use /tmp

-define(ORIGINAL_BIN_FOLDER, ?TEMP_FOLDER ++ "/orig").
-define(REFACTORED_BIN_FOLDER, ?TEMP_FOLDER ++ "/refac").

-define(ORIGINAL_SOURCE_FOLDER, ?TEMP_FOLDER ++ "/orig_source").
-define(REFACTORED_SOURCE_FOLDER, ?TEMP_FOLDER ++ "/refac_source").

-type ast()         :: erl_syntax:forms().
-type diffs()       :: [{filename(), {[line_num()], [line_num()]}}].
-type file_info()   :: {tokens(), ast()}. % TODO: This is a terrible name
-type filename()    :: string().
-type line_num()    :: integer().
-type tokens()      :: erl_scan:tokens().
