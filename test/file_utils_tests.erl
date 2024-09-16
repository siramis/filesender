-module(file_utils_tests).

-include_lib("eunit/include/eunit.hrl").

lookup_files_test() ->
  Parent = "/tmp/abcdefg",
  IgnoreFileName = ".skip_me",
  Files = create_directory_tree(Parent, IgnoreFileName),
  Found = file_utils:lookup_files(Parent, IgnoreFileName),
  ?assert(Files =:= Found),
  file:del_dir_r(Parent).

create_directory_tree(Parent, IgnoreFileName) ->
  file:make_dir(Parent),
  file:write_file(filename:join(Parent, "a1.txt"), <<>>),
  SubDir = filename:join(Parent, "a1"),
  file:make_dir(SubDir),
  file:write_file(filename:join(SubDir, "a11.txt"), <<>>),
  file:write_file(filename:join(SubDir, "a12.txt"), <<>>),
  file:write_file(filename:join(SubDir, "a13.txt"), <<>>),
  IgnoreContent = ["/tmp/abcdefg/.skip_me", "/tmp/abcdefg/a1/a13.txt"],
  create_test_file(filename:join(Parent, IgnoreFileName), IgnoreContent),
  {ok,
    #{ignore => ["/tmp/abcdefg/.skip_me", "/tmp/abcdefg/a1/a13.txt"],
      files =>
      [
        "/tmp/abcdefg/a1/a12.txt",
        "/tmp/abcdefg/a1/a11.txt",
        "/tmp/abcdefg/a1.txt"
      ]
    }}.

get_ignore_files_test() ->
  Parent = "/tmp",
  IgnorePath = ".skip_files",
  Result = ["/tmp/.skip_files"],
  ?assert(Result =:= file_utils:get_ignore_files(Parent, IgnorePath)),
  MoreResult = extend_result_file_content("/tmp/.skip_files", Result),
  ?assert(MoreResult =:= file_utils:get_ignore_files(Parent, IgnorePath)),
  file:delete(filename:join(Parent, IgnorePath)).

extend_result_file_content(FileName, Content) ->
  NewLine = "new line",
  {ok, File} = file:open(FileName, [append]),
  io:format(File, "~n~s~n", [NewLine]),
  file:close(File),
  Content ++ [NewLine].

get_dirs_tree_test() ->
  ParentDir = "/tmp/111",
  FileName = "/tmp/111/222/333/111.txt",
  ?assert(["222", "333"] =:= file_utils:get_dirs_tree(ParentDir, FileName)).

create_full_path_test() ->
  ParentDir = "/tmp/111",
  FileName = "/tmp/somedir/anotherdir/333/111.txt",
  SubDirs = ["222", "555"],
  ?assert("/tmp/111/222/555/111.txt" =:= file_utils:create_full_path(ParentDir, FileName, SubDirs)),
  ?assert("/tmp/111/111.txt" =:= file_utils:create_full_path(ParentDir, FileName, [])).

read_file_content_into_list_test() ->
  FileName = "/tmp/1234.txt",
  ContentWithEmptyLine = [
    "summer has come and passed",
    "the innocent can never last",
    "",
    "wake me up when september ends",
    "check empty line, bro!"
  ],
  create_test_file(FileName, ContentWithEmptyLine),
  Content1 = [
    "summer has come and passed",
    "the innocent can never last",
    "wake me up when september ends",
    "check empty line, bro!"
  ],
  {ok, Fd} = file:open(FileName, [read]),
  ?assert(Content1 =:= file_utils:read_file_content_into_list(Fd)),
  file:delete(FileName).

create_test_file(FileName, Content) ->
  {ok, File} = file:open(FileName, [write]),
  F = fun(Line) -> io:format(File, "~s~n", [Line]) end,
  [F(X) || X <- Content],
  file:close(File).
