%%%-------------------------------------------------------------------
%%% @doc File utils
%%%-------------------------------------------------------------------
-module(file_utils).
-author("rsibiev").

-export([lookup_files/2, get_dirs_tree/2, create_full_path/3, read_file_content_into_list/1]).
-export([get_ignore_files/2, save_file/2]).

%% TODO remove this hardcoded
-define(TARGET_DIR, "/tmp/2").

-spec lookup_files(Dir :: string(), IgnorePath :: string()) -> tuple().
lookup_files(Dir, IgnorePath) ->
  case filelib:is_dir(Dir) of
    true ->
      IgnoreFiles = get_ignore_files(Dir, IgnorePath),
      Files = lookup_files(Dir, IgnoreFiles, []),
      Result = #{files => Files, ignore => IgnoreFiles},
      {ok, Result};
    false -> {error, is_not_dir}
  end.

%% recursive looking for files except ignored
-spec lookup_files(Path :: string(), Ignore :: list(), Acc :: list()) -> list().
lookup_files(Path, Ignore, Acc) ->
  case filelib:is_dir(Path) of
    false ->
      case lists:member(Path, Ignore) of
        true -> Acc;
        false -> [Path | Acc]
      end;
    true ->
      {ok, Items} = file:list_dir(Path),
      lookup_files(Items, Path, Ignore, Acc)
  end.

-spec lookup_files(list(), string(), list(), list()) -> list().
lookup_files([], _Parent, _Ignore, Acc) -> Acc;
lookup_files([Item | Items], Parent, Ignore, Acc) ->
  FullPath = filename:join(Parent, Item),
  NewAcc = lookup_files(FullPath, Ignore, Acc),
  lookup_files(Items, Parent, Ignore, NewAcc).

-spec get_ignore_files(Parent :: string(), IgnorePath :: string()) -> list().
get_ignore_files(Parent, IgnorePath) ->
  SkipFilePath = filename:join(Parent, IgnorePath),
  case file:open(SkipFilePath, [read]) of
    {ok, File} ->
      Result = read_file_content_into_list(File),
      file:close(File),
      Result;
    {error, enoent} ->
      {ok, File} = file:open(SkipFilePath, [write]),
      file:write(File, SkipFilePath),
      file:close(File),
      get_ignore_files(Parent, IgnorePath)
  end.

-spec get_dirs_tree(ParentDir :: string(), FileName :: string()) -> list().
get_dirs_tree(ParentDir, FileName) ->
  DirName = filename:dirname(FileName),
  filename:split(DirName) -- filename:split(ParentDir).

-spec create_full_path(ParentDir :: string(), FileName :: string(), SubDirs :: string()) -> string().
create_full_path(ParentDir, FileName, SubDirs) ->
  if
    length(SubDirs) > 0 ->
      Sd = filename:join(SubDirs),
      Psd = filename:join(ParentDir, Sd),
      filename:join(Psd, filename:basename(FileName));
    true ->
      filename:join(ParentDir, filename:basename(FileName))
  end.

save_file(FullName, Bin) ->
  Name = filename:basename(FullName),
  Path = filename:join(?TARGET_DIR, Name),
  io:format("Saved file: ~p~n", [Path]),
  file:write_file(Path, Bin).

read_file_content_into_list(Fd) ->
  Line = io:get_line(Fd, ''),
  read_file_content_into_list(Fd, Line, []).

read_file_content_into_list(_Fd, eof, Acc) -> lists:reverse(Acc);
read_file_content_into_list(_Fd, {error, terminated}, Acc) -> lists:reverse(Acc);
read_file_content_into_list(Fd, Res, Acc) ->
  Trimmed = string:trim(Res),
  if
    length(Trimmed) > 0 -> read_file_content_into_list(Fd, io:get_line(Fd, ''), [Trimmed | Acc]);
    true -> read_file_content_into_list(Fd, io:get_line(Fd, ''), Acc)
  end.