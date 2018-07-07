:- module(
  ppm_generic,
  [
    ansi_format/2,             % +Attributes, +Format
    atom_phrase/2,             % :Dcg_0, ?Atom
    compare_version/3,         % ?Order, @Version1, @Version2
    directory_by_name/2,       % +Root, +SubDirectories
    directory_by_name/3,       % +Root, +SubDirectories, -Directory
    directory_file/2,          % +Directory, -File
    directory_path/2,          % +Directory, -File
    ensure_directory_exists/1, % +Directory
    file_by_name/2,            % +Directory, +File
    file_by_name/3,            % +Directory, +File, -Path
    ppm_dependencies/3,        % +User, +Repo, -Dependencies
    repository_directory/3,    % +User, +Repo, -Directory
    root_directory/1,          % -Root
    user_directory/2,          % +User, -Directory
    version//1                 % ?Version
  ]
).

/** <module> Prolog Package Manager (PPM): Generic support predicates

@author Wouter Beek
@version 2017-2018
*/

:- use_module(library(ansi_term)).
:- use_module(library(dcg/basics)).
:- use_module(library(http/json)).
:- use_module(library(filesex)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).

:- meta_predicate
    atom_phrase(//, ?).





%! ansi_format(+Attributes:list(term), +Format:string) is det.

ansi_format(Attrs, Format) :-
  ansi_format(Attrs, Format, []).



%! atom_phrase(:Dcg_0, +Atom:atom) is det.
%! atom_phrase(:Dcg_0, -Atom:atom) is det.

atom_phrase(Dcg_0, Atom) :-
  atom(Atom), !,
  atom_codes(Atom, Codes),
  phrase(Dcg_0, Codes).
atom_phrase(Dcg_0, Atom) :-
  phrase(Dcg_0, Codes),
  atom_codes(Atom, Codes).



%! compare_version(?Order:oneof([<,=,>]), @Version1, @Version2) is det.
%
% Determine or test the order between two semantic version numbers.

compare_version(Order, version(Ma1,Mi1,Pa1), version(Ma2,Mi2,Pa2)) :-
  compare(OrderMa, Ma1, Ma2),
  (   OrderMa == =
  ->  compare(OrderMi, Mi1, Mi2),
      (   OrderMi == =
      ->  compare(Order, Pa1, Pa2)
      ;   Order = OrderMi
      )
  ;   Order = OrderMa
  ).



%! directory_by_name(+Root:atom, +SubDirectories:atom) is semidet.
%! directory_by_name(+Root:atom, +SubDirectories:atom, -Directory:atom) is semidet.

directory_by_name(Root, SubDirs) :-
  directory_by_name(Root, SubDirs, _).


directory_by_name(Root, SubDirs, Dir) :-
  absolute_file_name(
    SubDirs,
    Dir,
    [access(read),file_errors(fail),file_type(directory),relative_to(Root)]
  ).



%! directory_file(+Directory:atom, -File:atom) is nondet.

directory_file(Dir, File) :-
  directory_files(Dir, Files),
  member(File, Files),
  \+ is_dummy_file(File).



%! directory_path(+Directory:atom, -File:atom) is nondet.
%
% Non-determinisitcally enumerates the Files that are in Dir.
%
% @arg Dir is an atom denoting a directory on the filesystem.
%
% @arg File is an atomic full path specifier of a file in Dir.
%
% The dummy files `.' and `..' are not included.

directory_path(Dir, Path) :-
  directory_file(Dir, File),
  directory_file_path(Dir, File, Path).



%! ensure_directory_exists(+Directory:atom) is det.

ensure_directory_exists(Dir) :-
  exists_directory(Dir), !.
ensure_directory_exists(Dir) :-
  make_directory(Dir).



% file_by_name(+Directory:atom, +File:atom) is semidet.
% file_by_name(+Directory:atom, +File:atom, -Path:atom) is semidet.

file_by_name(Dir, File) :-
  file_by_name(Dir, File, _).


file_by_name(Dir, File, Path) :-
  absolute_file_name(
    File,
    Path,
    [access(read),file_errors(fail),relative_to(Dir)]
  ).



%! get_dict(?Key, +Dict, +Default, -Value) is det.

get_dict(Key, Dict, _, Value) :-
  get_dict(Key, Dict, Value), !.
get_dict(_, _, Value, Value).



%! home_directory(-Directory:atom) is det.

home_directory(Dir) :-
  expand_file_name('~', [Dir|_]).



%! increment_version(+Kind:oneof([major,minor,patch]), +Version1:compound,
%!                   -Version2:compound) is det.

increment_version(major, version(Ma1,Mi,Pa), version(Ma2,Mi,Pa)) :- !,
  Ma2 is Ma1 + 1.
increment_version(minor, version(Ma,Mi1,Pa), version(Ma,Mi2,Pa)) :- !,
  Mi2 is Mi1 + 1.
increment_version(patch, version(Ma,Mi,Pa1), version(Ma,Mi,Pa2)) :-
  Pa2 is Pa1 + 1.



%! is_dummy_file(+File:atom) is semidet.
%
% Succeeds if File is the local name of a dummy file, i.e., `.' or
% `..'.

is_dummy_file(.).
is_dummy_file(..).



%! ppm_dependencies(+User:atom, +Repo:atom, -Dependencies:ordset(dict)) is semidet.

ppm_dependencies(User, Repo, Dependencies2) :-
  repository_directory(User, Repo, Dir),
  file_by_name(Dir, 'ppm.json', File),
  setup_call_cleanup(
    open(File, read, In),
    json_read_dict(In, Dict, [value_string_as(atom)]),
    close(In)
  ),
  get_dict(dependencies, Dict, [], Dependencies1),
  list_to_ord_set(Dependencies1, Dependencies2).



%! root_directory(-Directory:atom) is det.
%
% @arg Directory is bound to the directory used to store SWI packages
%      in.
%
% Creates Directory in case it does not yet exist.

root_directory(Dir) :-
  home_directory(Dir0),
  directory_file_path(Dir0, '.ppm', Dir).



%! repository_directory(+User:atom, +Repo:atom, -Dir:atom) is semidet.
%! repository_directory(+User:atom, -Repo:atom, -Dir:atom) is nondet.
%! repository_directory(-User:atom, -Repo:atom, -Dir:atom) is nondet.

repository_directory(User, Repo, RepoDir) :-
  root_directory(Root),
  % User directory.
  (   var(User)
  ->  directory_files(Root, Users),
      member(User, Users),
      \+ is_dummy_file(User)
  ;   true
  ),
  directory_file_path(Root, User, UserDir),
  % Repository directory.
  (   var(Repo)
  ->  directory_files(UserDir, Repos),
      member(Repo, Repos),
      \+ is_dummy_file(Repo)
  ;   true
  ),
  directory_file_path(UserDir, Repo, RepoDir),
  is_git_directory(RepoDir),
  % A package file must be present.
  file_by_name(RepoDir, 'ppm.json').



%! user_directory(+User:atom, -Directory:atom) is det.

user_directory(User, Dir) :-
  root_directory(Root),
  directory_file_path(Root, User, Dir),
  ensure_directory_exists(Dir).



%! version(+Version:compound)// is det.
%! version(-Version:compound)// is det.
%
% Parses/generates semantic versioning strings.
%
% @arg Version is a compound term of the form `version(int,int,int)'.

version(version(Ma,Mi,Pa)) -->
  "v",
  integer(Ma),
  ".",
  integer(Mi),
  ".",
  integer(Pa).
