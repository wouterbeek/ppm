:- module(
  'WACK',
  [
    wack_install/2, % +User, +Name
    wack_ls/0,
    wack_remove/1,  % +Name
    wack_update/1,  % +Name
    wack_updates/0
  ]
).

/** <module> WACK (Wouter pACK)

@author Wouter Beek
@version 2017/06, 2017/08
*/

:- use_module(library(ansi_term)).
:- use_module(library(apply)).
:- use_module(library(dcg/basics)).
:- use_module(library(filesex)).
:- use_module(library(git)).
:- use_module(library(http/http_open)).
:- use_module(library(http/json)).
:- use_module(library(lists)).
:- use_module(library(uri)).





%! wack_current(?User:atom, ?Name:atom, ?Version:compound) is nondet.
%
% Enumerates currently installed WACKs together with their semantic
% version.

wack_current(User, Repo, Version) :-
  repo_dir(Repo, RepoDir),
  github_info(RepoDir, User, Repo, Version).



%! wack_ls is det.
%
% Display all currently installed WACKs.
%
% TBD Display which packages a package is a dependency for.

wack_ls :-
  forall(
    wack_current(User, Repo, Version),
    wack_ls_row(User, Repo, Version)
  ).

wack_ls_row(User, Repo, Version) :-
  phrase(version(Version), Codes),
  format("~a\t~a\t~s\n", [User,Repo,Codes]).



%! wack_install(+User:atom, +Repo:atom) is semidet.
%
% Installs a WACK.  The latests version is chosen in case none is
% specified.

wack_install(User, Repo) :-
  wack_current(User, Repo, CurrentVersion), !,
  phrase(version(CurrentVersion), Codes),
  format("Package ~a's ‘~a’ is already installed (version ~s)\n",
         [User,Repo,Codes]),
  format("Use wack_update/1 to update a package.\n").
wack_install(User, Repo) :-
  wack_install(User, Repo, package).

wack_install(User, Repo, Kind) :-
  github_version_latest(User, Repo, Version),
  github_install(User, Repo, Version),
  repo_conf(Repo, Conf),
  _{dependencies: Deps1} :< Conf,
  collect_dependencies(Deps1, Deps2),
  maplist(wack_install_dependency, Deps2),
  phrase(version(Version), Codes),
  format("Successfully installed ~a ‘~a’, version ~s\n", [Kind,Repo,Codes]).

wack_install_dependency(Dep) :-
  _{repo: Repo, user: User} :< Dep,
  wack_install(User, Repo, dependency).



%! wack_remove(+Name:atom) is det.
%
% Removes a WACK.
%
% TBD: Also remove packages that depend on the removed package.

wack_remove(Repo) :-
  repo_conf(Repo, Conf),
  Repo = Conf.name, !,
  repo_dir(Repo, RepoDir),
  delete_directory_and_contents(RepoDir),
  format("Deleted package ‘~a’.", [Repo]).



%! wack_update(+Name:atom) is semidet.
%
% Updates an exisiting WACK.
%
% TBD: Also update packages that are dependencies.
%
% TBD: Also install new dependencies.

wack_update(Repo) :-
  wack_current(User, Repo, CurrentVersion),
  github_version_latest(User, Repo, LatestVersion),
  (   CurrentVersion == LatestVersion
  ->  format("No need to update.\n")
  ;   wack_remove(Repo),
      wack_install(User, Repo),
      LatestVersion =.. [version|T],
      format("Updated ~a's ‘~a’ to version ~d.~d.~d\n", [User,Repo|T])
  ).



%! wack_updates is det.
%
% Shows packages, if any, that can be updated using wack_update/1.

wack_updates :-
  forall(
    wack_current(User, Repo, CurrentVersion),
    (
      github_version_latest(User, Repo, LatestVersion),
      compare_version(Order, CurrentVersion, LatestVersion),
      wack_updates_row(User, Repo, Order, CurrentVersion, LatestVersion)
    )
  ).

wack_updates_row(_, _, =, _, _) :- !.
wack_updates_row(User, Repo, Order, CurrentVersion, LatestVersion) :-
  format("~a\t~a\t", [User,Repo]),
  order_colors(Order, Color1, Color2),
  phrase(version(CurrentVersion), CurrentCodes),
  ansi_format([fg(Color1)], "~s", [CurrentCodes]),
  format("\t»\t"),
  phrase(version(LatestVersion), LatestCodes),
  ansi_format([fg(Color2)], "~s", [LatestCodes]).

order_colors(<, red, green).
order_colors(>, green, red).





% VERSIONS %

%! compare_version(?Order:oneof([<,=,>]), @Version1, @Version2) is det.
%
% Determine or test the order between two semantic versions.

compare_version(Order, version(Major1,Minor1,Patch1),
                version(Major2,Minor2,Patch2)) :-
  compare(OrderMajor, Major1, Major2),
  (   OrderMajor == =
  ->  compare(OrderMinor, Minor1, Minor2),
      (   OrderMinor == =
      ->  compare(Order, Patch1, Patch2)
      ;   Order = OrderMinor
      )
  ;   Order = OrderMajor
  ).



%! version(?Version:compound)// is det.
%
% Parses/generates semantic versioning strings.
%
% @arg Version is a compound term of the form `version(int,int,int)'.

version(version(Major,Minor,Patch)) -->
  "v",
  integer(Major),
  ".",
  integer(Minor),
  ".",
  integer(Patch).





% SERVICE: GITHUB %

%! github_info(+Dir:atom, -User:atom, -Repo:atom, -Version:compound) is det.

github_info(Dir, User, Repo, Version) :-
  git([config,'--get','remote.origin.url'], [directory(Dir),output(Codes1)]),
  atom_codes(Atom1, Codes1),
  atom_concat(Uri, '\n', Atom1),
  uri_components(Uri, uri_components(https,'github.com',Path,_,_)),
  atomic_list_concat(['',User,Repo], /, Path),
  git([describe,'--tags'], [directory(Dir),output(Codes2)]),
  phrase(version(Version), Codes2, _Rest).



%! github_install(+User:atom, +Repo:atom, +Version:compound) is det.

github_install(User, Repo, Version) :-
  phrase(version(Version), Codes),
  atom_codes(Tag, Codes),
  atomic_list_concat(['',User,Repo], /, Path),
  uri_components(Uri, uri_components(https,'github.com',Path,_,_)),
  pack_dir(PackDir),
  git([clone,Uri,'--branch',Tag,'--depth',1], [directory(PackDir)]).



%! github_open(+Segments:list(atom), -In:stream) is det.

github_open(Segments, In) :-
  atomic_list_concat([''|Segments], /, Path),
  uri_components(Uri, uri_components(https,'api.github.com',Path,_,_)),
  http_open(
    Uri,
    In,
    [request_header('Accept'='application/vnd.github.v3+json')]
  ).



%! github_version(+User:atom, +Repo:atom, -Version:compound) is nondet.

github_version(User, Repo, Version) :-
  github_open([repos,User,Repo,tags], In),
  call_cleanup(
    json_read_dict(In, Tags, [value_string_as(atom)]),
    close(In)
  ),
  member(Tag, Tags),
  atom_codes(Tag.name, Codes),
  phrase(version(Version), Codes).



%! github_version_latest(+User:atom, +Repo:atom,
%!                       -LatestVersion:compound) is det.

github_version_latest(User, Repo, LatestVersion) :-
  aggregate_all(set(Version), github_version(User, Repo, Version), Versions),
  predsort(compare_version, Versions, [LatestVersion|_]).





% HELPERS %

%! collect_dependencies(+Deps1:list(dict), -Deps2:list(dict)) is det.

collect_dependencies(L1, L2) :-
  collect_dependencies(L1, [], L2).


collect_dependencies([], L, L) :- !.
collect_dependencies([H|T1], T2, L) :-
  get_dict(name, H, Repo),
  repo_dir(Repo, RepoDir),
  exists_directory(RepoDir), !,
  collect_dependencies(T1, T2, L).
collect_dependencies([H|T1], T2, L) :-
  \+ memberchk(H, T2), !,
  collect_dependencies(T1, [H|T2], L).
collect_dependencies([_|T1], T2, L) :-
  collect_dependencies(T1, T2, L).



%! directory_file(+Dir:atom, -File:atom) is nondet.

directory_file(Dir, File) :-
  directory_files(Dir, Files),
  member(File, Files),
  \+ is_dummy_file(File).



%! directory_path(+Dir:atom, -File:atom) is nondet.
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



%! is_dummy_file(+File:atom) is semidet.
%
% Succeeds if File is the local name of a dummy file, i.e., `.' or
% `..'.

is_dummy_file(.).
is_dummy_file(..).



%! pack_dir(-PackDir:atom) is det.
%
% @arg PackDir is bound to the directory used to store SWI packages
%      in.
%
% Creates PackDir in case it does not yet exist.

pack_dir(PackDir) :-
  absolute_file_name(pack(.), PackDir, [access(write),file_type(directory)]),
  (exists_directory(PackDir) -> true ; make_directory_path(PackDir)).



%! repo_conf(+Repo:atom, -Conf:dict) is det.
%! repo_conf(-Repo:atom, -Conf:dict) is nondet.

repo_conf(Repo, Conf) :-
  repo_dir(Repo, Dir),
  absolute_file_name(
    'WACK',
    File,
    [
      access(read),
      extensions([json]),
      file_errors(fail),
      relative_to(Dir),
      solutions(all)
    ]
  ),
  setup_call_cleanup(
    open(File, read, In),
    json_read_dict(In, Conf, [value_string_as(atom)]),
    close(In)
  ).



%! repo_dir(+Repo:atom, -Dir:atom) is det.
%! repo_dir(-Repo:atom, -Dir:atom) is nondet.

repo_dir(Repo, RepoDir) :-
  pack_dir(PackDir),
  (   var(Repo)
  ->  directory_path(PackDir, RepoDir)
  ;   directory_file_path(PackDir, Repo, RepoDir),
      absolute_file_name(RepoDir, _, [access(read),file_type(directory)])
  ),
  is_git_directory(RepoDir).
