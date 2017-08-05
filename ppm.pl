:- module(
  ppm,
  [
    ppm_install/2, % +User, +Name
    ppm_list/0,
    ppm_remove/1,  % +Name
    ppm_update/1,  % +Name
    ppm_updates/0
  ]
).

/** <module> Prolog Package Manager (PPM)

A very simple package manager for SWI-Prolog.

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
:- use_module(library(ordsets)).
:- use_module(library(prolog_pack)).
:- use_module(library(uri)).





%! ppm_current(?User:atom, ?Name:atom, ?Version:compound) is nondet.
%! ppm_current(?User:atom, ?Name:atom, ?Version:compound,
%!             -Deps:list(dict)) is nondet.
%
% Enumerates currently installed packages together with their semantic
% version number.

ppm_current(User, Repo, Version) :-
  repo_dir(Repo, RepoDir),
  github_info(RepoDir, User, Repo, Version).


ppm_current(User, Repo, Version, Deps) :-
  ppm_current(User, Repo, Version),
  repo_deps(Repo, Deps).



%! ppm_list is det.
%
% Display all currently installed PPMs.

ppm_list :-
  forall(
    ppm_current(User, Repo, Version, Deps),
    ppm_list_row(User, Repo, Version, Deps)
  ).

ppm_list_row(User, Repo, Version, Deps) :-
  phrase(version(Version), Codes),
  format("~a\t~a\t~s\n", [User,Repo,Codes]),
  maplist(ppm_list_dep_row, Deps).

ppm_list_dep_row(Dep) :-
  get_dict(repo, Dep, Repo),
  format("\t→ ~a\n", [Repo]).



%! ppm_install(+User:atom, +Repo:atom) is semidet.
%
% Installs a package.  The latests version is chosen in case none is
% specified.

ppm_install(User, Repo) :-
  ppm_current(User, Repo, CurrentVersion), !,
  phrase(version(CurrentVersion), Codes),
  format("Package ~a's ‘~a’ is already installed (version ~s)\n",
         [User,Repo,Codes]),
  format("Use ppm_update/1 to update a package.\n").
ppm_install(User, Repo) :-
  ppm_install(User, Repo, package).

ppm_install(User, Repo, Kind) :-
  github_version_latest(User, Repo, Version),
  github_clone(User, Repo, Version),
  prolog_pack_install(Repo),
  repo_deps(Repo, Deps1),
  collect_deps(Deps1, Deps2),
  maplist(ppm_install_dependency, Deps2),
  phrase(version(Version), Codes),
  format("Successfully installed ~a ‘~a’, version ~s\n", [Kind,Repo,Codes]).

prolog_pack_install(Repo) :-
  repo_is_prolog_pack(Repo),
  pack_rebuild(Repo).

ppm_install_dependency(Dep) :-
  _{repo: Repo, user: User} :< Dep,
  ppm_install(User, Repo, dependency).



%! ppm_remove(+Name:atom) is det.
%
% Removes a package.
%
% TBD: Support for removing otherwise unused dependencies.

ppm_remove(Repo) :-
  ppm_current(_, Repo, Version),
  repo_dir(Repo, RepoDir),
  delete_directory_and_contents(RepoDir),
  phrase(version(Version), Codes),
  format("Deleted package ‘~a’ (version ~s).", [Repo,Codes]).



%! ppm_update(+Name:atom) is semidet.
%
% Updates an exisiting package and all of its dependencies.

ppm_update(Repo) :-
  ppm_update(Repo, package).


ppm_update(Repo, Kind) :-
  ppm_current(User, Repo, CurrentVersion, Deps1),
  collect_deps(Deps1, Deps2),
  maplist(ppm_update_dependency, Deps2),
  github_version_latest(User, Repo, LatestVersion),
  (   CurrentVersion == LatestVersion
  ->  (   Kind == package
      ->  format("No need to update ~a ‘~a’.\n", [Kind,Repo])
      ;   true
      )
  ;   ppm_remove(Repo),
      ppm_install(User, Repo)
  ),
  % install new dependencies
  ppm_current(User, Repo, LatestVersion, Deps3),
  collect_deps(Deps3, Deps4),
  ord_subtract(Deps4, Deps2, Deps5),
  maplist(ppm_install_dependency, Deps5),
  % informational
  phrase(version(CurrentVersion), Codes1),
  phrase(version(LatestVersion), Codes2),
  format("Updated ‘~a’: ~s → ~s\n", [Repo,Codes1,Codes2]).

ppm_update_dependency(Dep) :-
  get_dict(repo, Dep, Repo),
  ppm_update(Repo, dependency).



%! ppm_updates is det.
%
% Shows packages, if any, that can be updated using ppm_update/1.

ppm_updates :-
  forall(
    ppm_current(User, Repo, CurrentVersion),
    (
      github_version_latest(User, Repo, LatestVersion),
      compare_version(Order, CurrentVersion, LatestVersion),
      ppm_updates_row(User, Repo, Order, CurrentVersion, LatestVersion)
    )
  ).

ppm_updates_row(_, _, =, _, _) :- !.
ppm_updates_row(User, Repo, Order, CurrentVersion, LatestVersion) :-
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
% Determine or test the order between two semantic version numbers.

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

%! github_clone(+User:atom, +Repo:atom, +Version:compound) is det.

github_clone(User, Repo, Version) :-
  phrase(version(Version), Codes),
  atom_codes(Tag, Codes),
  atomic_list_concat(['',User,Repo], /, Path),
  uri_components(Uri, uri_components(https,'github.com',Path,_,_)),
  pack_dir(PackDir),
  git([clone,Uri,'--branch',Tag,'--depth',1], [directory(PackDir)]).



%! github_info(+Dir:atom, -User:atom, -Repo:atom, -Version:compound) is det.

github_info(Dir, User, Repo, Version) :-
  git([config,'--get','remote.origin.url'], [directory(Dir),output(Codes1)]),
  atom_codes(Atom1, Codes1),
  atom_concat(Uri, '\n', Atom1),
  uri_components(Uri, uri_components(https,'github.com',Path,_,_)),
  atomic_list_concat(['',User,Repo], /, Path),
  git([describe,'--tags'], [directory(Dir),output(Codes2)]),
  phrase(version(Version), Codes2, _Rest).



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
  predsort(compare_version, Versions, SortedVersions),
  last(SortedVersions, LatestVersion).





% PPM-SPECIFIC HELPERS %

%! collect_deps(+Deps1:list(dict), -Deps2:ordset(dict)) is det.

collect_deps(L1, L2) :-
  collect_deps(L1, [], L2).


collect_deps([], L, Set) :- !,
  list_to_ord_set(L, Set).
collect_deps([H|T1], T2, L) :-
  get_dict(name, H, Repo),
  repo_dir(Repo, RepoDir),
  exists_directory(RepoDir), !,
  collect_deps(T1, T2, L).
collect_deps([H|T1], T2, L) :-
  \+ memberchk(H, T2), !,
  collect_deps(T1, [H|T2], L).
collect_deps([_|T1], T2, L) :-
  collect_deps(T1, T2, L).



%! repo_deps(+Repo:atom, -Deps:list(dict)) is det.

repo_deps(Repo, Deps) :-
  repo_dir(Repo, Dir),
  prolog_pack:pack_info_term(Dir, dependencies(Deps)).



%! repo_dir(+Repo:atom, -Dir:atom) is semidet.
%! repo_dir(-Repo:atom, -Dir:atom) is nondet.

repo_dir(Repo, RepoDir) :-
  pack_dir(PackDir),
  (   var(Repo)
  ->  directory_path(PackDir, RepoDir)
  ;   directory_file_path(PackDir, Repo, RepoDir),
      absolute_file_name(
        RepoDir,
        _,
        [access(read),file_errors(fail),file_type(directory)]
      )
  ),
  is_git_directory(RepoDir).



%! repo_is_prolog_pack(+Repo:atom) is semidet.

repo_is_prolog_pack(Repo) :-
  repo_dir(Repo, RepoDir),
  absolute_file_name(
    pack,
    _,
    [access(read),file_type(prolog),file_errors(fail),relative_to(RepoDir)]
  ).





% GENERIC HELPERS %

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



%! get_dict(?Key, +Dict, +Default, -Value) is det.

get_dict(Key, Dict, _, Value) :-
  get_dict(Key, Dict, Value), !.
get_dict(_, _, Value, Value).



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
