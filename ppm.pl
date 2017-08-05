:- module(
  ppm,
  [
    ppm_install/2, % +User, +Name
    ppm_list/0,
    ppm_publish/2, % +User, +Name
    ppm_publish/3, % +User, +Name, +Version
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

:- use_module(library(aggregate)).
:- use_module(library(ansi_term)).
:- use_module(library(apply)).
:- use_module(library(dcg/basics)).
:- use_module(library(debug)).
:- use_module(library(filesex)).
:- use_module(library(git), []).
:- use_module(library(http/http_json), []).
:- use_module(library(http/http_open)).
:- use_module(library(http/json)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(ordsets)).
:- use_module(library(readutil)).
:- use_module(library(settings)).
:- use_module(library(uri)).

:- debug(http(receive_reply)).
:- debug(http(send_request)).
:- debug(ppm(git)).
:- debug(ppm(github)).

:- setting(password, any, _, "Github password").





%! ppm_current(?User:atom, ?Name:atom, ?Version:compound) is nondet.
%! ppm_current(?User:atom, ?Name:atom, ?Version:compound,
%!             -Deps:list(dict)) is nondet.
%
% Enumerates currently installed packages together with their semantic
% version number.

ppm_current(User, Repo, Version) :-
  github_info(Repo, User, Repo, Version).


ppm_current(User, Repo, Version, Deps) :-
  ppm_current(User, Repo, Version),
  repo_deps(Repo, Deps).



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
  github_version_latest(User, Repo, Version), !,
  github_clone_version(User, Repo, Version),
  repo_deps(Repo, Deps1),
  collect_deps(Deps1, Deps2),
  maplist(ppm_install_dependency, Deps2),
  phrase(version(Version), Codes),
  ansi_format([fg(green)], "Successfully installed ~a ‘~a’, version ~s\n", [Kind,Repo,Codes]).
ppm_install(User, Repo, Kind) :-
  ansi_format([fg(red)], "Cannot find a version of ~a's ~a ‘~a’.", [User,Kind,Repo]).

ppm_install_dependency(Dep) :-
  _{repo: Repo, user: User} :< Dep,
  ppm_install(User, Repo, dependency).



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



%! ppm_publish(+User:atom, +Name:atom) is det.
%! ppm_publish(+User:atom, +Name:atom, +Version(compound)) is det.

ppm_publish(User, Repo) :-
  github_authorized(User),
  ppm_current(User, Repo, CurrentVersion),
  github_version_latest(User, Repo, LatestVersion),
  compare_version(Order, CurrentVersion, LatestVersion),
  (   Order == <
  ->  phrase(version(CurrentVersion), Codes1),
      phrase(version(LatestVersion), Codes2),
      ansi_format(
        [fg(red)],
        "Cannot publish package ‘~a’: local version (~s) is behind remote version (~s).\n",
        [Repo,Codes1,Codes2]
      )
  ;   (   Order == =
      ->  increment_version(patch, CurrentVersion, Version),
          phrase(version(Version), Codes3),
          format("Publishing package ‘~a’ as patch release ~s.\n", [Repo,Codes3])
      ;   Version = CurrentVersion
      ),
      ppm_publish_version(User, Repo, Version)
  ).


ppm_publish(User, Repo, LocalVersion) :-
  github_authorized(User),
  % make sure the new version is newer than the last version.
  (   github_version_latest(User, Repo, RemoteVersion)
  ->  compare_version(Order, LocalVersion, RemoteVersion),
      (   memberchk(Order, [<,=])
      ->  % informational
          phrase(version(LocalVersion), Codes1),
          phrase(version(RemoteVersion), Codes2),
          ansi_format(
            [fg(red)],
            "Cannot publish package ‘~a’: local version (~s) must be ahead of remote version (~s).\n",
            [Repo,Codes1,Codes2]
          )
      ;   ppm_publish_version(User, Repo, LocalVersion)
      )
  ;   ppm_publish_version(User, Repo, LocalVersion)
  ).

ppm_publish_version(User, Repo, Version) :-
  % create the Git tag
  repo_dir(Repo, RepoDir),
  phrase(version(Version), Codes),
  atom_codes(Tag, Codes),
  git_create_tag(RepoDir, Tag),
  % create the Github release
  github_create_release(User, Repo, Tag).



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
  % update existing dependencies
  collect_deps(Deps1, Deps2),
  maplist(ppm_update_dependency, Deps2),
  % update the package itself
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
  aggregate_all(
    set(update(User,Repo,CurrentVersion,Order,LatestVersion)),
    (
      ppm_current(User, Repo, CurrentVersion),
      github_version_latest(User, Repo, LatestVersion),
      compare_version(Order, CurrentVersion, LatestVersion),
      Order \== =
    ),
    Updates
  ),
  (   Updates == []
  ->  format("No updates available.\n")
  ;   maplist(ppm_updates_row, Updates)
  ).

ppm_updates_row(update(User,Repo,CurrentVersion,Order,LatestVersion)) :-
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



%! increment_version(+Kind:oneof([major,minor,patch]), +Version1:compound,
%!                   -Version2:compound) is det.

increment_version(major, version(Ma1,Mi,P), version(Ma2,Mi,P)) :- !,
  Ma2 is Ma1 + 1.
increment_version(minor, version(Ma,Mi1,P), version(Ma,Mi2,P)) :- !,
  Mi2 is Mi1 + 1.
increment_version(patch, version(Ma,Mi,P1), version(Ma,Mi,P2)) :-
  P2 is P1 + 1.



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





% VC: Git %

%! git_clone_tag(+Uri:atom, +Tag:atom) is det.

git_clone_tag(Uri, Tag) :-
  pack_dir(PackDir),
  git(PackDir, [clone,Uri,'--branch',Tag,'--depth',1]).



%! git_create_tag(+Dir:atom, +Tag:atom) is det.

git_create_tag(Dir, Tag) :-
  git(Dir, [tag,'-a',Tag,'-m',Tag]),
  git(Dir, [push,origin,Tag]).



%! git_current_version(+Dir:atom, -Version:compound) is det.

git_current_version(Dir, Version) :-
  git(Dir, [describe,'--tags'], Codes),
  phrase(version(Version), Codes, _Rest).



%! git_remote_uri(+Dir:atom, -Uri:atom) is det.

git_remote_uri(Dir, Uri) :-
  git(Dir, [config,'--get','remote.origin.url'], Codes),
  atom_codes(Atom, Codes),
  atom_concat(Uri, '\n', Atom).





% SERVICE: GITHUB %

%! github_authorized(+User:atom) is semidet.

github_authorized(User) :-
  github_open(User, [applications,grants], [], 200), !.
github_authorized(User) :-
  ansi_format([fg(red)], "You are not authorized for modifying Github account ‘~a’.\n", [User]),
  format("Consider using `set_setting(ppm:password, 'YOUR-PASSWORD')`.\n").



%! github_clone_version(+User:atom, +Repo:atom, +Version:compound) is det.

github_clone_version(User, Repo, Version) :-
  phrase(version(Version), Codes),
  atom_codes(Tag, Codes),
  atomic_list_concat(['',User,Repo], /, Path),
  uri_components(Uri, uri_components(https,'github.com',Path,_,_)),
  git_clone_tag(Uri, Tag).



%! github_create_release(+User:atom, +Repo:atom, +Tag:atom) is det.

github_create_release(User, Repo, Tag) :-
  github_open(User, [repos,User,Repo,releases], [post(json(_{tag_name: Tag}))], 201).



%! github_info(+Repo:atom, -User:atom, -Repo:atom, -Version:compound) is det.

github_info(Repo, User, Repo, Version) :-
  repo_dir(Repo, RepoDir),
  git_remote_uri(RepoDir, Uri),
  uri_components(Uri, Comps),
  uri_data(path, Comps, Path),
  atomic_list_concat(['',User,Repo], /, Path),
  git_current_version(RepoDir, Version).



%! github_open(+User:atom, +Segments:list(atom), +Options:list(compound),
%!             +Status:between(100,599)) is det.
%! github_open(+User:atom, +Segments:list(atom), +Options:list(compound),
%!             +Status:between(100,599), -In:stream) is det.

github_open(User, Segments, Options1, Status) :-
  github_open(User, Segments, Options1, Status, In),
  read_stream_to_codes(In, Codes),
  debug(ppm(github), "~s", [Codes]).


github_open(User, Segments, Options1, Status, In) :-
  atomic_list_concat([''|Segments], /, Path),
  uri_components(Uri, uri_components(https,'api.github.com',Path,_,_)),
  (   setting(password, Password),
      atom(Password)
  ->  merge_options([authorization(basic(User,Password))], Options1, Options2)
  ;   Options2 = Options1
  ),
  merge_options(
    [
      headers(Headers),
      request_header('Accept'='application/vnd.github.v3+json'),
      status_code(Status)
    ],
    Options2,
    Options3
  ),
  http_open(Uri, In, Options3),
  (debugging(http(receive_reply)) -> print_http_reply(Status, Headers) ; true).

print_http_reply(Status, Headers) :-
  debug(http(receive_reply), "~a", [Status]),
  maplist(print_http_header, Headers),
  debug(http(receive_reply), "", []).

print_http_header(Header) :-
  Header =.. [Key1,Value],
  atomic_list_concat(L, '_', Key1),
  atomic_list_concat(L, -, Key2),
  debug(http(receive_reply), "< ~a: ~a", [Key2,Value]).



%! github_version(+User:atom, +Repo:atom, ?Version:compound) is nondet.
%! github_version(+User:atom, +Repo:atom, ?Version:compound,
%!                -Id:atom) is nondet.

github_version(User, Repo, Version) :-
  github_version(User, Repo, Version, _).


github_version(User, Repo, Version, Id) :-
  github_open(User, [repos,User,Repo,releases], [], 200, In),
  call_cleanup(json_read_dict(In, Dicts, [value_string_as(atom)]), close(In)),
  member(Dict, Dicts),
  atom_codes(Dict.tag_name, Codes),
  phrase(version(Version), Codes),
  Id = Dict.id.



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



%! git(+Dir:atom, +Arguments:list(atom)) is det.
%! git(+Dir:atom, +Arguments:list(atom), -Output:list(code)) is det.

git(Dir, Args) :-
  git(Dir, Args, _).


git(Dir, Args, Output) :-
  git:git(Args, [directory(Dir),error(Error),output(Output)]),
  debug(ppm(git), "~s", [Output]),
  (Error == [] -> true ; ansi_format([fg(red)], "~s", [Error])).



%! repo_deps(+Repo:atom, -Deps:list(dict)) is det.

repo_deps(Repo, Deps) :-
  repo_dir(Repo, Dir),
  (   absolute_file_name(
        ppm,
        File,
        [access(read),directory(Dir),extensions([json]),file_errors(fail)]
      )
  ->  setup_call_cleanup(
        open(File, read, In),
        json_read_dict(In, Dict, [value_string_as(atom)]),
        close(In)
      ),
      get_dict(dependencies, Dict, Deps)
  ;   Deps = []
  ).



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





% MAINTENANCE %

%! git_delete_tag(+Dir:atom, +Tag:atom) is det.

git_delete_tag(Dir, Tag) :-
  git(Dir, [tag,'-d',Tag]),
  atom_concat(':refs/tags/', Tag, Arg),
  git(Dir, [push,origin,Arg]).



%! git_tag(+Dir:atom, -Tag:atom) is nondet.

git_tag(Dir, Tag) :-
  git(Dir, [tag], Codes),
  atom_codes(Atom, Codes),
  atomic_list_concat(Tags, '\n', Atom),
  member(Tag, Tags).



%! github_delete_version(+User:atom, +Repo:atom, +Version:compound) is det.

github_delete_version(User, Repo, Version) :-
  github_version(User, Repo, Version, Id),
  github_open(User, [repos,User,Repo,releases,Id], [method(delete)], 204).



%! github_tag(+User:atom, +Repo:atom, -Tag:atom) is nondet.

github_tag(User, Repo, Tag) :-
  github_open(User, [repos,User,Repo,tags], [], 200, In),
  call_cleanup(json_read_dict(In, Dicts, [value_string_as(atom)]), close(In)),
  member(Dict, Dicts),
  Tag = Dict.name.





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
