:- module(
  ppm,
  [
    ppm_help/0,
    ppm_install/2, % +User, +Repo
    ppm_list/0,
    ppm_remove/2,  % +User, +Repo
    ppm_run/2,     % +User, +Repo
    ppm_sync/0,
    ppm_update/2,  % +User, +Repo
    ppm_updates/0
  ]
).

/** <module> Prolog Package Manager (PPM)

A simple package manager for SWI-Prolog.

---

@author Wouter Beek
@version 2017-2018
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(filesex)).
:- use_module(library(http/http_json), []).
:- use_module(library(prolog_pack), []).

:- use_module(library(ppm_generic)).
:- use_module(library(ppm_git)).
:- use_module(library(ppm_github)).

:- initialization
   init_ppm.





%! ppm_current(?User:atom, ?Name:atom, ?Version:compound) is nondet.
%! ppm_current(?User:atom, ?Name:atom, ?Version:compound,
%!             -Dependencies:list(dict)) is nondet.
%
% Enumerates currently installed packages together with their semantic
% version number.

ppm_current(User, Repo, Version) :-
  repository_directory(User, Repo, Dir),
  git_current_version(Dir, Version).


ppm_current(User, Repo, Version, Dependencies) :-
  ppm_current(User, Repo, Version),
  ppm_dependencies(User, Repo, Dependencies).



%! ppm_help is det.

ppm_help :-
  ansi_format([fg(green)], "Welcome "),
  ansi_format([fg(red)], "to "),
  ansi_format([fg(blue)], "Prolog "),
  ansi_format([fg(yellow)], "Package "),
  ansi_format([fg(magenta)], "Manager"),
  format("!"),
  nl,
  format("We are so happy that you're here :-)"),
  nl,
  nl.



%! ppm_install(+User:atom, +Repo:atom) is semidet.
%
% Installs a package.  The latests version is chosen in case none is
% specified.

ppm_install(User, Repo) :-
  ppm_install(User, Repo, package).

ppm_install(User, Repo, Kind) :-
  ppm_current(User, Repo, _), !,
  ppm_update(User, Repo, Kind).
ppm_install(User, Repo, Kind) :-
  github_tag_latest(User, Repo, Version), !,
  github_clone_version(User, Repo, Version),
  ppm_dependencies(User, Repo, Dependencies),
  maplist(ppm_install_dependency, Dependencies),
  phrase(version(Version), Codes),
  ansi_format(
    [fg(green)],
    "Successfully installed ~a ‘~a’ (~s)\n",
    [Kind,Repo,Codes]
  ).
ppm_install(User, Repo, Kind) :-
  ansi_format(
    [fg(red)],
    "Could find a version tag in ~a's ~a ‘~a’.",
    [User,Kind,Repo]
  ),
  fail.

ppm_install_dependency(Dependency) :-
  _{name: Repo, user: User} :< Dependency,
  ppm_install(User, Repo, dependency).



%! ppm_list is det.
%
% Display all currently installed PPMs.

ppm_list :-
  aggregate_all(
    set(package(User,Repo,Version,Dependencies)),
    ppm_current(User, Repo, Version, Dependencies),
    Packages
  ),
  (   Packages == []
  ->  format("No packages are currently installed.\n")
  ;   maplist(ppm_list_row, Packages)
  ).

ppm_list_row(package(User,Repo,Version,Dependencies)) :-
  phrase(version(Version), Codes),
  format("~a/~a (~s)\n", [User,Repo,Codes]),
  maplist(ppm_list_dep_row, Dependencies).

ppm_list_dep_row(Dependency) :-
  _{name: Repo, user: User} :< Dependency,
  format("  ⤷ ~a/~a\n", [User,Repo]).



%! ppm_remove(+User:atom, +Repo:atom) is det.
%
% Removes a package.
%
% TBD: Support for removing otherwise unused dependencies.

ppm_remove(User, Repo) :-
  ppm_current(User, Repo, Version),
  repository_directory(User, Repo, RepoDir),
  delete_directory_and_contents(RepoDir),
  phrase(version(Version), Codes),
  format("Deleted package ‘~a/~a’ (~s).", [User,Repo,Codes]).



%! ppm_run(+User:atom, +Repo:atom) is semidet.

ppm_run(User, Repo) :-
  repository_directory(User, Repo, Dir),
  /*
  % Load the local configuration file, if available.
  (   file_by_name(RepoDir, 'conf.json', ConfFile)
  ->  set_cli_arguments([conf(ConfFile)])
  ;   true
  ),
  */
  (   file_by_name(Dir, 'run.pl', File)
  ->  consult(File)
  ;   ansi_format([fg(red)], "Package ‘~a/~a’ is currently not installed.\n", [User,Repo])
  ).



%! ppm_sync is det.
%
% Synchronizes the packages the current Prolog session has access to
% the to packages stored in `~/.ppm'.

ppm_sync :-
  root_directory(Root),
  ppm_sync_(Root).

ppm_sync_(Root) :-
  assertz(user:file_search_path(ppm, Root)),
  forall(
    repository_directory(User, Repo, _),
    (
      atomic_list_concat([User,Repo,prolog], /, SubDir),
      directory_by_name(Root, SubDir),
      assertz(user:file_search_path(library, ppm(SubDir)))
    )
  ).



%! ppm_update(+User, +Repo:atom) is semidet.
%
% Updates an exisiting package and all of its dependencies.

ppm_update(User, Repo) :-
  ppm_update(User, Repo, package).


ppm_update(User, Repo, Kind) :-
  ppm_current(User, Repo, CurrentVersion, Dependencies1),
  % update existing dependencies
  maplist(ppm_update_dependency, Dependencies1),
  % update the package itself
  github_tag_latest(User, Repo, LatestVersion),
  (   compare_version(<, CurrentVersion, LatestVersion)
  ->  ppm_remove(User, Repo),
      ppm_install(User, Repo),
      % informational
      phrase(version(CurrentVersion), Codes1),
      phrase(version(LatestVersion), Codes2),
      format("Updated ‘~a’: ~s → ~s\n", [Repo,Codes1,Codes2])
  ;   % informational
      (   Kind == package
      ->  format("No need to update ~a ‘~a’.\n", [Kind,Repo])
      ;   true
      )
  ),
  % install new dependencies
  ppm_current(User, Repo, LatestVersion, Dependencies2),
  ord_subtract(Dependencies2, Dependencies1, Dependencies3),
  maplist(ppm_install_dependency, Dependencies3).

ppm_update_dependency(Dependency) :-
  _{user: User, repo: Repo} :< Dependency,
  ppm_update(User, Repo, dependency).



%! ppm_updates is det.
%
% Shows packages, if any, that can be updated using ppm_update/1.

ppm_updates :-
  aggregate_all(
    set(update(User,Repo,CurrentVersion,Order,LatestVersion)),
    (
      ppm_current(User, Repo, CurrentVersion),
      github_tag_latest(User, Repo, LatestVersion),
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
  format("»"),
  phrase(version(LatestVersion), LatestCodes),
  ansi_format([fg(Color2)], "~s\n", [LatestCodes]).

order_colors(<, red, green).
order_colors(>, green, red).





% INITIALIZATION %

init_ppm :-
  root_directory(Root),
  ensure_directory_exists(Root),
  ppm_sync_(Root).
