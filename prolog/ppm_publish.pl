:- module(
  ppm_publish,
  [
    ppm_publish/4 % +Dir, +User, +Repo, +Version
  ]
).

/** <module> Prolog Package Manager (PPM): Publish packages

@author Wouter Beek
@version 2017-2018
*/

:- use_module(library(ansi_term)).

:- use_module(library(ppm_generic)).
:- use_module(library(ppm_git)).
:- use_module(library(ppm_github)).





%! ppm_publish(+Directory:atom, +User:atom, +Name:atom, +Version(compound)) is det.

ppm_publish(Dir, User, Repo, LocalVersion) :-
  github_uri(User, Repo, Uri),
  git_remote_exists(Uri), !,
  % make sure the local version is set as a Git tag
  atom_phrase(version(LocalVersion), Tag),
  git_create_tag(RepoDir, Tag),
  % make sure the local version is ahead of the remote version
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
          ),
          fail
      ;   true
      )
  ;   true
  ),
  % create the Github release
  github_create_release(User, Repo, Tag).
ppm_publish(User, Repo, LocalVersion) :-
  github_create_repository(Repo, Uri),
  repository_directory(User, Repo, RepoDir),
  git_add_remote_uri(RepoDir, Uri),
  git_initial_push(RepoDir),
  ppm_publish(User, Repo, LocalVersion).
