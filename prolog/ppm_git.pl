:- module(
  ppm_git,
  [
    git_add_remote_uri/2,  % +Directory, +Uri
    git_checkout/2,        % +Directory, +TagOrVersion
    git_clone/2,           % +Directory, +Uri
    git_create_tag/2,      % +Directory, +Tag
    git_current_version/2, % +Directory, -Version
    git_delete_tag/2,      % +Directory, +Tag
    git_fetch/1,           % +Directory
    git_initial_push/1,    % +Directory
    git_remote_exists/1,   % +Uri
    git_remote_uri/2,      % +Directory, -Uri
    git_tag/2,             % +Directory, -Tag
    git_version/2,         % +Directory, -Version
    git_version_latest/2   % +Directory, -LatestVersion
  ]
).

/** <module> Prolog Package Manager (PPM): Git support

Debug flag: `ppm(git)'.

---

@author Wouter Beek
@version 2017-2018
*/

:- use_module(library(ansi_term)).
:- use_module(library(debug)).
:- use_module(library(git), []).

:- use_module(library(ppm_generic)).





%! git_add_remote_uri(+Directory:atom, +Uri:atom) is det.

git_add_remote_uri(Dir, Uri) :-
  git(Dir, [remote,add,origin,Uri]).



%! git_checkout(+Directory:atom, +TagOrVersion:compound) is det.

git_checkout(Dir, tag(Tag)) :- !,
  git(Dir, [checkout,'-b',Tag,Tag]).
git_checkout(Dir, version(Version)) :-
  atom_phrase(version(Version), Tag),
  git_checkout(Dir, tag(Tag)).



%! git_clone(+Directory:atom, +Uri:atom) is det.

git_clone(Dir, Uri) :-
  git(Dir, [clone,Uri]).



%! git_create_tag(+Directory:atom, +Tag:atom) is det.

git_create_tag(Dir, Tag) :-
  git_tag(Dir, Tag), !.
git_create_tag(Dir, Tag) :-
  git(Dir, [tag,'-a',Tag,'-m',Tag]),
  git(Dir, [push,origin,Tag]).



%! git_current_version(+Directory:atom, -Version:compound) is det.

git_current_version(Dir, Version) :-
  % Git returns error code 128 when there are no tags
  git(Dir, [describe,'--tags'], Codes),
  phrase(version(Version), Codes, _Rest).



%! git_delete_tag(+Directory:atom, +Tag:atom) is det.

git_delete_tag(Dir, Tag) :-
  git(Dir, [tag,'-d',Tag]),
  atom_concat(':refs/tags/', Tag, Arg),
  git(Dir, [push,origin,Arg]).



%! git_fetch(+Directory:atom) is det.

git_fetch(Dir) :-
  git(Dir, [fetch]).



%! git_initial_push(+Directory:atom) is det.

git_initial_push(Dir) :-
  git(Dir, [push,'-u',origin,master]).



%! git_remote_exists(+Uri:atom) is semidet.
%
% Succeeds iff a Git repository is located at Uri.

git_remote_exists(Uri) :-
  git(['ls-remote',Uri]).



%! git_remote_uri(+Directory:atom, -Uri:atom) is det.

git_remote_uri(Dir, Uri) :-
  git(Dir, [config,'--get','remote.origin.url'], Codes),
  atom_codes(Atom, Codes),
  atom_concat(Uri, '\n', Atom).



%! git_tag(+Directory:atom, +Tag:atom) is semidet.
%! git_tag(+Directory:atom, -Tag:atom) is nondet.

git_tag(Dir, Tag) :-
  git(Dir, [tag], Codes),
  atom_codes(Atom, Codes),
  atomic_list_concat(Tags, '\n', Atom),
  (   var(Tag)
  ->  member(Tag, Tags),
      Tag \== ''
  ;   memberchk(Tag, Tags)
  ).



%! git_version(+Direction:atom, +Version:compound) is semidet.
%! git_version(+Direction:atom, -Version:compound) is nondet.

git_version(Dir, Version) :-
  git_tag(Dir, Tag),
  atom_phrase(version(Version), Tag).



%! git_version_latest(+Directory:atom, +Version:compound) is semidet.
%! git_version_latest(+Directory:atom, -Version:compound) is semidet.

git_version_latest(Dir, Version) :-
  aggregate_all(set(Version), git_version(Dir, Version), Versions),
  predsort(compare_version, Versions, SortedVersions),
  last(SortedVersions, Version).





% HELPERS %

%! git(+Arguments:list(atom)) is semidet.
%! git(+Dir:atom, +Arguments:list(atom)) is semidet.
%! git(+Dir:atom, +Arguments:list(atom), -Output:list(code)) is semidet.

git(Args) :-
  git(., Args).


git(Dir, Args) :-
  git(Dir, Args, _).


git(Dir, Args, Output) :-
  (   debugging(ppm(git))
  ->  atomic_list_concat([git|Args], ' ', Cmd),
      debug(ppm(git), "~a", [Cmd])
  ;   true
  ),
  git:git(Args, [directory(Dir),error(Error),output(Output),status(Status)]),
  (   Error == []
  ->  debug(ppm(git), "~s", [Output])
  ;   ansi_format([fg(red)], "~s", [Error])
  ),
  Status = exit(0).
