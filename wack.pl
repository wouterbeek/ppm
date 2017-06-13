:- module(
  wack,
  [
    wack/3,         % ?Owner:atom, ?Repo:atom, ?Version:compound
    wack_install/2, % +Owner:atom, +Repo:atom
    wack_install/3, % +Owner:atom, +Repo:atom, +Version:compound
    wack_update/2,  % +Owner:atom, +Repo:atom
    wack_version/3  % +Owner:atom, +Repo:atom, -Version:compound
  ]
).

/** <module> WACK (Wouter pACK)

A super simple SWI-Prolog Pack alternative.

@author Wouter Beek
@version 2017/06
*/

:- use_module(library(apply)).
:- use_module(library(dcg/basics)).
:- use_module(library(filesex)).
:- use_module(library(http/http_open)).
:- use_module(library(http/json)).
:- use_module(library(lists)).
:- use_module(library(process)).
:- use_module(library(readutil)).
:- use_module(library(uri)).



  

%! pack_dir(-PackDir:atom) is det.

pack_dir(PackDir) :-
  absolute_file_name(library(.), LibDir, [access(write),file_type(directory)]),
  directory_file_path(LibDir, 'swipl/pack', PackDir),
  (exists_directory(PackDir) -> true ; make_directory_path(PackDir)).



%! version(?Version:compound)// is det.

version(version(Major,Minor,Patch)) -->
  "V",
  integer(Major),
  ".",
  integer(Minor),
  ".",
  integer(Patch).



%! wack(?Owner:atom, ?Repo:atom, ?Version:compound) is nondet.
%
% Enumerates currently installed WACKs and shows their version.

wack(Owner, Repo, Version) :-
  wack_file(WackFile),
  setup_call_cleanup(
    open(WackFile, read, In),
    json_read_dict(In, Dict, []),
    close(In)
  ),
  _{name: Repo, owner: Owner} :< Dict.repository,
  atom_codes(Dict.version, Codes),
  phrase(version(Version), Codes).



%! wack_file(-WackFile:atom) is nondet.
%
% Enuemrated WACK files of currently installed WACKs.

wack_file(WackFile) :-
  pack_dir(PackDir),
  directory_path(PackDir, WackDir),
  absolute_file_name(
    wack,
    WackFile,
    [
      access(read),
      extensions([json]),
      file_errors(fail),
      relative_to(WackDir),
      solutions(all)
    ]
  ).



%! wack_install(+Owner:atom, +Repo:atom) is semidet.
%! wack_install(+Owner:atom, +Repo:atom, +Version:compound) is semidet.
%
% Installs a WACK.  The latests version is chosen in case none is
% specified.

wack_install(Owner, Repo) :-
  aggregate_all(
    set(Version),
    wack_version(Owner, Repo, Version),
    Versions
  ),
  reverse(Versions, [Version|_]),
  wack_install(Owner, Repo, Version).


wack_install(Owner, Repo, Version) :-
  phrase(version(Version), Codes),
  atom_codes(Tag, Codes),
  atomic_list_concat(['',Owner,Repo], /, Path),
  uri_components(Uri, uri_components(https,'github.com',Path,_,_)),
  git([clone,Uri,'--branch',Tag,'--depth',1]).



%! wack_update(+Owner:atom, +Repo:atom) is semidet.
%
% Updates an exisiting WACK

wack_update(Owner, Repo) :-
  wack_version(Owner, Repo, Version),
  writeln(Version).



%! wack_version(+Owner:atom, +Repo:atom, -Version:compound) is nondet.

wack_version(Owner, Repo, Version) :-
  github_version(Owner, Repo, Version).





% GITHUB %

%! github_open(+Segments:list(atom), -In:stream) is det.

github_open(Segments, In) :-
  atomic_list_concat([''|Segments], /, Path),
  uri_components(Uri, uri_components(https,'api.github.com',Path,_,_)),
  http_open(
    Uri,
    In,
    [request_header('Accept'='application/vnd.github.v3+json')]
  ).



%! github_version(+Owner:atom, +Repo:atom, -Version:compound) is nondet.

github_version(Owner, Repo, Version) :-
  github_open([repos,Owner,Repo,tags], In),
  call_cleanup(
    json_read_dict(In, Tags, [value_string_as(atom)]),
    close(In)
  ),
  member(Tag, Tags),
  atom_codes(Tag.name, Codes),
  phrase(version(Version), Codes).





% GIT %

%! git(+Args:list(atomic)) is det.

git(Args) :-
  pack_dir(PackDir),
  setup_call_cleanup(
    process_create(
      path(git),
      Args,
      [cwd(PackDir),process(Pid),stderr(pipe(Err)),stdout(pipe(Out))]
    ),
    (
      thread_create(print_err(Err), ErrId, [at_exit(close(Err))]),
      copy_stream_data(Out, user_output),
      process_wait(Pid, exit(OutStatus)),
      thread_signal(ErrId, thread_exit(ErrStatus)),
      print_status(ErrStatus),
      print_status(OutStatus)
    ),
    close(Out)
  ).





% HELPERS %

%! directory_path(+Dir, -Path) is nondet.

directory_path(Dir, Path) :-
  directory_files(Dir, Files),
  member(File, Files),
  \+ is_dummy_file(File),
  directory_file_path(Dir, File, Path).



%! is_dummy_file(+File) is semidet.

is_dummy_file(.).
is_dummy_file(..).



%! print_err(+Err:stream) is det.

print_err(Err) :-
  repeat,
  read_stream_to_codes(Err, Codes, []),
  (   Codes == end_of_file
  ->  !
  ;   string_codes(String, Codes),
      split_string(String, "\n", "", Strings),
      exclude(==(""), Strings, NonEmptyStrings),
      maplist(print_message(warning), NonEmptyStrings)
  ),
  fail.



%! print_status(+Status) is det.

print_status(0) :- !.
print_status(exit(Status)) :- !,
  print_status(Status).
print_status(Status) :-
  print_message(warning, status(Status)).
