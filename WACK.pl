:- module(
  'WACK',
  [
    wack_install/2, % +Owner, +Repo
    wack_ls/0,
    wack_remove/1,  % +Repo
    wack_update/2,  % +Owner, +Repo
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
:- use_module(library(process)).
:- use_module(library(readutil)).
:- use_module(library(uri)).



  

%! wack(?Owner:atom, ?Repo:atom, ?Version:compound) is nondet.
%
% Enumerates currently installed WACKs together with their semantic
% version.

wack(Owner, Repo, Version) :-
  wack0(_, WackDict),
  _{name: Repo, owner: Owner} :< WackDict.repository,
  atom_codes(WackDict.version, Codes),
  phrase(version(Version), Codes).

wack0(WackDir, WackDict) :-
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
  ),
  setup_call_cleanup(
    open(WackFile, read, In),
    json_read_dict(In, WackDict, []),
    close(In)
  ).



%! wack_ls is det.
%
% Display all currently installed WACKs.

wack_ls :-
  forall(
    wack(Owner, Repo, Version),
    wack_ls_row(Owner, Repo, Version)
  ).

wack_ls_row(Owner, Repo, Version) :-
  phrase(version(Version), Codes),
  format("~a\t~a\t~s", [Owner,Repo,Codes]).



%! wack_install(+Owner:atom, +Repo:atom) is semidet.
%
% Installs a WACK.  The latests version is chosen in case none is
% specified.

wack_install(Owner, Repo) :-
  wack(Owner, Repo, CurrentVersion), !,
  phrase(version(CurrentVersion), Codes),
  format("Package ~a's ‘~a’ is already installed (version ~s)\n",
         [Owner,Repo,Codes]),
  format("Use wack_update/2 to update a package.\n").
wack_install(Owner, Repo) :-
  wack_version_latest(Owner, Repo, LatestVersion),
  phrase(version(Version), Codes),
  atom_codes(Tag, Codes),
  atomic_list_concat(['',Owner,Repo], /, Path),
  uri_components(Uri, uri_components(https,'github.com',Path,_,_)),
  git([clone,Uri,'--branch',Tag,'--depth',1]),
  Version =.. [version|T],
  format("Successfully installed ~a's ‘~a’, version ~d.~d.~d\n",
         [Owner,Repo|T]).



%! wack_remove(+Repo:atom) is det.

wack_remove(Repo) :-
  wack0(WackDir, WackDict),
  Repo = WackDict.name, !,
  delete_directory(WackDir),
  format(user_output, "Deleted ‘~a’.", [Repo]).



%! wack_update(+Owner:atom, +Repo:atom) is semidet.
%
% Updates an exisiting WACK.

wack_update(Owner, Repo) :-
  wack(Owner, Repo, CurrentVersion),
  wack_version_latest(Owner, Repo, LatestVersion),
  (   CurrentVersion == LatestVersion
  ->  format(user_output, "No need to update.\n")
  ;   wack_remove(Repo),
      wack_install(Owner, Repo, LatestVersion),
      LatestVersion =.. [version|T],
      format(
        user_output,
        "Updated ~a's ‘~a’ to version ~d.~d.~d\n",
	      [Owner,Repo|T]
      )
  ).



%! wack_updates is det.
%
% Shows packages, if any, that can be updated using wack_update/2.

wack_updates :-
  forall(
    wack(Owner, Repo, CurrentVersion),
    (
      wack_version_latest(Owner, Repo, LatestVersion),
      compare_version(Order, CurrentVersion, LatestVersion),
      wack_updates_row(Owner, Repo, Order, CurrentVersion, LatestVersion)
    )
  ).

wack_updates_row(_, _, =, _, _) :- !.
wack_updates_row(Owner, Repo, Order, CurrentVersion, LatestVersion) :-
  format("~a\t~a\t", [Owner,Repo]),
  order_colors(Order, Color1, Color2),
  phrase(version(CurrentVersion), CurrentCodes),
  ansi_format([fg(Color1)], "~s", [CurrentCodes]),
  format("\t»\t"),
  phrase(version(LatestVersion), LatestCodes),
  ansi_format([fg(Color2)], "~s", [LatestCodes]).

order_colors(<, red, green).
order_colors(>, green, red).



%! wack_version(+Owner:atom, +Repo:atom, -Version:compound) is nondet.

wack_version(Owner, Repo, Version) :-
  github_version(Owner, Repo, Version).



%! wack_version_latest(+Owner:atom, +Repo:atom,
%!                     -LatestVersion:compound) is det.

wack_version_latest(Owner, Repo, LatestVersion) :-
  aggregate_all(
    set(Version),
    wack_version(Owner, Repo, Version),
    Versions
  ),
  reverse(Versions, [LatestVersion|_]).





% SERVICE: GITHUB %

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





% VC: GIT %

%! git(+Arguments:list(atomic)) is det.
%
% Generic Git invocation that takes care of routing content from the
% output and error streams.

git(Arguments) :-
  pack_dir(PackDir),
  git(Arguments, [directory(PackDir)]).





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
  version_indicator,
  integer(Major),
  ".",
  integer(Minor),
  ".",
  integer(Patch).
version_indicator --> "v", !.
version_indicator --> "V".





% HELPERS %

%! directory_path(+Directory:atom, -File:atom) is nondet.
%
% Non-determinisitcally enumerates the Files that are in Directory.
%
% @arg Directory is an atom denoting a directory on the filesystem.
%
% @arg File is an atomic full path specifier of a file in Directory.
%
% The dummy files `.' and `..' are not included.

directory_path(Directory, Path) :-
  directory_files(Directory, Files),
  member(File, Files),
  \+ is_dummy_file(File),
  directory_file_path(Directory, File, Path).



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



%! print_error(+Err:stream) is det.

print_error(Err) :-
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
