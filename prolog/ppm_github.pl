:- module(
  ppm_github,
  [
    github_clone_version/3,     % +User, +Repo, +Version
    github_create_release/3,    % +User. +Repo, +Tag
    github_create_repository/2, % +Repo, -Uri
    github_delete_version/3,    % +User, +Repo, +Version
    github_tag/3,               % +User, +Repo, ?Tag
    github_uri/3,               % +User, +Repo, -Uri
    github_version_latest/3     % +User, +Repo, -Version
  ]
).

/** <module> Prolog Package Manager (PPM): Github support

@author Wouter Beek
@version 2017-2018
*/

:- use_module(library(aggregate)).
:- use_module(library(debug)).
:- use_module(library(http/http_open)).
:- use_module(library(http/json)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(uri)).

:- use_module(library(ppm_generic)).
:- use_module(library(ppm_git)).

:- debug(ppm(github)).





%! github_clone_version(+User:atom, +Repo:atom, +Version:compound) is det.

github_clone_version(User, Repo, Version) :-
  atom_phrase(version(Version), Tag),
  atomic_list_concat(['',User,Repo], /, Path),
  uri_components(Uri, uri_components(https,'github.com',Path,_,_)),
  user_directory(User, Dir),
  git_clone_tag(Dir, Uri, Tag).



%! github_create_release(+User:atom, +Repo:atom, +Tag:atom) is det.

github_create_release(User, Repo, Tag) :-
  github_open_authorized(
    [repos,User,Repo,releases],
    [post(json(_{tag_name: Tag}))],
    201
  ).



%! github_create_repository+Repo:atom, -Uri:atom) is det.

github_create_repository(Repo, Uri) :-
  github_open_authorized([user,repos], [post(json(_{name: Repo}))], 201, In),
  call_cleanup(
    json_read_dict(In, Dict, [value_string_as(atom)]),
    close(In)
  ),
  Uri = Dict.html_url.



%! github_delete_version(+User:atom, +Repo:atom, +Version:compound) is det.

github_delete_version(User, Repo, Version) :-
  github_version(User, Repo, Version, Id),
  github_open_authorized([repos,User,Repo,releases,Id], [method(delete)], 204).



%! github_tag(+User:atom, +Repo:atom, +Tag:atom) is semidet.
%! github_tag(+User:atom, +Repo:atom, -Tag:atom) is nondet.

github_tag(User, Repo, Tag) :-
  github_open([repos,User,Repo,tags], [], 200, In),
  call_cleanup(
    json_read_dict(In, Dicts, [value_string_as(atom)]),
    close(In)
  ),
  member(Dict, Dicts),
  Tag = Dict.name.



%! github_uri(+User:atom, +Repo:atom, -Uri:atom) is det.

github_uri(User, Repo, Uri) :-
  atomic_list_concat(['',User,Repo], /, Path),
  uri_components(Uri, uri_components(https,'github.com',Path,_,_)).



%! github_version(+User:atom, +Repo:atom, ?Version:compound) is nondet.

github_version(User, Repo, Version) :-
  github_open([repos,User,Repo,releases], [], 200, In),
  call_cleanup(
    json_read_dict(In, Dicts, [value_string_as(atom)]),
    close(In)
  ),
  member(Dict, Dicts),
  atom_phrase(version(Version), Dict.tag_name).



%! github_version_latest(+User:atom, +Repo:atom,
%!                       -LatestVersion:compound) is det.

github_version_latest(User, Repo, LatestVersion) :-
  aggregate_all(set(Version), github_version(User, Repo, Version), Versions),
  predsort(compare_version, Versions, SortedVersions),
  last(SortedVersions, LatestVersion).





% HELPERS %

%! github_open(+Segments:list(atom), +Options:list(compound),
%!             +Status:between(100,599)) is det.
%! github_open(+Segments:list(atom), +Options:list(compound),
%!             +Status:between(100,599), -In:stream) is det.

github_open(Segments, Options, Status) :-
  github_open(Segments, Options, Status, In),
  read_stream_to_codes(In, Codes),
  debug(ppm(github), "~s", [Codes]).


github_open(Segments, Options1, Status, In) :-
  atomic_list_concat([''|Segments], /, Path),
  uri_components(Uri, uri_components(https,'api.github.com',Path,_,_)),
  merge_options(
    [
      headers(Headers),
      request_header('Accept'='application/vnd.github.v3+json'),
      status_code(Status)
    ],
    Options1,
    Options2
  ),
  http_open(Uri, In, Options2),
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



%! github_open_authorized(+Segments:list(atom), +Options:list(compound),
%!                        +Status:between(100,599)) is det.
%! github_open_authorized(+Segments:list(atom), +Options:list(compound),
%!                        +Status:between(100,599), -In:stream) is det.

github_open_authorized(Segments, Options1, Status) :-
  github_open_authorized_options(Options1, Options2),
  github_open(Segments, Options2, Status).


github_open_authorized(Segments, Options1, Status, In) :-
  github_open_authorized_options(Options1, Options2),
  github_open(Segments, Options2, Status, In).

github_open_authorized_options(Options1, Options2) :-
  ansi_format([fg(yellow)], "Github user name: "),
  read_line_to_codes(user_input, Codes1),
  ansi_format([fg(yellow)], "Github password: "),
  read_line_to_codes(user_input, Codes2),
  maplist(atom_codes, [User,Password], [Codes1,Codes2]),
  merge_options([authorization(basic(User,Password))], Options1, Options2).
