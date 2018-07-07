:- module(
  ppm_github,
  [
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
:- use_module(library(readutil)).
:- use_module(library(uri)).

:- use_module(library(ppm_generic)).
%:- use_module(library(ppm_git)).

:- debug(ppm(github)).

:- thread_local
   password/1,
   user/1.





%! github_uri(+User:atom, +Repo:atom, -Uri:atom) is det.

github_uri(User, Repo, Uri) :-
  atomic_list_concat(['',User,Repo], /, Path),
  uri_components(Uri, uri_components(https,'github.com',Path,_,_)).



%! github_version(+User:atom, +Repo:atom, +Version:compound) is semidet.
%! github_version(+User:atom, +Repo:atom, -Version:compound) is nondet.

github_version(User, Repo, Version) :-
  github_open([repos,User,Repo,tags], [], 200, In),
  call_cleanup(
    json_read_dict(In, Dicts, [value_string_as(atom)]),
    close(In)
  ),
  member(Dict, Dicts),
  atom_phrase(version(Version), Dict.name).



%! github_version_latest(+User:atom, +Repo:atom, +Version:compound) is semidet.
%! github_version_latest(+User:atom, +Repo:atom, -Version:compound) is nondet.

github_version_latest(User, Repo, Version) :-
  aggregate_all(set(Version), github_version(User, Repo, Version), Versions),
  predsort(compare_version, Versions, SortedVersions),
  last(SortedVersions, Version).





% HELPERS %

%! github_open(+Segments:list(atom), +Options:list(compound),
%!             +Status:between(100,599), -In:stream) is det.

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
  catch(http_open(Uri, In, Options2), E, true),
  (   var(E)
  ->  (   debugging(http(receive_reply))
      ->  print_http_reply(Status, Headers)
      ;   true
      )
  ;   E = error(permission_error(url,_Uri),context(_,status(403,_)))
  ->  % Unfortunately, library(http/http_open) throws away the reply
      % body for non-2xx replies.  This may be due to Github rate
      % limiting.
      ansi_format([bg(red)], "Github operation forbidden.  Maybe rate limiting?"),
      nl,
      fail
  ).

print_http_reply(Status, Headers) :-
  debug(http(receive_reply), "~a", [Status]),
  maplist(print_http_header, Headers),
  debug(http(receive_reply), "", []).

print_http_header(Header) :-
  Header =.. [Key1,Value],
  atomic_list_concat(L, '_', Key1),
  atomic_list_concat(L, -, Key2),
  debug(http(receive_reply), "< ~a: ~a", [Key2,Value]).
