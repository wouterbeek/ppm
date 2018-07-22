:- module(
  ppm_foreign,
  [
    ppm_make/2 % +User, +Repo
  ]
).

/** <module> Prolog Package Manager (PPM): Build foreign libraries

@author Wouter Beek
@version 2018
*/

:- use_module(library(filesex)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(process)).
:- use_module(library(prolog_pack), []).

:- use_module(library(ppm_generic)).





%! ppm_make(+User:atom, +Repo:atom) is det.

ppm_make(User, Repo) :-
  repository_directory(User, Repo, Dir),
  make(Dir).

make(Dir) :-
  catch(
    run_make(Dir, [distclean]),
    E,
    print_message(warning, E)
  ),
  post_install_foreign(Dir).

run_make(Dir, Targets) :-
  directory_file_path(Dir, 'Makefile', File),
  exists_file(File), !,
  prolog_pack:build_environment(Env),
  Options = [directory(Dir),env(Env)],
  forall(
    member(Target, Targets),
    prolog_pack:run_process(path(make), [Target], Options)
  ).
run_make(_, _).

post_install_foreign(Dir) :-
  prolog_pack:setup_path,
  prolog_pack:save_build_environment(Dir),
  prolog_pack:configure_foreign(Dir, []),
  prolog_pack:make_foreign(Dir, []).
