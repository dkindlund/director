{application, director_console,
 [{description, "director_console used to manage director_nodes"},
  {vsn, "0.01"},
  {modules, [
    director_console,
    director_console_app,
    director_console_sup,
    director_console_web,
    director_console_deps
  ]},
  {registered, []},
  {mod, {director_console_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
