{application, director,
 [{description, "director"},
  {vsn, "0.01"},
  {modules, [
    director,
    director_app,
    director_node,
    director_sup
  ]},
  {registered, []},
  {mod, {director_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.