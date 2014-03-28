{application, blah,
 [
  {description, "The blah application. Very important. Much requirements."},
  {vsn, "2014.1"},
  {modules,
   [
    blah_app,
    blah_sup,
    blah,
    blaher
   ]
  },
  {registered, []},
  {applications, [kernel, stdlib]},
  {mod, {blah_app, []}},
  {env, []}
 ]
}.
