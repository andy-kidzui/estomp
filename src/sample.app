{application, sample,
 [{description, "sample"},
  {vsn, "0.01"},
  {modules, [
    sample,
    sample_app,
    sample_supervisor,
    sample_gateway,
    stomp_supervisor,
    stomp_client
  ]},
  {registered, []},
  {mod, {sample_app, []}},
  {env, []},
  {applications, [kernel, stdlib]}]}.
