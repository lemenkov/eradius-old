%% -*- mode: Erlang; -*-

{application, eradius,
 [
  {description, "RADIUS authentication/accounting library"},
  {vsn, "0.5"},
  {id, "Eradius"},
  {modules, [
             eradius,
             eradius_acc,
             eradius_dict,
             eradius_lib,
             eradius_server
            ]},
  {registered, [
                eradius,
                eradius_acc,
                eradius_dict
               ]},
  {applications, [
                  kernel,
                  stdlib
                 ]},
  {env, []}
 ]
}.
