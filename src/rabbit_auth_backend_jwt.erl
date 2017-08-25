%% The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License
%% at http://www.mozilla.org/MPL/
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and
%% limitations under the License.
%%
%% The Original Code is RabbitMQ HTTP authentication.
%%
%% The Initial Developer of the Original Code is VMware, Inc.
%% Copyright (c) 2007-2017 Pivotal Software, Inc.  All rights reserved.
%%

-module(rabbit_auth_backend_jwt).

-include_lib("rabbit_common/include/rabbit.hrl").

-behaviour(rabbit_authn_backend).
-behaviour(rabbit_authz_backend).

-export([description/0, tkn/0]).

-export([user_login_authentication/2, user_login_authorization/1,
         check_vhost_access/3, check_resource_access/3, check_topic_access/4]).

tkn() ->
  <<"eyJ0eXAiOiJKV1QiLCJhbGciOiJSUzI1NiJ9.eyJzdWIiOiJhZG1pbkBhZG1pbi5jb20iLCJpYXQiOjE1MDM2NzY1OTksImp0aSI6IjY5YWZkYmI3LWEwNmItNDlmNC1iOThlLTM0NDljMTNlMzk3ZCIsImlzcyI6Ii8vMTkyLjE2OC4yOS4xNjU6MzAwMCIsImV4cCI6MTUwNzEzMjU5OSwibXF0dCI6IjE5Mi4xNjguMjkuMTY1Iiwib3NfdXBkYXRlX3NlcnZlciI6Imh0dHBzOi8vYXBpLmdpdGh1Yi5jb20vcmVwb3MvZmFybWJvdC9mYXJtYm90X29zL3JlbGVhc2VzL2xhdGVzdCIsImZ3X3VwZGF0ZV9zZXJ2ZXIiOiJodHRwczovL2FwaS5naXRodWIuY29tL3JlcG9zL0Zhcm1ib3QvZmFybWJvdC1hcmR1aW5vLWZpcm13YXJlL3JlbGVhc2VzL2xhdGVzdCIsImJvdCI6ImRldmljZV8yIn0.MO3IQRKIWFB0BkuzQ7_iEKjxb0n2JBAjOK4OomlI2bsP1gakncYRvbMtnRTnhjAhv5ikBuEc26qkFYRpj5DEmNfR-SR86tSAr41Aw9sBA64rqeQqXTDKNmgG5G5dv2U52jJX7IuONnzqSJ0izPg_n8B_tH8-U4VGq8_ZVgQTEzeOZ5BsBoSgCGDhBeiTdFtkfAwEICrxgiv0ct7vslYMDU52HBDRuVXURFGUIYqSR5Hl7a2e6aTbItNl4BX0TaXB8ACYldEiXg_rFwKE2n5PJxfuF9IlmSMv_b1q6MEUb4BmvOplGw5VPdYt7mE6bLFPP6xsS5K_EieZfSMXqV0Jfg">>.

%%--------------------------------------------------------------------

description() ->
    [{name, <<"JWT">>},
     {description, <<"JWT authentication / authorisation">>}].

%%--------------------------------------------------------------------

%% Decide if the user gave us a valid JWT.
user_login_authentication(Username, AuthProps) ->
  % Step 1: Validate that the JWT is is real.
  io:fwrite("auth: ~s ~w", [Username, AuthProps]),
  {error, not_implemented}.

user_login_authorization(Username) ->
  io:fwrite("authz: ~s", [Username]),
  case user_login_authentication(Username, []) of
      {ok, #auth_user{impl = Impl}} -> {ok, Impl};
      Else                          -> Else
  end.

check_vhost_access(AuthUser, Vhost, _Sock) ->
  io:fwrite("vhost: ~w ~s", [AuthUser, Vhost ]),
  {error, not_implemented}.

check_resource_access(AuthUser, Resource, Permission) ->
  io:fwrite("resource access: ~w ~w ~w", [AuthUser, Resource, Permission]),
  {error, not_implemented}.

check_topic_access(AuthUser, Resource, Permission, Context) ->
  io:fwrite("topic access: ~w ~w ~w ~w", [AuthUser, Resource, Permission, Context]),
  {error, not_implemented}.


%%--------------------------------------------------------------------
