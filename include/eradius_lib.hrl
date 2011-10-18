-ifndef(_ERADIUS_LIB).
-define(_ERADIUS_LIB , true).
%%%-------------------------------------------------------------------
%%% File        : oaml_radius_lib.hrl
%%% Author      : Martin Bjorklund <mbj@bluetail.com>
%%% Description : Definitions for RADIUS
%%% Created     :  7 Oct 2002 by Martin Bjorklund <mbj@bluetail.com>
%%%-------------------------------------------------------------------

-define(BYTE, integer-unit:8).    % Nice syntactic sugar...

%%- Set radius accounting attributes
-define(ACC_ATTR(Key,Val), {Key,Val}).

%% In a server we need to store properties of a NAS. This is used as
%% an mnesia record as well as in call processing.
-record(nas_prop, {
          ip,
          secret,
          mf,
          trace = false
         }).

%%- Radius accounting server info.
-record( radacct , {
	   servers,      % a list of [Ip,Port,Secret] tripplets
	   sockopts = [],% list of extra socket options
	   timeout}).    % timeout in seconds

-record(rad_pdu, {
	  reqid,
	  authenticator,
	  cmd              % one of the records below
	 }).

-record(rad_request, {
	  user,
	  passwd = <<>>,
	  nas_ip,
	  state = <<>>
	 }).

-record(rad_accept, {
	  user,
	  vendor_specifics = [],
	  attribs = [],
	  session_timeout = 0
	 }).

-record(rad_challenge, {
	  state,
	  reply_msgs = [], % list of binaries
	  session_timeout = 0
	 }).

-record(rad_reject, {
	  reply_msgs = []  % list of binaries
	 }).

-record(rad_accreq, {      % accounting request
	  status_type,
	  session_time,
	  login_time,      % erlang:now/0
	  logout_time,     % erlang:now/0
	  session_id,
	  vend_id = 1872,  % Alteon
	  vend_attrs = [], % list_of( {VendorId, list_of( {Id, Val} ) } )
	  std_attrs  = [], % list_of( {Id, Val} )
	  user,
	  nas_ip,
	  sockopts = [],   % list of extra socket options
	  servers,         % overrides the #radacct{} content
	  timeout = 5000,  % -- "" --  , default is 5 seconds
	  term_cause}).

-record(rad_accresp, {}).  % accounting response

-record(rad_statusreq, {}).  % status request

-endif.
