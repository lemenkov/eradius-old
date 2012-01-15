-module(eradius_lib).
%%%-------------------------------------------------------------------
%%% File        : eradius_lib.erl
%%% Author      : Martin Bjorklund <mbj@bluetail.com>
%%% Description : Radius encode/decode routines (RFC-2865).
%%% Created     :  7 Oct 2002 by Martin Bjorklund <mbj@bluetail.com>
%%%-------------------------------------------------------------------
-export([enc_pdu/1, enc_reply_pdu/2, dec_packet/1, enc_accreq/3]).
-export([mk_authenticator/0, mk_password/3]).

% Quick hack for EUnit
-export([type_conv/2]).
-export([dec_attr_val/2]).

-include("eradius_lib.hrl").
-include("eradius_dict.hrl").
-include("dictionary_freeradius_internal.hrl").
-include("dictionary_rfc2865.hrl").
-include("dictionary_rfc2866.hrl").

%%====================================================================
%% Create Attributes
%%====================================================================

%%% Generate an unpredictable 16 byte token.   FIXME !!
mk_authenticator() ->
    {_, A2, A3} = now(),
    A = erlang:phash(node(), A2),
    B = erlang:phash(A2, A3),
    erlang:md5(<<A3, A2, A, B>>).

mk_password(Secret, Auth, Passwd) ->
    scramble(Secret, Auth, Passwd).

scramble(Secret, Auth, Passwd) ->
    B = erlang:md5([Secret, Auth]),
    case xor16(Passwd, B) of
	{C, <<>>}   -> C;
	{C, Tail} -> list_to_binary([C, scramble(Secret, C, Tail)])
    end.

xor16(Passwd, B) when size(Passwd) < 16 ->
    xor16(list_to_binary([Passwd, list_to_binary(zero(16 - size(Passwd)))]), B);
xor16(<<P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16,T/binary>>,
      <<B1,B2,B3,B4,B5,B6,B7,B8,B9,B10,B11,B12,B13,B14,B15,B16>>) ->
    {<<(P1 bxor B1),
       (P2  bxor B2),
       (P3  bxor B3),
       (P4  bxor B4),
       (P5  bxor B5),
       (P6  bxor B6),
       (P7  bxor B7),
       (P8  bxor B8),
       (P9  bxor B9),
       (P10 bxor B10),
       (P11 bxor B11),
       (P12 bxor B12),
       (P13 bxor B13),
       (P14 bxor B14),
       (P15 bxor B15),
       (P16 bxor B16)>>,
     T}.

zero(0) -> [];
zero(N) -> [0 | zero(N-1)].

%%====================================================================
%% Encode/Decode Functions
%%====================================================================

%% Ret: io_list(). Specific format of io_list is relied on by
%% enc_reply_pdu/1
enc_pdu(Pdu) ->
    {Cmd, CmdPdu} = enc_cmd(Pdu#rad_pdu.cmd),
    [<<Cmd:8, (Pdu#rad_pdu.reqid):8, (io_list_len(CmdPdu) + 20):16>>,
     <<(Pdu#rad_pdu.authenticator):16/binary>>,
     CmdPdu].

%% This one includes the authenticator substitution required for
%% sending replies from the server.
enc_reply_pdu(Pdu, Secret) ->
    [Head, Auth, Cmd] = enc_pdu(Pdu),
    Reply_auth = erlang:md5([Head, Auth, Cmd, Secret]),
    [Head, Reply_auth, Cmd].

enc_attrib(Pos, R, Def, AttrName, Type) ->
    V = element(Pos, R),
    if  V == element(Pos, Def) ->
	    [];
	true ->
	    enc_attrib(AttrName, V, Type)
    end.

enc_attrib({_Vendor, Id}, V, Type) ->
    Val = type_conv(V, Type),
    <<Id, (size(Val) + 2):8, Val/binary>>;

enc_attrib(Id, V, Type) ->
    Val = type_conv(V, Type),
    <<Id, (size(Val) + 2):8, Val/binary>>.

% Ascend's binary filter format.
type_conv(V, abinary) when is_list(V) -> iolist_to_binary(V);
type_conv(V, abinary) when is_binary(V) -> V;
% 8 bit unsigned integer
type_conv(V, byte) ->  <<V:8>>;
% if length 4, is the same as the "ipaddr" type. if length 16, is the same as "ipv6addr" type.
type_conv(V = {A,B,C,D}, comboip) -> <<A:8, B:8, C:8, D:8>>;
type_conv(V = {A,B,C,D,E,F,G,H}, comboip) ->
	<<A:16, B:16, C:16, D:16, E:16, F:16, G:16, H:16>>;
% 32 bit value in big endian order - seconds since 00:00:00 GMT, Jan. 1, 1970
type_conv(V, date) when is_integer(V) -> <<V:32>>;
type_conv(V = {Mega, Secs, _}, date) ->
	TS = Mega*1000000 + Secs,
	<<TS:32>>;
% 6 octets of hh:hh:hh:hh:hh:hh where 'h' is hex digits, upper or lowercase.
type_conv(V, ether) -> throw ({error, unsupported});
type_conv(V, evs) -> throw ({error, unsupported});
type_conv(V, extended) -> throw ({error, unsupported});
type_conv(V, extendedflags) -> throw ({error, unsupported});
% 8 octets in network byte order
type_conv(V, ifid) -> throw ({error, unsupported});
% 32 bit value in big endian order (high byte first)
type_conv(V, integer) when is_integer(V) -> <<V:32>>;
% 64 bit value in big endian order (high byte first)
type_conv(V, integer64) when is_integer(V) -> <<V:64>>;
% 4 octets in network byte order
type_conv(V = {A,B,C,D}, ipaddr) -> <<A:8, B:8, C:8, D:8>>;
% 16 octets in network byte order
type_conv(V = {A,B,C,D,E,F,G,H}, ipv6addr) ->
	<<A:16, B:16, C:16, D:16, E:16, F:16, G:16, H:16>>;
% 18 octets in network byte order
type_conv(V, ipv6prefix) -> throw ({error, unsupported});
% raw octets, printed and input as hex strings. e.g.: 0x123456789abcdef
type_conv(V, octets) when is_list(V) -> iolist_to_binary(V);
type_conv(V, octets) when is_binary(V) -> V;
% 16-bit unsigned integer
type_conv(V, short) when is_integer(V), 0 =< V, V =< 65535 -> <<V:16>>;
% 4-octet signed integer in network byte order
type_conv(V, signed) when is_integer(V), -2147483648 =< V, V < 2147483648 -> <<V:32>>;
% 0-253 octets
type_conv(V, string) when is_list(V) -> iolist_to_binary(V);
type_conv(V, string) when is_binary(V) -> V;
% printable, generally UTF-8 encoded (subset of 'string')
type_conv(V, text) -> throw ({error, unsupported});
% type-length-value (see dictionary.wimax)
type_conv(V, tlv) -> throw ({error, unsupported});
type_conv(V, _) -> throw ({error, unsupported}).


enc_cmd(R) when is_record(R, rad_request) ->
    Def = #rad_request{},
    {?Val_Packet_Type_Access_Request,
     [enc_attrib(#rad_request.user,      R, Def, ?User_Name,       string),
      enc_attrib(#rad_request.passwd,    R, Def, ?User_Password,     string),
      enc_attrib(#rad_request.nas_ip,    R, Def, ?NAS_IP_Address,  ipaddr),
      enc_attrib(#rad_request.state,     R, Def, ?State,           octets)
     ]};
enc_cmd(R) when is_record(R, rad_accept) ->
    Def = #rad_accept{},
    {?Val_Packet_Type_Access_Accept,
     [enc_attrib(#rad_accept.user,        R, Def, ?User_Name,       string),
      lists:map(fun(RM) ->
			<<?Vendor_Specific:8, (size(RM)+2):8, RM/binary>>
		end,
		R#rad_accept.vendor_specifics)]
    };
enc_cmd(R) when is_record(R, rad_challenge) ->
    Def = #rad_challenge{},
    {?Val_Packet_Type_Access_Challenge,
     [enc_attrib(#rad_challenge.state,   R, Def, ?State,           octets),
      lists:map(fun(RM) -> <<?Reply_Message:8, (size(RM)+2):8, RM/binary>> end,
		R#rad_challenge.reply_msgs)]
    };
enc_cmd(R) when is_record(R, rad_reject) ->
    {?Val_Packet_Type_Access_Reject,
     lists:map(fun(RM) -> <<?Reply_Message:8, (size(RM)+2):8, RM/binary>> end,
	       R#rad_reject.reply_msgs)
    };
enc_cmd(R) when is_record(R, rad_accreq) ->
    Def = #rad_accreq{},
    {?Val_Packet_Type_Accounting_Request,
     [enc_attrib(#rad_accreq.status_type, R, Def, ?Acct_Status_Type,     integer),
      enc_attrib(#rad_accreq.session_time,R, Def, ?Acct_Session_Time,    integer),
      enc_attrib(#rad_accreq.session_id,  R, Def, ?Acct_Session_Id,      string),
      enc_attrib(#rad_accreq.term_cause,  R, Def, ?Acct_Terminate_Cause, integer),
      enc_attrib(#rad_accreq.user,        R, Def, ?User_Name,       string),
      enc_attrib(#rad_accreq.nas_ip,      R, Def, ?NAS_IP_Address,  ipaddr),
      enc_std_attrs(R),
      enc_vendor_attrs(R)
     ]};
enc_cmd(R) when is_record(R, rad_statusreq) ->
	{?Val_Packet_Type_Access_Accept,
		[]
	}.

enc_std_attrs(R) ->
    enc_attributes(R#rad_accreq.std_attrs).

enc_vendor_attrs(R) ->
    F = fun({Vid, Vs}, Acc) ->
		Vbins = enc_attributes(Vs),
		Size = io_list_len(Vbins),
		Tsz = Size + 6,
		[[<<?Vendor_Specific:8, Tsz:8, Vid:32 >> | Vbins] | Acc]
    end,
    lists:foldl(F, [], R#rad_accreq.vend_attrs).

enc_attributes(As) ->
    F = fun({Id, Val}, Acc) ->
		case eradius_dict:lookup(Id) of
		    [A] when is_record(A, attribute) ->
			[enc_attrib(Id, Val, A#attribute.type) | Acc];
		    _ ->
			Acc
		end
	end,
    lists:foldl(F, [], As).


io_list_len(L) -> io_list_len(L, 0).
io_list_len([H|T], N) ->
    if
	H >= 0, H =< 255 -> io_list_len(T, N+1);
	is_list(H) -> io_list_len(T, io_list_len(H,N));
	is_binary(H) -> io_list_len(T, size(H) + N)
    end;
io_list_len(H, N) when is_binary(H) ->
    size(H) + N;
io_list_len([], N) ->
    N.

%% Ret: #rad_pdu | Reason
dec_packet(Packet) ->
    case catch dec_packet0(Packet) of
	{'EXIT', _R} ->
	    io:format("_R = ~p~n",[_R]),
	    bad_pdu;
	Else ->
	    Else
    end.

dec_packet0(Packet) ->
    <<Cmd:8, ReqId:8, Len:16, Auth:16/binary, Attribs0/binary>> = Packet,
    Size = size(Attribs0),
    Attr_len = Len - 20,
    Attribs =
	if
	    Attr_len > Size ->
		throw(bad_pdu);
	    Attr_len == Size ->
		Attribs0;
	    true ->
		<<Attribs1:Attr_len/binary, _/binary>> = Attribs0,
		Attribs1
	end,
    P = #rad_pdu{reqid = ReqId, authenticator = Auth},
    case Cmd of
	?Val_Packet_Type_Access_Request ->
	    P#rad_pdu{cmd = {request, dec_attributes(Attribs)}};
	?Val_Packet_Type_Access_Accept ->
	    P#rad_pdu{cmd = {accept, dec_attributes(Attribs)}};
	?Val_Packet_Type_Access_Challenge ->
	    P#rad_pdu{cmd = {challenge, dec_attributes(Attribs)}};
	?Val_Packet_Type_Access_Reject ->
	    P#rad_pdu{cmd = {reject, dec_attributes(Attribs)}};
	?Val_Packet_Type_Accounting_Request ->
	    P#rad_pdu{cmd = {accreq, dec_attributes(Attribs)}};
	?Val_Packet_Type_Accounting_Response ->
	    P#rad_pdu{cmd = {accresp, dec_attributes(Attribs)}};
	?Val_Packet_Type_Status_Server ->
		P#rad_pdu{cmd = {statusreq, dec_attributes(Attribs)}}
    end.

-define(dec_attrib(A0, Type, Val, A1),
	<<Type:8, __Len0:8, __R/binary>> = A0,
	__Len1 = __Len0 - 2,
	<<Val:__Len1/binary, A1/binary>> = __R).


dec_attributes(As) ->
    dec_attributes(As, []).

dec_attributes(<<>>, Acc) -> Acc;
dec_attributes(A0, Acc) ->
    ?dec_attrib(A0, Type, Val, A1),
    case eradius_dict:lookup(Type) of
	[A] when is_record(A, attribute) ->
	    dec_attributes(A1, dec_attr_val(A,Val) ++ Acc);
	_ ->
	    dec_attributes(A1, [{Type, Val} | Acc])
    end.

dec_attr_val(A, Bin) when A#attribute.type == abinary ->
    [{A, Bin}];
dec_attr_val(A, Bin = <<Val:8>>) when A#attribute.type == byte ->
    [{A, Val}];
dec_attr_val(A, Bin = <<I0:8,I1:8,I2:8,I3:8>>) when A#attribute.type == comboip ->
    [{A, {I0,I1,I2,I3}}];
dec_attr_val(A, Bin = <<I0:16,I1:16,I2:16,I3:16,I4:16,I5:16,I6:16,I7:16>>) when A#attribute.type == comboip ->
    [{A, {I0,I1,I2,I3,I4,I5,I6,I7}}];
dec_attr_val(A, Bin = <<Val:32>>) when A#attribute.type == date ->
    Mega = Val div 1000000,
    Sec = Val rem 1000000,
    [{A, {Mega, Sec, 0}}];
dec_attr_val(A, I0) when A#attribute.type == integer ->
    L = size(I0)*8,
    case I0 of
        <<I:L/integer>> ->
            [{A, I}];
        _ ->
            [{A, I0}]
    end;
dec_attr_val(A, Bin = <<I:64>>) when A#attribute.type == integer64 ->
    [{A, I}];
dec_attr_val(A, <<B,C,D,E>>) when A#attribute.type == ipaddr ->
    [{A, {B,C,D,E}}];
dec_attr_val(A, Bin = <<I0:16,I1:16,I2:16,I3:16,I4:16,I5:16,I6:16,I7:16>>) when A#attribute.type == ipv6addr ->
    [{A, {I0,I1,I2,I3,I4,I5,I6,I7}}];
dec_attr_val(A, Bin) when A#attribute.type == octets ->
    case A#attribute.id of
	?Vendor_Specific ->
	    <<VendId:32/integer, VendVal/binary>> = Bin,
	    dec_vend_attr_val(VendId, VendVal);
	_ ->
	    [{A, Bin}]
    end;
dec_attr_val(A, Bin = <<S:16>>) when A#attribute.type == short ->
    [{A, S}];
dec_attr_val(A, Bin = <<S:32/signed>>) when A#attribute.type == signed ->
    [{A, S}];
dec_attr_val(A, Bin) when A#attribute.type == string ->
    [{A, binary_to_list(Bin)}];
dec_attr_val(A, Val) ->
    io:format("Uups...A=~p~n",[A]),
    [{A, Val}].

dec_vend_attr_val(_VendId, <<>>) -> [];
dec_vend_attr_val(VendId, <<Vtype:8, Vlen:8, Vbin/binary>>) ->
    Len = Vlen - 2,
    <<Vval:Len/binary,Vrest/binary>> = Vbin,
    Vkey = {VendId,Vtype},
    case eradius_dict:lookup(Vkey) of
	[A] when is_record(A, attribute) ->
	    dec_attr_val(A, Vval) ++ dec_vend_attr_val(VendId, Vrest);
	_ ->
	    [{Vkey,Vval} | dec_vend_attr_val(VendId, Vrest)]
    end.

%%% ====================================================================
%%% Radius Accounting specifics
%%% ====================================================================

enc_accreq(Id, Secret, Req) ->
    Rpdu = #rad_pdu{reqid = Id,
		    %%% An empty Acc-Req Authenticator
		    authenticator = <<0:16/?BYTE>>,
		    cmd = Req},
    PDU = enc_pdu(Rpdu),
    patch_authenticator(PDU, l2b(Secret)).

patch_authenticator(Req,Secret) ->
    case {erlang:md5([Req,Secret]),list_to_binary(Req)} of
	{Auth,<<Head:4/binary, _:16/binary, Rest/binary>>} ->
	    B = l2b(Auth),
	    <<Head/binary, B/binary, Rest/binary>>;
	_Urk ->
	    exit(patch_authenticator)
    end.

l2b(L) when is_list(L)   -> list_to_binary(L);
l2b(B) when is_binary(B) -> B.
