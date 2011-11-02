-module(eradius_lib_test).

-include("eradius_lib.hrl").
-include("eradius_dict.hrl").
-include("dictionary_freeradius_internal.hrl").
-include("dictionary_cisco.hrl").
-include("dictionary_rfc2866.hrl").
-include_lib("eunit/include/eunit.hrl").

vendor_specific_test() ->
	RadAcctServers = [[{127,0,0,1},1813,"testradacctpass"]],
	LoginTime = {1320,65461,569808}, % Taken from erlang:now()
	CallID = "testid@example.com",
	H323_Connect_Time = "2011-10-31 16:53:03",
	PacketId = 1,
	SharedSecret = "SharedSecret",

	Length = 73,
	Authenticator = <<51,38,146,124,116,92,62,238,101,253,174,229,47,15,22,71>>,
	Payload0 = <<40,6,0,0,0,1>>,
	Payload1 = <<44,20,116,101,115,116,105,100,64,101,120,97,109,112,108,101,46,99,111,109>>,
	Payload2 = <<26,27,0,0,0,9,28,21,50,48,49,49,45,49,48,45,51,49,32,49,54,58,53,51,58,48,51>>,

	{ok, _Pid} = eradius_dict:start(),
	ok = eradius_dict:load_tables(["dictionary_cisco", "dictionary_rfc2865", "dictionary_rfc2866"]),

	StartReq = #rad_accreq{
		servers = RadAcctServers,
		status_type = ?Val_Acct_Status_Type_Start,
		login_time =  LoginTime,
		std_attrs = [{?Acct_Session_Id, CallID}],
		vend_attrs = [{?Cisco, [{?h323_connect_time, H323_Connect_Time}]}]
	},

	StartReqBin = eradius_lib:enc_accreq(PacketId, SharedSecret, StartReq),

	?assertEqual(
		<<?Val_Packet_Type_Accounting_Request:8, PacketId:8, Length:16,
		Authenticator:16/binary,
		Payload0:6/binary, Payload1:20/binary, Payload2:27/binary>>,
		StartReqBin).

type_conv_test_() ->
	[
		{"Encoding to type abinary (from binary)",
			fun() -> ?assertEqual(<<"hello">>, eradius_lib:type_conv(<<"hello">>, abinary)) end
		},
		{"Encoding to type abinary (from text)",
			fun() -> ?assertEqual(<<"hello">>, eradius_lib:type_conv("hello", abinary)) end
		},
		{"Encoding to type byte",
			fun() -> ?assertEqual(<<42:8>>, eradius_lib:type_conv(42, byte)) end
		},
		{"Encoding to type comboip (from IPv4)",
			fun() -> ?assertEqual(<<127:8, 0:8, 0:8, 1:8>>, eradius_lib:type_conv({127,0,0,1}, comboip)) end
		},
		{"Encoding to type comboip (from IPv6)",
			fun() -> ?assertEqual(<<0:16, 0:16, 0:16, 0:16, 0:16, 0:16, 0:16, 1:16>>, eradius_lib:type_conv({0,0,0,0,0,0,0,1}, comboip)) end
		},
		{"Encoding to type date",
			fun() -> ?assertEqual(<<1320242684:32>>, eradius_lib:type_conv({1320,242684,282846}, date)) end
		},
		{"Encoding to type integer",
			fun() -> ?assertEqual(<<1:32>>, eradius_lib:type_conv(1, integer)) end
		},
		{"Encoding to type integer64",
			fun() -> ?assertEqual(<<1:64>>, eradius_lib:type_conv(1, integer64)) end
		},
		{"Encoding to type ipaddr",
			fun() -> ?assertEqual(<<127:8, 0:8, 0:8, 1:8>>, eradius_lib:type_conv({127,0,0,1}, ipaddr)) end
		},
		{"Encoding to type ipv6addr",
			fun() -> ?assertEqual(<<0:16, 0:16, 0:16, 0:16, 0:16, 0:16, 0:16, 1:16>>, eradius_lib:type_conv({0,0,0,0,0,0,0,1}, ipv6addr)) end
		},
		{"Encoding to type octets (from string)",
			fun() -> ?assertEqual(<<"hello">>, eradius_lib:type_conv("hello", octets)) end
		},
		{"Encoding to type octets (from binary)",
			fun() -> ?assertEqual(<<"hello">>, eradius_lib:type_conv(<<"hello">>, octets)) end
		},
		{"Encoding to type short",
			fun() -> ?assertEqual(<<42:16>>, eradius_lib:type_conv(42, short)) end
		},
		{"Encoding to type signed",
			fun() -> ?assertEqual(<<-42:32>>, eradius_lib:type_conv(-42, signed)) end
		},
		{"Encoding to type string (from string)",
			fun() -> ?assertEqual(<<"hello">>, eradius_lib:type_conv("hello", string)) end
		},
		{"Encoding to type string (from binary)",
			fun() -> ?assertEqual(<<"hello">>, eradius_lib:type_conv(<<"hello">>, string)) end
		}
	].

dec_attr_val_test_() ->
	[
		{"Decoding to type abinary",
			fun() -> ?assertEqual(
					[{#attribute{type = binary}, <<"hello">>}],
					eradius_lib:dec_attr_val(#attribute{type = binary}, <<"hello">>)
				) end
		},
		{"Decoding to type byte",
			fun() -> ?assertEqual(
					[{#attribute{type = byte}, 42}],
					eradius_lib:dec_attr_val(#attribute{type = byte}, <<42:8>>)
				) end
		},
		{"Decoding to type comboip (to IPv4)",
			fun() -> ?assertEqual(
					[{#attribute{type = comboip}, {127,0,0,1}}],
					eradius_lib:dec_attr_val(#attribute{type = comboip}, <<127:8, 0:8, 0:8, 1:8>>)
				) end
		},
		{"Decoding to type comboip (to IPv6)",
			fun() -> ?assertEqual(
					[{#attribute{type = comboip}, {0,0,0,0,0,0,0,1}}],
					eradius_lib:dec_attr_val(#attribute{type = comboip}, <<0:16, 0:16, 0:16, 0:16, 0:16, 0:16, 0:16, 1:16>>)
				) end
		},
		{"Decoding to type date",
			fun() -> ?assertEqual(
					[{#attribute{type = date}, {1320,242684,0}}],
					eradius_lib:dec_attr_val(#attribute{type = date}, <<1320242684:32>>)
				) end
		},
		{"Decoding to type integer",
			fun() -> ?assertEqual(
					[{#attribute{type = integer}, 1}],
					eradius_lib:dec_attr_val(#attribute{type = integer}, <<1:32>>)
				) end
		},
		{"Decoding to type integer64",
			fun() -> ?assertEqual(
					[{#attribute{type = integer64}, 1}],
					eradius_lib:dec_attr_val(#attribute{type = integer64}, <<1:64>>)
				) end
		},
		{"Decoding to type ipaddr",
			fun() -> ?assertEqual(
					[{#attribute{type = ipaddr}, {127,0,0,1}}],
					eradius_lib:dec_attr_val(#attribute{type = ipaddr}, <<127:8, 0:8, 0:8, 1:8>>)
				) end
		},
		{"Decoding to type ipv6addr",
			fun() -> ?assertEqual(
					[{#attribute{type = ipv6addr}, {0,0,0,0,0,0,0,1}}],
					eradius_lib:dec_attr_val(#attribute{type = ipv6addr}, <<0:16, 0:16, 0:16, 0:16, 0:16, 0:16, 0:16, 1:16>>)
				) end
		},
		{"Decoding to type octets",
			fun() -> ?assertEqual(
					[{#attribute{type = octets}, <<"hello">>}],
					eradius_lib:dec_attr_val(#attribute{type =  octets}, <<"hello">>)
				) end
		},
		{"Decoding to type short",
			fun() -> ?assertEqual(
					[{#attribute{type = short}, 42}],
					eradius_lib:dec_attr_val(#attribute{type = short}, <<42:16>>)
				) end
		},
		{"Decoding to type signed",
			fun() -> ?assertEqual(
					[{#attribute{type = signed}, -42}],
					eradius_lib:dec_attr_val(#attribute{type = signed}, <<-42:32>>)
				) end
		},
		{"Decoding to type string",
			fun() -> ?assertEqual(
					[{#attribute{type = string}, "hello"}],
					eradius_lib:dec_attr_val(#attribute{type = string}, <<"hello">>)
				) end
		}
	].
