implementation module Crypto.SSL

// Compile with -l -lcrypto -l -lssl
// https://wiki.openssl.org/index.php/SSL/TLS_Client

import StdBool
import StdInt
import StdList
import StdString
import StdTuple

from Data.Func import $
import Internet.HTTP
import System._Pointer
import System.FilePath

:: SSLMethod :== Pointer
:: SSLCTX :== Pointer
:: BIO :== Pointer

Start w = initialise w
import StdDebug

HOST :== "www.random.org"
PORT :== 443

initialise :: *World -> *(String, *World)
initialise w
#! w        = setup w
#! (meth,w) = SSLv23_method w
| meth == 0 = ("Method was 0", w)
#! (ctx,w)  = SSL_CTX_new meth w
| meth == 0 = ("CTX was 0", w)
#! w        = SSL_CTX_set_verify ctx SSL_VERIFY_PEER w
#! w        = SSL_CTX_set_verify_depth ctx 4 w
#! w        = SSL_CTX_set_options ctx [SSL_OP_NO_SSLv2, SSL_OP_NO_SSLv3, SSL_OP_NO_COMPRESSION] w
#! (res,w)  = SSL_CTX_load_verify_locations_file ctx "/etc/ssl/certs/ca-certificates.crt" w
| res <> 1  = ("LV Res was not 1", w)
#! (web,w)  = BIO_new_ssl_connect ctx w
| web == 0  = ("BIO was 0", w)
#! (res,w)  = BIO_set_conn_hostname web HOST PORT w
| res <> 1  = ("CH Res was not 1", w)
#! (ssl,w)  = BIO_get_ssl web w
| ssl == 0  = ("SSL was 0", w)
#! (res,w)  = SSL_set_cipher_list ssl "HIGH:!aNULL:!kRSA:!PSK:!SRP:!MD5:!RC4" w
| res <> 1  = ("CL Res was not 1", w)
#! (res,w)  = SSL_set_tlsext_host_name ssl HOST w
| res <> 1  = ("TH Res was not 1", w)
#! (res,w)  = BIO_do_connect web w
| res <> 1  = ("DC was not 1", w)
#! (res,w)  = BIO_do_handshake web w
| res <> 1  = ("DH was not 1", w)
#! w        = BIO_puts web (toString req) w
#! (resp,w) = BIO_read_all web w
= (resp, w)
where
	setup :: !*World -> *World
	setup w = code {
		ccall SSL_library_init ":V:A"
		ccall SSL_load_error_strings ":V:A"
		pushI 0
		ccall OPENSSL_config "p:V:A"
	}

	req = { newHTTPRequest
		& req_path = "/cgi-bin/randbyte?nbytes=32&format=h"
		, server_name = HOST
		, server_port = PORT
		}

SSLv23_method :: !*World -> *(!SSLMethod, !*World)
SSLv23_method w = code {
	ccall SSLv23_method ":p:A"
}

SSL_CTX_new :: !SSLMethod !*World -> *(!SSLCTX, !*World)
SSL_CTX_new m w = code {
	ccall SSL_CTX_new "p:p:A"
}

SSL_CTX_set_verify :: !SSLCTX !Int !*World -> *World
SSL_CTX_set_verify ctx mode w = set_verify ctx mode 0 w
where
	set_verify :: !SSLCTX !Int !Pointer !*World -> *World
	set_verify ctx mode callback w = code {
		ccall SSL_CTX_set_verify "pII:V:A"
	}

SSL_CTX_set_verify_depth :: !SSLCTX !Int !*World -> *World
SSL_CTX_set_verify_depth ctx depth w = code {
	ccall SSL_CTX_set_verify_depth "pI:V:A"
}

SSL_CTX_load_verify_locations_file :: !SSLCTX !FilePath !*World -> *(!Int, !*World)
SSL_CTX_load_verify_locations_file ctx file w
	= load_verify_locations ctx (packString file) 0 w
where
	load_verify_locations :: !SSLCTX !String !Pointer !*World -> *(!Int, !*World)
	load_verify_locations ctx file dir w = code {
		ccall SSL_CTX_load_verify_locations "psp:I:A"
	}

SSL_CTX_set_options :: !SSLCTX [SSLOption] *World -> *World
SSL_CTX_set_options ctx opts w = snd $ SSL_CTX_ctrl ctx SSL_CTRL_OPTIONS (foldr (bitor) 0 opts) 0 w

SSL_CTX_ctrl :: !SSLCTX !Int !Int !Pointer !*World -> *(!Int, !*World)
SSL_CTX_ctrl ctx cmd larg parg w = code {
	ccall SSL_CTX_ctrl "pIIp:I:A"
}

SSL_set_cipher_list :: !Pointer !String !*World -> *(!Int, !*World)
SSL_set_cipher_list ssl list w = set_cipher_list ssl (packString list) w
where
	set_cipher_list :: !Pointer !String !*World -> *(!Int, !*World)
	set_cipher_list ssl list w = code {
		ccall SSL_set_cipher_list "ps:I:A"
	}

SSL_set_tlsext_host_name :: !Pointer !String !*World -> *(!Int, !*World)
SSL_set_tlsext_host_name ssl host w
	= SSL_ctrl_string ssl SSL_CTRL_SET_TLSEXT_HOSTNAME TLSEXT_NAMETYPE_host_name host w
where TLSEXT_NAMETYPE_host_name = 0

SSL_ctrl_string :: !Pointer !Int !Int !String !*World -> *(!Int, !*World)
SSL_ctrl_string ssl cmd larg parg w = call ssl cmd larg (packString parg) w
where
	call :: !Pointer !Int !Int !String !*World -> *(!Int, !*World)
	call ssl cmd larg parg w = code {
		ccall SSL_ctrl "pIIs:I:A"
	}

BIO_new_ssl_connect :: !SSLCTX !*World -> *(!BIO, *World)
BIO_new_ssl_connect ctx w = code {
	ccall BIO_new_ssl_connect "p:p:A"
}

BIO_set_conn_hostname :: !BIO !String !Int !*World -> *(!Int, !*World)
BIO_set_conn_hostname bio host port w
	= BIO_ctrl_string bio BIO_C_SET_CONNECT 0 (host +++ ":" +++ toString port) w

BIO_get_ssl :: !BIO !*World -> *(!Pointer, !*World)
BIO_get_ssl bio w = ctrl bio BIO_C_GET_SSL 0 w
where
	ctrl :: !BIO !Int !Int !*World -> *(!Pointer, !*World)
	ctrl bio cmd arg w = code {
		ccall BIO_ctrl "pII:VI:A"
	}

BIO_do_connect :== BIO_do_handshake
BIO_do_handshake :: !BIO !*World -> *(!Int, !*World)
BIO_do_handshake bio w = BIO_ctrl_ptr bio BIO_C_DO_STATE_MACHINE 0 0 w

BIO_ctrl_ptr :: !BIO !Int !Int !Pointer !*World -> *(!Int, !*World)
BIO_ctrl_ptr bio cmd larg parg w = code {
	ccall BIO_ctrl "pIIp:I:A"
}

BIO_ctrl_string :: !BIO !Int !Int !String !*World -> *(!Int, !*World)
BIO_ctrl_string bio cmd larg parg w = call bio cmd larg (packString parg) w
where
	call :: !BIO !Int !Int !String !*World -> *(!Int, !*World)
	call bio cmd larg parg w = code {
		ccall BIO_ctrl "pIIs:I:A"
	}

BIO_puts :: !BIO !String !*World -> *World
BIO_puts bio s w = puts bio (packString s) w
where
	puts :: !BIO !String !*World -> *World
	puts bio s w = code {
		ccall BIO_puts "ps:V:A"
	}

BIO_read_all :: !BIO !*World -> *(!String, !*World)
BIO_read_all bio w
#! (p,w) = malloc READ_SIZE w
#! (s,w) = read_all bio p w
// TODO should free
= (s,w)
where
	READ_SIZE = 1536

	read_all :: !BIO !Pointer !*World -> *(!String, !*World)
	read_all bio p w
	#! (n,w) = BIO_read bio p READ_SIZE w
	#! (r,w) = BIO_test_flags bio BIO_FLAGS_SHOULD_RETRY w
	#! (s,w) = (derefString p,w)
	#! s     = s % (0, n-1)
	| n <= 0 && not r = (s, w)
	#! (s2,w) = read_all bio p w
	= (s +++ s2, w)

BIO_read :: !BIO !Pointer !Int !*World -> *(!Int, !*World)
BIO_read bio p n w = code {
	ccall BIO_read "ppI:I:A"
}

BIO_test_flags :: !BIO !Int !*World -> *(!Bool, !*World)
BIO_test_flags bio f w = code {
	ccall BIO_test_flags "pI:I:A"
}

malloc :: !Int !*World -> *(!Pointer, !*World)
malloc n w = code {
	ccall malloc "I:p:A"
}
