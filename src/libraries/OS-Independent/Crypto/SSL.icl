implementation module Crypto.SSL

// Compile with -l -lcrypto -l -lssl
// https://wiki.openssl.org/index.php/SSL/TLS_Client

import code from "ssl_help."
import StdBool
import StdInt
import StdList
import StdMisc
import StdString
import StdTuple

from Data.Func import $
import Internet.HTTP
import System._Pointer
import System.FilePath

:: SSLMethod :== Pointer
:: SSLCTX :== Pointer
:: BIO :== Pointer


Start w
# (bio,w)  = initSSL host port w
# w        = BIO_puts bio (toString req) w
# (resp,w) = BIO_read_all bio w
= resp
where
	host = "www.random.org"
	port = 443

	req = { newHTTPRequest
		& req_path = "/cgi-bin/randbyte?nbytes=32&format=h"
		, server_name = host
		, server_port = port
		}

initSSL :: !String !Int !*World -> *(!BIO, !*World)
initSSL host port w
#! (_,w)    = SSL_library_init w
#! (meth,w) = SSLv23_method w
| meth == 0 = abort "Method was 0\n"
#! (ctx,w)  = SSL_CTX_new meth w
| ctx == 0  = abort "CTX was 0\n"
//#! w        = SSL_CTX_set_verify ctx SSL_VERIFY_PEER w
//#! w        = SSL_CTX_set_verify_depth ctx 4 w
#! w        = SSL_CTX_set_options ctx [SSL_OP_NO_SSLv2, SSL_OP_NO_SSLv3, SSL_OP_NO_COMPRESSION] w
#! (res,w)  = SSL_CTX_load_verify_locations_file ctx "/etc/ssl/certs/ca-certificates.crt" w
| res <> 1  = abort "LV Res was not 1\n"
#! (web,w)  = BIO_new_ssl_connect ctx w
| web == 0  = abort "BIO was 0\n"
#! (res,w)  = BIO_set_conn_hostname web host port w
| res <> 1  = abort "CH Res was not 1\n"
#! (ssl,w)  = BIO_get_ssl web w
| ssl == 0  = abort "SSL was 0\n"
#! (res,w)  = SSL_set_cipher_list ssl "HIGH:!aNULL:!kRSA:!PSK:!SRP:!MD5:!RC4" w
| res <> 1  = abort "CL Res was not 1\n"
//#! (res,w)  = SSL_set_tlsext_host_name ssl host w
//| res <> 1  = abort "TH Res was not 1\n"
#! (res,w)  = BIO_do_connect web w
| res <> 1  = abort ("DC was not 1: " +++ toString res +++ "\n")
#! (res,w)  = BIO_do_handshake web w
| res <> 1  = abort "DH was not 1\n"
= (web,w)
where
	SSL_library_init :: !*World -> *(!Int, !*World)
	SSL_library_init w = code {
		ccall SSL_library_init ":I:A"
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
SSL_CTX_set_verify ctx mode w = set_verify ctx mode w
where
	set_verify :: !SSLCTX !Int /*!Pointer*/ !*World -> *World
	set_verify ctx mode /*callback*/ w = code {
		ccall SSL_CTX_set_verify_help "pI:V:A"
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

import StdString, StdArray
SSL_ctrl_string :: !Pointer !Int !Int !String !*World -> *(!Int, !*World)
SSL_ctrl_string ssl cmd larg parg w = call ssl cmd (size parg) /*larg*/ (packString parg) w
where
	call :: !Pointer !Int !Int !String !*World -> *(!Int, !*World)
	call ssl cmd larg parg w = code {
		ccall SSL_ctrl_help "pIIs:I:A"
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

// A static buffer, because BIO_read_all has an int argument after the char ptr
read_buffer =: malloc READ_SIZE
READ_SIZE = 1536

BIO_read_all :: !BIO !*World -> *(!String, !*World)
BIO_read_all bio w
#! (n,w) = BIO_read bio read_buffer READ_SIZE w
#! (r,w) = BIO_test_flags bio BIO_FLAGS_SHOULD_RETRY w
#! (s,w) = (derefString read_buffer,w)
#! s     = s % (0, n-1)
| n <= 0 && not r = (s, w)
#! (s2,w) = BIO_read_all bio w
= (s +++ s2, w)

BIO_read :: !BIO !Pointer !Int !*World -> *(!Int, !*World)
BIO_read bio p n w = code {
	ccall BIO_read "ppI:I:A"
}

BIO_test_flags :: !BIO !Int !*World -> *(!Bool, !*World)
BIO_test_flags bio f w = code {
	ccall BIO_test_flags "pI:I:A"
}

malloc :: !Int -> Pointer
malloc n = code {
	ccall malloc "I:p"
}
