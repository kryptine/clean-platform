implementation module Crypto.SSL

// Compile with -l -lcrypto -l -lssl
// https://wiki.openssl.org/index.php/SSL/TLS_Client

import StdBool
import StdInt
import StdList
import StdMisc
import StdString
import StdTuple

from Data.Func import $
import Internet.HTTP
import System.FilePath
import System._Pointer
import System._Posix

import code from "sslhelp."

:: SSLMethod :== Pointer
:: SSLCTX :== Pointer
:: BIO :== Pointer


Start w
#! (bio,w)  = initSSL host port w
#! w        = BIO_puts bio (toString req) w
#! (resp,w) = BIO_read_all bio w
= resp
where
	host = "badssl.com"
	//host = "untrusted-root.badssl.com"
	//host = "revoked.com"
	//host = "rc4-md5.badssl.com"
	//host = "wrong.host.badssl.com"
	//host = "sha1-intermediate.badssl.com"
	//host = "pinning-test.badssl.com"
	//host = "wrong.host.badssl.com"
	//host = "self-signed.badssl.com"
	//host = "expired.badssl.com"
	port = 443

	req = { newHTTPRequest
		& req_path = "/"
		, server_name = host
		, server_port = port
		}

initSSL :: !String !Int !*World -> *(!BIO, !*World)
initSSL host port w
#! (_,w)    = OPENSSL_init_ssl 0 0 w
#! (meth,w) = TLS_method w
| meth == 0 = abort "Method was 0\n"
#! (ctx,w)  = SSL_CTX_new meth w
| ctx == 0  = abort "CTX was 1\n"
#! (prm,w)  = SSL_CTX_get0_param ctx w
#! (_,w)    = X509_VERIFY_PARAM_set_hostflags prm 4 /* X509_CHECK_FLAG_NO_PARTIAL_WILDCARDS */ w
#! w        = X509_VERIFY_PARAM_set1_host prm host w
#! w        = SSL_CTX_set_verify ctx SSL_VERIFY_PEER w
#! (res,w)  = SSL_CTX_set_verify_depth ctx 4 w
#! w        = SSL_CTX_set_options ctx [SSL_OP_NO_SSLv2, SSL_OP_NO_SSLv3, SSL_OP_NO_COMPRESSION] w
#! (res,w)  = SSL_CTX_load_verify_locations_file ctx "/etc/ssl/certs/ca-certificates.crt" w
| res <> 1  = abort "LV Res was not 1\n"
#! (web,w)  = BIO_new_ssl_connect ctx w
| web == 0  = abort "BIO was 0\n"
#! (res,w)  = BIO_set_conn_hostname web host port w
| res <> 1  = abort "CH Res was not 1\n"
#! (ssl,w)  = BIO_get_ssl web w
| ssl == 0  = abort "SSL was 0\n"
#! (res,w)  = SSL_set_cipher_list ssl "HIGH:!aNULL:!kRSA:!PSK:!SRP:!MD5:!RC4:!SHA1" w
| res <> 1  = abort "CL Res was not 1\n"
#! (res,w)  = SSL_set_tlsext_host_name ssl host w
| res <> 1  = abort "TH Res was not 1\n"
#! (res,w)  = BIO_do_connect web w
| res <> 1  = abort ("DC was not 1: " +++ toString res +++ "\n")
#! (res,w)  = BIO_do_handshake web w
| res <> 1  = abort "DH was not 1\n"
= (web,w)
where
	OPENSSL_init_ssl :: !Int !Pointer !*World -> *(!Int, !*World)
	OPENSSL_init_ssl _ _ w = code {
		ccall OPENSSL_init_ssl "Ip:I:A"
	}

TLS_method :: !*World -> *(!SSLMethod, !*World)
TLS_method w = code {
	ccall TLS_method ":p:A"
}

SSL_CTX_new :: !SSLMethod !*World -> *(!SSLCTX, !*World)
SSL_CTX_new m w = code {
	ccall SSL_CTX_new "p:p:A"
}

SSL_CTX_get0_param :: !SSLCTX !*World -> *(!Pointer, !*World)
SSL_CTX_get0_param ctx w = code {
	ccall SSL_CTX_get0_param "p:p:A"
}

X509_VERIFY_PARAM_set_hostflags :: !Pointer !Int !*World -> *(!Int, !*World)
X509_VERIFY_PARAM_set_hostflags param flags w = code {
	ccall X509_VERIFY_PARAM_set_hostflags_help "pI:I:A"
}

X509_VERIFY_PARAM_set1_host :: !Pointer !String !*World -> *World
X509_VERIFY_PARAM_set1_host param s w = snd (set param (packString s) 0 w)
where
	set :: !Pointer !String !Int !*World -> *(!Int, !*World)
	set param s len w = code {
		ccall X509_VERIFY_PARAM_set1_host "psI:I:A"
	}

SSL_CTX_set_verify :: !SSLCTX !Int !*World -> *World
SSL_CTX_set_verify ctx mode w = snd (set_verify ctx mode w)
where
	set_verify :: !SSLCTX !Int /*!Pointer*/ !*World -> *(!Int, !*World)
	set_verify ctx mode /*callback*/ w = code {
		ccall SSL_CTX_set_verify_help "pI:I:A"
	}

SSL_CTX_set_verify_depth :: !SSLCTX !Int !*World -> *(!Int, !*World)
SSL_CTX_set_verify_depth ctx depth w = code {
	ccall SSL_CTX_set_verify_depth_help "pI:I:A"
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
	ccall SSL_CTX_ctrl "pIpp:p:A"
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
SSL_ctrl_string ssl cmd larg parg w = call ssl cmd /*(size parg)*/ larg (packString parg) w
where
	call :: !Pointer !Int !Int !String !*World -> *(!Int, !*World)
	call ssl cmd larg parg w = code {
		ccall SSL_ctrl "pIps:p:A"
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
		ccall BIO_ctrl "pIp:Vp:A"
	}

BIO_do_connect :== BIO_do_handshake
BIO_do_handshake :: !BIO !*World -> *(!Int, !*World)
BIO_do_handshake bio w = BIO_ctrl_ptr bio BIO_C_DO_STATE_MACHINE 0 0 w

BIO_ctrl_ptr :: !BIO !Int !Int !Pointer !*World -> *(!Int, !*World)
BIO_ctrl_ptr bio cmd larg parg w = code {
	ccall BIO_ctrl "pIpp:p:A"
}

BIO_ctrl_string :: !BIO !Int !Int !String !*World -> *(!Int, !*World)
BIO_ctrl_string bio cmd larg parg w = call bio cmd larg (packString parg) w
where
	call :: !BIO !Int !Int !String !*World -> *(!Int, !*World)
	call bio cmd larg parg w = code {
		ccall BIO_ctrl "pIps:p:A"
	}

BIO_puts :: !BIO !String !*World -> *World
BIO_puts bio s w = snd (puts bio (packString s) w)
where
	puts :: !BIO !String !*World -> *(!Int, !*World)
	puts bio s w = code {
		ccall BIO_puts "ps:I:A"
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
| n <= 0 && r == 0 = (s, w)
#! (s2,w) = BIO_read_all bio w
= (s +++ s2, w)

BIO_read :: !BIO !Pointer !Int !*World -> *(!Int, !*World)
BIO_read bio p n w = code {
	ccall BIO_read "ppI:I:A"
}

BIO_test_flags :: !BIO !Int !*World -> *(!Int, !*World)
BIO_test_flags bio f w = code {
	ccall BIO_test_flags "pI:I:A"
}
