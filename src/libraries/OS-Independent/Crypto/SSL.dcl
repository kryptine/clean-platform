definition module Crypto.SSL

from StdInt import bitor

:: SSLOption :== Int

SSL_VERIFY_NONE                 :== 0x00
SSL_VERIFY_PEER                 :== 0x01
SSL_VERIFY_FAIL_IF_NO_PEER_CERT :== 0x02
SSL_VERIFY_CLIENT_ONCE          :== 0x04

SSL_OP_MICROSOFT_SESS_ID_BUG                    :== 0x00000001
SSL_OP_NETSCAPE_CHALLENGE_BUG                   :== 0x00000002
SSL_OP_LEGACY_SERVER_CONNECT                    :== 0x00000004
SSL_OP_NETSCAPE_REUSE_CIPHER_CHANGE_BUG         :== 0x00000008
SSL_OP_TLSEXT_PADDING                           :== 0x00000010
SSL_OP_MICROSOFT_BIG_SSLV3_BUFFER               :== 0x00000020
SSL_OP_SAFARI_ECDHE_ECDSA_BUG                   :== 0x00000040
SSL_OP_SSLEAY_080_CLIENT_DH_BUG                 :== 0x00000080
SSL_OP_TLS_D5_BUG                               :== 0x00000100
SSL_OP_TLS_BLOCK_PADDING_BUG                    :== 0x00000200
SSL_OP_MSIE_SSLV2_RSA_PADDING                   :== 0x0
SSL_OP_SSLREF2_REUSE_CERT_TYPE_BUG              :== 0x0
SSL_OP_DONT_INSERT_EMPTY_FRAGMENTS              :== 0x00000800
SSL_OP_ALL                                      :== 0x80000BFF
SSL_OP_NO_QUERY_MTU                             :== 0x00001000
SSL_OP_COOKIE_EXCHANGE                          :== 0x00002000
SSL_OP_NO_TICKET                                :== 0x00004000
SSL_OP_CISCO_ANYCONNECT                         :== 0x00008000
SSL_OP_NO_SESSION_RESUMPTION_ON_RENEGOTIATION   :== 0x00010000
SSL_OP_NO_COMPRESSION                           :== 0x00020000
SSL_OP_ALLOW_UNSAFE_LEGACY_RENEGOTIATION        :== 0x00040000
SSL_OP_SINGLE_ECDH_USE                          :== 0x00080000
SSL_OP_SINGLE_DH_USE                            :== 0x00100000
SSL_OP_EPHEMERAL_RSA                            :== 0x0
SSL_OP_CIPHER_SERVER_PREFERENCE                 :== 0x00400000
SSL_OP_TLS_ROLLBACK_BUG                         :== 0x00800000
SSL_OP_NO_SSLv2                                 :== 0x01000000
SSL_OP_NO_SSLv3                                 :== 0x02000000
SSL_OP_NO_TLSv1                                 :== 0x04000000
SSL_OP_NO_TLSv1_2                               :== 0x08000000
SSL_OP_NO_TLSv1_1                               :== 0x10000000
SSL_OP_PKCS1_CHECK_1                            :== 0x0
SSL_OP_PKCS1_CHECK_2                            :== 0x0
SSL_OP_NETSCAPE_CA_DN_BUG                       :== 0x20000000
SSL_OP_NETSCAPE_DEMO_CIPHER_CHANGE_BUG          :== 0x40000000
SSL_OP_CRYPTOPRO_TLSEXT_BUG                     :== 0x80000000

SSL_CTRL_NEED_TMP_RSA                       :== 1
SSL_CTRL_SET_TMP_RSA                        :== 2
SSL_CTRL_SET_TMP_DH                         :== 3
SSL_CTRL_SET_TMP_ECDH                       :== 4
SSL_CTRL_SET_TMP_RSA_CB                     :== 5
SSL_CTRL_SET_TMP_DH_CB                      :== 6
SSL_CTRL_SET_TMP_ECDH_CB                    :== 7
SSL_CTRL_GET_SESSION_REUSED                 :== 8
SSL_CTRL_GET_CLIENT_CERT_REQUEST            :== 9
SSL_CTRL_GET_NUM_RENEGOTIATIONS             :== 10
SSL_CTRL_CLEAR_NUM_RENEGOTIATIONS           :== 11
SSL_CTRL_GET_TOTAL_RENEGOTIATIONS           :== 12
SSL_CTRL_GET_FLAGS                          :== 13
SSL_CTRL_EXTRA_CHAIN_CERT                   :== 14
SSL_CTRL_SET_MSG_CALLBACK                   :== 15
SSL_CTRL_SET_MSG_CALLBACK_ARG               :== 16
SSL_CTRL_SET_MTU                            :== 17
SSL_CTRL_SESS_NUMBER                        :== 20
SSL_CTRL_SESS_CONNECT                       :== 21
SSL_CTRL_SESS_CONNECT_GOOD                  :== 22
SSL_CTRL_SESS_CONNECT_RENEGOTIATE           :== 23
SSL_CTRL_SESS_ACCEPT                        :== 24
SSL_CTRL_SESS_ACCEPT_GOOD                   :== 25
SSL_CTRL_SESS_ACCEPT_RENEGOTIATE            :== 26
SSL_CTRL_SESS_HIT                           :== 27
SSL_CTRL_SESS_CB_HIT                        :== 28
SSL_CTRL_SESS_MISSES                        :== 29
SSL_CTRL_SESS_TIMEOUTS                      :== 30
SSL_CTRL_SESS_CACHE_FULL                    :== 31
SSL_CTRL_OPTIONS                            :== 32
SSL_CTRL_MODE                               :== 33
SSL_CTRL_GET_READ_AHEAD                     :== 40
SSL_CTRL_SET_READ_AHEAD                     :== 41
SSL_CTRL_SET_SESS_CACHE_SIZE                :== 42
SSL_CTRL_GET_SESS_CACHE_SIZE                :== 43
SSL_CTRL_SET_SESS_CACHE_MODE                :== 44
SSL_CTRL_GET_SESS_CACHE_MODE                :== 45
SSL_CTRL_GET_MAX_CERT_LIST                  :== 50
SSL_CTRL_SET_MAX_CERT_LIST                  :== 51
SSL_CTRL_SET_MAX_SEND_FRAGMENT              :== 52
SSL_CTRL_SET_TLSEXT_SERVERNAME_CB           :== 53
SSL_CTRL_SET_TLSEXT_SERVERNAME_ARG          :== 54
SSL_CTRL_SET_TLSEXT_HOSTNAME                :== 55
SSL_CTRL_SET_TLSEXT_DEBUG_CB                :== 56
SSL_CTRL_SET_TLSEXT_DEBUG_ARG               :== 57
SSL_CTRL_GET_TLSEXT_TICKET_KEYS             :== 58
SSL_CTRL_SET_TLSEXT_TICKET_KEYS             :== 59
SSL_CTRL_SET_TLSEXT_OPAQUE_PRF_INPUT        :== 60
SSL_CTRL_SET_TLSEXT_OPAQUE_PRF_INPUT_CB     :== 61
SSL_CTRL_SET_TLSEXT_OPAQUE_PRF_INPUT_CB_ARG :== 62
SSL_CTRL_SET_TLSEXT_STATUS_REQ_CB           :== 63
SSL_CTRL_SET_TLSEXT_STATUS_REQ_CB_ARG       :== 64
SSL_CTRL_SET_TLSEXT_STATUS_REQ_TYPE         :== 65
SSL_CTRL_GET_TLSEXT_STATUS_REQ_EXTS         :== 66
SSL_CTRL_SET_TLSEXT_STATUS_REQ_EXTS         :== 67
SSL_CTRL_GET_TLSEXT_STATUS_REQ_IDS          :== 68
SSL_CTRL_SET_TLSEXT_STATUS_REQ_IDS          :== 69
SSL_CTRL_GET_TLSEXT_STATUS_REQ_OCSP_RESP    :== 70
SSL_CTRL_SET_TLSEXT_STATUS_REQ_OCSP_RESP    :== 71
SSL_CTRL_SET_TLSEXT_TICKET_KEY_CB           :== 72
SSL_CTRL_SET_TLS_EXT_SRP_USERNAME_CB        :== 75
SSL_CTRL_SET_SRP_VERIFY_PARAM_CB            :== 76
SSL_CTRL_SET_SRP_GIVE_CLIENT_PWD_CB         :== 77
SSL_CTRL_SET_SRP_ARG                        :== 78
SSL_CTRL_SET_TLS_EXT_SRP_USERNAME           :== 79
SSL_CTRL_SET_TLS_EXT_SRP_STRENGTH           :== 80
SSL_CTRL_SET_TLS_EXT_SRP_PASSWORD           :== 81
SSL_CTRL_TLS_EXT_SEND_HEARTBEAT             :== 85
SSL_CTRL_GET_TLS_EXT_HEARTBEAT_PENDING      :== 86
SSL_CTRL_SET_TLS_EXT_HEARTBEAT_NO_REQUESTS  :== 87
DTLS_CTRL_GET_TIMEOUT                       :== 73
DTLS_CTRL_HANDLE_TIMEOUT                    :== 74
DTLS_CTRL_LISTEN                            :== 75
SSL_CTRL_GET_RI_SUPPORT                     :== 76
SSL_CTRL_CLEAR_OPTIONS                      :== 77
SSL_CTRL_CLEAR_MODE                         :== 78
SSL_CTRL_GET_EXTRA_CHAIN_CERTS              :== 82
SSL_CTRL_CLEAR_EXTRA_CHAIN_CERTS            :== 83
SSL_CTRL_CHECK_PROTO_VERSION                :== 119

BIO_C_SET_CONNECT                       :== 100
BIO_C_DO_STATE_MACHINE                  :== 101
BIO_C_SET_NBIO                          :== 102
BIO_C_SET_PROXY_PARAM                   :== 103
BIO_C_SET_FD                            :== 104
BIO_C_GET_FD                            :== 105
BIO_C_SET_FILE_PTR                      :== 106
BIO_C_GET_FILE_PTR                      :== 107
BIO_C_SET_FILENAME                      :== 108
BIO_C_SET_SSL                           :== 109
BIO_C_GET_SSL                           :== 110
BIO_C_SET_MD                            :== 111
BIO_C_GET_MD                            :== 112
BIO_C_GET_CIPHER_STATUS                 :== 113
BIO_C_SET_BUF_MEM                       :== 114
BIO_C_GET_BUF_MEM_PTR                   :== 115
BIO_C_GET_BUFF_NUM_LINES                :== 116
BIO_C_SET_BUFF_SIZE                     :== 117
BIO_C_SET_ACCEPT                        :== 118
BIO_C_SSL_MODE                          :== 119
BIO_C_GET_MD_CTX                        :== 120
BIO_C_GET_PROXY_PARAM                   :== 121
BIO_C_SET_BUFF_READ_DATA                :== 122
BIO_C_GET_CONNECT                       :== 123
BIO_C_GET_ACCEPT                        :== 124
BIO_C_SET_SSL_RENEGOTIATE_BYTES         :== 125
BIO_C_GET_SSL_NUM_RENEGOTIATES          :== 126
BIO_C_SET_SSL_RENEGOTIATE_TIMEOUT       :== 127
BIO_C_FILE_SEEK                         :== 128
BIO_C_GET_CIPHER_CTX                    :== 129
BIO_C_SET_BUF_MEM_EOF_RETURN            :== 130
BIO_C_SET_BIND_MODE                     :== 131
BIO_C_GET_BIND_MODE                     :== 132
BIO_C_FILE_TELL                         :== 133
BIO_C_GET_SOCKS                         :== 134
BIO_C_SET_SOCKS                         :== 135
BIO_C_SET_WRITE_BUF_SIZE                :== 136
BIO_C_GET_WRITE_BUF_SIZE                :== 137
BIO_C_MAKE_BIO_PAIR                     :== 138
BIO_C_DESTROY_BIO_PAIR                  :== 139
BIO_C_GET_WRITE_GUARANTEE               :== 140
BIO_C_GET_READ_REQUEST                  :== 141
BIO_C_SHUTDOWN_WR                       :== 142
BIO_C_NREAD0                            :== 143
BIO_C_NREAD                             :== 144
BIO_C_NWRITE0                           :== 145
BIO_C_NWRITE                            :== 146
BIO_C_RESET_READ_REQUEST                :== 147
BIO_C_SET_MD_CTX                        :== 148
BIO_C_SET_PREFIX                        :== 149
BIO_C_GET_PREFIX                        :== 150
BIO_C_SET_SUFFIX                        :== 151
BIO_C_GET_SUFFIX                        :== 152
BIO_C_SET_EX_ARG                        :== 153
BIO_C_GET_EX_ARG                        :== 154

BIO_FLAGS_READ          :== 0x01
BIO_FLAGS_WRITE         :== 0x02
BIO_FLAGS_IO_SPECIAL    :== 0x04
BIO_FLAGS_SHOULD_RETRY  :== 0x08
BIO_FLAGS_UPLINK        :== 0
BIO_FLAGS_RWS :== BIO_FLAGS_READ bitor BIO_FLAGS_WRITE bitor BIO_FLAGS_IO_SPECIAL
