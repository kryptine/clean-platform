#include <openssl/ssl.h>
#include <openssl/ocsp.h>

int SSL_CTX_set_verify_help(SSL_CTX *ctx, int mode)
{
	SSL_CTX_set_verify(ctx, mode, NULL);
	return 1;
}

int X509_VERIFY_PARAM_set_hostflags_help(X509_VERIFY_PARAM *param, unsigned int flags)
{
	X509_VERIFY_PARAM_set_hostflags(param, flags);
	return 1;
}

int SSL_CTX_set_verify_depth_help(SSL_CTX *ctx, int depth)
{
	SSL_CTX_set_verify_depth(ctx, depth);
	return 1;
}
