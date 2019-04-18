#include <stdio.h>
#include <stddef.h>

#ifdef _WIN32
#include <winsock2.h>
#include <ws2tcpip.h>
#else
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#endif
int main(void)
{
	printf("AF_INET :== %lu\n", AF_INET);
	//printf("AF_UNIX :== %lu\n", AF_UNIX);
	printf("AF_INET6 :== %lu\n", AF_INET6);
	printf("AF_IPX :== %lu\n", AF_IPX);
	printf("AF_APPLETALK :== %lu\n", AF_APPLETALK);
	printf("AF_IRDA :== %lu\n", AF_IRDA);
	printf("SOCK_STREAM :== %lu\n", SOCK_STREAM);
	printf("SOCK_DGRAM :== %lu\n", SOCK_DGRAM);

	printf("sockaddr_in offsets:\n");
	printf("sin_family: %lu\n", offsetof(struct sockaddr_in, sin_family));
	printf("sin_port: %lu\n", offsetof(struct sockaddr_in, sin_port));
	printf("sin_addr: %lu\n", offsetof(struct sockaddr_in, sin_addr));
	printf("in_addr offsets:\n");
	printf("s_addr: %lu\n", offsetof(struct in_addr, s_addr));

	printf("sockaddr_in6 offsets:\n");
	printf("sin6_family: %lu\n",
		offsetof(struct sockaddr_in6, sin6_family));
	printf("sin6_port: %lu\n", offsetof(struct sockaddr_in6, sin6_port));
	printf("sin6_flowinfo: %lu\n",
		offsetof(struct sockaddr_in6, sin6_flowinfo));
	printf("sin6_addr: %lu\n", offsetof(struct sockaddr_in6, sin6_addr));
	printf("sin6_scope_id: %lu\n",
		offsetof(struct sockaddr_in6, sin6_scope_id));
	printf("in6_addr offsets:\n");
	printf("s6_addr: %lu\n", offsetof(struct in6_addr, s6_addr));

	return 0;
}
