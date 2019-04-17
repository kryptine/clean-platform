#include <stdio.h>
#include <stddef.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
int main(void)
{
	printf("AF_INET :== %lu\n", AF_INET);
	printf("AF_UNIX :== %lu\n", AF_UNIX);
	printf("AF_INET6 :== %lu\n", AF_INET6);
	printf("AF_IPX :== %lu\n", AF_IPX);
	printf("AF_APPLETALK :== %lu\n", AF_APPLETALK);
	printf("AF_IRDA :== %lu\n", AF_IRDA);
	printf("SOCK_STREAM :== %lu\n", SOCK_STREAM);
	printf("SOCK_DGRAM :== %lu\n", SOCK_DGRAM);
	return 0;
}
