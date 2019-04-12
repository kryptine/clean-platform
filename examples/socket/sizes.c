#include <stdio.h>
#include <stddef.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
int main(void)
{
	printf("AF_INET: %lu\n", AF_INET);
	printf("SOCK_STREAM: %lu\n", SOCK_STREAM);
	printf("SOCK_DGRAM: %lu\n", SOCK_DGRAM);
	printf("INADDR_ANY: %lu\n", INADDR_ANY);
	printf("sizeof(sockaddr_in): %lu\n", sizeof (struct sockaddr_in));

	printf("offset sockaddr_in.sin_family: %lu\n",
		offsetof(struct sockaddr_in, sin_family));
	printf("offset sockaddr_in.sin_port: %lu\n",
		offsetof(struct sockaddr_in, sin_port));
	printf("offset sockaddr_in.sin_addr: %lu\n",
		offsetof(struct sockaddr_in, sin_addr));

	return 0;
}
