#include <errno.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <termios.h>
#include <time.h>

#include "Clean.h"

#ifdef DEBUG
#define debug(s) {puts(s); fflush(stdout);}
#else
#define debug(s) ;
#endif

#define INITIAL_BUFFERSIZE 2

#define die(s) {perror(s);exit(EXIT_FAILURE);}

static speed_t baudrates[] = {B0, B50, B75, B110, B134, B150, B200, B300, B600,
	B1200, B1800, B2400, B4800, B9600, B19200, B38400, B57600, B115200,
	B230400};
static int bytesizes[4] = {CS5, CS6, CS7, CS8};
static char *error = "";

struct termioslist
{
	int fd;
	struct termios to;
	struct termioslist *next;
};

struct termioslist *head = NULL;

static void *my_malloc(size_t s)
{
	void *r = malloc(s);
	if(r == NULL)
		die("malloc");
	return r;
}

static struct termios *getTermios(int fd)
{
	struct termioslist *h = head;
	while(h != NULL)
		if(h->fd == fd)
			return &h->to;
	return NULL;
}

static void remTermios(int fd)
{
	struct termioslist *beforeit = NULL;
	struct termioslist *it = head;
	while(it != NULL){
		if(it->fd == fd){
			if(beforeit == NULL)
				head = it->next;
			else
				beforeit->next = it->next;
			free(it);
			break;
		}
		beforeit = it;
		it = it->next;
	}
}

static char *cleanStringToCString(CleanString s)
{
	unsigned long len = CleanStringLength(s);
	char *cs = (char *)my_malloc(len+1);
	memcpy(cs, CleanStringCharacters(s), len);
	cs[len] = '\0';
	return cs;
}

static void addTermios(int fd, struct termios *t)
{
	struct termioslist *new = my_malloc(sizeof(struct termioslist));
	new->fd = fd;
	memcpy(&new->to, t, sizeof(struct termios));
	new->next = NULL;
	if(head == NULL){
		head = new;
	} else {
		struct termioslist *h = head;
		while(h->next != NULL)
			h = h->next;
		h->next = new;
	}
}

void ttyopen(CleanString fn, int baudrate, int bytesize, int parity,
	int stopbits, int xonoff, int *status, int *fd)
{
	debug("ttyopen");
	struct termios tio;
	char *cs_fn = cleanStringToCString(fn);
	*fd = open(cs_fn, O_RDWR | O_NOCTTY | O_NDELAY | O_NONBLOCK);
	*status = 0;
	fcntl(*fd, F_SETFL, 0);
	if(*fd < 0){
		error = strerror(errno);
	} else {
		//Get
		tcgetattr(*fd, &tio);
		addTermios(*fd, &tio);
		//Baudrate
		cfsetispeed(&tio, baudrates[baudrate]);
		//Bytesize
		tio.c_cflag &= ~CSIZE;
		tio.c_cflag |= bytesizes[bytesize];
		//Parity
		if(parity == 0) {
			tio.c_cflag &= ~PARENB | ~INPCK;
		} else if(parity == 1) {
			tio.c_cflag |= PARODD | PARENB;
		} else if(parity == 2) {
			tio.c_cflag |= PARENB;
			tio.c_cflag &= ~PARODD;
		} else if(parity == 3) {
			tio.c_cflag |= PARENB | CMSPAR;
			tio.c_cflag &= ~PARODD;
		} else if( parity == 4) {
			tio.c_cflag |= PARENB | CMSPAR | PARODD;
		}
		//Stopbits
		if(stopbits != 0)
			tio.c_cflag |= CSTOPB;
		else
			tio.c_cflag &= ~CSTOPB;
		//Xonoff
		if(xonoff == 1)
			tio.c_cflag |= IXON;
		else
			tio.c_cflag &= ~IXON;
		//Set
		tio.c_oflag = 0;
		tio.c_lflag |= ICANON;

		tio.c_cc[VMIN]=5;
		tio.c_cc[VTIME]=0;
		tcsetattr(*fd, TCSANOW, &tio);

		*status = 1;
		error = strerror(errno);
	}
	free(cs_fn);
	debug("ttyopen-done");
}

unsigned long *errcl = NULL;
void ttyerror(CleanString *result)
{
	debug("ttyerror");
	if(errcl == NULL)
		free(errcl);
	errcl = my_malloc(
		sizeof(unsigned long)*CleanStringSizeInts(strlen(error)));
	*result = (CleanString) errcl;
	memcpy(CleanStringCharacters(errcl), error, strlen(error));
	CleanStringLength(errcl) = strlen(error);
	debug("ttyerror-done");
}

unsigned long *readlinecl = NULL;
void ttyreadline(int fd, CleanString *result, int *fdo)
{
	debug("ttyreadline");
	size_t bufsize = INITIAL_BUFFERSIZE;
	char *buf = NULL;
	ssize_t cr, charsread = 0; 
	while(buf == NULL || buf[charsread-1] != '\n'){
		if((buf = realloc(buf, (bufsize*=2)+1)) == NULL)
			die("realloc");
		if((cr = read(fd, buf+charsread, bufsize-charsread)) < 0)
			die("read");
		charsread += cr;
	}
	buf[charsread] = '\0';

	if(readlinecl == NULL)
		free(readlinecl);
	readlinecl = my_malloc(
		sizeof(unsigned long)*CleanStringSizeInts(charsread));
	*result = (CleanString) readlinecl;
	memcpy(CleanStringCharacters(readlinecl), buf, charsread);
	CleanStringLength(readlinecl) = charsread;
	*fdo = fd;

	free(buf);
	debug("ttyreadline-done");
}

void ttyavailable(int fd, int *r, int *fdo)
{
	debug("ttyavailable");
	fd_set fds;
	struct timeval tv;
	tv.tv_sec = 0;
	tv.tv_usec = 0;

	FD_ZERO(&fds);
	FD_SET(fd, &fds);

	*r = select(fd+1, &fds, NULL, NULL, &tv);
	if(*r == -1)
		die("select");
	*fdo = fd;
	debug("ttyavailable-done");
}

int ttywrite(int fd, CleanString s)
{
	debug("ttywrite");
	write(fd, CleanStringCharacters(s), CleanStringLength(s));
	tcdrain(fd);
	debug("ttywrite-done");
	return fd;
}

int ttyclose(int fd)
{
	debug("ttyclose");
	struct termios *to = getTermios(fd);
	tcsetattr(fd, TCSANOW, to);
	remTermios(fd);
	int ret = close(fd);
	error = strerror(errno);
	debug("ttyclose-done");
	return ret + 1;
}
