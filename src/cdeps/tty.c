//Windows imports
#ifdef _WIN32
#include <windows.h>

typedef HANDLE ttyhandle;
//Posix imports
#else
#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include <termios.h>
#include <sys/ioctl.h>
typedef int ttyhandle;
#endif

//Common imports
#include <stdio.h>
#include <stdbool.h>

#include "Clean.h"

#ifdef DEBUG
#define debug(s) {printf("%s\n", s);}
#else
#define debug(s) ;
#endif

#ifdef __APPLE__
int CMSPAR = 0;
#endif

#define die(s) {perror(s);exit(EXIT_FAILURE);}

#ifdef _WIN32
static int baudrates[] = {0, 50, 75, 110, 134, 150, 200, 300, 600, 1200, 1800, 
	2400, 4800, 9600, 19200, 38400, 57600, 115200, 230400};
static int bytesizes[4] = {5, 6, 7, 8};
#else
static speed_t baudrates[] = {B0, B50, B75, B110, B134, B150, B200, B300, B600,
	B1200, B1800, B2400, B4800, B9600, B19200, B38400, B57600, B115200,
	B230400};
static int bytesizes[4] = {CS5, CS6, CS7, CS8};
#endif

static char *error = "NoError";

#ifdef _WIN32
#endif
static void *my_malloc(size_t s)
{
	void *r;
#ifdef _WIN32
	r = HeapAlloc(GetProcessHeap(), 0, s);
#else
	if((r = malloc(s)) == NULL)
		die("my_malloc");
#endif
	return r;
}

static void my_free(void *p)
{
#ifdef _WIN32
	HeapFree(GetProcessHeap(), 0, p);
#else
	free(p);
#endif

}

static char *cleanStringToCString(CleanString s)
{
	unsigned long len = CleanStringLength(s);
	char *cs = (char *)my_malloc(len+1);
	memcpy(cs, CleanStringCharacters(s), len);
	cs[len] = '\0';
	return cs;
}


#ifndef _WIN32
//This buggery is needed for linux systems that don't reset the termios settings...
struct termioslist
{
	int fd;
	struct termios to;
	struct termioslist *next;
};
struct termioslist *head = NULL;
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
			my_free(it);
			break;
		}
		beforeit = it;
		it = it->next;
	}
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
#endif

void ttyopen(CleanString fn, int baudrate, int bytesize, int parity,
	int stopbits, int xonoff, int sleepTime, int *status, ttyhandle *fd)
{
	debug("ttyopen");
	char *cs_fn = cleanStringToCString(fn);
	debug(cs_fn);
	*status = 0;
#ifdef _WIN32
	*fd = CreateFile(cs_fn, GENERIC_READ | GENERIC_WRITE, 0, 0, OPEN_EXISTING, 0, 0);
	debug("Opened");
	if(*fd == INVALID_HANDLE_VALUE){
		debug("Error opening");
		error = strerror(errno);
		return;
	}
	DCB dcb;
	FillMemory(&dcb, sizeof(dcb), 0);
	dcb.DCBlength = sizeof(dcb);

	//Get
	if (!GetCommState(*fd, &dcb)){
		error = strerror(errno);
		return;
	}
	//Baudrate
	dcb.BaudRate = baudrates[baudrate];
	//Bytesize
	dcb.ByteSize = bytesizes[bytesize];
	//Parity
	if(parity == 0) {
		dcb.fParity = false;
	} else if(parity == 1) {
		dcb.fParity = true;
		dcb.Parity = ODDPARITY;
	} else if(parity == 2) {
		dcb.fParity = true;
		dcb.Parity = EVENPARITY;
	} else if(parity == 3) {
		dcb.fParity = true;
		//Parity space???
	} else if( parity == 4) {
		dcb.fParity = true;
		dcb.Parity = MARKPARITY;
	}
	//Stopbits
	if(stopbits != 0)
		dcb.StopBits = ONESTOPBIT;
	else
		dcb.StopBits = TWOSTOPBITS;
	//Xonoff
	if(xonoff == 1)
		dcb.fTXContinueOnXoff = true;
	else
		dcb.fTXContinueOnXoff = false;
	//Set
	//tio.c_oflag = 0;
	//tio.c_lflag &= ~(ECHO | ECHONL | ICANON | IEXTEN | ISIG);
	if (!SetCommState(*fd, &dcb)){
		error = strerror(errno);
		return;
	}
	*status = 1;
	
	if(sleepTime > 0){
		//Sleep on windows is in milliseconds...
		Sleep(sleepTime*1000);
	}
#else
	struct termios tio;
	*fd = open(cs_fn, O_RDWR | O_NOCTTY | O_NONBLOCK);
	fcntl(*fd, F_SETFL, 0);
	if(*fd < 0){
		error = strerror(errno);
		return;
	}
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
	tio.c_lflag &= ~(ECHO | ECHONL | ICANON | IEXTEN | ISIG);

	#ifdef __APPLE__
	tio.c_cflag |= CLOCAL;
	#endif
	tio.c_cc[VMIN]=1;
	tio.c_cc[VTIME]=0;
	tcsetattr(*fd, TCSANOW, &tio);

	*status = 1;
	error = strerror(errno);

	if(sleepTime > 0){
		sleep(sleepTime);
		tcflush(*fd, TCIOFLUSH);
	}
#endif

	my_free(cs_fn);
	debug("ttyopen-done");
}

unsigned long *errcl = NULL;
void ttyerror(CleanString *result)
{
	debug("ttyerror");
	if(errcl != NULL)
		my_free(errcl);
	errcl = my_malloc(
		sizeof(unsigned long)*CleanStringSizeInts(strlen(error)));
	*result = (CleanString) errcl;
	memcpy(CleanStringCharacters(errcl), error, strlen(error));
	CleanStringLength(errcl) = strlen(error);
	debug("ttyerror-done");
}

void ttyread(ttyhandle fd, int *ch, ttyhandle *fdo)
{
	debug("ttyread");
	unsigned int c;
#ifdef _WIN32
	long unsigned int bytes_read;
	if(!ReadFile(fd, &c, 1, &bytes_read, NULL)){
		die("ReadFile failed");
	}
#else
	if(read(fd, &c, 1) == -1){
		die("read");
	}
#endif
	*ch = (int) c;
	*fdo = fd;
	debug("ttyread done");
}

void ttyavailable(ttyhandle fd, int *r, int *e, ttyhandle *fdo)
{
	debug("ttyavailable");
	*e = 0;
#ifdef _WIN32
	COMSTAT cs;
	if(ClearCommError(fd, NULL, &cs) == 0){
		error = strerror(errno);
		*e = 1;
		return;
	}
	*r = cs.cbInQue > 0;
#else
	fd_set rfds, efds;
	struct timeval tv;
	tv.tv_sec = 0;
	tv.tv_usec = 0;

	FD_ZERO(&rfds);
	FD_SET(fd, &rfds);

	FD_ZERO(&efds);
	FD_SET(fd, &efds);

	*r = select(fd+1, &rfds, NULL, &efds, &tv);

	if (FD_ISSET(fd, &efds)){
		*e = 1;
		*fdo = fd;
		return;
	}

	if(*r == -1)
		die("select");
#endif
	*fdo = fd;
//	debug("ttyavailable-done");
}

ttyhandle ttywrite(CleanString s, ttyhandle fd)
{
	debug("ttywrite");
#ifdef _WIN32
	long unsigned int bytes_written;
	WriteFile(fd, (void *)CleanStringCharacters(s), CleanStringLength(s), &bytes_written, NULL);
	//TODO flush?
#else
	write(fd, (void *)CleanStringCharacters(s), CleanStringLength(s));
	tcdrain(fd);
#endif
	debug("ttywrite-done");
	return fd;
}

int ttyclose(ttyhandle fd)
{
	debug("ttyclose");
	int ret = 0;
#ifdef _WIN32
	ret = CloseHandle(fd);
	error = strerror(errno);
#else
	struct termios *to = getTermios(fd);
	tcsetattr(fd, TCSANOW, to);
	remTermios(fd);
	ret = close(fd) == 0;
	error = strerror(errno);
#endif
	debug("ttyclose-done");
	return ret;
}
