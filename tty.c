#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <termios.h>

#include "Clean.h"

#define INITIAL_BUFFERSIZE 1024
#define die(s) {perror(s);exit(EXIT_FAILURE);}

static speed_t baudrates[] = {B0, B50, B75, B110, B134, B150, B200, B300, B600,
	B1200, B1800, B2400, B4800, B9600, B19200, B38400, B57600, B115200,
	B230400};
static int bytesizes[4] = {CS5, CS6, CS7, CS8};
//static int stopbits[3] = {StopbitsOne, StopbitsOnePointFive, StopbitsTwo};

static char *error = "";

static char *cleanStringToCString(CleanString s)
{
	unsigned long len = CleanStringLength(s);
	char *cs = (char *)malloc(len+1);
	if(cs == NULL)
		die("malloc");
	memcpy(cs, CleanStringCharacters(s), len);
	cs[len] = '\0';
	return cs;
}	

void ttyopen(CleanString fn, int baudrate, int bytesize, int parity,
	int stopbits, int xonoff, int *status, FILE **f)
{
	struct termios tio;
	char *cs_fn = cleanStringToCString(fn);
	int fd = open(cs_fn, O_RDWR | O_NOCTTY);
	*status = 0;
	if(fd < 0){
		error = strerror(errno);
	} else {
		//Get
		tcgetattr(fd, &tio);
		//Baudrate
		cfsetispeed(&tio, baudrates[baudrate]);
		//Bytesize
		tio.c_cflag |= CS5 | CS6 | CS7 | CS8;
		tio.c_cflag -= CS5 | CS6 | CS7 | CS8;
		tio.c_cflag |= bytesizes[bytesize];
		//Parity
		tio.c_cflag |= PARENB | PARODD | CMSPAR;
		tio.c_cflag -= PARENB | PARODD | CMSPAR;
		if(parity == 1)
			tio.c_cflag |= PARENB | PARODD;
		else if(parity == 2)
			tio.c_cflag |= PARENB;
		else if(parity == 3)
			tio.c_cflag |= PARODD | PARENB | CMSPAR;
		else if(parity == 4)
			tio.c_cflag |= PARENB | CMSPAR;
		//Stopbits
		tio.c_cflag |= CSTOPB;
		tio.c_cflag -= stopbits == 0 ? 0 : CSTOPB;
		//Xonoff
		tio.c_cflag |= IXON;
		tio.c_cflag -= xonoff == 1 ? 0 : IXON;
		//Set
		tcsetattr(fd, TCSANOW, &tio);

		*f = fdopen(fd, "r+");
		if(*f == NULL){
			printf("Couldn't open\n");
			fflush(stdout);
		} else {
			printf("Succesfully opened\n");
			fflush(stdout);
			setbuf(*f, NULL);
			*status = 1;
		}
		error = strerror(errno);
	}
	free(cs_fn);
}

void ttyerror(CleanString *result)
{
	CleanStringVariable(clean_string, strlen(error));
	*result = (CleanString) clean_string;
	memcpy(CleanStringCharacters(clean_string), error, strlen(error));
	CleanStringLength(clean_string) = strlen(error);
}

void ttyreadc(FILE *fd, int *c, FILE **fdo)
{
	*c = fgetc(fd);
	*fdo = fd;
}

void ttyreadline(FILE *fd, CleanString *result, FILE **fdo)
{
	size_t bufsize = INITIAL_BUFFERSIZE;
	char *buf = (char *)malloc(bufsize+1);
	int c, i = 0;

	if(buf == NULL)
		die("malloc");

	while((c = fgetc(fd)) != EOF && c != '\n'){
		if(i >= bufsize)
			if((buf = realloc(buf, bufsize *= 2)) == NULL)
				die("realloc");
		buf[i++] = c;
	}
	buf[i] = '\0';

	CleanStringVariable(cleanOutput, strlen(buf));
	*result = (CleanString) cleanOutput;
	memcpy(CleanStringCharacters(cleanOutput), buf, strlen(buf));
	CleanStringLength(cleanOutput) = strlen(buf);
	*fdo = fd;
	free(buf);
}

int ttywrite(FILE *fd, CleanString s, FILE **fdo)
{
	char *cs_s = cleanStringToCString(s);
	fwrite(s, 1, strlen(cs_s), fd);
	
	free(cs_s);
	*fdo = fd;
}

int ttyclose(FILE *fd)
{
	return fclose(fd) == 0 ? 1 : 0;
}
