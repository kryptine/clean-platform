#include <Clean.h>
#include <time.h>
#include <string.h>
#include <stdlib.h>

#define BUFSIZE 256

int clockC() {
	return (int) clock();
}
int timeC() {
	return (int) time(NULL);
}

void copyFromTmStruct(struct tm *t, CleanIntArray a) {	

	a[0] = t->tm_sec;
	a[1] = t->tm_min;
	a[2] = t->tm_hour;
	a[3] = t->tm_mday;
	a[4] = t->tm_mon;
	a[5] = t->tm_year;
	a[6] = t->tm_wday;
	a[7] = t->tm_yday;
	a[8] = t->tm_isdst;
}
void copyToTmStruct(struct tm *t, CleanIntArray a) {	

	t->tm_sec   = a[0];
	t->tm_min   = a[1];
	t->tm_hour  = a[2];
	t->tm_mday  = a[3];
	t->tm_mon   = a[4];
	t->tm_year  = a[5];
	t->tm_wday  = a[6];
	t->tm_yday  = a[7];
	t->tm_isdst = a[8];
}

void gmTimeC(CleanIntArray a) {

	time_t t;
	struct tm tm;
	
	time(&t);
	gmtime_r(&t, &tm);
	copyFromTmStruct(&tm, a);
}

void localTimeC(CleanIntArray a) {

	time_t t;
	struct tm tm;
	
	time(&t);
	localtime_r(&t, &tm);
	copyFromTmStruct(&tm, a);
}

int mkTimeC(CleanIntArray a) {
	time_t t;
	struct tm tm;

	copyToTmStruct(&tm, a);

	return (int) mktime(&tm);
}

int toStringTmC(CleanIntArray a, CleanString s) { 

	struct tm tm;

	copyToTmStruct(&tm, a);
	strncpy(CleanStringCharacters(s), asctime(&tm), 24);

	return 0;
}
int toStringTimeC(int t, CleanString s) {

	strncpy(CleanStringCharacters(s), ctime((time_t *) &t), 24);

	return 0;
}

void strfTimeC(CleanString format, CleanIntArray a, int *len_out, int *ptr_out) {

	struct tm tm;
	char *s = calloc(BUFSIZE, sizeof(char));

	copyToTmStruct(&tm, a);

	*len_out = (int) strftime(s, BUFSIZE, CleanStringCharacters(format), &tm);
	*ptr_out = (int) s;
}

int copyStringC(int len, int ptr, CleanString dest) {

	char *src = (char*) ptr;

	strncpy(CleanStringCharacters(dest), src, len);
	free(src);
	
	return 0;
}
