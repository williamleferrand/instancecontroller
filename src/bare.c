/*
 * You talkin' to me? 
 */

// A bare C implementation of a program that wait for a process that it was spawned by sb else


#include <sys/cdefs.h>
#include <sys/types.h>
#include <sys/ptrace.h>
#include <errno.h>
#include <stdio.h>
#include <signal.h>
#include <unistd.h>
#include <sys/wait.h>

int main (int argc, char** argv) {
  printf (">> process waiter\n") ; 
  return 0 ;
}


