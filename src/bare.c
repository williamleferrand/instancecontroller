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


int main(int argc, char** argv) {

    int pid = atoi(argv[1]);
    int status;
    siginfo_t si;

    /* Attach the process *****************************************************/
    
    int rc ;

    rc = ptrace(PT_ATTACH, pid, NULL, 0); 

    if (rc) {
      printf ("PANIC, can't attach\n"); 
      return 1 ;
    }
    
    while (1) {
      
      int status; 
      waitpid (pid, &status, WUNTRACED); 
      
      if (WIFSTOPPED(status)) {
        // Child is stoppped
        printf ("STOPPED: %d\n", WSTOPSIG(status)); 
        

        if (ptrace(PT_STEP, pid, 1, WSTOPSIG(status))) {
          printf ("PANIC, can't CONTINUE\n"); 
          return 1; 
          } 

        // rc = ptrace(PT_DETACH, pid, 0, 0); 
        //printf ("RC: %d\n", rc) ;
        //return 0 ;
        
        
        continue; 

      }

      if (WIFSIGNALED(status)) {
        // Child has terminated 
        printf ("END : SIGNALED %d\n", WTERMSIG(status)); 
        return 0; 
      }
      
      if (WIFEXITED(status)) {
        printf ("END : EXITED %d\n", WEXITSTATUS(status)); 
        return 0 ; 
      }
    }
}
