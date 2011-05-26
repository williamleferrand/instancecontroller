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

    switch (ptrace(PTRACE_ATTACH, pid, NULL)) {
        case 0:
            break;
        case -ESRCH:
        case -EPERM:
            return 0;
        default:
            fprintf(stderr, "Failed to attach child\n");
            return 1;
    }
    if (pid != wait(&status)) {
        fprintf(stderr, "wrong wait signal\n");
        return 1;
    }
    if (!WIFSTOPPED(status) || (WSTOPSIG(status) != SIGSTOP))  {
        /* The pid might not be running */
        if (!kill(pid, 0)) {
            fprintf(stderr, "SIGSTOP didn't stop child\n");
            return 1;
        } else {
            return 0;
        }
    }
    if (ptrace(PTRACE_CONT, pid, 0, 0)) {
        fprintf(stderr, "Failed to restart child\n");
        return 1;
    }

    while (1) {
        if (waitid(P_PID, pid, &si, WSTOPPED | WEXITED)) {
            // an error occurred.
            if (errno == ECHILD)
                return 0;
            return 1;
        }
        errno = 0;

        if (si.si_code & (CLD_STOPPED | CLD_TRAPPED)) {
            /* If the child gets stopped, we have to PTRACE_CONT it
             * this will happen when the child has a child that exits.
             **/
            if (ptrace(PTRACE_CONT, pid, 1, si.si_status)) {
                if (errno == ENOSYS) {
                    /* Wow, we're stuffed. Stop and return */
                    return 0;
                }
            }
            continue;
        }

        if (si.si_code & (CLD_EXITED | CLD_KILLED | CLD_DUMPED)) {
            return si.si_status;
        }
        // Fall through to exiting.
        return 1;
    }
}
