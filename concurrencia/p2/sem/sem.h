#ifndef __SEM_H__
#define __SEM_H__

#include <errno.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

typedef struct sem_t sem_t;

struct sem_t
{
    int count;
    pthread_mutex_t m;
    pthread_cond_t c;
};

int sem_init(sem_t *s, int value);
int sem_destroy(sem_t *s);

int sem_p(sem_t *s);
int sem_v(sem_t *s);
int sem_tryp(sem_t *s); // 0 on sucess, -1 if already locked

#endif