#ifndef __RW_MUTEX_H__
#define __RW_MUTEX_H__

#include <errno.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

typedef struct rw_mutex_t rw_mutex_t;

struct rw_mutex_t
{
    int lectores;
    int escritores;
    pthread_mutex_t m;
    pthread_cond_t condLectores;
    pthread_cond_t condEscritores;
};

int rw_mutex_init(rw_mutex_t *m);
int rw_mutex_destroy(rw_mutex_t *m);
int rw_mutex_readlock(rw_mutex_t *m);
int rw_mutex_writelock(rw_mutex_t *m);
int rw_mutex_readunlock(rw_mutex_t *m);
int rw_mutex_writeunlock(rw_mutex_t *m);

#endif