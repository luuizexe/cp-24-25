#include "rw_mutex.h"

int rw_mutex_init(rw_mutex_t *m)
{
    if (pthread_mutex_init(&m->m, NULL) != 0)
    {
        return -1;
    }
    if (pthread_cond_init(&m->condLectores, NULL) != 0)
    {
        pthread_mutex_destroy(&m->m);
        return -1;
    }
    if (pthread_cond_init(&m->condEscritores, NULL) != 0)
    {
        pthread_cond_destroy(&m->condLectores);
        pthread_mutex_destroy(&m->m);
        return -1;
    }
    m->lectores = 0;
    m->escritores = 0;
    return 0;
}

int rw_mutex_destroy(rw_mutex_t *m)
{
    if (pthread_mutex_destroy(&m->m) != 0 ||
        pthread_cond_destroy(&m->condLectores) != 0 ||
        pthread_cond_destroy(&m->condEscritores) != 0)
    {
        return -1;
    }
    return 0;
}

int rw_mutex_readlock(rw_mutex_t *m)
{
    pthread_mutex_lock(&m->m);
    while (m->escritores > 0)
    {
        pthread_cond_wait(&m->condLectores, &m->m);
    }
    m->lectores++;
    pthread_mutex_unlock(&m->m);
    return 0;
}

int rw_mutex_writelock(rw_mutex_t *m)
{
    pthread_mutex_lock(&m->m);
    while (m->lectores || m->escritores > 0)
    {
        pthread_cond_wait(&m->condEscritores, &m->m);
    }
    m->escritores++;
    pthread_mutex_unlock(&m->m);
    return 0;
}

int rw_mutex_readunlock(rw_mutex_t *m)
{
    pthread_mutex_lock(&m->m);
    m->lectores--;
    if (m->lectores == 0)
    {
        pthread_cond_signal(&m->condEscritores);
    }
    pthread_mutex_unlock(&m->m);
    return 0;
}

int rw_mutex_writeunlock(rw_mutex_t *m)
{
    pthread_mutex_lock(&m->m);
    m->escritores--;
    pthread_cond_broadcast(&m->condLectores);
    pthread_cond_signal(&m->condEscritores);
    pthread_mutex_unlock(&m->m);
    return 0;
}
