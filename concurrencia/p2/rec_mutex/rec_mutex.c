#include "rec_mutex.h"

int rec_mutex_init(rec_mutex_t *m)
{
    if (pthread_mutex_init(&m->m, NULL) != 0)
    {
        return -1;
    }
    if (pthread_cond_init(&m->c, NULL) != 0)
    {
        pthread_mutex_destroy(&m->m);
        return -1;
    }
    m->locker = 0;
    m->times = 0;
    return 0;
}

int rec_mutex_destroy(rec_mutex_t *m)
{
    if (pthread_mutex_destroy(&m->m) != 0 || pthread_cond_destroy(&m->c) != 0)
    {
        return -1;
    }
    return 0;
}

int rec_mutex_lock(rec_mutex_t *m)
{
    pthread_t self = pthread_self();
    pthread_mutex_lock(&m->m);
    // Es el mismo
    if (pthread_equal(m->locker, self))
    {
        m->times++;
        pthread_mutex_unlock(&m->m);
        return 0;
    }
    // Es otro
    while (m->times > 0)
    {
        pthread_cond_wait(&m->c, &m->m);
    }
    m->locker = self;
    m->times = 1;
    pthread_mutex_unlock(&m->m);
    return 0;
}

int rec_mutex_unlock(rec_mutex_t *m)
{
    pthread_t self = pthread_self();
    pthread_mutex_lock(&m->m);
    // Es el mismo hilo
    if (pthread_equal(m->locker, self))
    {
        m->times--;
        if (m->times == 0)
        {
            m->locker = 0;
            pthread_cond_broadcast(&m->c);
        }
        pthread_mutex_unlock(&m->m);
        return 0;
    }
    // Es otro
    pthread_mutex_unlock(&m->m);
    return -1;
}

int rec_mutex_trylock(rec_mutex_t *m)
{
    pthread_t self = pthread_self();
    if (pthread_mutex_trylock(&m->m) != 0)
        return -1;
    if (pthread_equal(m->locker, self))
    {
        m->times++;
        pthread_mutex_unlock(&m->m);
        return 0;
    }
    if (m->times == 0)
    {
        m->locker = self;
        m->times = 1;
        pthread_mutex_unlock(&m->m);
        return 0;
    }
    pthread_mutex_unlock(&m->m);
    return -1;
}
