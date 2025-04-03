#include "sem.h"

int sem_init(sem_t *s, int value)
{
    if (value < 0)
        return -1;
    if (pthread_mutex_init(&s->m, NULL) != 0)
    {
        return -1;
    }
    if (pthread_cond_init(&s->c, NULL) != 0)
    {
        pthread_mutex_destroy(&s->m);
        return -1;
    }
    s->count = value;
    return 0;
}

// Preguntar si es correcto destruir asi
int sem_destroy(sem_t *s)
{
    if (pthread_mutex_destroy(&s->m) != 0 ||
        pthread_cond_destroy(&s->c) != 0)
    {
        return -1;
    }
    return 0;
}

int sem_p(sem_t *s)
{
    pthread_mutex_lock(&s->m);
    while (s->count == 0)
    {
        pthread_cond_wait(&s->c, &s->m);
    }
    s->count--;
    pthread_mutex_unlock(&s->m);
    return 0;
}

int sem_v(sem_t *s)
{
    pthread_mutex_lock(&s->m);
    s->count++;
    pthread_cond_signal(&s->c);
    pthread_mutex_unlock(&s->m);
    return 0;
}

int sem_tryp(sem_t *s)
{
    pthread_mutex_lock(&s->m);
    if (s->count == 0)
    {
        pthread_mutex_unlock(&s->m);
        return -1;
    }
    else
    {
        s->count--;
        pthread_mutex_unlock(&s->m);
    }

    return 0;
}
