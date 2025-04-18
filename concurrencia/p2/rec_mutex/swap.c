/*
INTEGRANTES:
ALBERTO PAZ PEREZ a.pazp@udc.es
LUIS ALFONSO RODRIGUEZ RODRIGUEZ luis.rodriguezr@udc.es
*/
#include <errno.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "op_count.h"
#include "options.h"
#include "rec_mutex.h"

struct buffer
{
    int *data;
    int size;
};

struct thread_info
{
    pthread_t thread_id; // id returned by pthread_create()
    int thread_num;      // application defined thread #
};

struct args
{
    int thread_num; // application defined thread #
    int delay;      // delay between operations
    int iterations;
    struct buffer *buffer; // Shared buffer
    rec_mutex_t *mutexs;   // Array de mutex
};

void *swap(void *ptr)
{
    struct args *args = ptr;
    while (args->iterations--)
    {
        int i, j, tmp;
        i = rand() % args->buffer->size;
        j = rand() % args->buffer->size;

        int primero = (i < j) ? i : j;
        int segundo = (i < j) ? j : i;
        rec_mutex_lock(&args->mutexs[primero]);
        rec_mutex_lock(&args->mutexs[segundo]);

        printf("Thread %d swapping positions %d (== %d) and %d (== %d)\n",
               args->thread_num, i, args->buffer->data[i], j, args->buffer->data[j]);

        tmp = args->buffer->data[i];
        if (args->delay)
            usleep(args->delay); // Force a context switch

        args->buffer->data[i] = args->buffer->data[j];
        if (args->delay)
            usleep(args->delay);

        args->buffer->data[j] = tmp;
        if (args->delay)
            usleep(args->delay);
        inc_count();
        rec_mutex_unlock(&args->mutexs[segundo]);
        rec_mutex_unlock(&args->mutexs[primero]);
    }
    return NULL;
}

int cmp(int *e1, int *e2)
{
    if (*e1 == *e2)
        return 0;
    if (*e1 < *e2)
        return -1;
    return 1;
}

void print_buffer(struct buffer buffer)
{
    int i;

    for (i = 0; i < buffer.size; i++)
        printf("%i ", buffer.data[i]);
    printf("\n");
}

void start_threads(struct options opt)
{
    int i;
    struct thread_info *threads;
    struct args *args;
    struct buffer buffer;
    rec_mutex_t *mutexs;
    srand(time(NULL));

    if ((buffer.data = malloc(opt.buffer_size * sizeof(int))) == NULL)
    {
        printf("Out of memory\n");
        exit(1);
    }
    buffer.size = opt.buffer_size;

    for (i = 0; i < buffer.size; i++)
        buffer.data[i] = i;

    printf("creating %d threads\n", opt.num_threads);
    threads = malloc(sizeof(struct thread_info) * opt.num_threads);
    args = malloc(sizeof(struct args) * opt.num_threads);
    mutexs = malloc(sizeof(rec_mutex_t) * opt.buffer_size);
    if (threads == NULL || args == NULL || mutexs == NULL)
    {
        printf("Not enough memory\n");
        exit(1);
    }
    for (i = 0; i < opt.buffer_size; i++)
        rec_mutex_init(&mutexs[i]);

    printf("Buffer before: ");
    print_buffer(buffer);

    // Create num_thread threads running swap()
    for (i = 0; i < opt.num_threads; i++)
    {
        threads[i].thread_num = i;

        args[i].thread_num = i;
        args[i].buffer = &buffer;
        args[i].delay = opt.delay;
        args[i].iterations = opt.iterations;
        args[i].mutexs = mutexs;
        if (0 != pthread_create(&threads[i].thread_id, NULL,
                                swap, &args[i]))
        {
            printf("Could not create thread #%d", i);
            exit(1);
        }
    }

    // Wait for the threads to finish
    for (i = 0; i < opt.num_threads; i++)
        pthread_join(threads[i].thread_id, NULL);

    // Print the buffer
    printf("Buffer after:  ");
    qsort(buffer.data, opt.buffer_size, sizeof(int), (int (*)(const void *, const void *))cmp);
    print_buffer(buffer);

    printf("iterations: %d\n", get_count());
    for (i = 0; i < opt.buffer_size; i++)
        rec_mutex_destroy(&mutexs[i]);
    free(args);
    free(threads);
    free(buffer.data);
    free(mutexs);
}

int main(int argc, char **argv)
{
    struct options opt;

    // Default values for the options
    opt.num_threads = 10;
    opt.buffer_size = 10;
    opt.iterations = 100;
    opt.delay = 10;

    read_options(argc, argv, &opt);

    start_threads(opt);

    exit(0);
}
