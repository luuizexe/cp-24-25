#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "options.h"
#include "sem.h"

#define SILLAS 5
struct thread_info
{
    pthread_t thread_id;
    int thread_num;
};

// Variables comunes entre clientes y barberos
struct info
{
    int *finish; // Flag para que terminen los barberos
    sem_t *barbers;
    sem_t *customers;
    sem_t *freeChairs;
};
struct args
{
    int id; // Identificador de barberos/clientes
    int cut_time;
    struct info info; // Estructura con datos comunes
};

void *customer(void *ptr)
{
    struct args *args = ptr;
    if (sem_tryp(args->info.freeChairs) == 0)
    {
        sem_v(args->info.customers);
        sem_p(args->info.barbers);
        printf("Cliente %d recibe corte de pelo \n", args->id);
        sem_v(args->info.freeChairs);
        usleep(args->cut_time);
    }
    else
    {
        printf("Cliente %d encuentra la sala de espera llena y se marcha\n", args->id);
    }
    return NULL;
}

void *barber(void *ptr)
{
    struct args *args = ptr;
    while (1)
    {
        sem_v(args->info.barbers);
        sem_p(args->info.customers);
        if (*(args->info.finish))
            break;
        printf("Barbero %d le corta el pelo a un cliente \n", args->id);
        usleep(args->cut_time);
    }
    return NULL;
}

void start_threads(struct options opt)
{
    int i;
    struct thread_info *barber_threads;
    struct thread_info *customer_threads;
    struct args *barber_args;
    struct args *customer_args;
    struct info shared;
    srand(time(NULL));

    // Inicializamos las variables comunes entre clientes y barberos
    shared.barbers = malloc(sizeof(sem_t));
    shared.customers = malloc(sizeof(sem_t));
    shared.freeChairs = malloc(sizeof(sem_t));
    shared.finish = malloc(sizeof(int));
    *(shared.finish) = 0;
    sem_init(shared.barbers, 0);
    sem_init(shared.customers, 0);
    sem_init(shared.freeChairs, SILLAS);

    printf("Creating %d barber threads\n", opt.barbers);
    printf("Creating %d customer threads\n", opt.customers);
    barber_threads = malloc(sizeof(struct thread_info) * (opt.barbers));
    customer_threads = malloc(sizeof(struct thread_info) * (opt.customers));
    barber_args = malloc(sizeof(struct args) * opt.barbers);
    customer_args = malloc(sizeof(struct args) * opt.customers);

    if (barber_threads == NULL || customer_threads == NULL || barber_args == NULL || customer_args == NULL)
    {
        printf("Not enough memory\n");
        exit(1);
    }

    // Creamos los hilos de los barberos
    for (i = 0; i < opt.barbers; i++)
    {
        barber_args[i].info = shared;
        barber_args[i].cut_time = opt.cut_time;
        barber_args[i].id = i;

        if (0 != pthread_create(&barber_threads[i].thread_id, NULL, barber, &barber_args[i]))
        {
            printf("No se pudo crear el barbero #%d\n", i);
            exit(1);
        }
    }

    // Creamos los hilos de los clientes
    for (i = 0; i < opt.customers; i++)
    {
        customer_args[i].info = shared;
        customer_args[i].cut_time = opt.cut_time;
        customer_args[i].id = i;

        if (0 != pthread_create(&customer_threads[i].thread_id, NULL, customer, &customer_args[i]))
        {
            printf("No se pudo crear el cliente #%d\n", i);
            exit(1);
        }
    }

    for (i = 0; i < opt.customers; i++)
        pthread_join(customer_threads[i].thread_id, NULL);

    // NO hace falta un mutex para esta variable porque solo se va a cambiar una vez en todo el programa
    *(shared.finish) = 1;

    for (i = 0; i < opt.barbers; i++)
        sem_v(shared.customers);

    for (i = 0; i < opt.barbers; i++)
        pthread_join(barber_threads[i].thread_id, NULL);

    sem_destroy(shared.barbers);
    sem_destroy(shared.customers);
    sem_destroy(shared.freeChairs);

    free(shared.finish);
    free(shared.barbers);
    free(shared.customers);
    free(shared.freeChairs);
    free(customer_threads);
    free(barber_threads);
    free(barber_args);
    free(customer_args);
}

int main(int argc, char **argv)
{
    struct options opt;

    // Default values for the options
    opt.barbers = 5;
    opt.customers = 50;
    opt.cut_time = 100;

    read_options(argc, argv, &opt);

    start_threads(opt);
    return 0;
}
