#ifndef __OPTIONS_H__
#define __OPTIONS_H__

struct options {
	int num_threads;
	int buffer_size;
	int iterations;
	int delay;
	int print_wait;
};

int read_options(int argc, char **argv, struct options *opt);


#endif
