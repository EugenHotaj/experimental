#include <math.h>
#include <stdlib.h>


double max(double values[], int n_values) {
	double max_ = values[0];
	for (int i = 1; i < n_values; i++) {
		if (values[i] > max_) {
			max_ = values[i];
		}
	}
	return max_;
}


double min(double values[], int n_values) {
	double min_ = values[0];
	for (int i = 1; i < n_values; i++) {
		if (values[i] < min_) {
			min_= values[i];
		}
	}
	return min_;
}


double sqr(double x) {
	return x * x;
}


// Generates a sample from the uniform distribution. 
// Assumes that srand() has already been called.
double uniform() {
	return (double) rand() / RAND_MAX;	
}


// Generates a sample from the standard normal distribution.
// Based on the implementation from Numerical Recipies and 
// https://dl.acm.org/doi/pdf/10.1145/138351.138364.
double normal() {
	double u, v, x, y, q;
	do {
		u = uniform();
		v = 1.7156 * (uniform() - 0.5);
		x = u - 0.449871;
		y = fabs(v) + 0.386595;
		q = sqr(x) + y * (0.19600 * y - 0.25472 * x);
	} while (q > 0.27597 
		&& (q > 0.27846 || sqr(v) > -4.0 * log(u) * sqr(u)));
	return v / u;
}


// Computes the safe softmax of values[] inplace.
// NOTE: This is a slow version of softmax which uses 3 loads from memory.
// It's mostly here to contrast with the fast, online version below which 
// uses only 2 loads.
void softmax_slow(double values[], int n_values) {
    double max_value = max(values, n_values);

    double Z = 0.0;
    for (int i = 0; i < n_values; i++) {
        Z += exp(values[i] - max_value);
    }

    for (int i = 0; i < n_values; i++) {
        values[i] = exp(values[i] - max_value) / Z;
    }
}


// Computes the safe softmax of values[] inplace.
void softmax(double values[], int n_values) {
    double max_value = -INFINITY;
    double Z = 0;
    for (int i = 0; i < n_values; i++) {
        double prev_max = max_value;
        if (values[i] > max_value) {
            max_value = values[i];
        }
        Z *= exp(prev_max - max_value);
        Z += exp(values[i] - max_value);
    }

    for (int i = 0; i < n_values; i++) {
        values[i] = exp(values[i] - max_value) / Z;
    }
}