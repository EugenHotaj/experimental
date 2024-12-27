#include <SDL2/SDL.h>
#include <stdio.h>
#include <time.h>
#include "plot.c"


void print_doubles(double values[], int n_values) {
	printf("[ ");
	for (int i = 0; i < n_values; i++) {
		printf("%f, ", values[i]);
	}
	printf("]\n");
}


void test_draw_gaussian() {
    SDL_Init(SDL_INIT_VIDEO);
    SDL_Window* window = SDL_CreateWindow(
        "Histogram",
        SDL_WINDOWPOS_UNDEFINED,
        SDL_WINDOWPOS_UNDEFINED,
        SCREEN_WIDTH,
        SCREEN_HEIGHT,
        SDL_WINDOW_SHOWN
    );
    SDL_Renderer *renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
	clear_screen(renderer);

	int n = 10000, n_bins = 50;
	double samples[n];
	for (int i = 0; i < n; i++) {
		samples[i] = normal();
	}
	draw_histogram(renderer, samples, n, n_bins);
	show_and_wait(renderer);

    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(window);
    SDL_Quit();
} 


void test_draw_gaussian_mixture() {
    SDL_Init(SDL_INIT_VIDEO);
    SDL_Window* window = SDL_CreateWindow(
        "Histogram",
        SDL_WINDOWPOS_UNDEFINED,
        SDL_WINDOWPOS_UNDEFINED,
        SCREEN_WIDTH,
        SCREEN_HEIGHT,
        SDL_WINDOW_SHOWN
    );
    SDL_Renderer *renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
	clear_screen(renderer);

	// Samples from a mixture of two Gaussians.
	int n = 10000, n_bins = 50;
	double mixture[n];
	for (int i = 0; i < n; i++) {
		if (uniform() < 0.3) {
			mixture[i] = normal() * 1.0 + 2.38;
		} else {
			mixture[i] = normal() * 1.2 - 1.3;
		}
	}
	draw_histogram(renderer, mixture, n, n_bins);
	show_and_wait(renderer);

    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(window);
    SDL_Quit();
}


void test_draw_linear_regression() {
    SDL_Init(SDL_INIT_VIDEO);
    SDL_Window* window = SDL_CreateWindow(
        "Histogram",
        SDL_WINDOWPOS_UNDEFINED,
        SDL_WINDOWPOS_UNDEFINED,
        SCREEN_WIDTH,
        SCREEN_HEIGHT,
        SDL_WINDOW_SHOWN
    );
    SDL_Renderer *renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
	clear_screen(renderer);

	draw_axes(renderer, -0.1, 1.1, -1, 5);

	// Sample some points from y = 3x + 1 with added Gaussian noise in N(0, 0.2).
	int n = 10;
	double x[n], y[n];
	for (int i = 0; i < n; i++){
		x[i] = uniform();
		y[i] = 3 * x[i] + 1;
		y[i] += normal() * 0.2;
	}
	draw_scatter_plot(renderer, x, y, n, -0.1, 1.1, -1, 5);

	// Draw the line y = 3x + 1.
	int n_line = 1000;
	double line_x[n_line], line_y[n_line];
	for (int i = 0; i < n_line; i++) {
		line_x[i] = (double) i / n_line;
		line_y[i] = 3 * line_x[i] + 1;
	}
	draw_points(renderer, line_x, line_y, n_line, -0.1, 1.1, -1, 5);

	show_and_wait(renderer);
    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(window);
    SDL_Quit();
}


void test_softmax() {
	int n_values = 15;
	double values[n_values];
	for (int i = 0; i < n_values; i++) {
		values[i] = normal();
	}
	print_doubles(values, n_values);
	softmax(values, n_values);
	print_doubles(values, n_values);
	double sum_p = 0.0;
	for (int i = 0; i < n_values; i++) {
		sum_p += values[i];
	}
	printf("Total: %f\n", sum_p);	
}


int main(void) {
	srand(time(NULL));
	// test_draw_gaussian();
	// test_draw_gaussian_mixture();
	// test_draw_linear_regression();

	test_softmax();

	return 0;
}