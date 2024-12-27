#include <SDL2/SDL.h>
#include "math.c"

//Screen dimension constants.
const int SCREEN_WIDTH = 640;
const int SCREEN_HEIGHT = 480;

const int MAIN_RGBA[] = {0, 120, 255, 255};
const int ACCENT_RGBA[] = {0, 60, 120, 255};


// TODO(eugenhotaj): We can speed this up by using binary search.
double* _histogram(double data[], int n_data, int n_bins) {
	double* bins = calloc(n_bins, sizeof(double));
	double x_min = min(data, n_data);
	double x_max = max(data, n_data);
	double step_size = (x_max - x_min) / n_bins;
	for (int i = 0; i < n_data; i++) {
		for (int j = 0; j < n_bins; j++) {
			double boundary = x_min + (j+1) * step_size;
			if (data[i] < boundary || j == n_bins - 1) {
				bins[j] += 1.0 / n_data;
				break;
			}
		}
	}
	return bins;
}


void draw_histogram(SDL_Renderer *renderer, double data[], int n_data, int n_bins) { 
	double* bins = _histogram(data, n_data, n_bins);
	double y_max = max(bins, n_bins);

    double width = (double) SCREEN_WIDTH / n_bins;
    for (int i = 0; i < n_bins; i++) {
    	// Draw bar.
        int height = bins[i] / y_max * SCREEN_HEIGHT;
        SDL_Rect rect = {
            .x = width * i,
            .y = SCREEN_HEIGHT - height,
            .w = width - 1,  // Leave 1 pixel gap between bars
            .h = height
        };
        SDL_SetRenderDrawColor(renderer, MAIN_RGBA[0], MAIN_RGBA[1], MAIN_RGBA[2], MAIN_RGBA[3]);
        SDL_RenderFillRect(renderer, &rect);
        
        // Draw outline.
        SDL_SetRenderDrawColor(renderer, ACCENT_RGBA[0], ACCENT_RGBA[1], ACCENT_RGBA[2], ACCENT_RGBA[3]);
        SDL_RenderDrawRect(renderer, &rect);
    }
}


void draw_scatter_plot(
	SDL_Renderer *renderer, 
	double x[], 
	double y[], 
	int n, 
	double x_min, 
	double x_max, 
	double y_min,
	double y_max
) {
    SDL_SetRenderDrawColor(renderer, MAIN_RGBA[0], MAIN_RGBA[1], MAIN_RGBA[2], MAIN_RGBA[3]);
    for (int i = 0; i < n; i++) {
        int px = (x[i] - x_min) / (x_max - x_min) * SCREEN_WIDTH;
        int py = SCREEN_HEIGHT - (y[i] - y_min) / (y_max - y_min) * SCREEN_HEIGHT;
        
        SDL_Rect point = {px - 2, py - 2, 4, 4};
        SDL_RenderFillRect(renderer, &point);
    }
}


void draw_points(
	SDL_Renderer *renderer, 
	double x[], 
	double y[], 
	int n, 
	double x_min, 
	double x_max, 
	double y_min, 
	double y_max
) {
    SDL_SetRenderDrawColor(renderer, MAIN_RGBA[0], MAIN_RGBA[1], MAIN_RGBA[2], MAIN_RGBA[3]);
    for (int i = 0; i < n; i++) {
        int px = (x[i] - x_min) / (x_max - x_min) * SCREEN_WIDTH;
        int py = SCREEN_HEIGHT - (y[i] - y_min) / (y_max - y_min) * SCREEN_HEIGHT;
        
        SDL_Rect point = {px - 1, py - 1, 2, 2};
        SDL_RenderFillRect(renderer, &point);
    }

}


void draw_axes(SDL_Renderer *renderer, double x_min, double x_max, double y_min, double y_max) {
    int tick_half_size = 3;
    SDL_SetRenderDrawColor(renderer, 64, 64, 64, 255);

    // Draw x-axis.
    int y_origin = SCREEN_HEIGHT - (0 - y_min) / (y_max - y_min) * SCREEN_HEIGHT;
    SDL_RenderDrawLine(renderer, 0, y_origin, SCREEN_WIDTH, y_origin);

    // Draw y-axis.
    int x_origin = (0 - x_min) / (x_max - x_min) * SCREEN_WIDTH;
    SDL_RenderDrawLine(renderer, x_origin, 0, x_origin, SCREEN_HEIGHT);

    // Draw tick marks on x-axis.
    int num_ticks = 10;
    for (int i = -num_ticks; i <= num_ticks; i++) {
        int x = x_origin + i * (SCREEN_WIDTH - x_origin) / num_ticks;
        SDL_RenderDrawLine(renderer, x, y_origin - tick_half_size, x, y_origin + tick_half_size);
    }

    // Draw tick marks on y-axis.
    for (int i = -num_ticks; i <= num_ticks; i++) {
        int y = y_origin - i * y_origin / num_ticks;
        SDL_RenderDrawLine(renderer, x_origin - tick_half_size, y, x_origin + tick_half_size, y);
    }
}


void clear_screen(SDL_Renderer *renderer) {
    SDL_SetRenderDrawColor(renderer, 255, 255, 255, 255);
    SDL_RenderClear(renderer);
}


void show_and_wait(SDL_Renderer *renderer) {
	SDL_RenderPresent(renderer);
    SDL_Event e;
    int quit = 0;
    while (quit == 0) {
        while (SDL_PollEvent(&e)) {
            if (e.type == SDL_QUIT) {
                quit = 1;
            }
        }
    }
	clear_screen(renderer);
}