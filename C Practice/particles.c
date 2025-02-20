#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <SDL2/SDL.h>

#define WINDOW_WIDTH 800
#define WINDOW_HEIGHT 600
#define PARTICLE_SIZE 5
#define MAX_PARTICLES 100

typedef struct {
    int x, y;
    int vx, vy;
    int lifetime;
} Particle;

void spawnParticle(Particle particles[], int index) {
    particles[index].x = rand() % WINDOW_WIDTH;
    particles[index].y = WINDOW_HEIGHT - PARTICLE_SIZE;
    particles[index].vx = -2 + rand() % 5; // Random x velocity between -2 and 2
    particles[index].vy = -10 - rand() % 5; // Random y velocity between -10 and -15
    particles[index].lifetime = 100 + rand() % 400; // Random lifetime between 100 and 500
}

int main() {
    if (SDL_Init(SDL_INIT_VIDEO) != 0) {
        printf("SDL initialization failed: %s\n", SDL_GetError());
        return 1;
    }

    SDL_Window* window = SDL_CreateWindow("Particle Animation", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, WINDOW_WIDTH, WINDOW_HEIGHT, 0);
    if (!window) {
        printf("Failed to create window: %s\n", SDL_GetError());
        SDL_Quit();
        return 1;
    }

    SDL_Renderer* renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_ACCELERATED);
    if (!renderer) {
        printf("Failed to create renderer: %s\n", SDL_GetError());
        SDL_DestroyWindow(window);
        SDL_Quit();
        return 1;
    }

    srand(time(NULL));

    Particle particles[MAX_PARTICLES];

    int i;
    for (i = 0; i < MAX_PARTICLES; i++) {
        spawnParticle(particles, i);
    }

    SDL_Event event;
    int quit = 0;

    while (!quit) {
        while (SDL_PollEvent(&event)) {
            if (event.type == SDL_QUIT) {
                quit = 1;
            }
        }

        // Move and update particles
        for (i = 0; i < MAX_PARTICLES; i++) {
            if (particles[i].lifetime > 0) {
                particles[i].x += particles[i].vx;
                particles[i].y += particles[i].vy;
                particles[i].lifetime--;
            } else {
                spawnParticle(particles, i); // Respawn particles when they reach the end of their lifetime
            }
        }

        // Clear the screen with a black color
        SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255);
        SDL_RenderClear(renderer);

        // Draw particles
        SDL_SetRenderDrawColor(renderer, 255, 255, 255, 255);
        for (i = 0; i < MAX_PARTICLES; i++) {
            if (particles[i].lifetime > 0) {
                SDL_Rect particleRect = { particles[i].x, particles[i].y, PARTICLE_SIZE, PARTICLE_SIZE };
                SDL_RenderFillRect(renderer, &particleRect);
            }
        }

        // Render the scene
        SDL_RenderPresent(renderer);

        // Add a delay to control the animation speed (you can adjust this value)
        SDL_Delay(10);
    }

    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(window);
    SDL_Quit();

    return 0;
}
