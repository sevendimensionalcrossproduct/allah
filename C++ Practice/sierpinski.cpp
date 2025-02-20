#include <SDL2/SDL.h>
#include <iostream>

const int SCREEN_WIDTH = 640;
const int SCREEN_HEIGHT = 480;
const int DEPTH = 0; // Adjust the recursion depth as needed

// Function to draw the Sierpinski triangle
void drawSierpinski(SDL_Renderer* renderer, int x, int y, int size, int depth) {
    if (depth == 0) {
        // Draw a filled triangle
        SDL_SetRenderDrawColor(renderer, 255, 255, 255, 255); // White color
        SDL_RenderDrawLine(renderer, x, y, x + size, y);
        SDL_RenderDrawLine(renderer, x, y, x + size / 2, y + size);
        SDL_RenderDrawLine(renderer, x + size / 2, y + size, x + size, y);
    } else {
        // Recursively draw three smaller triangles
        int halfSize = size / 2;
        drawSierpinski(renderer, x, y, halfSize, depth - 1);
        drawSierpinski(renderer, x + halfSize, y, halfSize, depth - 1);
        drawSierpinski(renderer, x + halfSize / 2, y + halfSize, halfSize, depth - 1);
    }
}

int main(int argc, char* argv[]) {
    if (SDL_Init(SDL_INIT_VIDEO) < 0) {
        std::cerr << "SDL initialization failed: " << SDL_GetError() << std::endl;
        return 1;
    }

    SDL_Window* window = SDL_CreateWindow("Sierpinski Triangle", SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED, SCREEN_WIDTH, SCREEN_HEIGHT, SDL_WINDOW_SHOWN);
    if (!window) {
        std::cerr << "Window creation failed: " << SDL_GetError() << std::endl;
        return 1;
    }

    SDL_Renderer* renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_ACCELERATED);
    if (!renderer) {
        std::cerr << "Renderer creation failed: " << SDL_GetError() << std::endl;
        return 1;
    }

    SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255); // Black background
    SDL_RenderClear(renderer);

    drawSierpinski(renderer, 10, 10, SCREEN_WIDTH - 20, DEPTH);

    SDL_RenderPresent(renderer);

    bool quit = false;
    SDL_Event e;

    while (!quit) {
        while (SDL_PollEvent(&e) != 0) {
            if (e.type == SDL_QUIT) {
                quit = true;
            }
        }
    }

    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(window);
    SDL_Quit();

    return 0;
}
