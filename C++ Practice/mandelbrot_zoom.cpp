#include <iostream>
#include <complex>
#include <numeric>
#include <SDL2/SDL.h>

using complex = std::complex<double>;

const int SCREEN_WIDTH = 640;
const int SCREEN_HEIGHT = 480;

//z = z**2 + c
int mandelbrot(const complex& c){
	complex z(0,0);

	for(int i = 0; i < 200; i++){
		z = std::pow(z,2) + c;
		if(std::norm(z) > 10){
			return i;
		}
	} 
	return 205;
}

int main(){
	if (SDL_Init(SDL_INIT_VIDEO) < 0) {
        std::cerr << "SDL initialization failed: " << SDL_GetError() << std::endl;
        return 1;
    	}

    	SDL_Window* window = SDL_CreateWindow("mandelcock", SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED, SCREEN_WIDTH, SCREEN_HEIGHT, SDL_WINDOW_SHOWN);
    	if (!window) {
        	std::cerr << "Window creation failed: " << SDL_GetError() << std::endl;
        	return 1;
    	}

    	SDL_Renderer* renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_ACCELERATED);
    	if (!renderer) {
        	std::cerr << "Renderer creation failed: " << SDL_GetError() << std::endl;
        	return 1;
    	}
	
	SDL_Surface* icon = SDL_LoadBMP("/home/serna/Documents/arsarasd.bmp");
	if (!icon) {
		std::cerr << "Failed to load icon: " << SDL_GetError() << std::endl;
	} else {
		SDL_SetWindowIcon(window, icon);
		SDL_FreeSurface(icon);
	}

	SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255); // Black background
    	SDL_RenderClear(renderer);
	
	SDL_RenderPresent(renderer);
	
	for(int x = 0; x < SCREEN_WIDTH; x++){
		for(int y = 0; y < SCREEN_HEIGHT; y++){

			double re = (x / static_cast<double>(SCREEN_WIDTH)) * 0.07 - 0.6 ;
			double im = (y / static_cast<double>(SCREEN_HEIGHT)) * 0.07- 0.6;
			complex c(re, im);

			int iter = mandelbrot(c);
			
			Uint8 r = 000 + iter * 5;
        		Uint8 g = 000 + iter * 5;
        		Uint8 b = 000 + iter * 255;
        		
			SDL_SetRenderDrawColor(renderer, r, g, b, 255);
        		SDL_RenderDrawPoint(renderer, x, y);

		}
	}

	SDL_RenderPresent(renderer); // Update the renderer with the point			
	
	SDL_Event event;
	bool main_loop = true;

	while(main_loop){
		while(SDL_PollEvent(&event)){
			if(event.type == SDL_QUIT){
				main_loop = false;
				break;
			}
			else if (event.type == SDL_KEYDOWN){
				if (event.key.keysym.sym == SDLK_ESCAPE){
					main_loop = false;
					break;
				}
			}
		}
	}
	
	SDL_DestroyRenderer(renderer);
	SDL_DestroyWindow(window);
	SDL_Quit();
    	
	return 0;
}
