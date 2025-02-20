#include <iostream>
#include <random>
#include <ctime>
#include <SDL2/SDL.h>

int windowWidth = 640;
int windowHeight = 480;
int particleSize = 7;
int maxParticles = 130;

struct particleColor{
	Uint8 red, green, blue, alpha;
};

class particle{
	public:
		int x, y;
		int xVelocity, yVelocity;
		int lifetime;
		particleColor color;
};

void spawnParticle(particle particles[], int index){
	particles[index].x = rand() % windowWidth;
	particles[index].y = 0;

	particles[index].xVelocity = 2 - rand() % 5;
	particles[index].yVelocity = 5 + rand() % 10;

	particles[index].lifetime = 100 + rand() % 220;

	particles[index].color.red = rand() % 256;
	particles[index].color.green = rand() % 256;
	particles[index].color.blue = rand() % 256;
	particles[index].color.alpha = 255;
}

int main(){
    	if (SDL_Init(SDL_INIT_VIDEO) != 0) {
        	printf("SDL initialization failed: %s\n", SDL_GetError());
        	return 1;
    	}

    	SDL_Window* window = SDL_CreateWindow("Confetti", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, windowWidth, windowHeight, 0);
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
	
	SDL_SetRenderDrawColor(renderer,0,0,0,255);
	SDL_RenderClear(renderer);
	SDL_RenderPresent(renderer);
	
	srand(time(NULL));
	particle particles[maxParticles];
	
	for(int i = 0; i < maxParticles; i++){
		spawnParticle(particles, i);
	}

	//Main Loop
	SDL_Event event;
	bool main_loop = true;
	while(main_loop){
		
		//Quit Event
		while(SDL_PollEvent(&event)){
			if (event.type == SDL_QUIT){
				main_loop = false;
				break;}}

		//Update particle movement
		for(int i = 0; i < maxParticles; i++){
			if(particles[i].lifetime > 0){
				particles[i].x += particles[i].xVelocity;
				particles[i].y += particles[i].yVelocity;
				particles[i].lifetime--;
			} else {spawnParticle(particles, i);
			}
		}

		//Draw particles
		SDL_SetRenderDrawColor(renderer,0,0,0,255);
		SDL_RenderClear(renderer);
		for(int i = 0; i < maxParticles; i++){
			SDL_SetRenderDrawColor(renderer, particles[i].color.red, particles[i].color.green, particles[i].color.blue, particles[i].color.alpha);			
			if(particles[i].lifetime > 0){
				SDL_Rect particleRect = {particles[i].x, particles[i].y, particleSize, particleSize};
				SDL_RenderFillRect(renderer, &particleRect);
			}
		}
		
		//Present scene and delay
		SDL_RenderPresent(renderer);
		SDL_Delay(15);

	}

    	SDL_DestroyRenderer(renderer);
    	SDL_DestroyWindow(window);
    	SDL_Quit();

	return 0;
}
