#include <iostream>
#include <complex>
#include <vector>
#include <SDL2/SDL.h>
#include <SDL2/SDL_mixer.h>

using complex = std::complex<double>;

const int SCREEN_WIDTH = 640;
const int SCREEN_HEIGHT = 480;
const int NUM_FRAMES = 14;

//z = z**2 + c
int mandelbrot(const complex& c, double mandelbrotColor){
	complex z(0,0);

	for(int i = 0; i < 80; i++){
		z = std::pow(z,2) + c;
		if(std::norm(z) > 20){
			return i;
		}
	} 
	return mandelbrotColor;
}

void mandelbrotDraw(SDL_Renderer* renderer, double redValue, double greenValue, double blueValue, double mandelbrotColor){	
		for(int x = 0; x < SCREEN_WIDTH; x++){
		for(int y = 0; y < SCREEN_HEIGHT; y++){

			double re = (x / static_cast<double>(SCREEN_WIDTH)) * 2.0 - 1.5 ;
			double im = (y / static_cast<double>(SCREEN_HEIGHT)) * 2.0 - 1.0;
			complex c(re, im);

			int iter = mandelbrot(c, mandelbrotColor);
			
			Uint8 r = 000 + iter * redValue;
        		Uint8 g = 000 + iter * greenValue;
        		Uint8 b = 000 + iter * blueValue;
        		
			SDL_SetRenderDrawColor(renderer, r, g, b, 255);
        		SDL_RenderDrawPoint(renderer, x, y);

		}
	}
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
    
	if (SDL_Init(SDL_INIT_VIDEO | SDL_INIT_AUDIO) < 0) {
        	std::cerr << "SDL initialization failed: " << SDL_GetError() << std::endl;
        	return 1;
    	}

    	// Initialize SDL_mixer
    	if (Mix_OpenAudio(44100, MIX_DEFAULT_FORMAT, 2, 2048) < 0) {
        	std::cerr << "SDL_mixer initialization failed: " << Mix_GetError() << std::endl;
        	return 1;
    	}

    	// Load music
    	Mix_Music* music = Mix_LoadMUS("/home/serna/Downloads/rdfsfsdf.mp3");
    	if (!music) {
        	std::cerr << "Failed to load music: " << Mix_GetError() << std::endl;
        	return 1;
    	}
	
	SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255); // Black background
    	SDL_RenderClear(renderer);
    	SDL_RenderPresent(renderer); // Initial screen update	
				 
    	
	struct ColorCombination {
        	double red;
        	double green;
        	double blue;
		double mandelbrot;
 	};

    	ColorCombination colorCombinations[NUM_FRAMES] = {
        	{60.0, 2.0, 3.0, 0.0},
        	{16.0, 10.0, 30.0, 0.0},
        	{235.0, 105.0, 95.0, 175.0},
        	{1.0, 60.0, 7.0, 0.0},
		{255.0, 255.0, 255.0,0.0},
        	{255.0, 255.0, 205.0, 0.0},
		{3.0, 2.0, 1.0,0.0},
		{2.0,3.0,1.0,0.0},
		{1.0,2.0,3.0,255.0},
        	{65.0, 65.0, 65.0, 0.0},
        	{35.0, 65.0, 95.0, 65.0},
		{75.0,185.0,55.0,190.0},
		{230.0,230.0,230.0,0.0},
		{140.0,200.0,150.0,0.0},
		
    	};	
	
	std::vector<SDL_Surface*> mandelbrotFrames(NUM_FRAMES);

	//render & store in vector
	for (int i = 0; i < NUM_FRAMES; i++){
		SDL_Surface* surface = SDL_CreateRGBSurface(0, SCREEN_WIDTH, SCREEN_HEIGHT, 32, 0, 0, 0, 0);
		SDL_Renderer* tempRenderer = SDL_CreateSoftwareRenderer(surface);
		SDL_RenderClear(tempRenderer);

		mandelbrotDraw(tempRenderer, colorCombinations[i].red, colorCombinations[i].green, colorCombinations[i].blue, colorCombinations[i].mandelbrot);

		mandelbrotFrames[i] = surface;

		SDL_DestroyRenderer(tempRenderer);
	}

	int numCombinations = sizeof(colorCombinations) / sizeof(colorCombinations[0]);
	int currentCombination = 0;	

    	// Play the music in an infinite loop
    	Mix_VolumeMusic(64);
	Mix_PlayMusic(music, -1);

	SDL_Event event;
	bool main_loop = true;

    	while (main_loop) {
        	while (SDL_PollEvent(&event)) {
            		if (event.type == SDL_QUIT) {
                		main_loop = false;
                		break;
            		} else if (event.type == SDL_KEYDOWN) {
               	 		if (event.key.keysym.sym == SDLK_ESCAPE) {
                    			main_loop = false;
                    			break;
                		}
            		}
        	}

        	// Display the Mandelbrot animation by cycling through frames
        	for (int frameIndex = 0; frameIndex < NUM_FRAMES && main_loop; frameIndex++) {
            		SDL_BlitSurface(mandelbrotFrames[frameIndex], NULL, SDL_GetWindowSurface(window), NULL);
            		SDL_UpdateWindowSurface(window);
            		SDL_Delay(85); // Adjust the delay as needed for animation speed
        	}
    	}

    	// Free allocated surfaces
    	for (int i = 0; i < NUM_FRAMES; i++) {
        	SDL_FreeSurface(mandelbrotFrames[i]);
    	}

    Mix_FreeMusic(music);
    Mix_CloseAudio();


    	SDL_DestroyRenderer(renderer);
	SDL_DestroyWindow(window);
	SDL_Quit();	
	return 0;
}
