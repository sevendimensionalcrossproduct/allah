#include <iostream>
#include <string>
#include <fstream>

std::string generateCode(void){
	std::string boilerplate = 
R"(#include <iostream>
#include <SDL2/SDL.h>
	
int windowWidth = 640;
int windowHeight = 480;

int main(){
	if (SDL_Init(SDL_INIT_VIDEO) != 0) {
		printf("SDL Initialization Failed: %s\n", SDL_GetError());
		return 1;
	}
		
	SDL_Window* window = SDL_CreateWindow("New Window!", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, windowWidth, windowHeight, 0); //Change window name here
	if(!window){
		printf("Failed to create window: %s\n", SDL_GetError());
		SDL_Quit();
		return 1;
	}

	SDL_Renderer* renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_ACCELERATED);
	if(!renderer){
		printf("Failed to create render: %s\n", SDL_GetError());
		SDL_DestroyWindow(window);
		SDL_Quit();
		return 1;
	}

	SDL_SetRenderDrawColor(renderer,0,0,0,255); //Change background color here
	SDL_RenderClear(renderer);
	SDL_RenderPresent(renderer);

	SDL_Event event;
	bool mainLoop = true;

	while(mainLoop){
		while(SDL_PollEvent(&event)){
			if(event.type == SDL_QUIT){
				mainLoop = false;
				break;
			}
		}
		//Write your SDL application here

	}
		
	SDL_DestroyRenderer(renderer);
	SDL_DestroyWindow(window);
	SDL_Quit();

	return 0;
})";

	return boilerplate;
}

bool fileExists(const std::string& filePath){
	std::ifstream fileThatMaybeExists(filePath);
	return fileThatMaybeExists.good();
}

bool confirmOverwrite(const std::string& filePath){
	std::cout << "Overwrite? (y/n, default n): ";
	
	std::string response;
	std::getline(std::cin, response);

	return response.empty() ? false : (response == "y" || response == "Y");
}

void saveFile(const std::string& directory, const std::string& fileName, const std::string& code){
	std::string filePath(directory + '/' + fileName + ".cpp");
	
	if(fileExists(filePath)){
		if (!confirmOverwrite(filePath)){
			std::cout << "Aborted." << std::endl;
			return;
		}
	}
	
	std::ofstream fileOutput(filePath);

	if (fileOutput.is_open()){
		fileOutput << code;
		fileOutput.close();

		if (directory == "."){
			std::cout << "File saved as " << fileName << ".cpp in current directory. Don't forget to link before compiling." << std::endl;
		} else { 
			std::cout << "File saved as " << fileName << ".cpp in directory: " << directory << std::endl;
			std::cout << "Don't forget to link before compiling." << std::endl;
		}

	} else {
		std::cerr << "Error: Unable to create/open file." <<std::endl;
	}
}


int main(int argc, char* argv[]){
	if (argc < 2 || argc > 3){
		std::cerr << "Syntax: " << argv[0] << " <directory> <filename> (leave directory string empty to write to current directory.)" << std::endl;
		return 1;
	}
	
	std::string directory = (argc == 3) ? argv[1] : ".";
	std::string fileName = argv[argc - 1];
	std::string code = generateCode();

	saveFile(directory, fileName, code);

	return 0;
}

