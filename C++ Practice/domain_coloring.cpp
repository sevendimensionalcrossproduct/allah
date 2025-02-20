#include <SDL2/SDL.h>
#include <cmath>
#include <complex>

 int WIDTH = 340;
 int HEIGHT = 250;
double zoom = 4.0; // Initial zoom level
double offsetX = 0.0, offsetY = 0.0; // Offsets for dragging
bool dragging = false;
int dragStartX, dragStartY;

void setPixel(SDL_Surface* surface, int x, int y, Uint32 color) {
    Uint32* pixels = (Uint32*)surface->pixels;
    if (x >= 0 && x < WIDTH && y >= 0 && y < HEIGHT) {
        pixels[y * surface->w + x] = color;
    }
}

Uint32 getColor(std::complex<double> z) {
    double angle = std::arg(z);
    double radius = std::abs(z);

    double normalizedAngle = -(angle + M_PI) / (2 * M_PI); // Shift angle to [0, 1]

    // Calculate RGB values based on the angle (hue)
    Uint8 r_color = static_cast<Uint8>((0.5 + 0.5 * -sin(normalizedAngle * 2 * M_PI + M_PI / 2)) * 255);
    Uint8 g_color = static_cast<Uint8>((0.5 + 0.5 * -sin(normalizedAngle * 2 * M_PI - 2 * M_PI / 3 + M_PI / 2)) * 255);
    Uint8 b_color = static_cast<Uint8>((0.5 + 0.5 * -sin(normalizedAngle * 2 * M_PI - 4 * M_PI / 3 + M_PI / 2)) * 255);
    
        // Blend towards white based on the radius
    double blendToWhiteFactor = 0.2 / (0.2 + radius); //change the 0.2 to change the rate at which the origin goes to white
    r_color = static_cast<Uint8>(r_color + blendToWhiteFactor * (255 - r_color));
    g_color = static_cast<Uint8>(g_color + blendToWhiteFactor * (255 - g_color));
    b_color = static_cast<Uint8>(b_color + blendToWhiteFactor * (255 - b_color));

        // Scale luminosity based on the radius
    double luminosityScale = 8.0 / (8.0 + radius); // change the 8.0 to change the rate at which it goes to black as it grows
    r_color = static_cast<Uint8>(r_color * luminosityScale);
    g_color = static_cast<Uint8>(g_color * luminosityScale);
    b_color = static_cast<Uint8>(b_color * luminosityScale);

    return SDL_MapRGB(SDL_AllocFormat(SDL_PIXELFORMAT_ARGB8888), r_color, g_color, b_color);
}

std::complex<double> transform(std::complex<double> z) {
        return std::complex<double>(1,0)*z;
}

void render(SDL_Surface* surface) {
    for (int x = 0; x < WIDTH; ++x) {
        for (int y = 0; y < HEIGHT; ++y) {
            double real = (x - WIDTH / 2.0 + offsetX) / (WIDTH / zoom);
            double imag = (y - HEIGHT / 2.0 + offsetY) / (HEIGHT / zoom);
            std::complex<double> z(real, imag);
            
            // Apply the transformation
            std::complex<double> transformedZ = transform(z);

            // Get the color based on the transformed complex number
            Uint32 color = getColor(transformedZ);
            setPixel(surface, x, y, color);
        }
    }
}

int main(int argc, char* argv[]) {
    SDL_Init(SDL_INIT_VIDEO);
    SDL_Window* window = SDL_CreateWindow("Complex Plane Visualization",
                               SDL_WINDOWPOS_CENTERED,
                               SDL_WINDOWPOS_CENTERED,
                               WIDTH,
                               HEIGHT,
                               SDL_WINDOW_RESIZABLE);    SDL_Surface* surface = SDL_GetWindowSurface(window);

    render(surface);
    SDL_UpdateWindowSurface(window);

    SDL_Event event;
    bool running = true;
    while (running) {
        while (SDL_PollEvent(&event)) {
            if (event.type == SDL_QUIT) {
                running = false;
            } else if (event.type == SDL_MOUSEWHEEL) {
                if (event.wheel.y > 0) {
                    zoom *= 0.9; // Zoom in
                } else if (event.wheel.y < 0) {
                    zoom *= 1.1; // Zoom out
                }
                render(surface);
                SDL_UpdateWindowSurface(window);
            } else if (event.type == SDL_MOUSEBUTTONDOWN) {
                if (event.button.button == SDL_BUTTON_LEFT) {
                    dragging = true;
                    dragStartX = event.button.x;
                    dragStartY = event.button.y;
                }
            } else if (event.type == SDL_MOUSEBUTTONUP) {
                if (event.button.button == SDL_BUTTON_LEFT) {
                    dragging = false;
                }
            } else if (event.type == SDL_MOUSEMOTION && dragging) {
                int dx = -(event.motion.x - dragStartX);
                int dy = -(event.motion.y - dragStartY);
                // Reverse direction and increase speed
                offsetX += dx * (zoom / WIDTH * 8.0);
                offsetY += dy * (zoom / HEIGHT * 14.0);
                dragStartX = event.motion.x;
                dragStartY = event.motion.y;
                render(surface);
                SDL_UpdateWindowSurface(window);
            } else if (event.type == SDL_WINDOWEVENT) {
            if (event.window.event == SDL_WINDOWEVENT_RESIZED) {
                WIDTH = event.window.data1;  // Update WIDTH
                HEIGHT = event.window.data2; // Update HEIGHT
                surface = SDL_GetWindowSurface(window); // Re-obtain the surface
                render(surface); // Re-render the new surface
                SDL_UpdateWindowSurface(window); // Update the window
            }}
        }
    }

    SDL_DestroyWindow(window);
    SDL_Quit();
    return 0;
}
