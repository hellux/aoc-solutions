#include <stdlib.h>
#include <stdio.h>

#include <SDL2/SDL.h>

#include "../intcode.h"

#define SCREEN_WIDTH 37
#define SCREEN_HEIGHT 23
#define PIXEL_SIZE 40

#define EMPTY 0
#define WALL 1
#define BLOCK 2
#define PADDLE 3
#define BALL 4

#define NEUTRAL 0
#define LEFT -1
#define RIGHT 1

const int COLORS[][3] = {
    {0x00, 0x00, 0x00}, /* empty */
    {0xff, 0x00, 0xff}, /* wall */
    {0xff, 0x00, 0x00}, /* block */
    {0x00, 0xff, 0x00}, /* paddle */
    {0xff, 0xff, 0xff}, /* ball */
};

void draw_tile(SDL_Renderer* renderer, int x, int y, int tile) {
    const int *col = COLORS[tile];
    SDL_SetRenderDrawColor(renderer, col[0], col[1], col[2], 0xff);

    SDL_Rect r;
    r.x = x*PIXEL_SIZE; r.w = PIXEL_SIZE;
    r.y = y*PIXEL_SIZE; r.h = PIXEL_SIZE;
    SDL_RenderFillRect(renderer, &r);
}

void play(struct context *cpu, int joystick,
          SDL_Renderer *renderer, int *score) {
    while (run_until_stop(cpu) == STATUS_WAIT_OUT) {
        int x = (int) take(cpu);
        int y = (int) take(cpu);
        int tile = (int) take(cpu);

        if (x == -1) {
            *score = tile;
        } else {
            draw_tile(renderer, x, y, tile);
        }
    }

    if (run_until_stop(cpu) == STATUS_WAIT_IN) {
        give(cpu, joystick);
    } 
}

int main(void) {
    SDL_Init(SDL_INIT_VIDEO);
    SDL_Window *window = SDL_CreateWindow(
        "AOC breakout",
        SDL_WINDOWPOS_UNDEFINED,
        SDL_WINDOWPOS_UNDEFINED,
        SCREEN_WIDTH*PIXEL_SIZE, SCREEN_HEIGHT*PIXEL_SIZE,
        SDL_WINDOW_SHOWN
    );
    SDL_Renderer *renderer = SDL_CreateRenderer(
        window, -1, SDL_RENDERER_ACCELERATED
    );

    struct context cpu;
    init_context(&cpu);
    cpu.mem[0] = 2;

    int quit = 0;
    int score = 0;
    SDL_Event event;

    while (!quit && cpu.status != STATUS_HALT)  {
        int joystick = NEUTRAL;
        while (SDL_PollEvent(&event) != 0) {
            switch (event.type) {
            case SDL_QUIT:
                quit = 1;
                break;
            case SDL_KEYDOWN:
                switch (event.key.keysym.scancode) {
                case SDL_SCANCODE_ESCAPE:
                    quit = 1;
                    break;
                case SDL_SCANCODE_LEFT:
                    joystick = LEFT;
                    break;
                case SDL_SCANCODE_RIGHT:
                    joystick = RIGHT;
                    break;
                }
            }
        }

        play(&cpu, joystick, renderer, &score);

        SDL_RenderPresent(renderer);
        SDL_UpdateWindowSurface(window);
        SDL_Delay(100);
    }

    return EXIT_SUCCESS;
}
