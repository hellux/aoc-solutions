package main

import (
    "fmt"
    "github.com/veandco/go-sdl2/sdl"
    "math"
    "image"
)

func power_level(x, y, serial int) int {
    rack_id := x + 10
    return (((rack_id*y+serial)*rack_id) % 1000)/100 - 5
}

func scale(renderer *sdl.Renderer, bounds image.Rectangle) float64 {
    width_win, height_win, _ := renderer.GetOutputSize()
    return math.Min(float64(width_win)/float64(bounds.Dx()+1),
                    float64(height_win)/float64(bounds.Dy()+1))
}

func drawGrid(renderer *sdl.Renderer, level func(int, int) int, bounds image.Rectangle) {
    scale := scale(renderer, bounds)
    for y := bounds.Min.Y; y <= bounds.Max.Y; y++ {
        y_start := int32(float64(y-bounds.Min.Y)*scale)
        y_end := int32((float64(y-bounds.Min.Y)+1)*scale)
        for x := bounds.Min.X; x <= bounds.Max.X; x++ {
            x_start := int32(float64(x-bounds.Min.X)*scale)
            x_end := int32((float64(x-bounds.Min.X)+1)*scale)
            rect := &sdl.Rect{x_start, y_start, x_end-x_start, y_end-y_start}
            power := level(x, y)
            r := uint8(0)
            g := uint8(0)
            if power > 0 {
                g = uint8(255*power/4)
            } else {
                r = uint8(255*-power/5)
            }

            renderer.SetDrawColor(r, g, 0, 255)
            renderer.FillRect(rect)
        }
    }
}

func main() {
    serial := 0

	sdl.Init(sdl.INIT_EVENTS);
	window, _ := sdl.CreateWindow("msg", 0, 0, 800, 600, sdl.WINDOW_RESIZABLE)
    renderer, _ := sdl.CreateRenderer(window, -1, sdl.RENDERER_ACCELERATED);

    quit := false
    bounds := image.Rect(1, 1, 300, 300)
	for !quit {
		event := sdl.WaitEvent()
        switch t := event.(type) {
        case *sdl.QuitEvent:
            quit = true
        case *sdl.MouseButtonEvent:
            if t.State == sdl.PRESSED {
                scale := int32(scale(renderer, bounds))
                x := bounds.Min.X + int(t.X/scale)
                y := bounds.Min.Y + int(t.Y/scale)
                fmt.Printf("(%d %d): %d\n", x, y, power_level(x, y, serial))
            }
        case *sdl.KeyboardEvent:
            if t.State == sdl.PRESSED {
                key := sdl.GetKeyName(sdl.GetKeyFromScancode(t.Keysym.Scancode))
                switch key {
                case "S":
                    fmt.Printf("serial: ")
                    fmt.Scanf("%d", &serial)
                case "B":
                    fmt.Printf("(x, y)-(x, y): ")
                    fmt.Scanf("%d %d %d %d", &bounds.Min.X, &bounds.Min.Y,
                                             &bounds.Max.X, &bounds.Max.Y)
                }
            }
        }

        renderer.SetDrawColor(0, 0, 0, 255)
        renderer.Clear()
        drawGrid(renderer, func(x, y int) int {
            return power_level(x, y, serial)
        }, bounds)
        renderer.Present()

	}
}
