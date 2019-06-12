package main

import (
    "io/ioutil"
    "os"
    "strings"
    "fmt"
    "image"
    "github.com/veandco/go-sdl2/sdl"
    "math"
)

type pixel struct {
    pos image.Point
    vel image.Point
}

const MOVE_RATIO = 10
const RESIZE_FACTOR = 1.1

func drawPoints(renderer *sdl.Renderer, points []pixel,
                center_x, center_y, scale float64) {
    width, height, _ := renderer.GetOutputSize()
    for _, p := range points {
        x_start := width/2 + int32((float64(p.pos.X)-center_x)*scale)
        y_start := height/2 + int32((float64(p.pos.Y)-center_y)*scale)
        x_end := width/2 + int32((float64(p.pos.X)+1-center_x)*scale)
        y_end := height/2 + int32((float64(p.pos.Y)+1-center_y)*scale)
        if x_start == x_end {
            x_end++
        }
        if y_start == y_end {
            y_end++
        }
        rect := &sdl.Rect{x_start, y_start, x_end-x_start, y_end-y_start}
        renderer.FillRect(rect)
    }
}

func movePoints(points []pixel, time int) []pixel {
    points_copy := make([]pixel, len(points))
    copy(points_copy, points)
    for i := 0; i < len(points); i++ {
        p := &points_copy[i]
        p.pos.X += time*p.vel.X
        p.pos.Y += time*p.vel.Y
    }
    return points_copy
}

const MARGIN = 1.3

func center(renderer *sdl.Renderer, points []pixel) (float64,
                                                     float64, float64) {
    min := image.Point{points[0].pos.X, points[0].pos.Y}
    max := image.Point{points[0].pos.X, points[0].pos.Y}
    for _, p := range points {
        if p.pos.X < min.X {
            min.X = p.pos.X
        } else if p.pos.X > max.X {
            max.X = p.pos.X
        }
        if p.pos.Y < min.Y {
            min.Y = p.pos.Y
        } else if p.pos.Y > max.Y {
            max.Y = p.pos.Y
        }
    }

    center_x := float64(max.X+min.X)/2
    center_y := float64(max.Y+min.Y)/2
    width, height, _ := renderer.GetOutputSize()
    scale := math.Min(float64(width)/float64(max.X-min.X),
                      float64(height)/float64(max.Y-min.Y))/MARGIN

    return center_x, center_y, scale
}

func main() {
    bytes, _ := ioutil.ReadAll(os.Stdin)
    lines := strings.Split(string(bytes), "\n")
    lines = lines[:len(lines)-1] // remove empty string
    n := len(lines)

    pixels := make([]pixel, n)
    for i, line := range lines {
        pos := image.Point{0, 0}
        vel := image.Point{0, 0}
        fmt.Sscanf(line, "position=<%d,%d> velocity=<%d,%d>",
                   &pos.X, &pos.Y, &vel.X, &vel.Y)
        pixels[i] = pixel{pos, vel}
    }

	sdl.Init(sdl.INIT_EVENTS);
	window, _ := sdl.CreateWindow("msg", 0, 0, 800, 600, sdl.WINDOW_RESIZABLE)
    renderer, _ := sdl.CreateRenderer(window, -1, sdl.RENDERER_ACCELERATED);

    center_x, center_y, scale := center(renderer, pixels)
    time := 0
    time_speed := 1
    quit := false
	for !quit {
		event := sdl.WaitEvent()
        switch t := event.(type) {
        case *sdl.QuitEvent:
            quit = true
        case *sdl.KeyboardEvent:
            if t.State == sdl.PRESSED {
                key := sdl.GetKeyName(sdl.GetKeyFromScancode(t.Keysym.Scancode))
                switch key {
                case "J":
                    center_y += MOVE_RATIO/scale
                case "K":
                    center_y -= MOVE_RATIO/scale
                case "L":
                    center_x += MOVE_RATIO/scale
                case "H":
                    center_x -= MOVE_RATIO/scale
                case "+":
                    scale *= RESIZE_FACTOR
                case "-":
                    scale /= RESIZE_FACTOR
                case "Up":
                    time_speed *= 2
                case "Down":
                    time_speed /= 2
                    if time_speed < 1 {
                        time_speed = 1
                    }
                case "Return":
                    time += time_speed
                    fmt.Println(time)
                case "Backspace":
                    time -= time_speed
                    fmt.Println(time)
                case "G":
                    center_x, center_y, scale =
                        center(renderer, movePoints(pixels, time))
                }
            }
        }

        renderer.SetDrawColor(0, 0, 0, 255)
        renderer.Clear()
        renderer.SetDrawColor(255, 255, 255, 255)
        drawPoints(renderer, movePoints(pixels, time),
                   center_x, center_y, scale)
        renderer.Present()
	}
}
