package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type Point struct {
	X, Y int
}

func abs(x int) int {
	if x < 0 {
		return -x
	}
	return x
}

func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}

func max(a, b int) int {
	if a > b {
		return a
	}
	return b
}

type Edge struct {
	P1, P2 Point
}

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		fmt.Println("Error opening file:", err)
		return
	}
	defer file.Close()

	var points []Point
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		parts := strings.Split(line, ",")
		if len(parts) != 2 {
			continue
		}
		x, _ := strconv.Atoi(strings.TrimSpace(parts[0]))
		y, _ := strconv.Atoi(strings.TrimSpace(parts[1]))
		points = append(points, Point{X: x, Y: y})
	}

	maxArea := 0
	n := len(points)

	var edges []Edge
	for i := 0; i < n; i++ {
		p1 := points[i]
		p2 := points[(i+1)%n]
		edges = append(edges, Edge{P1: p1, P2: p2})
	}

	for i := 0; i < n; i++ {
		for j := i + 1; j < n; j++ {
			p1 := points[i]
			p2 := points[j]

			minX := min(p1.X, p2.X)
			maxX := max(p1.X, p2.X)
			minY := min(p1.Y, p2.Y)
			maxY := max(p1.Y, p2.Y)

			width := maxX - minX + 1
			height := maxY - minY + 1
			area := width * height

			if area <= maxArea {
				continue
			}

			// Check if valid
			if isValid(minX, maxX, minY, maxY, edges) {
				maxArea = area
			}
		}
	}

	fmt.Println(maxArea)
}

func isValid(minX, maxX, minY, maxY int, edges []Edge) bool {
	if minX < maxX && minY < maxY {
		for _, e := range edges {

			if e.P1.X == e.P2.X {
				edgeX := e.P1.X

				if edgeX > minX && edgeX < maxX {

					eMinY := min(e.P1.Y, e.P2.Y)
					eMaxY := max(e.P1.Y, e.P2.Y)

					overlapStart := max(minY, eMinY)
					overlapEnd := min(maxY, eMaxY)
					
					if overlapStart < overlapEnd {
						return false
					}
				}
			} else {
				edgeY := e.P1.Y

				if edgeY > minY && edgeY < maxY {

					eMinX := min(e.P1.X, e.P2.X)
					eMaxX := max(e.P1.X, e.P2.X)
					
					overlapStart := max(minX, eMinX)
					overlapEnd := min(maxX, eMaxX)
					
					if overlapStart < overlapEnd {
						return false 
					}
				}
			}
		}
	}

	midX := float64(minX+maxX) / 2.0
	midY := float64(minY+maxY) / 2.0

	intersections := 0
	for _, e := range edges {

		
		if e.P1.X == e.P2.X {
			minEy := min(e.P1.Y, e.P2.Y)
			maxEy := max(e.P1.Y, e.P2.Y)
			
			if midY >= float64(minEy) && midY < float64(maxEy) {

				if float64(e.P1.X) > midX {
					intersections++
				}
			}
		}
	}

	return intersections%2 != 0
}
