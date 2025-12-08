package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
	"strconv"
	"strings"
)

type Point struct {
	ID int
	X, Y, Z int
}

type Pair struct {
	P1, P2 int
	DistSq int
}

func main() {
	file, _ := os.Open("input.txt")
	defer file.Close()

	var points []Point
	scanner := bufio.NewScanner(file)
	idCounter := 0
	for scanner.Scan() {
		line := scanner.Text()
		parts := strings.Split(line, ",")
		if len(parts) != 3 {
			continue
		}
		x, _ := strconv.Atoi(parts[0])
		y, _ := strconv.Atoi(parts[1])
		z, _ := strconv.Atoi(parts[2])
		points = append(points, Point{ID: idCounter, X: x, Y: y, Z: z})
		idCounter++
	}

	var pairs []Pair
	for i := 0; i < len(points); i++ {
		for j := i + 1; j < len(points); j++ {
			p1 := points[i]
			p2 := points[j]
			dx := p1.X - p2.X
			dy := p1.Y - p2.Y
			dz := p1.Z - p2.Z
			distSq := dx*dx + dy*dy + dz*dz
			pairs = append(pairs, Pair{P1: i, P2: j, DistSq: distSq})
		}
	}

	sort.Slice(pairs, func(i, j int) bool {
		return pairs[i].DistSq < pairs[j].DistSq
	})

	parent := make([]int, len(points))
	for i := range parent {
		parent[i] = i
	}

	numComponents := len(points)

	var find func(int) int
	find = func(i int) int {
		if parent[i] != i {
			parent[i] = find(parent[i])
		}
		return parent[i]
	}

	union := func(i, j int) bool {
		rootI := find(i)
		rootJ := find(j)
		if rootI != rootJ {
			parent[rootI] = rootJ
			return true
		}
		return false
	}

	for _, p := range pairs {
		if union(p.P1, p.P2) {
			numComponents--
			if numComponents == 1 {
				p1 := points[p.P1]
				p2 := points[p.P2]
				fmt.Println(p1.X * p2.X)
				return
			}
		}
	}
}
