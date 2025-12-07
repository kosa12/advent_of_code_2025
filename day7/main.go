package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

func main() {
	file, _ := os.Open("input.txt")
	defer file.Close()

	var grid []string
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		grid = append(grid, scanner.Text())
	}

	activeCols := make(map[int]uint64)
	if len(grid) > 0 {
		idx := strings.Index(grid[0], "S")
		if idx != -1 {
			activeCols[idx] = 1
		}
	}

	for r := 0; r < len(grid); r++ {
		nextActiveCols := make(map[int]uint64)
		row := grid[r]
		for col, count := range activeCols {
			if col < 0 || col >= len(row) {
				continue
			}
			char := row[col]
			if char == '^' {
				if col-1 >= 0 {
					nextActiveCols[col-1] += count
				}
				if col+1 < len(row) {
					nextActiveCols[col+1] += count
				}
			} else {
				nextActiveCols[col] += count
			}
		}
		activeCols = nextActiveCols
	}

	var totalTimelines uint64
	for _, count := range activeCols {
		totalTimelines += count
	}
	fmt.Println(totalTimelines)
}
