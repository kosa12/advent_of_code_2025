package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		fmt.Println("Error opening file:", err)
		return
	}
	defer file.Close()

	adj := make(map[string][]string)
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		parts := strings.Split(line, ":")
		if len(parts) != 2 {
			continue
		}
		node := strings.TrimSpace(parts[0])
		neighborsStr := strings.TrimSpace(parts[1])
		if neighborsStr == "" {
			continue
		}
		neighbors := strings.Split(neighborsStr, " ")
		adj[node] = neighbors
	}


	nSvrDac := countPaths("svr", "dac", adj, make(map[string]int))
	nDacFft := countPaths("dac", "fft", adj, make(map[string]int))
	nFftOut := countPaths("fft", "out", adj, make(map[string]int))
	
	paths1 := nSvrDac * nDacFft * nFftOut

	nSvrFft := countPaths("svr", "fft", adj, make(map[string]int))
	nFftDac := countPaths("fft", "dac", adj, make(map[string]int))
	nDacOut := countPaths("dac", "out", adj, make(map[string]int))

	paths2 := nSvrFft * nFftDac * nDacOut

	fmt.Println(paths1 + paths2)
}

func countPaths(current, target string, adj map[string][]string, memo map[string]int) int {
	if current == target {
		return 1
	}
	if val, ok := memo[current]; ok {
		return val
	}

	count := 0
	if neighbors, ok := adj[current]; ok {
		for _, neighbor := range neighbors {
			count += countPaths(neighbor, target, adj, memo)
		}
	}

	memo[current] = count
	return count
}
