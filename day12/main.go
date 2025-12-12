package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		fmt.Println("Error opening file:", err)
		return
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	shapes := make(map[int][][2]int)
	var currentShapeIdx int = -1
	var currentShapeLines []string
	parsingShapes := true

	var problems []struct {
		width, height int
		counts        []int
	}

	for scanner.Scan() {
		line := scanner.Text()

		if parsingShapes {
			if strings.Contains(line, "x") {

				parsingShapes = false

				if currentShapeIdx >= 0 && len(currentShapeLines) > 0 {
					shapes[currentShapeIdx] = parseShapeLines(currentShapeLines)
				}
			} else if strings.HasSuffix(line, ":") {

				if currentShapeIdx >= 0 && len(currentShapeLines) > 0 {
					shapes[currentShapeIdx] = parseShapeLines(currentShapeLines)
				}

				idx, _ := strconv.Atoi(strings.TrimSuffix(line, ":"))
				currentShapeIdx = idx
				currentShapeLines = nil
			} else if line != "" {
				currentShapeLines = append(currentShapeLines, line)
			}
		}

		if !parsingShapes {
			if strings.Contains(line, "x") {
				parts := strings.Split(line, ":")
				dimParts := strings.Split(parts[0], "x")
				width, _ := strconv.Atoi(dimParts[0])
				height, _ := strconv.Atoi(dimParts[1])

				countStrs := strings.Fields(parts[1])
				counts := make([]int, len(countStrs))
				for i, s := range countStrs {
					counts[i], _ = strconv.Atoi(s)
				}

				problems = append(problems, struct {
					width, height int
					counts        []int
				}{width, height, counts})
			}
		}
	}

	shapeSizes := make(map[int]int)
	
	shapeParityDiff := make(map[int][]int)

	for idx, cells := range shapes {
		shapeSizes[idx] = len(cells)
		variants := generateVariants(cells)
		for _, v := range variants {
			black := 0
			white := 0
			for _, c := range v {
				if (c[0]+c[1])%2 == 0 {
					black++
				} else {
					white++
				}
			}
			shapeParityDiff[idx] = append(shapeParityDiff[idx], black-white)
		}
	}

	result := 0
	for _, p := range problems {
		totalCells := 0
		for shapeIdx, count := range p.counts {
			totalCells += count * shapeSizes[shapeIdx]
		}
		gridArea := p.width * p.height
		if totalCells <= gridArea {
			result++
		}
	}

	fmt.Println(result)
}

func canFitFast(width, height int, counts []int, shapeSizes map[int]int, shapeParityDiff map[int][]int) bool {
	gridArea := width * height

	totalCells := 0
	for shapeIdx, count := range counts {
		totalCells += count * shapeSizes[shapeIdx]
	}

	if totalCells != gridArea {
		return false
	}

	gridBlack := 0
	for y := 0; y < height; y++ {
		for x := 0; x < width; x++ {
			if (x+y)%2 == 0 {
				gridBlack++
			}
		}
	}
	gridWhite := gridArea - gridBlack
	targetDiff := gridBlack - gridWhite
	return canAchieveParity(counts, shapeParityDiff, targetDiff)
}

func canAchieveParity(counts []int, shapeParityDiff map[int][]int, target int) bool {

	
	achievable := map[int]bool{0: true}
	
	for shapeIdx, count := range counts {
		diffs := shapeParityDiff[shapeIdx]
		uniqueDiffs := make(map[int]bool)
		for _, d := range diffs {
			uniqueDiffs[d] = true
		}
		diffList := make([]int, 0, len(uniqueDiffs))
		for d := range uniqueDiffs {
			diffList = append(diffList, d)
		}

		for i := 0; i < count; i++ {
			newAchievable := make(map[int]bool)
			for curr := range achievable {
				for _, d := range diffList {
					newAchievable[curr+d] = true
				}
			}
			achievable = newAchievable
		}
	}
	
	return achievable[target]
}

func parseShapeLines(lines []string) [][2]int {
	var cells [][2]int
	for y, line := range lines {
		for x, c := range line {
			if c == '#' {
				cells = append(cells, [2]int{x, y})
			}
		}
	}
	return cells
}

func generateVariants(cells [][2]int) [][][2]int {
	seen := make(map[string]bool)
	var variants [][][2]int

	current := cells
	for rot := 0; rot < 4; rot++ {
		norm := normalize(current)
		key := cellsToKey(norm)
		if !seen[key] {
			seen[key] = true
			variants = append(variants, norm)
		}

		flipped := flipH(current)
		norm = normalize(flipped)
		key = cellsToKey(norm)
		if !seen[key] {
			seen[key] = true
			variants = append(variants, norm)
		}

		current = rotate90(current)
	}

	return variants
}

func rotate90(cells [][2]int) [][2]int {
	result := make([][2]int, len(cells))
	for i, c := range cells {
		result[i] = [2]int{-c[1], c[0]}
	}
	return result
}

func flipH(cells [][2]int) [][2]int {
	result := make([][2]int, len(cells))
	for i, c := range cells {
		result[i] = [2]int{-c[0], c[1]}
	}
	return result
}

func normalize(cells [][2]int) [][2]int {
	if len(cells) == 0 {
		return cells
	}
	minX, minY := cells[0][0], cells[0][1]
	for _, c := range cells {
		if c[0] < minX {
			minX = c[0]
		}
		if c[1] < minY {
			minY = c[1]
		}
	}
	result := make([][2]int, len(cells))
	for i, c := range cells {
		result[i] = [2]int{c[0] - minX, c[1] - minY}
	}
	return result
}

func cellsToKey(cells [][2]int) string {
	sorted := make([][2]int, len(cells))
	copy(sorted, cells)
	for i := 0; i < len(sorted); i++ {
		for j := i + 1; j < len(sorted); j++ {
			if sorted[j][1] < sorted[i][1] || (sorted[j][1] == sorted[i][1] && sorted[j][0] < sorted[i][0]) {
				sorted[i], sorted[j] = sorted[j], sorted[i]
			}
		}
	}
	var sb strings.Builder
	for _, c := range sorted {
		sb.WriteString(fmt.Sprintf("%d,%d;", c[0], c[1]))
	}
	return sb.String()
}

