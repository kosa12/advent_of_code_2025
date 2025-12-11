package main

import (
	"bufio"
	"fmt"
	"math"
	"os"
	"regexp"
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
	totalPresses := 0

	for scanner.Scan() {
		line := scanner.Text()
		if strings.TrimSpace(line) == "" {
			continue
		}
		presses := solveMachine(line)
		if presses != -1 {
			totalPresses += presses
		}
	}

	fmt.Println(totalPresses)
}

func solveMachine(line string) int {
	braceStart := strings.Index(line, "{")
	braceEnd := strings.Index(line, "}")
	targetStr := line[braceStart+1 : braceEnd]
	targetParts := strings.Split(targetStr, ",")
	target := make([]float64, len(targetParts))
	for i, p := range targetParts {
		val, _ := strconv.Atoi(p)
		target[i] = float64(val)
	}

	numCounters := len(target)
	re := regexp.MustCompile(`\(([\d,]+)\)`)
	matches := re.FindAllStringSubmatch(line, -1)
	
	numButtons := len(matches)
	matrix := make([][]float64, numCounters)
	for i := range matrix {
		matrix[i] = make([]float64, numButtons)
	}

	for btnIdx, match := range matches {
		parts := strings.Split(match[1], ",")
		for _, p := range parts {
			counterIdx, _ := strconv.Atoi(p)
			if counterIdx < numCounters {
				matrix[counterIdx][btnIdx] = 1.0
			}
		}
	}

	return solveSystem(matrix, target)
}

func solveSystem(matrix [][]float64, target []float64) int {
	rows := len(matrix)
	cols := len(matrix[0])

	aug := make([][]float64, rows)
	for i := range aug {
		aug[i] = make([]float64, cols+1)
		copy(aug[i], matrix[i])
		aug[i][cols] = target[i]
	}

	pivotRow := 0
	pivots := make([]int, cols)
	for i := range pivots {
		pivots[i] = -1
	}

	for col := 0; col < cols && pivotRow < rows; col++ {
		sel := -1
		for r := pivotRow; r < rows; r++ {
			if math.Abs(aug[r][col]) > 1e-9 {
				sel = r
				break
			}
		}

		if sel == -1 {
			continue
		}

		aug[pivotRow], aug[sel] = aug[sel], aug[pivotRow]
		pivots[col] = pivotRow

		div := aug[pivotRow][col]
		for c := col; c <= cols; c++ {
			aug[pivotRow][c] /= div
		}

		for r := 0; r < rows; r++ {
			if r != pivotRow {
				factor := aug[r][col]
				if math.Abs(factor) > 1e-9 {
					for c := col; c <= cols; c++ {
						aug[r][c] -= factor * aug[pivotRow][c]
					}
				}
			}
		}

		pivotRow++
	}

	for r := pivotRow; r < rows; r++ {
		if math.Abs(aug[r][cols]) > 1e-9 {
			return -1
		}
	}

	var freeVars []int
	var pivotVars []int
	for c := 0; c < cols; c++ {
		if pivots[c] == -1 {
			freeVars = append(freeVars, c)
		} else {
			pivotVars = append(pivotVars, c)
		}
	}

	minTotal := -1

	var bounds []int
	for _, fCol := range freeVars {
		minB := math.MaxInt32
		colHasOne := false
		for r := 0; r < rows; r++ {
			if matrix[r][fCol] > 0.5 {
				if int(target[r]) < minB {
					minB = int(target[r])
				}
				colHasOne = true
			}
		}
		if !colHasOne {
			minB = 0
		}
		bounds = append(bounds, minB)
	}

	var search func(idx int, currentFreeValues []int, currentFreeSum int)
	search = func(idx int, currentFreeValues []int, currentFreeSum int) {
		if minTotal != -1 && currentFreeSum >= minTotal {
			return
		}

		if idx == len(freeVars) {
			currentTotal := currentFreeSum

			valid := true
			for _, pCol := range pivotVars {
				r := pivots[pCol]
				val := aug[r][cols]
				for i, fCol := range freeVars {
					val -= aug[r][fCol] * float64(currentFreeValues[i])
				}

				if val < -1e-9 {
					valid = false
					break
				}
				
				intVal := int(math.Round(val))
				if math.Abs(val - float64(intVal)) > 1e-9 {
					valid = false
					break
				}
				currentTotal += intVal
			}

			if valid {
				if minTotal == -1 || currentTotal < minTotal {
					minTotal = currentTotal
				}
			}
			return
		}

		for v := 0; v <= bounds[idx]; v++ {
			currentFreeValues[idx] = v
			search(idx+1, currentFreeValues, currentFreeSum + v)
		}
	}

	freeValues := make([]int, len(freeVars))
	search(0, freeValues, 0)

	return minTotal
}
