const fs = require('fs')

const solvePart1 = (caloriesPerElf) => {
    return Math.max(...caloriesPerElf);
}

const solvePart2 = (caloriesPerElf) => {
    return caloriesPerElf.sort((a, b) => b - a).splice(0,3).reduce((a, b) => a + b, 0);
}

let caloriesPerElf = fs.readFileSync("input.txt").toString().trim().split('\n\n').map(elf =>
    elf.split('\n').map(caloriesString => parseInt(caloriesString))
        .reduce((total, calories) => total + calories, 0));
console.log(process.env.part === "part1" ? solvePart1(caloriesPerElf) : solvePart2(caloriesPerElf));