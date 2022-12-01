const fs = require('fs')

const getSolutionPart1 = (caloriesPerElf) => { // 70116
    return Math.max(...caloriesPerElf);
}

const getSolutionPart2 = (caloriesPerElf) => { // 206582
    return caloriesPerElf.sort((a, b) => b - a).splice(0,3).reduce((a, b) => a + b, 0);
}

let caloriesPerElf = fs.readFileSync("input.txt").toString().trim().split('\n\n').map(elf =>
    elf.split('\n').map(caloriesString => parseInt(caloriesString))
        .reduce((total, calories) => total + calories, 0));
console.log(process.env.part === "part1" ? getSolutionPart1(caloriesPerElf) : getSolutionPart2(caloriesPerElf));