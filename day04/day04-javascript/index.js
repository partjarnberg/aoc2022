const fs = require('fs')

const range = (start, end) => {
    return [...Array(end - start + 1).keys()].map(index => index + start);
}

const solvePart1 = (pairs) => {
    return pairs.filter(pair => pair[0].every(id =>
        pair[1].includes(id)) || pair[1].every(id => pair[0].includes(id))).length;
}

const solvePart2 = (pairs) => {
    return pairs.filter(pair => pair[0].some(id => pair[1].includes(id))).length;
}

let pairs = fs.readFileSync("input.txt").toString().trim().split('\n').map(pair =>
    pair.split(',').map(rangeString => {
        let arr = rangeString.split("-");
        return range(parseInt(arr[0]), parseInt(arr[1]));
    }));
console.log(process.env.part === "part1" ? solvePart1(pairs) : solvePart2(pairs));