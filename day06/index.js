const fs = require('fs')

const find_first_distinct_sequence = (datastream, amount) => {
    let from = [...Array(datastream.length).keys()].find(from => {
        let window = Array.from(datastream.substring(from, from + amount));
        return window.every((value, index, self) => self.indexOf(value) === index);
    });
    return from + amount;
}

const solvePart1 = (datastream) => {
    return find_first_distinct_sequence(datastream, 4);
}

const solvePart2 = (datastream) => {
    return find_first_distinct_sequence(datastream, 14);
}

let datastream = fs.readFileSync("input.txt").toString().trim();
console.log(process.env.part === "part1" ? solvePart1(datastream) : solvePart2(datastream));