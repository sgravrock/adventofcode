const fs = require('fs');
const {parseInput, solve} = require('./puzzle.js');

const input = fs.readFileSync(0, {encoding: 'utf8'});
console.log(solve(parseInput(input), 2000000));
