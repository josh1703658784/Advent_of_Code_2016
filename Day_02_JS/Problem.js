const fs        = Object.freeze(require('fs'));
const rawInput  = Object.freeze(fs.readFileSync('input', 'utf8'));
const keypad    = Object.freeze([[1,2,3],[4,5,6],[7,8,9]]);
const action    = Object.freeze({"L": [0,-1], "R": [0,+1], "U": [-1,0], "D": [+1,0]});

const formatInput = dirtyInput   => dirtyInput.split("\n").map(line => Array.from(line));
const addPoints   = (a,b)        => [a[0]+b[0], a[1]+b[1]];
const updateLoc   = (move, loc)  => addPoints(move,loc).reduce((acc, num) => acc && num >= 0 && num <= 2, true) ? addPoints(move,loc) : loc;
const traverse    = (insts, loc) => insts.map(line => loc = line.reduce((prevLoc, i) => updateloc(action[i], prevLoc), loc)).map(l => keypad[l[0]][l[1]]);
const _main       = ()           => traverse(formatInput(rawInput), [1,1]); //solution: 98575, start at 5 on keypad

console.log(_main());
