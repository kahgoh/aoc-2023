import * as fs from "node:fs" 
import * as process from "node:process"
import { Broadcaster } from "./broadcaster";
import { FlipFlop } from "./flipflop";
import { Conjection } from "./conjection";
import { Module } from "./module";
import { Pulse } from "./pulse";
import { count } from "node:console";
import { Component } from "./component";

const starterName : string = "broadcaster";

if (process.argv.length < 2) {
    console.log("Missing data file argument");
    process.exit(1);
}
console.log(`Args: ${process.argv[2]}`);

let content : string = fs.readFileSync(process.argv[2], { encoding: 'utf8'});
console.log(`Content: ${content}`);

const lines = content.split(/\r?\n/);

let modules : Record<string, Module> = {};

lines.forEach(line => {
    let lineParts = line.split("->");
    let rawName = lineParts[0].trim();
    let name = extractName(rawName);

    switch (rawName.substring(0, 1)) {
        case "%":
            modules[name] = new Module(name, new FlipFlop());
            break;
        case "&":
            modules[name] = new Module(name, new Conjection());
            break;
        default:
            if (name === starterName) {
                modules[name] = new Module(name, new Broadcaster());
            } else {
                modules[name] = new Module(name, new Component());
            }
    }
});

lines.forEach(line => {
    let lineParts = line.split("->");
    let source = extractName(lineParts[0]);
    let sinks = lineParts[1].split(",");

    modules[source].connections = sinks.map(s => s.trim());
    modules[source].connections.forEach(s => {
        if (!(s in modules)) {
            modules[s] = new Module(s, new Component());
        } 
        modules[s].addInput(source);
    });
});

const order = lines.map(line => {
    let lineParts = line.split("->");
    return extractName(lineParts[0]);
})

let seen : Record<string, number> = {};
let start = -1;
let period = -1;
let scores : [number, number][] = [];
const maxLoops = 1000;
for (let i = 0; i < maxLoops; i++) {
    // console.log(`--- iteration ${i}`);
    dispatch("button", starterName, Pulse.Low);

    let key = keyState();
    let loopScore = tally();
    console.log(`${i}   ${key}  ${loopScore[0]}     ${loopScore[1]}`)
    if (key in seen) {
        start = seen[key];
        period = i - seen[key];
        break;
    }
    seen[key] = i;
    scores.push(loopScore);
}

if (start == -1 || period == -1) {
    // Means tno cycles
    let total = scores.reduce((acc, current) => {
        acc[0] = acc[0] + current[0];
        acc[1] = acc[1] + current[1];
        return acc;
    }, [0, 0]);
    console.log(`Total scores: ${total}`);
    console.log(`Result: ${total[0] * total[1]}`)
    process.exit(0);
}

console.log(`Cycle starts at ${start} with period ${period}`);

// Now perform some maths ...
let startSum : [number, number] = [0, 0];
for (let i = 0; i < start; i++) {
    let s = scores[i];
    startSum[0] = startSum[0] + s[0];
    startSum[1] = startSum[1] + s[1];
}

let periodSum : [number, number] = [0, 0];
let endSum : [number, number] = [0, 0];
let remaining = ((maxLoops - start) % period);
let endIndex = start + remaining;
for (let i = start; i < scores.length; i++) {
    periodSum[0] = periodSum[0] + scores[i][0];
    periodSum[1] = periodSum[1] + scores[i][1];

    if (remaining > 0 && i === endIndex) {
        endSum[0] = periodSum[0];
        endSum[1] = periodSum[1];
    }
}

let factor = Math.floor((maxLoops - start) / period);
periodSum[0] = periodSum[0] * factor;
periodSum[1] = periodSum[1] * factor;

let total = [startSum[0] + periodSum[0] + endSum[0], startSum[1] + periodSum[1] + endSum[1]];
console.log(`Totals: ${total}`);
console.log(`Answer: ${total[0] * total[1]}`)

function keyState() : string {
    return order.map(n => `${n}=${modules[n].keyState()}`).join("-");
}

function tally() : [number, number] {
    let total : [number, number] = [0, 0];
    for (let name in modules) {
        let count = modules[name].getCounts();
        total[0] = total[0] + count[0];
        total[1] = total[1] + count[1];
        modules[name].resetCounts();
    }
    return total;
}

function extractName(raw : string) : string {
    let name = raw.trim();

    switch (name.substring(0, 1)) {
        case "%": case "&":
            return name.substring(1);
        default:
            return name;
    }
}

function dispatch(from : string, to: string, pulse : Pulse) {
    let queue : [string, string, Pulse][] = [[from, to, pulse]];
    while (queue.length > 0) {
        let item = queue.shift() as [string, string, Pulse];
        let itemFrom = item[0];
        let itemTo = item[1];
        let itemPulse = item[2];
        let nextPulse = modules[itemTo].receive(itemFrom, itemPulse);

        if (nextPulse != null) {
            modules[itemTo].connections.forEach(to => {
                queue.push([itemTo, to, nextPulse as Pulse]);
            });
        }
    }
}
