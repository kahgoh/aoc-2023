import * as fs from "node:fs" 
import * as process from "node:process"
import { Broadcaster } from "./broadcaster";
import { FlipFlop } from "./flipflop";
import { Conjection } from "./conjection";
import { Module } from "./module";
import { Pulse } from "./pulse";
import { Component } from "./component";
import { it } from "node:test";

const starterName : string = "broadcaster";

if (process.argv.length < 2) {
    console.log("Missing data file argument");
    process.exit(1);
}
//console.log(`Args: ${process.argv[2]}`);

let content : string = fs.readFileSync(process.argv[2], { encoding: 'utf8'});
//console.log(`Content: ${content}`);

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

// These are populated during dispatch
let periods : Record<string, number | undefined> = {};
let seen : Record<string, number | undefined> = {};


let count = 1;
while (dispatch(count, "button", starterName, Pulse.Low) && count < 100000) {
    count++;
}

console.log("Periods:");
Object.keys(periods).forEach(name => {
    console.log(`${name}: ${periods[name]}`);
});

const result = Object.values(periods).reduce((num1, num2) => {
    if (num1 && num2) {
        return lcm(num1, num2)
    } 
    console.error("Unknown period");
    return 0;
});

console.log(`Result: ${result}`);

function lcm(num1 : number, num2 : number) : number {
    let less = Math.min(num1, num2);
    let more = Math.max(num1, num2);
    let now = more;

    while (now % less != 0) {
        now = now + more;
    }
    return now;
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

function dispatch(count : number, from : string, to: string, pulse : Pulse) : boolean {
    let queue : [string, string, Pulse][] = [[from, to, pulse]];
    while (queue.length > 0) {
        let item = queue.shift() as [string, string, Pulse];
        let itemFrom = item[0];
        let itemTo = item[1];
        let itemPulse = item[2];
        let nextPulse = modules[itemTo].receive(itemFrom, itemPulse);

        if (itemPulse === Pulse.High && itemTo === "sq") {
            let lastSeen = seen[itemFrom];
            seen[itemFrom] = count;
            if (lastSeen) {
                periods[itemFrom] = count - (lastSeen as number);
            }
        }

        if (nextPulse != null) {
            modules[itemTo].connections.forEach(to => {
                queue.push([itemTo, to, nextPulse as Pulse]);
            });
        }
    }

    return true;
}
