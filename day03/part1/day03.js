import * as fs from "fs";

const wasmBuffer = fs.readFileSync("./day03.wat");
const wasmModule = new WebAssembly.Module(wasmBuffer);

const wasmInstance = new WebAssembly.Instance(wasmModule, {
    env: {
        memory: new WebAssembly.Memory({initial: 1})
    }
});

const wasmArray = new Uint8Array(wasmInstance.exports.memory.buffer, 0, 2048);
for (let i=0; i < wasmArray.length; i++) {
    wasmArray[i] = i;
}

const result = wasmInstance.exports.solve(wasmArray.byteOffset);
console.log(`result: ${result}`);