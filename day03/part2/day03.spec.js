import { compileWat, WasmRunner } from "@exercism/wasm-lib";
import * as fs from "fs";

let wasmModule;
let currentInstance;

beforeAll(async () => {
  try {
    const watPath = new URL("./day03.wat", import.meta.url);
    const { buffer } = await compileWat(watPath);
    wasmModule = await WebAssembly.compile(buffer);
  } catch (err) {
    console.log(`Error compiling *.wat: ${err}`);
  }
});

function loadData() {
  const inputBufferOffset = 64;
  const inputBufferCapacity = 1024 * 20;
  let content = fs.readFileSync("./input.data");
  let text = content.toString("utf-8");

  const inputLengthEncoded = new TextEncoder().encode(text).length;
  if (inputLengthEncoded > inputBufferCapacity) {
    throw new Error(
      `String (${inputLengthEncoded}) is too large for buffer of size ${inputBufferCapacity} bytes`
    );
  }

  currentInstance.set_mem_as_utf8(inputBufferOffset, inputLengthEncoded, text);
  
  return currentInstance.exports.solve(inputBufferOffset);
}

describe("Day 03", () => {

  beforeEach(async () => {
    currentInstance = null;

    if (!wasmModule) {
      return Promise.reject();
    }
    try {
      currentInstance = await new WasmRunner(wasmModule);
      return Promise.resolve();
    } catch (err) {
      console.log(`Error instantiating WebAssembly module: ${err}`);
      return Promise.reject();
    }
  });

  test("Load data", () => {
    expect(loadData()).toBe(100);
  });
});
