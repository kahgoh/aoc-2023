import { Component } from "./component";
import { Pulse } from "./pulse";

export class Module {
    private _name : string;

    private _inputs : string[] = [];

    private _component : Component;
    
    private _lastPulse : Pulse | undefined = undefined;

    lastOut : Pulse | null = null;

    connections : string[] = [];

    constructor(name : string, component : Component) {
        this._name = name;
        this._component = component;
    }

    getInputs() : string[] {
        return this._inputs;
    }

    addInput(from : string) : void {
        this._inputs.push(from);
        this._component.addInput(from);
    }

    receive(from : string, pulse : Pulse) : Pulse | null {
        this._lastPulse = pulse;
        let next = this._component.receive(from, pulse);
        this.lastOut = next;
        return next;
    }

    lastReceived() : Pulse | undefined {
        return this._lastPulse;
    }

    getCounts() : [number, number] {
        return this._component.getCounts();
    }

    resetCounts() {
        this._component.resetCounts();
    }

    resetLastOut() {
        this.lastOut = null;
    }

    keyState() : string {
        return this._component.keyState();
    }
}