import { Pulse } from "./pulse";

export class Component {
    _highPulses : number = 0;

    _lowPulses : number = 0;
       
    receive(from : string, pulse : Pulse) : Pulse | null {
        if (pulse === Pulse.High) {
            this._highPulses++;
        } else if (pulse === Pulse.Low) {
            this._lowPulses++;
        }
        return this.doReceive(from, pulse);
    }

    doReceive(_from : string, _pulse : Pulse) : Pulse | null {
        return null;
    }

    keyState() : string {
        return `${this._lowPulses}:${this._highPulses}`;
    }

    addInput(_from : string) : void {
        // By default, do nothing
    }

    getCounts() : [number, number] {
        return [this._lowPulses, this._highPulses];
    }

    resetCounts() {
        this._highPulses = 0;
        this._lowPulses = 0;
    }
}