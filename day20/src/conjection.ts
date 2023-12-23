import { Component } from "./component";
import { Pulse} from "./pulse";

export class Conjection extends Component {

    private _received : Record<string, Pulse> = {}

    private _order : string[] = [];

    addInput(input : string) {
        if (!(input in this._received)) {
            this._received[input] = Pulse.Low;
            this._order.push(input);
        }
    }

    doReceive(from : string, pulse : Pulse) {
        this._received[from] = pulse;
        let allHigh : boolean = true;
        for (let key in this._received) {
            if (this._received[key] != Pulse.High) {
                allHigh = false;
                break;
            }
        }

        return allHigh ? Pulse.Low : Pulse.High;
    }

    keyState(): string {
        return this._order.map(s => `${this._received[s]}`).join(':')
    }
}