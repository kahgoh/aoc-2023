import { Component } from "./component";
import { Pulse } from "./pulse";

export class Broadcaster extends Component {

    received : Pulse = Pulse.Low

    doReceive(_from : string, pulse : Pulse) : Pulse {
        return pulse;
    }

    keyState(): string {
        return `${this.received}`
    }
}