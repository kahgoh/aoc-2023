import { Component } from "./component";
import { Pulse } from "./pulse";

export enum State {
    Off,
    On
}

export class FlipFlop extends Component {
    state : State = State.Off;

    doReceive(_from : string, pulse : Pulse) : Pulse | null {
        let wasState = this.state;
        if (pulse === Pulse.Low) {
            this.state = flip(this.state);
            return wasState === State.Off ? Pulse.High : Pulse.Low;
        }
        return null;
    }

    keyState(): string {
        return `${this.state}`
    }
}

function flip(state : State) : State {
    if (state === State.Off) {
        return State.On;
    }
    return State.Off;
}
