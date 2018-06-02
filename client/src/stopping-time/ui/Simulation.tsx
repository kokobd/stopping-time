import * as React from "react";
import Paper from "@material-ui/core/Paper"
import Button from "@material-ui/core/Button";
import TextField from "@material-ui/core/TextField";

import fixThis from "stopping-time/model/FixThis";
const api = require("stopping-time/api");

export interface SimulationProps {
  income: number[];
  cost: number[];
  stop: boolean[];

  onFinish: (result: number) => void;
  onError?: (message: string) => void;
  onWaiting?: () => void;
}

export interface SimulationState {
  count: number
}

export default class Simulation extends React.Component<SimulationProps, SimulationState> {
  public render(): React.ReactNode {
    return (
      <Paper style={Simulation.paperStyle}>
        <div style={Simulation.leftDivStyle}>
          <Button
            color="primary"
            variant="outlined"
            onClick={this.handleRunSimulation}
          >Run Simulation</Button>
        </div>
        <div style={Simulation.rightDivStyle}>
          <TextField label="Number of simulations" type="number"
            onChange={this.handleCountChange}
            value={this.state.count}
          />
        </div>
      </Paper>
    );
  }

  public constructor(props: SimulationProps) {
    super(props);
    fixThis(this);

    this.state =
      {
        count: 100000
      };
  }

  public static defaultProps: Partial<SimulationProps> =
    {
      onError: () => { },
      onWaiting: () => { }
    }

  private handleRunSimulation(event: React.MouseEvent<HTMLElement>) {
    const reqBody =
      {
        income: this.props.income,
        cost: this.props.cost,
        stop: this.props.stop,
        count: this.state.count
      };
    const onSuccess = (response: any) => {
      if (typeof response === "number") {
        this.props.onFinish(response);
      } else {
        this.props.onError!("Ill-formatted response");
      }
    }
    const onError = (jqXHR: any, textStatus: string, errorThrown: string) => {
      this.props.onError!(errorThrown);
    };
    this.props.onWaiting!();
    api.postApiSimulation(reqBody, onSuccess, onError);
  }

  private handleCountChange(event: React.ChangeEvent<HTMLInputElement>) {
    this.setState({ count: Number(event.target.value) });
  }

  private static paperStyle: React.CSSProperties =
    {
      padding: "0.5em",
      display: "flex",
      flexFlow: "row wrap",
      alignContent: "stretch"
    };

  private static leftDivStyle: React.CSSProperties =
    {
      flexGrow: 1,
      flexBasis: 0,
      display: "flex",
      justifyContent: "flex-end",
      marginRight: "0.5em",
    };

  private static rightDivStyle: React.CSSProperties =
    {
      flexGrow: 1,
      flexBasis: 0,
      display: "flex",
      justifyContent: "flex-start",
      marginLeft: "0.5em",
    };
}
