import * as React from "react";
import Paper from "@material-ui/core/Paper"
import Button from "@material-ui/core/Button";
import TextField from "@material-ui/core/TextField";

import fixThis from "stopping-time/model/FixThis";
import AwardModel from "stopping-time/model/Award";
import OptimalStrategy from "stopping-time/ui/OptimalStrategy";
import Typography from "@material-ui/core/Typography";
const api = require("stopping-time/api");

export interface SimulationProps {
  awards: AwardModel[];
  devaluationRate: number;

  onError?: (message: string) => void;
  onWaiting?: () => void;
  onFinish?: () => void;
}

export interface SimulationState {
  count: number;
  stopValue: number;
  averageProfit?: number;
}

export default class Simulation extends React.Component<SimulationProps, SimulationState> {
  public render(): React.ReactNode {
    return (
      <Paper style={{ padding: "0.5em" }}>
        <div style={Simulation.flexContainerStyle}>
          <div style={Simulation.innerStyle}>
            <OptimalStrategy
              awards={this.props.awards}
              devaluationRate={this.props.devaluationRate}
              onCalculationFinish={this.handleCalculationFinish}
              onError={this.props.onError}
              onWaiting={this.props.onWaiting}
            />
          </div>
          <div style={Simulation.innerStyle}>
            <Button
              color="primary"
              variant="outlined"
              fullWidth
              onClick={this.handleRunSimulation}
            >Run Simulation</Button>
          </div>
          <div style={Simulation.innerStyle}>
            <TextField label="Count of simulations" type="number"
              onChange={this.handleCountChange}
              value={this.state.count}
              InputProps={{
                inputProps:
                {
                  step: 1,
                  min: 1,
                  max: 1000000
                }
              }}
              fullWidth
            />
          </div>
          <div style={Simulation.innerStyle}>
            <TextField
              label="Stop Value" type="number"
              value={this.state.stopValue}
              onChange={this.handleStopValueChange}
              fullWidth
            />
          </div>
        </div>
        <Typography style={{ margin: "0.5em" }} variant="body2">
          Average Profit: {this.state.averageProfit ? this.state.averageProfit : "none calculated"}
        </Typography>
      </Paper>
    );
  }

  public constructor(props: SimulationProps) {
    super(props);
    fixThis(this);

    this.state =
      {
        count: 100000,
        stopValue: 1
      };
  }

  public static defaultProps: Partial<SimulationProps> =
    {
      onError: () => { },
      onWaiting: () => { },
      onFinish: () => { }
    }

  private handleRunSimulation(event: React.MouseEvent<HTMLElement>) {
    const reqBody =
    {
      awards: this.props.awards.map(x => [x.value, x.probability]),
      devaluationRate: this.props.devaluationRate,
      stopValue: this.state.stopValue,
      count: this.state.count
    };
    const onSuccess = (response: any) => {
      if (typeof response === "number") {
        this.onSimulationFinish(response);
        this.props.onFinish!();
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

  private onSimulationFinish(averageProfit: number) {
    this.setState({ averageProfit });
  }

  private handleCalculationFinish(stopValue: number) {
    this.setState({ stopValue });
    this.props.onFinish!();
  }

  private handleStopValueChange(event: React.ChangeEvent<HTMLInputElement>) {
    this.setState({ stopValue: event.target.valueAsNumber });
  }

  private static flexContainerStyle: React.CSSProperties =
    {
      display: "flex",
      flexDirection: "row",
      flexWrap: "wrap",
      alignContent: "stretch",
      alignItems: "flex-end",
      justifyContent: "space-around",
    };

  private static innerStyle: React.CSSProperties =
    {
      margin: "0.5em",
      flexGrow: 1,
      flexShrink: 1,
      flexBasis: "auto"
    };
}
