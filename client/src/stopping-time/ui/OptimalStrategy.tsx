import * as React from "react";
import fixThis from "stopping-time/model/FixThis";
import Paper from "@material-ui/core/Paper"
import Button from "@material-ui/core/Button";
import TextField from "@material-ui/core/TextField";
import AwardModel from "stopping-time/model/Award";
const api = require("stopping-time/api");

export interface OptimalStrategyProps {
  incomeVector: number[],
  costVector: number[],
  onCalculationFinish: (stoppingSet: boolean[]) => void;
  onError?: (message: string) => void;
  onWaiting?: () => void;
}

interface State {
  iterations: number;
}

export default class OptimalStrategy extends React.Component<OptimalStrategyProps, State> {
  public static defaultProps: Partial<OptimalStrategyProps> =
    {
      onError: () => { },
      onWaiting: () => { }
    };

  public constructor(props: OptimalStrategyProps) {
    super(props);
    fixThis(this);

    this.state =
      {
        iterations: 10
      };
  }

  public render(): React.ReactNode {
    return (
      <Paper style={{ padding: "0.5em" }} >
        <div style={{
          display: "table",
          margin: "0px auto"
        }}>
          <Button
            color="primary"
            style={{ marginRight: "1.5em" }}
            variant="outlined"
            onClick={this.handleCalculate}
          >Calculate optimal strategy</Button>
          <TextField label="Number of iterations" type="number"
            value={this.state.iterations}
            onChange={this.handleIterationsChange}
          />
        </div>
      </Paper>
    );
  }

  private handleIterationsChange(event: any) {
    this.setState({ iterations: Number(event.target.value) });
  }

  private handleCalculate(event: any) {
    const reqBody =
      {
        income: this.props.incomeVector,
        cost: this.props.costVector,
        iterations: this.state.iterations
      };
    const onError = this.props.onError!;
    const onWaiting = this.props.onWaiting!;
    onWaiting();
    api.postApiOptimalstrategy(reqBody,
      (response: any) => {
        if (Array.isArray(response)
          && (response.length === 0 || typeof response[0] === "boolean")
          && response.length === this.props.incomeVector.length) {
          this.props.onCalculationFinish(response);
        } else {
          onError("Ill-formatted response");
        }
      },
      (jqXHR: any, textStatus: string, errorThrown: string) => {
        onError(errorThrown);
      });
  }
}