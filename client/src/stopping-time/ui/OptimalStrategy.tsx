import * as React from "react";
import fixThis from "stopping-time/model/FixThis";
import Paper from "@material-ui/core/Paper"
import Button from "@material-ui/core/Button";
import TextField from "@material-ui/core/TextField";
import AwardModel from "stopping-time/model/Award";
const api = require("stopping-time/api");

export interface OptimalStrategyProps {
  awards: AwardModel[];
  devaluationRate: number;

  onCalculationFinish: (stopValue: number) => void;
  onError?: (message: string) => void;
  onWaiting?: () => void;
}

export default class OptimalStrategy extends React.Component<OptimalStrategyProps> {
  public static defaultProps: Partial<OptimalStrategyProps> =
    {
      onError: () => { },
      onWaiting: () => { }
    };

  public constructor(props: OptimalStrategyProps) {
    super(props);
    fixThis(this);
  }

  public render(): React.ReactNode {
    return (
      <Button variant="outlined" color="primary" fullWidth>
        Calculate Optimal Strategy
      </Button>
    );
  }

  private handleIterationsChange(event: React.ChangeEvent<HTMLInputElement>) {
    this.setState({ iterations: Number(event.target.value) });
  }

  private handleCalculate(event: any) {
    const reqBody =
      {
        awards: this.props.awards.map(x => [x.value, x.probability]),
        devaluationRate: this.props.devaluationRate
      };
    const onError = this.props.onError!;
    const onWaiting = this.props.onWaiting!;
    onWaiting();
    api.postApiOptimalstrategy(reqBody,
      (response: any) => {
        if (typeof response === "number") {
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