import * as React from "react";

import AwardModel from "stopping-time/model/Award";
import Award from "stopping-time/ui/Award";
import AwardTable from "stopping-time/ui/AwardTable";
import Simulation from "stopping-time/ui/Simulation";
import OptimalStrategy from "stopping-time/ui/OptimalStrategy";
import Dialog from "@material-ui/core/Dialog";
import CircularProgress from "@material-ui/core/CircularProgress";

import fixThis from "stopping-time/model/FixThis";

interface State {
  awards: AwardModel[];
  devaluationRate: number;

  waiting: boolean;
}

export default class Main extends React.Component<{}, State> {
  public constructor(props: {}) {
    super(props);
    fixThis(this);

    this.state =
      {
        awards: [
          new AwardModel()
        ],
        devaluationRate: 1,
        waiting: false
      };
  }

  public render(): React.ReactNode {
    return (<>
      <div style={{
        display: "table",
        margin: "0px auto",
        maxWidth: "640px",
        width: "80%",
      }}>
        <AwardTable
          awards={this.state.awards}
          onUpdate={this.handleUpdate}
          devaluationRate={this.state.devaluationRate}
          onUpdateDevaluationRate={rate => this.setState({ devaluationRate: rate })}
        />
        <div style={{ marginTop: "1.5em" }}>
          <Simulation
            awards={this.state.awards}
            devaluationRate={this.state.devaluationRate}
            onError={this.handleSimulationError}
            onWaiting={() => this.setState({ waiting: true })}
            onFinish={() => this.setState({ waiting: false })}
          />
        </div>
      </div>
      <Dialog open={this.state.waiting} fullScreen PaperProps={{
        style: {
          backgroundColor: "rgba(255, 255, 255, 1%)",
          display: "flex",
          flexDirection: "column",
          justifyContent: "center"
        }
      }}>
        <div style={{
          display: "flex",
          flexDirection: "row",
          justifyContent: "center"
        }}>
          <CircularProgress size={120} />
        </div>
      </Dialog>
    </>);
  }

  private handleUpdate(newAwards: AwardModel[]) {
    this.setState({ awards: newAwards });
  }

  private handleSimulationError(message: string) {
    console.log("Error: " + message);
    this.setState({ waiting: false });
  }
}