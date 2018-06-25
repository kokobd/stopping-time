import * as React from "react";

import AwardModel from "stopping-time/model/Award";
import AwardTable from "stopping-time/ui/AwardTable";
import Simulation from "stopping-time/ui/Simulation";
import Dialog from "@material-ui/core/Dialog";
import CircularProgress from "@material-ui/core/CircularProgress";
import ErrorDialog from "stopping-time/ui/ErrorDialog";

import fixThis from "stopping-time/model/FixThis";

interface State {
  awards: AwardModel[];
  devaluationRate: number;

  waiting: boolean;

  errorDialogOpen: boolean;
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
        waiting: false,
        errorDialogOpen: false
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
      <ErrorDialog
        open={this.state.errorDialogOpen}
        onClose={() => { this.setState({ errorDialogOpen: false }) }}
      />
    </>);
  }

  private handleUpdate(newAwards: AwardModel[]) {
    this.setState({ awards: newAwards });
  }

  private handleSimulationError(message: string) {
    this.setState({ errorDialogOpen: true, waiting: false });
  }
}