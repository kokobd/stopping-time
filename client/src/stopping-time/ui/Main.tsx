import * as React from "react";

import AwardModel from "stopping-time/model/Award";
import Award from "stopping-time/ui/Award";
import AwardTable from "stopping-time/ui/AwardTable";
import OptimalStrategy from "stopping-time/ui/OptimalStrategy";
import Dialog from "@material-ui/core/Dialog";
import CircularProgress from "@material-ui/core/CircularProgress";

import fixThis from "stopping-time/model/FixThis";

interface State {
  awards: AwardModel[];
  waiting: boolean;
}

export default class Main extends React.Component<{}, State> {
  public constructor(props: {}) {
    super(props);
    fixThis(this);

    this.state =
      {
        awards: [
          { income: 1, cost: 1, stop: false },
          { income: 2, cost: 1, stop: false },
          { income: 3, cost: 1, stop: false },
          { income: 4, cost: 1, stop: false },
          { income: 5, cost: 1, stop: false },
          { income: 0, cost: 1, stop: false },
        ],
        waiting: false
      };
  }

  public render(): React.ReactNode {
    const incomeVector: number[] = this.state.awards.map(x => x.income);
    const costVector: number[] = this.state.awards.map(x => x.cost);
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
          onAdd={this.handleAdd}
          onDelete={this.handleDelete}
        />
        <div style={{ marginTop: "0.5em" }}>
          <OptimalStrategy
            incomeVector={incomeVector}
            costVector={costVector}
            onCalculationFinish={this.handleCalculationFinish}
            onError={this.handleCalculationError}
            onWaiting={() => this.setState({ waiting: true })}
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

  private copyArray<T>(xs: T[]): T[] {
    let ys: T[] = [];
    Object.assign(ys, xs);
    return ys;
  }

  private handleUpdate(index: number, newEntry: AwardModel) {
    let newAwards = this.copyArray(this.state.awards);
    newAwards[index] = newEntry;
    this.setState({ awards: newAwards });
  }

  private handleAdd(newEntry: AwardModel) {
    let newAwards = this.copyArray(this.state.awards);
    newAwards.push(newEntry);
    this.setState({ awards: newAwards });
  }

  private handleDelete(index: number) {
    let newAwards = this.copyArray(this.state.awards);
    newAwards.splice(index, 1);
    this.setState({ awards: newAwards });
  }

  private handleCalculationFinish(stoppingSet: boolean[]) {
    let newAwards = this.copyArray(this.state.awards);
    for (let i = 0; i < stoppingSet.length; ++i) {
      let newAward =
        {
          ...newAwards[i],
          stop: stoppingSet[i]
        };
      newAwards[i] = newAward;
    }
    this.setState({ awards: newAwards, waiting: false });
  }

  private handleCalculationError(message: string) {
    console.log("Error: " + message);
    this.setState({ waiting: false });
  }
}