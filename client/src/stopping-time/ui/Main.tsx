import * as React from "react";

import AwardModel from "stopping-time/model/Award";
import Award from "stopping-time/ui/Award";
import AwardTable from "stopping-time/ui/AwardTable";

import fixThis from "stopping-time/model/FixThis";

interface State {
  awards: AwardModel[];
}

export default class Main extends React.Component<{}, State> {
  public constructor(props: {}) {
    super(props);
    fixThis(this);

    this.state =
      {
        awards: []
      };
  }

  public render(): React.ReactNode {
    return (
      <AwardTable
        awards={this.state.awards}
        onUpdate={this.handleUpdate}
        onAdd={this.handleAdd}
        onDelete={this.handleDelete}
      />
    );
  }

  private handleUpdate(index: number, newEntry: AwardModel) {
    let newAwards: AwardModel[] = [];
    Object.assign(newAwards, this.state.awards);
    newAwards[index] = newEntry;
    this.setState({ awards: newAwards });
  }

  private handleAdd(newEntry: AwardModel) {
    let newAwards: AwardModel[] = [];
    Object.assign(newAwards, this.state.awards);
    newAwards.push(newEntry);
    this.setState({ awards: newAwards });
  }

  private handleDelete(index: number) {
    let newAwards: AwardModel[] = [];
    Object.assign(newAwards, this.state.awards);
    newAwards.splice(index, 1);
    this.setState({ awards: newAwards });
  }
}