import * as React from "react";
import {
  Table,
  TableBody
} from "@material-ui/core";
import AwardModel from "stopping-time/model/Award";
import Award from "stopping-time/ui/Award";

import fixThis from "stopping-time/model/FixThis";

interface State {
  awards: AwardModel[];
}

export default class Main extends React.Component<{}, State> {
  public constructor(props: {}) {
    super(props);
    fixThis(this);

    this.state = {
      awards: [{ income: 100, cost: 50, stop: false }]
    };
  }

  public render(): React.ReactNode {
    return (
      <div style={{ display: "table", margin: "0px auto", maxWidth: "640px", width: "80%" }}>
        <Table style={{ width: "100%", tableLayout: "fixed" }} cellSpacing={0} cellPadding={0} >
          <TableBody>
            <Award model={this.state.awards[0]} onChange={this.onChange} />
          </TableBody>
        </Table>
      </div>
    );
  }

  private onChange(model: AwardModel) {
    this.state.awards[0] = model;
    this.forceUpdate();
  }
}