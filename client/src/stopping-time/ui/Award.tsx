import * as React from "react";
import {
  Checkbox,
  TextField,
  TableRow,
  TableCell
} from "@material-ui/core"

import AwardModel from "stopping-time/model/Award";
import fixThis from "stopping-time/model/FixThis";

interface Props {
  model: AwardModel;
  onChange: (newModel: AwardModel) => void;
}

export default class Award extends React.Component<Props, {}> {
  public constructor(props: Props) {
    super(props);
    fixThis(this);
  }

  public render(): React.ReactNode {
    const cellStyle: React.CSSProperties =
      {
        textAlign: "right",
        paddingRight: "1em",
        paddingLeft: "0"
      };

    return (
      <TableRow
        hover
        selected={this.props.model.stop}
      >
        <TableCell style={{ ...cellStyle, width: "4em" }}>
          <Checkbox checked={this.props.model.stop} onChange={this.handleStopChange} />
        </TableCell>
        <TableCell style={cellStyle}>
          <TextField
            type="number"
            value={this.props.model.income}
            onChange={this.handleIncomeChange}
            style={{ direction: "rtl" }}
            fullWidth
          />
        </TableCell>
        <TableCell style={cellStyle}>
          <TextField
            type="number"
            value={this.props.model.cost}
            onChange={this.handleCostChange}
            style={{ direction: "rtl" }}
            fullWidth
          />
        </TableCell>
      </TableRow >
    );
  }

  private handleIncomeChange(event: any) {
    const newModel =
      {
        ...this.props.model,
        income: event.target.value
      };
    this.props.onChange(newModel);
  }

  private handleCostChange(event: any) {
    const newModel =
      {
        ...this.props.model,
        cost: event.target.value
      };
    this.props.onChange(newModel);
  }

  private handleStopChange(event: any, checked: boolean) {
    const newModel =
      {
        ...this.props.model,
        stop: checked
      };
    this.props.onChange(newModel);
  }

}
