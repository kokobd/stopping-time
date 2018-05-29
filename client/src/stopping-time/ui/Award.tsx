import * as React from "react";
import Checkbox from "@material-ui/core/Checkbox";
import TableRow from "@material-ui/core/TableRow";
import TableCell from "@material-ui/core/TableCell";
import IconButton from "@material-ui/core/IconButton";
import TextField from "@material-ui/core/TextField";
import DeleteIcon from "@material-ui/icons/Delete"

import AwardModel from "stopping-time/model/Award";
import fixThis from "stopping-time/model/FixThis";

interface Props {
  model: AwardModel;
  onUpdate: (newModel: AwardModel) => void;
  onDelete: () => void;
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
        <TableCell style={cellStyle}>
          <IconButton onClick={this.handleDelete}>
            <DeleteIcon />
          </IconButton>
        </TableCell>
        <TableCell style={cellStyle}>
          <Checkbox color="primary"
            checked={this.props.model.stop} onChange={this.handleStopChange} />
        </TableCell>
        <TableCell style={cellStyle}>
          <TextField
            type="number"
            value={this.props.model.income}
            onChange={this.handleIncomeChange}
            InputProps={{ inputProps: { style: { textAlign: "right" } } }}
            fullWidth
          />
        </TableCell>
        <TableCell style={cellStyle}>
          <TextField
            type="number"
            value={this.props.model.cost}
            onChange={this.handleCostChange}
            InputProps={{ inputProps: { style: { textAlign: "right" } } }}
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
        income: Number(event.target.value)
      };
    this.props.onUpdate(newModel);
  }

  private handleCostChange(event: any) {
    const newModel =
      {
        ...this.props.model,
        cost: Number(event.target.value)
      };
    this.props.onUpdate(newModel);
  }

  private handleStopChange(event: any, checked: boolean) {
    const newModel =
      {
        ...this.props.model,
        stop: checked
      };
    this.props.onUpdate(newModel);
  }

  private handleDelete(event: any) {
    this.props.onDelete();
  }

}
