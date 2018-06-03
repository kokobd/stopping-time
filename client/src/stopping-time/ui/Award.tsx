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
  probEditable: boolean;
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
        style={{ minWidth: 0 }}
        hover
      >
        <TableCell style={cellStyle}>
          <IconButton onClick={this.handleDelete}>
            <DeleteIcon />
          </IconButton>
        </TableCell>
        <TableCell style={cellStyle}>
          <TextField
            type="number"
            value={this.props.model.value}
            onChange={this.handleValueChange}
            InputProps={{
              inputProps:
                {
                  style: { textAlign: "right", size: 0 }
                }
            }}
            fullWidth
          />
        </TableCell>
        <TableCell style={cellStyle}>
          <TextField
            type="number"
            value={this.props.model.probability.toFixed(4)}
            onChange={this.handleProbChange}
            InputProps={{
              inputProps:
                {
                  style: { textAlign: "right" },
                  size: 0,
                  step: 0.0001,
                  min: 0,
                  max: 1
                }
            }}
            disabled={!this.props.probEditable}
            fullWidth
          />
        </TableCell>
      </TableRow >
    );
  }

  private handleValueChange(event: React.ChangeEvent<HTMLInputElement>) {
    this.props.onUpdate(
      this.props.model.setValue(event.target.valueAsNumber));
  }

  private handleProbChange(event: React.ChangeEvent<HTMLInputElement>) {
    this.props.onUpdate(
      this.props.model.setProbability(event.target.valueAsNumber));
  }

  private handleDelete(event: any) {
    this.props.onDelete();
  }

}
