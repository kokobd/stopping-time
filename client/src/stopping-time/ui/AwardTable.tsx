import * as React from "react";
import AwardModel from "stopping-time/model/Award";
import Award from "stopping-time/ui/Award";
import fixThis from "stopping-time/model/FixThis";

import Paper from "@material-ui/core/Paper";
import Button from "@material-ui/core/Button";
import Table from "@material-ui/core/Table";
import TableBody from "@material-ui/core/TableBody";
import TableHead from "@material-ui/core/TableHead";
import TableRow from "@material-ui/core/TableRow";
import TableCell from "@material-ui/core/TableCell";
import Toolbar from "@material-ui/core/Toolbar";
import Typography from "@material-ui/core/Typography";

export interface AwardTableProps {
  awards: AwardModel[];
  onUpdate?: (index: number, newEntry: AwardModel) => void;
  onAdd?: (newEntry: AwardModel) => void;
  onDelete?: (index: number) => void;
}

export default class AwardTable extends React.Component<AwardTableProps> {
  public constructor(props: AwardTableProps) {
    super(props);
    fixThis(this);
  }

  public render(): React.ReactNode {
    const paperStyle: React.CSSProperties =
      {
        display: "table",
        margin: "0px auto",
        maxWidth: "640px",
        width: "80%",
        paddingLeft: "1.5em",
        paddingRight: "1.5em",
        paddingBottom: "1.5em"
      }
    const headerCellStyle: React.CSSProperties =
      {
        padding: "2em",
        textAlign: "right"
      };
    return (
      <Paper style={paperStyle}>
        <Toolbar>
          <Typography variant="title">Lottery Settings</Typography>
        </Toolbar>
        <Table style={{ width: "100%", tableLayout: "fixed", marginBottom: "1em" }} cellSpacing={0} cellPadding={0}>
          <TableHead>
            <TableRow>
              <TableCell style={{ ...headerCellStyle, width: "2em" }}></TableCell>
              <TableCell style={{ ...headerCellStyle, width: "2em" }}>Stop?</TableCell>
              <TableCell style={headerCellStyle}>Income</TableCell>
              <TableCell style={headerCellStyle}>Cost</TableCell>
            </TableRow>
          </TableHead>
          <TableBody>
            {
              this.props.awards.map((value, index) => {
                return <Award
                  key={index}
                  model={value}
                  onUpdate={model => this.handleRowChange(index, model)}
                  onDelete={() => this.handleRowDelete(index)}
                />;
              })
            }
          </TableBody>
        </Table>
        <Button fullWidth variant="outlined" onClick={this.handleRowAdd}>Add Entry</Button>
      </Paper>
    );
  }

  private handleRowChange(index: number, model: AwardModel) {
    const { onUpdate } = this.props;
    if (onUpdate)
      onUpdate(index, model);
  }

  private handleRowAdd(event: any) {
    const { onAdd } = this.props;
    if (onAdd) {
      onAdd(
        {
          income: 100,
          cost: 100,
          stop: false
        }
      );
    }
  }

  private handleRowDelete(index: number) {
    const { onDelete } = this.props;
    if (onDelete)
      onDelete(index);
  }
}