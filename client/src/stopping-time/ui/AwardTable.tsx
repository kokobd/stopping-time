import * as React from "react";
import AwardModel from "stopping-time/model/Award";
import Award from "stopping-time/ui/Award";
import fixThis from "stopping-time/model/FixThis";

import FormControlLabel from "@material-ui/core/FormControlLabel";
import Switch from "@material-ui/core/Switch";
import Paper from "@material-ui/core/Paper";
import Button from "@material-ui/core/Button";
import Table from "@material-ui/core/Table";
import TableBody from "@material-ui/core/TableBody";
import TableHead from "@material-ui/core/TableHead";
import TableRow from "@material-ui/core/TableRow";
import TableCell from "@material-ui/core/TableCell";
import Toolbar from "@material-ui/core/Toolbar";
import Typography from "@material-ui/core/Typography";
import TextField from "@material-ui/core/TextField";

export interface AwardTableProps {
  awards: AwardModel[];
  onUpdate: (newAwards: AwardModel[]) => void;
  devaluationRate: number;
  onUpdateDevaluationRate: (value: number) => void;
}

export interface AwardTableState {
  defaultProbs: boolean;
}

export default class AwardTable extends React.Component<AwardTableProps, AwardTableState> {
  public constructor(props: AwardTableProps) {
    super(props);
    fixThis(this);

    this.state = { defaultProbs: false };

    let newAwards: AwardModel[] = [];
    Object.assign(newAwards, this.props.awards);
    this.setDefaultProbability(newAwards);
    this.props.onUpdate(newAwards);
  }

  public render(): React.ReactNode {
    const paperStyle: React.CSSProperties =
      {
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
        <Toolbar style={{ display: "flex", paddingLeft: 0, paddingRight: 0 }}>
          <Typography variant="title" style={{ marginRight: "1em" }}>
            Optimal Stopping
          </Typography>
          <div style={{ flexGrow: 1 }}></div>
          <TextField
            label="Devaluation Rate"
            type="number"
            value={this.props.devaluationRate}
            onChange={this.handleUpdateDevRate}
            style={{ maxWidth: "8em", flexGrow: 0, marginRight: "1em" }}
            fullWidth
            InputProps={{
              inputProps: {
                min: 0,
                max: 1,
                step: 0.01
              }
            }}
          />
          <FormControlLabel
            control={
              <Switch
                checked={this.state.defaultProbs}
                onChange={this.handleDefaultProbsChange}
                color="primary"
              />
            }
            label="default probs"
            style={{ flexGrow: 0 }}
          />
        </Toolbar>
        <Table style={{ width: "100%", tableLayout: "fixed", marginBottom: "1em" }} cellSpacing={0} cellPadding={0}>
          <TableHead>
            <TableRow>
              <TableCell style={{ ...headerCellStyle, width: "2em" }}></TableCell>
              <TableCell style={headerCellStyle}>Income</TableCell>
              <TableCell style={headerCellStyle}>Probability</TableCell>
            </TableRow>
          </TableHead>
          <TableBody>
            {
              this.props.awards.map((value, index) => {
                return <Award
                  key={index}
                  probEditable={!this.state.defaultProbs}
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
    let newAwards: AwardModel[] = [];
    Object.assign(newAwards, this.props.awards);
    newAwards[index] = model;

    this.props.onUpdate(newAwards);
  }

  private handleRowAdd(event: any) {
    let newAwards: AwardModel[] = [];
    Object.assign(newAwards, this.props.awards);

    newAwards.push(new AwardModel());
    if (this.state.defaultProbs)
      this.setDefaultProbability(newAwards);
    this.props.onUpdate(newAwards);
  }

  private handleRowDelete(index: number) {
    let newAwards: AwardModel[] = [];
    Object.assign(newAwards, this.props.awards);

    newAwards.splice(index, 1);

    if (this.state.defaultProbs)
      this.setDefaultProbability(newAwards);

    this.props.onUpdate(newAwards);
  }

  private handleDefaultProbsChange(event: any, checked: boolean) {
    this.setState({ defaultProbs: checked });
    if (checked) {
      let newAwards: AwardModel[] = [];
      Object.assign(newAwards, this.props.awards);
      this.setDefaultProbability(newAwards);
      this.props.onUpdate(newAwards);
    }
  }

  private setDefaultProbability(awards: AwardModel[]): number {
    const n = awards.length;
    const p = 1 / (n + 1);
    for (let i = 0; i < n; ++i) {
      const newModel = awards[i].setProbability(p);
      awards[i] = newModel;
    }
    return p;
  }

  private handleUpdateDevRate(event: React.ChangeEvent<HTMLInputElement>) {
    this.props.onUpdateDevaluationRate(event.target.valueAsNumber);
  }
}