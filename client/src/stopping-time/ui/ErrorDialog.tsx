import * as React from "react";
import Dialog from "@material-ui/core/Dialog";
import DialogContent from "@material-ui/core/DialogContent";
import DialogContentText from "@material-ui/core/DialogContentText";

interface Props {
  open: boolean;
  onClose: () => void;
}

export default class ErrorDialog extends React.Component<Props> {
  public constructor(props: Props) {
    super(props);
  }

  public render(): React.ReactNode {
    return (
      <Dialog open={this.props.open} onClose={this.props.onClose}>
        <DialogContent>
          <DialogContentText>
            An error occured. Please check your network connection
            and the parameters.
          </DialogContentText>
        </DialogContent>
      </Dialog>
    );
  }
}