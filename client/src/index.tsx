import * as React from "react";
import * as ReactDOM from "react-dom";
import Main from "stopping-time/ui/Main";
import { MyObj, do_some_thing } from "stopping-time/api/demo";

do_some_thing({ msg: "Hi" });

ReactDOM.render(
  <Main />,
  document.getElementById("root")
);