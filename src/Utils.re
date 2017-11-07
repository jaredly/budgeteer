let optMap = (fn, value) =>
  switch value {
  | None => None
  | Some(v) => Some(fn(v))
  };

let optBind = (fn, value) =>
  switch value {
  | None => None
  | Some(v) => fn(v)
  };

let optOr = (default, value) =>
  switch value {
  | None => default
  | Some(v) => v
  };

let str = ReasonReact.stringToElement;

let spring = <div style=ReactDOMRe.Style.(make(~flexGrow="1", ())) />;

let spacer = (~key=?, num) =>
  <div ?key style=ReactDOMRe.Style.(make(~flexBasis=string_of_int(num) ++ "px", ())) />;

let evtValue = (event) => ReactDOMRe.domElementToObj(ReactEventRe.Form.target(event))##value;
