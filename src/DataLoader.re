
open Utils;

let module Styles = {
  open Glamor;
  let container = css [
    width "800px",
    alignSelf "center",
    maxWidth "100%",
  ];
  let input = css [
    padding "8px",
    fontFamily "inherit",
    fontWeight "inherit",
    fontSize "16px",
  ];
  let form = css [
    padding "8px",
    boxShadow "0 2px 5px #aaa",
    alignSelf "center",
    width "400px",
    maxWidth "100%",
  ];
  let title = css [
    fontSize "20px",
    alignSelf "center",
    letterSpacing "1px",
  ];
  let button = css [
    fontFamily "inherit",
    fontWeight "inherit",
    fontSize "20px",
    cursor "pointer",
    padding "8px",
    backgroundColor "#f0f0f0",
    border "none",
    Selector ":hover" [
      backgroundColor "#ddd",
    ],
    Selector ":disabled" [
      backgroundColor "white",
    ],
  ];
};

let module Form = {
  type location;
  external location: location = "" [@@bs.val];
  external hash: location => string = "" [@@bs.get];
  external setHash: location => string => unit = "hash" [@@bs.set];
  type state = {spreadsheet: string, budgets: string, transactions: string};
  let parseFromLocation () => {
    let search = Js.String.sliceToEnd from::1 (hash location);
    switch (Js.String.split "::" search) {
    | [|spreadsheet, budgets, transactions|] => {spreadsheet, budgets, transactions}
    | _ => {spreadsheet: "", budgets: "", transactions: ""}
    }
  };
  let component = ReasonReact.reducerComponent "Form";
  let make ::loading ::onSubmit _children => ReasonReact.{
    ...component,
    initialState: fun () => parseFromLocation (),
    reducer: fun action _ => ReasonReact.Update action,
    render: fun {state: {spreadsheet, budgets, transactions}, reduce} => {
      <div className=Styles.form>
        <div className=Styles.title>
          (str "Specify source sheet")
        </div>
        (spacer 8)
        <input
          value=spreadsheet
          className=Styles.input
          placeholder="Spreadsheet ID"
          onChange=(reduce (fun evt => {spreadsheet: evtValue evt, budgets, transactions}))
        />
        (spacer 8)
        <input
          value=budgets
          className=Styles.input
          placeholder="Budgets sheet name"
          onChange=(reduce (fun evt => {spreadsheet, budgets: evtValue evt, transactions}))
        />
        (spacer 8)
        <input
          value=transactions
          className=Styles.input
          placeholder="Mint data sheet name"
          onChange=(reduce (fun evt => {spreadsheet, budgets, transactions: evtValue evt}))
        />
        (spacer 8)
        <button
          className=Styles.button
          disabled=(Js.Boolean.to_js_boolean loading)
          onClick=(fun _ => {
            setHash location (spreadsheet ^ "::" ^ budgets ^ "::" ^ transactions);
            onSubmit {spreadsheet, budgets, transactions}
          })
        >
          (str (loading ? "Loading...": "Load"))
        </button>
      </div>
    }
  };
};

type state =
  | Initial
  | Loading
  | Error Js.Promise.error
  | Success (array (array Js.Json.t), array (array Js.Json.t))
  ;

let component = ReasonReact.reducerComponent "DataLoader";
let make ::accessToken ::render _children => ReasonReact.{
  ...component,
  initialState: fun () => Initial,
  reducer: fun action state => ReasonReact.Update action,
  render: fun {state, reduce} => {
    <div className=Styles.container>
      (spacer 32)
      <Form loading=(state === Loading) onSubmit=(fun {Form.spreadsheet, budgets, transactions} => {
        (reduce (fun _ => Loading)) ();
        Js.Promise.all2 (
          Sheet.values spreadsheet budgets accessToken,
          Sheet.values spreadsheet transactions accessToken
        )
        |> Js.Promise.then_ (fun (budgetData, transactionData) => {
          /* Sheet.setItem "budgetData" (Js.Json.stringifyAny budgetData); */
          /* Sheet.setItem "transactionData" (Js.Json.stringifyAny budgetData); */
          (reduce (fun _ => Success (budgetData, transactionData))) ();
          Js.Promise.resolve ()
        })
        |> Js.Promise.catch (fun err => {
          (reduce (fun _ => Error err)) ();
          Js.Promise.resolve ()
        }) |> ignore;
      })
      />
      (spacer 16)
      {switch state {
      | Initial => <div>(str "Fill in the form")</div>
      | Loading => <div>(str "Loading...")</div>
      | Error err => <div>(str "Failed")</div>
      | Success (budgetData, transactionData) => {
        render (budgetData, transactionData)
      }
      }}
    </div>
  },
};