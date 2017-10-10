open Utils;
open Types;

let module Styles = {
  open Glamor;
  let base = [padding "8px", fontVariantNumeric "tabular-nums"];
  let goal = css [textAlign "right", ...base];
  let actual = css [textAlign "right", ...base];
  let diff = css [textAlign "right", ...base];
  let name = css [padding "8px 8px 8px 32px"];
  let sumName = css [padding "8px 16px", fontWeight "600"];
  let calcName = css [padding "8px 16px", fontWeight "600"];
  let title = css [padding "8px 16px", marginTop "16px"];
};

let dollars num => {
  Printf.sprintf "$%0.2f" num
};

let component = ReasonReact.statelessComponent "ReportView";
/** TODO maybe add state about enabled budget items */

let make ::budgets ::transactions ::year ::month _children => ReasonReact.{
  ...component,
  render: fun _ => {
    let budget = Budget.latestBudget budgets year month;
    let amounts = try (Some (Budget.findAmounts budget.items transactions)) {
    | err => {Js.log err; None}
    };
    [%guard let Some amounts = amounts][@else <div>(str "Errored")</div>];
    let (amounts, untouched) = amounts;
    Js.log amounts;

    <div>
      (spacer 32)
      <table>
      <thead>
        <tr>
          <th>(str "Name")</th>
          <th>(str "Goal")</th>
          <th>(str "Actual")</th>
          <th>(str "Diff")</th>
          <th>(str "YTD")</th>
        </tr>
      </thead>
      <tbody>
        {
          Array.map
          (fun item => switch item {
            | Title name row => <tr> <td className=Styles.title>(str name)</td> </tr>
            | Item name row _ _ goal => <tr>
                <td className=Styles.name> (str name)  </td>
                <td className=Styles.goal> (str (goal |> optMap dollars |> optOr "")) </td>
                <td className=Styles.actual> (str (dollars amounts.(row))) </td>
                <td className=Styles.diff> (str (goal |> optMap (fun n => n -. amounts.(row)) |> optMap dollars |> optOr "")) </td>
                </tr>
            | Calculated name row _ goal => <tr>
                <td className=Styles.calcName> (str name)  </td>
                <td className=Styles.goal> (str (dollars goal)) </td>
                <td className=Styles.actual> (str (dollars amounts.(row))) </td>
                <td className=Styles.diff> (str (dollars @@ goal -. amounts.(row))) </td>
              </tr>
            | Sum name row _ => <tr>
                <td className=Styles.sumName> (str name)  </td>
                <td className=Styles.goal> /** TODO the sum of the goals */ </td>
                <td className=Styles.actual> (str (dollars amounts.(row))) </td>
                </tr>
          })
          budget.items
          |> ReasonReact.arrayToElement
        }
      </tbody>
      </table>
      <div>
        <h3>(str "Untouched categories")</h3>
        (Array.map
        (fun name => {
          let cat = (Js.Dict.unsafeGet transactions name);
          <div>
          <div>(str name) (str @@ string_of_float cat.monthTotal)</div>
          <div className=Glamor.(css[paddingLeft "16px"])>
            (List.map
            (fun tr => {
              <div>
                (str tr.description)
              </div>
            })
            cat.monthTransactions
            |> Array.of_list
            |> ReasonReact.arrayToElement)
          </div>
        </div>
        })
        untouched
        |> ReasonReact.arrayToElement)
      </div>
    </div>
  }
};

/* type state =  */
/* (list Types.budget, list string), (Js.Dict.t Types.category, list string) */