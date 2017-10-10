open Utils;
open Types;

let module Styles = {
  open Glamor;
  let base = [padding "8px", fontVariantNumeric "tabular-nums"];
  let goal = css [textAlign "right", ...base];
  let actual = css [textAlign "right", ...base];
  let name = css [padding "8px 8px 8px 32px"];
  let sumName = css [padding "8px 16px", fontWeight "600"];
  let calcName = css [padding "8px 16px", fontWeight "600"];
  let title = css [padding "8px 16px", marginTop "16px"];

  let diff = css [textAlign "right", ...base];
  let goodDiff = css [backgroundColor "#efe", textAlign "right", ...base];
  let badDiff = css [backgroundColor "#fee", textAlign "right", ...base];

  let date = css [fontFamily "monospace"];

  let originalDescription = css [fontFamily "monospace"];
};

let dollars num => {
  Printf.sprintf "$%0.2f" num
};

let maybeFlip flip num => flip ? -. num : num;

let component = ReasonReact.reducerComponent "ReportView";
/** TODO maybe add state about enabled budget items */

let make ::budgets ::categoryMap ::year ::month _children => ReasonReact.{
  ...component,
  initialState: fun () => None,
  reducer: fun action _ => ReasonReact.Update action,
  render: fun {state, reduce} => {
    let budget = Budget.latestBudget budgets year month;
    let amounts = try (Some (Budget.findAmounts budget.items categoryMap)) {
    | err => {Js.log err; None}
    };
    [%guard let Some amounts = amounts][@else <div>(str "Errored")</div>];
    let (amounts, untouched) = amounts;
    Js.log amounts;

    <div>
      (spacer 32)
      {Array.length untouched > 0 ?
        <div>
        <h3>(str "Untouched categories")</h3>
        (Array.map
        (fun name => {
          let cat = (Js.Dict.unsafeGet categoryMap name);
          <div>
          <div>(str @@ name ^ " : ") (str @@ string_of_float cat.monthTotal)</div>
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
      : ReasonReact.nullElement
      }
      <table className=Glamor.(css[borderCollapse "collapse", width "100%"])>
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
          (fun item => {
            let key = string_of_int (Budget.row item);
            let row = switch item {
            | Title name row => <tr key> <td className=Styles.title>(str name)</td> </tr>
            | Item name row isYearly categories goal => {
                let (g, m, y) = amounts.(row);
                <tr
                  key
                  onClick=(reduce (fun _ => state == Some (item, categories) ? None : Some (item, categories)))
                  className=Glamor.(css[
                    cursor "pointer",
                    Selector ":hover" [
                      backgroundColor "#fafafa",
                    ],
                  ])
                >
                  <td className=Styles.name> (str name)  </td>
                  <td className=Styles.goal> (str (goal |> optMap dollars |> optOr "")) </td>
                  <td className=Styles.actual> (str (dollars m)) </td>
                  {
                    let amount = goal |> optMap (fun n => n -. (isYearly ? y : m));
                    let className = switch amount { | None => "" | Some x when x >= 0. => Styles.goodDiff | _ => Styles.badDiff };
                    <td className> (str (amount |> optMap dollars |> optOr "")) </td>
                  }
                  <td className=Styles.actual> (str (dollars y)) </td>
                </tr>
              }
            | Calculated name row isYearly _ goal flip => {
                let (g, m, y) = amounts.(row);
                <tr key>
                  <td className=Styles.calcName> (str name) </td>
                  <td className=Styles.goal> (str (dollars goal)) </td>
                  <td className=Styles.actual> (str (dollars m)) </td>
                  {
                    let amount = maybeFlip flip @@ goal -. (isYearly ? y : m);
                    let className = amount > 0. ? Styles.goodDiff : Styles.badDiff;
                    <td className> (str (dollars @@ amount)) </td>
                  }
                  <td className=Styles.actual> (str (dollars y)) </td>
                </tr>
              }
            | Sum name row isYearly _ => {
                let (g, m, y) = amounts.(row);
                <tr key>
                <td className=Styles.sumName> (str name)  </td>
                <td className=Styles.goal> (str (dollars g)) </td>
                <td className=Styles.actual> (str (dollars m)) </td>
                {
                    let amount = g -. (isYearly ? y : m);
                    let className = amount > 0. ? Styles.goodDiff : Styles.badDiff;
                    <td className> (str (dollars @@ amount)) </td>
                }
                <td className=Styles.actual> (str (dollars y)) </td>
                </tr>
              }
            };
            switch (state) {
            | Some (i, cat) when i == item => {
              let transactionRow = <tr key=(key ^ "list")>
                <td className=Glamor.(css[padding "16px", ]) colSpan=5>
                  <div className=Glamor.(css[padding "16px",border "3px solid #eee"])>
                  <table className=Glamor.(css[width "100%"])>
                    /* <thead>
                      <tr>
                        <th>(str "Date")</th>
                        <th>(str "Description")</th>
                        <th>(str "Amount")</th>
                        <th>(str "Type")</th>
                      </tr>
                    </thead> */
                    <tbody>
                      (Array.map
                      (fun cat => (List.map
                      (fun tr => {
                        <tr>
                          <td className=Styles.date>(str (tr.date |> Transactions.serialDate |> Js.Date.toDateString))</td>
                          <td>(str tr.description)</td>
                          <td className=Styles.actual>(str @@ dollars tr.amount)</td>
                          <td className=Styles.originalDescription title=tr.originalDescription>(str @@ Js.String.slice from::0 to_::10 tr.originalDescription)</td>
                        </tr>
                      })
                      (Js.Dict.unsafeGet categoryMap cat).monthTransactions
                      )
                      |> (fun items => [ <tr> <td colSpan=4 className=Glamor.(css[textAlign "center", fontWeight "600"])>(str cat)</td> </tr> , ...items])
                      |> Array.of_list)
                      cat
                      |> Array.to_list
                      |> Array.concat
                      |> ReasonReact.arrayToElement)
                    </tbody>
                  </table>
                  </div>
                </td>
              </tr>;
              [|row, transactionRow|]
            }
            | _ => [|row|]
            }
          })
          budget.items
          |> Array.to_list
          |> Array.concat
          |> ReasonReact.arrayToElement
        }
      </tbody>
      </table>
    </div>
  }
};

/* type state =  */
/* (list Types.budget, list string), (Js.Dict.t Types.category, list string) */