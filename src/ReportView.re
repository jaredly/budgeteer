open Utils;
open Types;

let latestBudget budgets year month => {
  switch budgets {
  | [] => failwith "No budgets"
  | [first, ...rest] =>
    List.fold_left
    (fun current next => {
      if (next.startYear !== year || next.startMonth > month) {
        current
      } else {
        next
      }
    })
    first
    rest
  }
};

let row item => switch item {
| Title _ num
| Calculated _ num _ _
| Sum _ num _
| Item _ num _ _ _ => num
};

let force opt => switch opt { | Some v => v | None => failwith "Forced a none"};

let module StringSet = Set.Make {type t = string; let compare = compare};

/* type calcState = Unresolved | Resolving | Resolved float; */
let findAmounts items categoryMap => {
  let maxRow = Array.fold_left (fun num item => max num (row item)) 0 items;
  let amounts = Array.make (maxRow + 1) `Unresolved;
  let itemsByRow = Array.make (maxRow + 1) None;
  let touchedCategories = ref StringSet.empty;
  Array.iter (fun item => itemsByRow.(row item) = Some item) items;
  let rec resolve item => {
    let thisRow = row item;
    switch amounts.(thisRow) {
    | `Resolved value => value
    | `Resolving => failwith "Recursive definition"
    | `Unresolved => {
        /* Js.log3 thisRow item amounts.(thisRow); */
        amounts.(thisRow) = `Resolving;
        let value = switch item {
        | Title _ => 0.
        | Item _ _ _ categories _ => (Array.fold_left
            (fun total category => {
              touchedCategories := StringSet.add category !touchedCategories;
              total +. (Js.Dict.get categoryMap category |> optMap (fun c => c.monthTotal) |> optOr 0.)
            })
            0.
            categories)
        | Calculated _ _ calc _ => doCalc calc
        | Sum _ _ (fromRow, toRow) => {
          /* Js.log3 "summing" fromRow toRow; */
          let total = ref 0.;
          for i in fromRow to toRow {
            let next = switch itemsByRow.(i - 1) {
            | Some item => resolve item
            | None => 0.
            };
            /* Js.log2 i next; */
            total := !total +. next;
          };
          /* Js.log2 "Summed" !total; */
          !total
        }
        };
        /* Js.log3 "Finished" value item; */
        amounts.(row item) = `Resolved value;
        value
      }
    }
  } and doCalc calc => switch calc {
  | Unit => 0.
  | Plus one two => doCalc one +. doCalc two
  | Minus one two => {
    Js.log3 "minus" one two;
    let a = doCalc one;
    let b = doCalc two;
    Js.log4 "was" a b (one, two);
    a -. b
  }
  | Row at => resolve (force itemsByRow.(at - 1)) /* TODO may be off by 1 */
  }
  ;
  for i in 0 to (Array.length items - 1) {
    resolve (items.(i)) |> ignore;
  };
  let amounts = Array.map (fun amount => switch amount {
  | `Unresolved => 0.
  | `Resolving => 0.
  | `Resolved value => value
  })
  amounts;
  let untouched = Js.Dict.keys categoryMap
  |> Js.Array.filter
  (fun cat => not (StringSet.mem cat !touchedCategories) && (Js.Dict.unsafeGet categoryMap cat).monthTotal > 0.);
  (amounts, untouched)
};

let component = ReasonReact.statelessComponent "ReportView";
/** TODO maybe add state about enabled budget items */

let make ::budgets ::transactions ::year ::month _children => ReasonReact.{
  ...component,
  render: fun _ => {
    let budget = latestBudget budgets year month;
    let amounts = try (Some (findAmounts budget.items transactions)) {
    | err => {Js.log err; None}
    };
    [%guard let Some amounts = amounts][@else <div>(str "Errored")</div>];
    let (amounts, untouched) = amounts;
    Js.log amounts;
    <div>
      (str "hi")
      <table>
      <thead>
        <th>(str "Name")</th>
        <th>(str "Goal")</th>
        <th>(str "Actual")</th>
        <th>(str "Diff")</th>
        <th>(str "YTD")</th>
      </thead>
      <tbody>
        {
          Array.map
          (fun item => switch item {
            | Title name row => <tr> <td className=Glamor.(css[paddingTop "16px"])>(str name)</td> </tr>
            | Item name row _ _ goal => <tr>
                <td> (str name)  </td>
                <td> (str (goal |> optMap string_of_float |> optOr "")) </td>
                <td> (str (string_of_float amounts.(row))) </td>
                <td> (str (goal |> optMap (fun n => n -. amounts.(row)) |> optMap string_of_float |> optOr "")) </td>
                </tr>
            | Calculated name row _ goal => <tr>
                <td className=Glamor.(css[fontWeight "600"])> (str name)  </td>
                <td> (str (string_of_float goal)) </td>
                <td> (str (string_of_float amounts.(row))) </td>
                <td> (str (string_of_float @@ goal -. amounts.(row))) </td>
              </tr>
            | Sum name row _ => <tr>
                <td className=Glamor.(css[fontWeight "600"])> (str name)  </td>
                <td> /** TODO the sum of the goals */ </td>
                <td> (str (string_of_float amounts.(row))) </td>
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