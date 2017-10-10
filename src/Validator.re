open Utils;

let currentMonthYear () => {
  let date = Js.Date.make ();
  (Js.Date.getFullYear date |> int_of_float, (Js.Date.getMonth date |> int_of_float) - 1)
  /** because previous month is what we're probably doing */
};

let component = ReasonReact.reducerComponent "Validator";
let make ::budgetData ::transactionData ::render _children => ReasonReact.{
  ...component,
  initialState: fun () => currentMonthYear (),
  reducer: fun action _ => ReasonReact.Update action,
  render: fun {state: (year, month), reduce} => {
    let (budgets, warnings) = Budget.parseBudgets budgetData;
    let (categoryMap, transactionWarnings, months) = Transactions.parseTransactions year month transactionData;

    <div>
        <div className=Glamor.(css[
          padding "8px 16px",
          backgroundColor "#eee",
        ])>
          (str "Warnings:")
          (List.map str warnings |> Array.of_list |> ReasonReact.arrayToElement)
          (List.map str transactionWarnings |> Array.of_list |> ReasonReact.arrayToElement)
        </div>
        (spacer 16)
        <div className=Glamor.(css[flexDirection "row", justifyContent "center"])>
        <button onClick=(reduce (fun _ => month === 0 ? (year - 1, 11) : (year, month - 1)))>
          (str "<")
        </button>
        (str @@ (string_of_int year) ^ ":" ^ (string_of_int (month + 1)))
        <button onClick=(reduce (fun _ => month === 11 ? (year + 1, 0) : (year, month + 1)))>
          (str ">")
        </button>
        </div>
        (spacer 8)
        {render budgets categoryMap year month}
    </div>
  }
}
