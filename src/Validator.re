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
  render: fun {state: (year, month)} => {
    let (budgets, warnings) = Budget.parseBudgets budgetData;
    let (transactions, transactionWarnings, months) = Transactions.parseTransactions year month transactionData;

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
        (str @@ (string_of_int year) ^ ":" ^ (string_of_int month))
        (spacer 8)
        {render budgets transactions year month}
    </div>
  }
}
