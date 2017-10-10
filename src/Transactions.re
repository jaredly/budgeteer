
open Types;
open Utils;

let getCol values row col => if (col >= Array.length values.(row)) { None } else {
  let value = (values.(row).(col));
  switch (Js.Json.classify value) {
  | Js.Json.JSONString "" => None
  | json => Some json
  }
};

let getString item => switch item {
| Js.Json.JSONString value => Some value
| _ => None
};

let parseDate num => {
  let utcSeconds = floor (num -. 25568.) *. 86400.;
  let date = Js.Date.fromFloat (utcSeconds *. 1000.);
  (Js.Date.getFullYear date |> int_of_float, Js.Date.getMonth date |> int_of_float)
};

let module MonthSet = Set.Make {
  type t = (int, int);
  let compare = compare;
};

let parseTransactions currentYear currentMonth data => {
  let categories = Js.Dict.empty ();
  let warnings = ref [];
  let warn warning => warnings := [warning, ...!warnings];
  let months = ref MonthSet.empty;

  let addTransaction transaction => {
    let cat = switch (Js.Dict.get categories transaction.category) {
    | None => {name: transaction.category, monthTotal: 0., yearTotal: 0., yearTransactions: [], monthTransactions: []}
    | Some cat => cat
    };
    let cat = {
      ...cat,
      monthTotal: (transaction.month === currentMonth ? transaction.amount : 0.) +. cat.monthTotal,
      yearTotal: cat.yearTotal +. transaction.amount,
      yearTransactions: [transaction, ...cat.yearTransactions],
      monthTransactions: transaction.month === currentMonth ? [transaction, ...cat.monthTransactions] : cat.monthTransactions,
    };
    Js.Dict.set categories transaction.category cat;
  };

/* Date	Description	Original Description	Amount	Transaction Type	Category	Account Name	Labels	Notes */
  for i in (Array.length data - 1) downto 1 {
    let warn message => {
      Js.log2 message data.(i);
      warn message;
    };
    let getCol = getCol data i;
    open Js.Json;
    [%guard let true = Array.length data.(i) > 0][@else ()];
    [%guard let Some (JSONNumber date) = getCol 0][@else warn "No date"];
    let (year, month) = parseDate date;
    months := MonthSet.add (year, month) !months;
    [%guard let true = year === currentYear && month <= currentMonth][@else ()];
    [%guard let Some (JSONString description) = getCol 1][@else warn "No description"];
    [%guard let Some (JSONString originalDescription) = getCol 2][@else warn "No original description"];
    [%guard let Some (JSONNumber amount) = getCol 3][@else warn "No amount"];
    let isCredit = (getCol 4 |> optBind getString) == Some "credit";
    [%guard let Some (JSONString category) = getCol 5][@else warn "No category"];
    [%guard let Some (JSONString account) = getCol 6][@else warn "No category"];
    let labels = getCol 7 |> optBind getString;
    let notes = getCol 8 |> optBind getString;
    addTransaction {
      date,
      month,
      year,
      description,
      originalDescription,
      amount,
      isCredit,
      category,
      account,
      labels,
      notes,
    };
    ()
  };
  (categories, !warnings, !months)
};
