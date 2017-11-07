open Types;

open Utils;

let getCol = (values, row, col) =>
  if (col >= Array.length(values[row])) {
    None
  } else {
    let value = values[row][col];
    switch (Js.Json.classify(value)) {
    | Js.Json.JSONString("") => None
    | json => Some(json)
    }
  };

let getString = (item) =>
  switch item {
  | Js.Json.JSONString(value) => Some(value)
  | _ => None
  };

let serialDate = (num) => {
  let utcSeconds = floor(num -. 25568.) *. 86400.;
  Js.Date.fromFloat(utcSeconds *. 1000.)
};

let parseDate = (num) => {
  let date = serialDate(num);
  (Js.Date.getFullYear(date) |> int_of_float, Js.Date.getMonth(date) |> int_of_float)
};

module MonthSet =
  Set.Make(
    {
      type t = (int, int);
      let compare = compare;
    }
  );

let parseTransactions = (currentYear, currentMonth, data) => {
  let categories = Js.Dict.empty();
  let warnings = ref([]);
  let warn = (warning) => warnings := [warning, ...warnings^];
  let months = ref(MonthSet.empty);
  let addTransaction = (transaction) => {
    let cat =
      switch (Js.Dict.get(categories, transaction.category)) {
      | None => {
          name: transaction.category,
          byMonth: IntPairMap.empty,
          /* monthTotal: 0.,
          yearTotal: 0.,
          yearTransactions: [],
          monthTransactions: [] */
        }
      | Some(cat) => cat
      };
    let key = (transaction.year, transaction.month);
    let (total, items) = IntPairMap.mem(key, cat.byMonth) ? IntPairMap.find(key, cat.byMonth) : (0.0, []);
    let current = (total +. transaction.amount, [transaction, ...items]);
    let cat = {
      ...cat,
      byMonth: IntPairMap.add(key, current, cat.byMonth)
      /* monthTotal: (transaction.month === currentMonth ? transaction.amount : 0.) +. cat.monthTotal,
      yearTotal: cat.yearTotal +. transaction.amount,
      yearTransactions: [transaction, ...cat.yearTransactions],
      monthTransactions:
        transaction.month === currentMonth ?
          [transaction, ...cat.monthTransactions] : cat.monthTransactions */
    };
    Js.Dict.set(categories, transaction.category, cat)
  };
  /* Date	Description	Original Description	Amount	Transaction Type	Category	Account Name	Labels	Notes */
  for (i in Array.length(data) - 1 downto 1) {
    let warn = (message) => {
      Js.log2(message, data[i]);
      warn(message)
    };
    let getCol = getCol(data, i);
    open Js.Json;
    [@else ()] [%guard let true = Array.length(data[i]) > 0];
    [@else warn("No date")] [%guard let Some(JSONNumber(date)) = getCol(0)];
    let (year, month) = parseDate(date);
    months := MonthSet.add((year, month), months^);
    [@else ()] [%guard let true = year === currentYear && month <= currentMonth];
    [@else warn("No description")] [%guard let Some(JSONString(description)) = getCol(1)];
    [@else warn("No original description")]
    [%guard let Some(JSONString(originalDescription)) = getCol(2)];
    [@else warn("No amount")] [%guard let Some(JSONNumber(amount)) = getCol(3)];
    let isCredit = getCol(4) |> optBind(getString) == Some("credit");
    [@else warn("No category")] [%guard let Some(JSONString(category)) = getCol(5)];
    [@else warn("No category")] [%guard let Some(JSONString(account)) = getCol(6)];
    let labels = getCol(7) |> optBind(getString);
    let notes = getCol(8) |> optBind(getString);
    addTransaction({
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
      notes
    });
    ()
  };
  (categories, warnings^, months^)
};
