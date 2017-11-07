open Utils;

open Types;

module Styles = {
  open Glamor;
  let base = [padding("8px"), fontVariantNumeric("tabular-nums")];
  let goal = css([textAlign("right"), ...base]);
  let actual = css([textAlign("right"), ...base]);
  let actualSelected = css([backgroundColor("#eef"), ...base]);
  let name = css([padding("8px 8px 8px 32px")]);
  let sumName = css([padding("8px 16px"), fontWeight("600")]);
  let calcName = css([padding("8px 16px"), fontWeight("600")]);
  let title = css([padding("8px 16px"), marginTop("16px")]);
  let diff = css([textAlign("right"), ...base]);
  let goodDiff = css([backgroundColor("#efe"), textAlign("right"), ...base]);
  let badDiff = css([backgroundColor("#fee"), textAlign("right"), ...base]);
  let date = css([fontFamily("monospace")]);
  let originalDescription = css([fontFamily("monospace")]);
  let spacer = css([width("32px")]);
};

let dollars = (num) => Printf.sprintf("$%0.2f", num);

let maybeFlip = (flip, num) => flip ? -. num : num;

type ly = Yearly|Monthly;
let component = ReasonReact.reducerComponent("ReportView");

let renderTransaction = (tr) => {
  <tr>
    <td className=Styles.date>
      (
        str(
          tr.date
          |> Transactions.serialDate
          |> Js.Date.toDateString
        )
      )
    </td>
    <td> (str(tr.description)) </td>
    <td className=Styles.actual>
      (str @@ dollars(tr.amount))
    </td>
    <td
      className=Styles.originalDescription
      title=tr.originalDescription>
      (
        str @@
        Js.String.slice(~from=0, ~to_=10, tr.originalDescription)
      )
    </td>
  </tr>
};

let module OptionButtons = {
  let shared = Glamor.([
    padding("4px 8px"),
    border("1px solid #eee"),
    borderRadius("8px"),
    cursor("pointer")
  ]);
  let selected = Glamor.(css([
    backgroundColor("#aaa"),
    ...shared
  ]));
  let unselected = Glamor.(css([
    backgroundColor("white"),
    ...shared
  ]));

  let component = ReasonReact.statelessComponent("OptionButtons");
  let make = (~current, ~options, ~onChange, _) => {
    ...component,
    render: (_) => {
      <div className=Glamor.(css([flexDirection("row"), justifyContent("center")]))>
        (List.map(
        (((value, title)) => {
          <div onClick=((_) => onChange(value))
            className=(value == current ? selected : unselected)
          >
            (str(title))
          </div>
        }), options)
        |> Array.of_list
        |> ReasonReact.arrayToElement)
      </div>
    }
  };
};

let renderTransactionRow = (~key, ~cat, ~categoryMap, ~ly, ~onChangeLy, ~currentYear, ~currentMonth) => {
  <tr key=(key ++ "list")>
    <td className=Glamor.(css([])) colSpan=8>
      <div
      >
        <OptionButtons
          current=ly
          options=[(Monthly, "This Month"), (Yearly, "YTD")]
          onChange=onChangeLy
        />
      </div>
      <div className=Glamor.(css([padding("16px"), border("3px solid #eee")]))>
        <table className=Glamor.(css([width("100%")]))>
            <tbody>
              (
                Array.map(
                  (cat) =>
                    List.map(
                      (tr) =>
                        renderTransaction(tr),
                      Js.Dict.get(categoryMap, cat)
                      |> optMap((a) => ly === Yearly ? Cat.ytdTransactions(a, currentYear, currentMonth) : Cat.transactions(a, (currentYear, currentMonth)))
                      |> optOr([])
                    )
                    |> (
                      (items) => [
                        <tr>
                          <td
                            colSpan=4
                            className=Glamor.(css([ textAlign("center"), fontWeight("600") ]))
                          >
                            (str(cat))
                          </td>
                        </tr>,
                        ...items
                      ]
                    )
                    |> Array.of_list,
                  cat
                )
                |> Array.to_list
                |> Array.concat
                |> ReasonReact.arrayToElement
              )
            </tbody>
          </table>
      </div>
    </td>
  </tr>;
};

let renderUntouched = (~untouched, ~categoryMap, ~year, ~month) => {
  <div>
    <h3> (str("Non-organized categories")) </h3>
    (
      Array.map(
        (name) => {
          let cat = Js.Dict.unsafeGet(categoryMap, name);
          <div>
            <div> (str @@ name ++ " : ") (str @@ string_of_float(Cat.total(cat, (year, month)))) </div>
            <div className=Glamor.(css([paddingLeft("16px")]))>
              (
                List.map((tr) => <div> (str(tr.description)) </div>, Cat.transactions(cat, (year, month)))
                |> Array.of_list
                |> ReasonReact.arrayToElement
              )
            </div>
          </div>
        },
        untouched
      )
      |> ReasonReact.arrayToElement
    )
    (spacer(32))
  </div>
};

let renderItem = (~m, ~y, ~item, ~categories, ~categoryMap, ~key, ~reduce, ~name, ~goal, ~isYearly, ~state, ~year, ~month, ~budgets) => {
  let monthSelected = state == Some((item, categories, Monthly));
  let yearSelected = state == Some((item, categories, Yearly));
  let isSelected = monthSelected || yearSelected;
  let totalItems =
    Array.fold_left(
      (total, cat) =>
        total
        + (
          Js.Dict.get(categoryMap, cat)
          |> optMap((a) => List.length(Cat.transactions(a, (year, month))))
          |> optOr(0)
        ),
      0,
      categories
    );
  let expectedYtd = if (! isYearly) {
    let total = ref(0.);
    for (i in 0 to month) {
      let goal =
        Budget.latestBudget(budgets, year, i)
        |> optBind((budget) => Budget.goalByName(budget, name))
        |> optOr(0.);
      /* Js.log3("Reporting") */
      total := total^ +. goal;
    };
    total^ > 0. ? Some(total^) : None
  } else {
    goal |> optMap((goal) => goal *. float_of_int(month) /. 12.)
  };
  <tr
    key
    onClick=(
      reduce(
        (_) => isSelected ? None : Some((item, categories, Monthly))
      )
    )
    className=Glamor.(
                css([
                  cursor("pointer"),
                  Selector(":hover", [backgroundColor("#fafafa")])
                ])
              )>
    <td className=Styles.name>
      <div className=Glamor.(css([flexDirection("row")]))>
        (str(name))
        spring
        <div> (str(string_of_int(totalItems))) </div>
      </div>
    </td>
    <td className=Styles.goal>
      (str(goal |> optMap(dollars) |> optOr("")))
    </td>
    <td
      className=(Styles.actual ++ " " ++ (monthSelected ? Styles.actualSelected : ""))
    > (str(dollars(m))) </td>
    {
      let amount = goal |> optMap((n) => n -. (isYearly ? y : m));
      let className =
        switch amount {
        | None => ""
        | Some(x) when x >= 0. => Styles.goodDiff
        | _ => Styles.badDiff
        };
      <td className> (str(amount |> optMap(dollars) |> optOr(""))) </td>
    }
    <td className=Styles.spacer />
    <td className=(Styles.actual ++ " " ++ (yearSelected ? Styles.actualSelected : ""))
    onClick=(
      reduce(
        (evt) => {
          ReactEventRe.Mouse.stopPropagation(evt);
          isSelected
            ? None
            : Some((item, categories, Yearly))
        }
      )
    )
    > (str(dollars(y))) </td>
    <td className=Styles.actual>
        (
          (expectedYtd) |> optMap((value) => str(dollars(value))) |> optOr(ReasonReact.nullElement)
        )
    </td>
    {
      [@else ReasonReact.nullElement]
      [%guard let Some(ytd) = expectedYtd];
      let diff = ytd -. y;
      let className = diff >= 0. ? Styles.goodDiff : Styles.badDiff;
      <td className>
        (str(dollars(diff)))
      </td>
    }
  </tr>
};

/*** TODO maybe add state about enabled budget items */
let make = (~budgets, ~categoryMap, ~year, ~month, _children) =>
  ReasonReact.{
    ...component,
    initialState: () => None,
    reducer: (action, _) => ReasonReact.Update(action),
    render: ({state, reduce}) => {
      [@else failwith("No budget to be found")]
      [%guard let Some(budget) = Budget.latestBudget(budgets, year, month)];
      let amounts =
        try (Some(Budget.findAmounts(~currentYear=year, ~currentMonth=month, budget.items, categoryMap))) {
        | err =>
          Js.log(err);
          None
        };
      [@else <div> (str("Errored")) </div>] [%guard let Some(amounts) = amounts];
      let (amounts, untouched) = amounts;
      /* Js.log amounts; */
      <div className=Glamor.(css([paddingBottom("128px")]))>
        (spacer(32))
        (
          Array.length(untouched) > 0 ?
            renderUntouched(~untouched, ~categoryMap, ~year, ~month) :
            ReasonReact.nullElement
        )
        <table className=Glamor.(css([borderCollapse("collapse"), width("100%")]))>
          <thead>
            <tr>
              <th> (str("Name")) </th>
              <th> (str("Goal")) </th>
              <th> (str("Actual")) </th>
              <th> (str("Diff")) </th>
              <th className=Styles.spacer />
              <th> (str("YTD")) </th>
              <th> (str("Expected YTD")) </th>
              <th> (str("YTD Diff")) </th>
            </tr>
          </thead>
          <tbody>
            (
              Array.map(
                (item) => {
                  let key = string_of_int(Budget.row(item));
                  let row =
                    switch item {
                    | Title(name, row) =>
                      <tr key> <td className=Styles.title> (str(name)) </td> </tr>
                    | Item(name, row, isYearly, categories, goal) => {
                      let (_, m, y) = amounts[row];
                      renderItem(~m, ~y, ~item, ~categories, ~categoryMap, ~key, ~reduce, ~name, ~goal, ~isYearly, ~state, ~year, ~month, ~budgets)
                    }
                    | Calculated(name, row, isYearly, _, goal, flip) =>
                      let (g, m, y) = amounts[row];
                      <tr key className=Styles.calcName>
                        <td className=Styles.calcName> (str(name)) </td>
                        <td className=Styles.goal> (str(dollars(goal))) </td>
                        <td className=Styles.actual> (str(dollars(m))) </td>
                        {
                          let amount = maybeFlip(flip) @@ goal -. (isYearly ? y : m);
                          let className = amount > 0. ? Styles.goodDiff : Styles.badDiff;
                          <td className> (str(dollars @@ amount)) </td>
                        }
                        <td className=Styles.spacer />
                        <td className=Styles.actual> (str(dollars(y))) </td>
                      </tr>
                    | Sum(name, row, isYearly, _) =>
                      let (g, m, y) = amounts[row];
                      <tr key className=Styles.sumName>
                        <td className=Styles.sumName> (str(name)) </td>
                        <td className=Styles.goal> (str(dollars(g))) </td>
                        <td className=Styles.actual> (str(dollars(m))) </td>
                        {
                          let amount = g -. (isYearly ? y : m);
                          let className = amount > 0. ? Styles.goodDiff : Styles.badDiff;
                          <td className> (str(dollars @@ amount)) </td>
                        }
                        <td className=Styles.spacer />
                        <td className=Styles.actual> (str(dollars(y))) </td>
                      </tr>
                    };
                  switch state {
                  | Some((i, cat, ly)) when i == item =>
                    let transactionRow = renderTransactionRow(~key, ~cat, ~categoryMap, ~ly,
                      ~onChangeLy=((value) => reduce(() => Some((i, cat, value)))()),
                      ~currentYear=year,
                      ~currentMonth=month
                    );
                    [|row, transactionRow|]
                  | _ => [|row|]
                  }
                },
                budget.items
              )
              |> Array.to_list
              |> Array.concat
              |> ReasonReact.arrayToElement
            )
          </tbody>
        </table>
      </div>
    }
  };
/* type state =  */
/* (list Types.budget, list string), (Js.Dict.t Types.category, list string) */
