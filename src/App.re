let component = ReasonReact.statelessComponent("App");

let str = ReasonReact.stringToElement;

let make = (~token, ~name, ~email, _children) =>
  ReasonReact.{
    ...component,
    render: (_) =>
      <div>
      <DataLoader
        accessToken=token
        render=(
          ((budgetData, transactionData)) =>
            <Validator
              budgetData
              transactionData
              render=(
                (budgets, categoryMap, year, month) => {
                  Js.log2(budgets, categoryMap);
                  <ReportView budgets categoryMap year month />
                }
              )
            />
        )
      />
      <button
        className=Glamor.(css([
          width("100px"),
          padding("8px 24px"),
          position("absolute"),
          top("16px"),
          left("16px"),
          backgroundColor("white"),
          border("1px solid #aaa"),
          borderRadius("8px"),
          cursor("pointer")
        ]))
      onClick=((_) => {
        [%bs.raw "(gapi.auth2.getAuthInstance().signOut(),location.reload())"]
        /* Auth.signOut(auth); */
      })
      >
      (Utils.str("Sign out"))
      </button>
      </div>
  };
