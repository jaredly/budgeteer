
let component = ReasonReact.statelessComponent "App";
let str = ReasonReact.stringToElement;

let make ::token ::name ::email _children => ReasonReact.{
  ...component,
  render: fun _ => {

    /* let sheetId = "1ceZD5lJ8JPbGaRPcA3jFrxKB7nIelkd2ntP675A4YLk";
    let range = "Budgets"; */

    /* Sheet.values sheetId range (Auth.token user)
    |> Js.Promise.then_ (fun values => {
      Budget.parseBudgets values |> Js.log;
      Js.Promise.resolve ()})
    |> ignore; */

    <DataLoader
      accessToken=token
      render=(fun (budgetData, transactionData) => {
        <Validator
          budgetData
          transactionData
          render=(fun budgets categoryMap year month => {
            Js.log2 budgets categoryMap;
            <ReportView budgets categoryMap year month />
          })
        />
      })
    />
  }
}
