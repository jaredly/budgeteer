
open Types;
open Js.Json;

let nonnullString item => {
  switch (classify item) {
  | JSONString text when text !== "" => Some text
  | _ => None
  }
};
let optMap fn value => switch value { | None => None | Some v => Some (fn v) };
let optBind fn value => switch value { | None => None | Some v => fn v };

let maybe_int value => try (Some (int_of_string value)) {
| _ => None
};

let parseSum text => {
  let rx = [%bs.re "/^\\=sum\\([A-Z]+(\\d+):[A-Z]+(\\d+)\\)$/"];
  Js.String.match_ rx text |> optBind (fun items => switch items {
  | [|_, fromString, toString|] => switch (maybe_int fromString, maybe_int toString) {
    | (Some a, Some b) => Some (a, b)
    | _ => None
    }
  | _ => None
  })
};

let getRow single => {
  let rx = [%bs.re "/\\d+/g"];
  Js.String.match_ rx single |> optBind (fun items => switch items {
  | [|number|] => maybe_int number
  | _ => None
  });
};

let parseCalc text => {
  [%guard let '=' = text.[0]][@else None];
  let text = Js.String.sliceToEnd from::1 text;
  let rx = [%bs.re "/\\+/g"];
  let parts = Js.String.splitByRe rx text;
  Js.log2 text parts;
  Array.fold_left (fun current next => {
    [%guard let Some current = current][@else None];

    switch (Js.String.splitByRe [%bs.re "/\\-/g"] next) {
    | [|single|] => getRow single |> optMap (fun n => Row n)
    | minuses => {
        Js.log2 next minuses;
        Array.fold_left 
        (fun current next => {
          switch current {
          | None => None
          | Some current => getRow next |> optMap (fun row => Minus current (Row row))
          }
        })
        (getRow minuses.(0) |> optMap (fun n => Row n))
        (Js.Array.sliceFrom 1 minuses)
      }
    } |> (fun parsed => switch parsed {
    | Some item => Some (Plus current item)
    | None => None
    })
  })
  (Some Unit)
  parts
};

let parseBudgets values => {
  let budgets = ref [];
  let warnings = ref [];
  let warn warning => warnings := [warning, ...!warnings];
  /** 5 columns and a space between */
  let numBudgets = (Array.length values.(0) + 3) / 6;
  Js.log numBudgets;
  Js.log values;
  Js.log (Array.length values.(0) + 3);
  for i in 0 to (numBudgets - 1) {
    let off = i * 6;
    [%guard let JSONNumber startMonth = classify (values.(0).(off + 1))][@else warn "No starting month"];
    [%guard let JSONNumber startYear = classify (values.(0).(off + 2))][@else warn "No starting year"];
    let items = ref [];
    let add item => items := [item, ...!items];
    /** NOTE headers are just for show. Maybe in future I'd detect which columns are which. */
    for row in 2 to (Array.length values - 1) {
      let getCol col => if (col >= Array.length values.(row)) { None } else {
        let value = (values.(row).(col));
        switch (classify value) {
        | JSONString "" => None
        | _ => Some value
        }
      };

      /** If this isn't a named thing, ditch it */
      [%guard let true = Array.length values.(row) >= 3 + off][@else ()];
      [%guard let Some (JSONString name) = getCol (off + 2) |> optMap classify][@else warn "No name"];
      switch (getCol off |> optMap classify) {
      | None => {
        switch (getCol (off + 4) |> optMap classify) {
        | None => add (Title name row)
        | Some (JSONString calcReport) => {
            switch (getCol (off + 3) |> optMap classify) {
            | None => warn "Missing goal"
            | Some (JSONString calcGoal) => switch (parseSum calcReport) {
              /** TODO warn if sums are in the wrong columns, or if calcGoal doesn't match */
              | Some (fromRow, toRow) => add (Sum name row (fromRow, toRow))
              | None => warn "Bad sum"
              }
            | Some (JSONNumber goal) => switch (parseCalc calcReport) {
              | Some calc => add (Calculated name row calc goal)
              | None => warn "Bad calc report"
              }
            | _ => ()
            }
          }
        | _ => ()
        }
      }
      | Some (JSONString categories) => {
        let categories = Js.String.split "|" categories;
        let goal = switch (getCol (off + 3) |> optMap classify) {
        | Some (JSONNumber goal) => Some (goal)
        | None => None
        | v => {Js.log v; warn "Bad goal"; None}
        };
        let isYearly = (getCol (off + 1) !== None);
        add (Item name row isYearly categories goal)
      }
      | _ => ()
      };
    };
    let budget = {
      startMonth: int_of_float startMonth,
      startYear: int_of_float startYear,
      items: Array.of_list !items |> Js.Array.reverseInPlace,
    };
    budgets := [budget, ...!budgets];
    ()    
  };
  (!budgets, !warnings)
};
