
open Utils;
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

    let colCategories = off;
    let colYearly = off + 1;
    let colFlip = off + 2;
    let colName = off + 3;
    let colGoal = off + 4;
    let colReport = off + 5;
    let colStartMonth = off + 1;
    let colStartYear = off + 2;

    [%guard let JSONNumber startMonth = classify (values.(0).(colStartMonth))][@else warn "No starting month"];
    [%guard let JSONNumber startYear = classify (values.(0).(colStartYear))][@else warn "No starting year"];
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
      [%guard let true = Array.length values.(row) >= colName + 1][@else ()];
      [%guard let Some (JSONString name) = getCol colName |> optMap classify][@else warn "No name"];
      switch (getCol colCategories |> optMap classify) {
      | None => {
        switch (getCol colReport |> optMap classify) {
        | None => add (Title name row)
        | Some (JSONString calcReport) => {
            switch (getCol colGoal |> optMap classify) {
            | None => warn "Missing goal"
            | Some (JSONString calcGoal) => switch (parseSum calcReport) {
              /** TODO warn if sums are in the wrong columns, or if calcGoal doesn't match */
              | Some (fromRow, toRow) => add (Sum name row (getCol colYearly != None) (fromRow, toRow))
              | None => warn "Bad sum"
              }
            | Some (JSONNumber goal) => switch (parseCalc calcReport) {
              | Some calc => add (Calculated name row (getCol colYearly != None) calc goal (getCol colFlip !== None))
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
        let goal = switch (getCol colGoal |> optMap classify) {
        | Some (JSONNumber goal) => Some (goal)
        | None => None
        | v => {Js.log v; warn "Bad goal"; None}
        };
        let isYearly = (getCol colYearly != None);
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
| Calculated _ num _ _ _ _
| Sum _ num _ _
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
    | `Resolved (goal, month, year) => (goal, month, year)
    | `Resolving => failwith "Recursive definition"
    | `Unresolved => {
        /* Js.log3 thisRow item amounts.(thisRow); */
        amounts.(thisRow) = `Resolving;
        let value = switch item {
        | Title _ => (0., 0., 0.)
        | Item _ _ _ categories goal => {
            let (month, year) = (Array.fold_left
              (fun (month, year) category => {
                touchedCategories := StringSet.add category !touchedCategories;
                let catVal = Js.Dict.get categoryMap category;
                (
                  month +. (catVal |> optMap (fun c => c.monthTotal) |> optOr 0.),
                  year +. (catVal |> optMap (fun c => c.yearTotal) |> optOr 0.)
                )
              })
              (0., 0.)
              categories
            );
            (optOr 0. goal, month, year)
          }
        | Calculated _ _ _ calc goal _ => {
          let (m, y) = doCalc calc;
          (goal, m, y) /** TODO */
        }
        | Sum _ _ _ (fromRow, toRow) => {
          /* Js.log3 "summing" fromRow toRow; */
          let goal = ref 0.;
          let month = ref 0.;
          let year = ref 0.;
          for i in fromRow to toRow {
            let (nextGoal, nextMonth, nextYear) = switch itemsByRow.(i - 1) {
            | Some item => resolve item
            | None => (0., 0., 0.)
            };
            /* Js.log2 i next; */
            goal := !goal +. nextGoal;
            month := !month +. nextMonth;
            year := !year +. nextYear;
            /* total := !total +. next; */
          };
          /* Js.log2 "Summed" !total; */
          (!goal, !month, !year)
        }
        };
        /* Js.log3 "Finished" value item; */
        amounts.(row item) = `Resolved value;
        value
      }
    }
  } and doCalc calc => switch calc {
  | Unit => (0., 0.)
  | Plus one two => {
      let (am, ay) = doCalc one;
      let (bm, by) = doCalc two;
      (am +. bm, ay +. by);
  }
  | Minus one two => {
    /* Js.log3 "minus" one two; */
    let (am, ay) = doCalc one;
    let (bm, by) = doCalc two;
    /* Js.log4 "was" a b (one, two); */
    (am -. bm, ay -. by)
  }
  | Row at => {
    let (_, m, y) = resolve (force itemsByRow.(at - 1));
    (m, y)
  }
  }
  ;
  for i in 0 to (Array.length items - 1) {
    resolve (items.(i)) |> ignore;
  };
  let amounts = Array.map (fun amount => switch amount {
  | `Unresolved => (0., 0., 0.)
  | `Resolving => (0., 0., 0.)
  | `Resolved value => value
  })
  amounts;
  let untouched = Js.Dict.keys categoryMap
  |> Js.Array.filter
  (fun cat => not (StringSet.mem cat !touchedCategories) && (Js.Dict.unsafeGet categoryMap cat).monthTotal > 0.);
  (amounts, untouched)
};




