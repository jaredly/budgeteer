type calc =
  | Unit
  | Plus(calc, calc)
  | Minus(calc, calc)
  /* | Times calc calc */
  /* | Const float */
  | Row(/* row */ int);

type budgetItem =
  | Title(string, int)
  /* name row isYearly categories goal */
  | Item(string, int, bool, array(string), option(float))
  /* name row isYearly calc goal flip */
  | Calculated(string, int, bool, calc, float, bool)
  /* name row isYearly (fromrow, torow) */
  | Sum(string, int, bool, (int, int));

type budget = {
  startMonth: int,
  startYear: int,
  /*** The bool indicates whether the
   * item is enabled at the moment or not
   */
  items: array(budgetItem)
};

type transaction = {
  date: float,
  month: int,
  year: int,
  description: string,
  originalDescription: string,
  amount: float,
  isCredit: bool,
  category: string,
  account: string,
  labels: option(string),
  notes: option(string)
};

/* let module IntMap = Map.Make {type t = int; let compare = compare }; */
let module IntPairMap = Map.Make {type t = (int, int); let compare = compare };

type category = {
  name: string,
  byMonth: IntPairMap.t ((float, list(transaction)))
};
  /* monthTotal: float,
  yearTotal: float, */
  /* totalsByMonth: IntPairMap.t float,
  totalsByYear: IntMap.t float,
  monthTransactions: list(transaction),
  yearTransactions: list(transaction) */

let module Cat = {
  let total = (cat, key) => IntPairMap.mem(key, cat.byMonth) ? (IntPairMap.find(key, cat.byMonth) |> fst) : 0.;
  let transactions = (cat, key) => IntPairMap.mem(key, cat.byMonth) ? (IntPairMap.find(key, cat.byMonth) |> snd) : [];

  let fromMap = (catMap, cat, key) => Js.Dict.get(catMap, cat)
        |> Utils.optMap((cat) => total(cat, key))
        |> Utils.optOr(0.);

  let transactionsFromMap = (catMap, cat, key) => Js.Dict.get(catMap, cat)
        |> Utils.optMap((cat) => transactions(cat, key))
        |> Utils.optOr([]);

  let ytd = (cat, year, month) => {
    let res = ref(0.);
    for (i in 0 to month) {
      let v = total(cat, (year, i));
      res := res^ +. v;
    };
    res^
  };

  let ytdTransactions = (cat, year, month) => {
    let total = ref([]);
    for (i in 0 to month) {
      let v = transactions(cat, (year, i));
      total := total^ @ v;
    };
    total^
  };
};