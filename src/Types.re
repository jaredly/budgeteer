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

type category = {
  name: string,
  monthTotal: float,
  yearTotal: float,
  monthTransactions: list(transaction),
  yearTransactions: list(transaction)
};
