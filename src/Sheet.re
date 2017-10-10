
type config = Js.t {.
  clientId: string,
  scope: string
};

type window;

external window: window = "" [@@bs.val];
external onSignIn: window => (Auth.user => unit) => unit = "" [@@bs.set];
let onSignIn = onSignIn window;

type response 't;
external json: response 't => Js.Promise.t 't = "" [@@bs.send];
external fetch: string => Js.t {..headers: Js.t{..}} => Js.Promise.t 't = "" [@@bs.val];

let fetchGet url accessToken => fetch url {"headers": {"Authorization": "Bearer " ^ accessToken}};

type valuesResponse = Js.t {.
  values: array (array Js.Json.t),
  range: string,
  majorDimension: string
};
let values sheetId range accessToken => fetchGet {j|https://sheets.googleapis.com/v4/spreadsheets/$sheetId/values/$range?valueRenderOption=FORMULA|j} accessToken
  |> Js.Promise.then_ json
  |> Js.Promise.then_ (fun (data: valuesResponse) => data##values |> Js.Promise.resolve);

external setItem: string => 'a => unit = "" [@@bs.val] [@@bs.scope "localStorage"];
external getItem: string => Js.nullable 'a = "" [@@bs.val] [@@bs.scope "localStorage"];

