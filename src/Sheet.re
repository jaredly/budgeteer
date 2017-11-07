type config = {. "clientId": string, "scope": string};

type window;

[@bs.val] external window : window = "";

[@bs.set] external onSignIn : (window, Auth.user => unit) => unit = "";

let onSignIn = onSignIn(window);

type response('t);

[@bs.send] external json : response('t) => Js.Promise.t('t) = "";

[@bs.val] external fetch : (string, {.. "headers": Js.t({..})}) => Js.Promise.t('t) = "";

let fetchGet = (url, accessToken) =>
  fetch(url, {"headers": {"Authorization": "Bearer " ++ accessToken}});

type valuesResponse = {
  .
  "values": array(array(Js.Json.t)), "range": string, "majorDimension": string
};

let values = (sheetId, range, accessToken) =>
  fetchGet(
    {j|https://sheets.googleapis.com/v4/spreadsheets/$sheetId/values/$range?valueRenderOption=FORMULA|j},
    accessToken
  )
  |> Js.Promise.then_(json)
  |> Js.Promise.then_((data: valuesResponse) => data##values |> Js.Promise.resolve);

[@bs.val] [@bs.scope "localStorage"] external setItem : (string, 'a) => unit = "";

[@bs.val] [@bs.scope "localStorage"] external getItem : string => Js.nullable('a) = "";
