type googlePromise('a) = Js.Promise.t('a);

[@bs.send]
external handle : (googlePromise('a), 'a => 'b, Js.Promise.error => 'b) => Js.Promise.t('b) =
  "then";

[@bs.new]
external newPromise : (('a => unit, Js.Promise.error => unit) => unit) => Js.Promise.t('a) =
  "Promise";

let promise = (prom) =>
  newPromise(
    (resolve, reject) =>
      handle(prom, (value) => resolve({"value": value}), (err) => reject(err)) |> ignore
  );

[@bs.val] [@bs.scope "gapi"] external load : ([@bs.as "auth2"] _, unit => unit) => unit = "load";

type gapi;

[@bs.val] external gapi : gapi = "";

type auth2;

[@bs.get] external auth2 : gapi => auth2 = "auth2";

type authInst;

[@bs.send]
external init : (auth2, {. "client_id": string, "scope": string}) => googlePromise(authInst) =
  "";

let init = (config) => init(auth2(gapi), config) |> promise;

[@bs.send] [@bs.scope "isSignedIn"] external isSignedIn : authInst => bool = "get";

type user;

[@bs.send] external signIn : authInst => Js.Promise.t(user) = "";

[@bs.send] [@bs.scope "currentUser"] external currentUser : authInst => user = "get";

type profile;

[@bs.send] external getBasicProfile : user => profile = "";

[@bs.send] external getName : profile => string = "";

[@bs.send] external getEmail : profile => string = "";

type authResponse = {. "access_token": string};

[@bs.send] external getAuthResponse : user => authResponse = "";

let token = (user) => getAuthResponse(user)##access_token;
