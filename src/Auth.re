
type googlePromise 'a = Js.Promise.t 'a;

external handle: googlePromise 'a => ('a => 'b) => (Js.Promise.error => 'b) => Js.Promise.t 'b = "then" [@@bs.send];
external newPromise: (('a => unit) => (Js.Promise.error => unit) => unit) => Js.Promise.t 'a = "Promise" [@@bs.new];
let promise prom => newPromise (fun resolve reject => handle prom (fun value => resolve {"value": value}) (fun err => reject err) |> ignore);

external load: (_ [@bs.as "auth2"]) => (unit => unit) => unit = "load" [@@bs.val] [@@bs.scope "gapi"];

type gapi;
external gapi: gapi = "" [@@bs.val];

type auth2;
external auth2: gapi => auth2 = "auth2" [@@bs.get];

type authInst;
external init: auth2 => Js.t {.
  client_id: string,
  scope: string
} => googlePromise authInst = "" [@@bs.send];
let init config => init (auth2 gapi) config |> promise;

external isSignedIn: authInst => bool = "get" [@@bs.send] [@@bs.scope "isSignedIn"];

type user;
external signIn: authInst => Js.Promise.t user = "" [@@bs.send];
external currentUser: authInst => user = "get" [@@bs.send] [@@bs.scope "currentUser"];

type profile;
external getBasicProfile: user => profile = "" [@@bs.send];
external getName: profile => string = "" [@@bs.send];
external getEmail: profile => string = "" [@@bs.send];

type authResponse = Js.t {.access_token: string};
external getAuthResponse: user => authResponse = "" [@@bs.send];

let token user => (getAuthResponse user)##access_token;
