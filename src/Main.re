
external config: Sheet.config = "../../../config.js" [@@bs.module];

let signIn () => {
  let init = Auth.init {
    "client_id": config##clientId,
    "scope": "https://www.googleapis.com/auth/spreadsheets"
  };
  init |> Js.Promise.then_ (fun auth => {
    let auth = auth##value;
    if (Auth.isSignedIn auth) {
      Auth.currentUser auth
      |> Js.Promise.resolve;
    } else {
      Auth.signIn auth
    }
  })
};

Auth.load (fun () => {
  signIn () |> Js.Promise.then_ (fun (user) => {
    let profile = Auth.getBasicProfile user;
    let name = Auth.getName profile;
    let email = Auth.getEmail profile;
    /* [%bs.raw "window.user = user"]; */

    ReactDOMRe.renderToElementWithId <App token=(Auth.token user) name email /> "root";

    Js.Promise.resolve ();
  }) |> ignore;
});
