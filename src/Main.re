[@bs.module] external config : Sheet.config = "../../../config.js";

let signIn = () => {
  let init =
    Auth.init({
      "client_id": config##clientId,
      "scope": "https://www.googleapis.com/auth/spreadsheets"
    });
  init
  |> Js.Promise.then_(
       (auth) => {
         let auth = auth##value;
         if (Auth.isSignedIn(auth)) {
           Auth.currentUser(auth) |> Js.Promise.resolve
         } else {
           Js.Promise.make((~resolve, ~reject) => {
            ReactDOMRe.renderToElementWithId(<button
              className=Glamor.(css([
                width("200px"),
                padding("8px 24px"),
                backgroundColor("white"),
                margin("64px"),
                alignSelf("center"),
                border("1px solid #aaa"),
                borderRadius("8px"),
                cursor("pointer")
              ]))
              onClick=((_) => {
                Auth.signIn(auth)
                |> Js.Promise.then_((user) => {
                  [@bs]resolve(user);
                  Js.Promise.resolve();
                }) |> ignore
              })
            >
              (Utils.str("Sign in with google"))
            </button>, "root");
           });
         }
       }
     )
};

Auth.load(
  () =>
    signIn()
    |> Js.Promise.then_(
         (user) => {
           let profile = Auth.getBasicProfile(user);
           let name = Auth.getName(profile);
           let email = Auth.getEmail(profile);
           [%bs.raw "window.user = user"];
           ReactDOMRe.renderToElementWithId(<App token=(Auth.token(user)) name email />, "root");
           Js.Promise.resolve()
         }
       )
    |> ignore
);
