import "./style.css";
import { Elm } from "./src/Main.elm";

if (process.env.NODE_ENV === "development") {
  const ElmDebugTransform = await import("elm-debug-transformer")

  ElmDebugTransform.register({
    simple_mode: true
  })
}

const root = document.querySelector("#app div");
const app = Elm.Main.init({ node: root });

document.addEventListener('submit', (event) => {
  const form = event.submitter.form;
  const fields = Array.from(new FormData(form).entries());
  event.preventDefault();
  app.ports.onSubmit.send({
    fields,
    id: form.id,
    action: form.action,
    method: form.method
  })
  return false;
}, { capture: true })
