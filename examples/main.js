import "./style.css";
import { Elm } from "./src/Main.elm";

if (process.env.NODE_ENV === "development") {
  const ElmDebugTransform = await import("elm-debug-transformer");

  ElmDebugTransform.register({
    simple_mode: true,
  });
}
// https://stackoverflow.com/a/10592810
Object.defineProperty(SubmitEvent.prototype, "fields", {
  get: function fields() {
    let formData = new FormData(this.currentTarget);
    if (this.submitter?.name) {
      formData.append(this.submitter.name, this.submitter.value);
    }
    return [...formData.entries()];
  },
});

const root = document.querySelector("#app div");

const app = Elm.Main.init({ node: root });
