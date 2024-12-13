module Main

open Feliz
open App
open Browser.Dom
open Fable.Core.JsInterop

importSideEffects "./styling.scss"
importSideEffects "./tailwind.scss"

let root = ReactDOM.createRoot(document.getElementById "feliz-app")
root.render(Components.annoBlockwithSwate())