namespace App

open Feliz
open Feliz.Bulma
open ARCtrl
open Feliz.DaisyUI
open Shared
open Fable.Core
open Fable.Core.JsInterop

module private Helperfuncs =
    let updateAnnotation (func:Annotation -> Annotation, indx: int, annoState: Annotation list, setState: Annotation list -> unit) =
                let nextA = func annoState[indx]
                annoState |> List.mapi (fun i a ->
                    if i = indx  then nextA else a 
                ) |> setState

module Searchblock =

    let TermOrUnitizedSwitch ( a: int, annoState: Annotation list, setState: Annotation list -> unit) =
        React.fragment [
            Daisy.button.a [
                join.item
                let isActive = annoState[a].Search.Body.isTerm
                if isActive then button.info
                prop.onClick (fun _ -> 
                    (annoState |> List.mapi (fun i e ->
                        if i = a then {e with Search.Body = e.Search.Body.ToTermCell()}
                        else e
                    )) |> setState
                )
                prop.text "Term"
            ]
            Daisy.button.a [
                join.item
                let isActive = annoState[a].Search.Body.isUnitized
                if isActive then button.info
                prop.onClick (fun _ -> 
                    (annoState |> List.mapi (fun i e ->
                        if i = a then {e with Search.Body = e.Search.Body.ToUnitizedCell()}
                        else e
                    )) |> setState
                )
                prop.text "Unit"
            ]
        ]

    [<ReactComponent>]
    let SearchElementKey (ui, setUi,annoState, setAnnoState, a) = //missing ui and setui for dropdown
        let element = React.useElementRef()
        Html.div [
            prop.ref element // The ref must be place here, otherwise the portalled term select area will trigger daisy join syntax
            prop.style [style.position.relative]
            prop.children [
                Daisy.join [
                    prop.className "w-full z-50"
                    prop.children [
                        // Choose building block type dropdown element
                        // Dropdown building block type choice
                        BuildingBlock.Dropdown.Main(ui, setUi, annoState, setAnnoState, a)
                        // Term search field
                        // if model.HeaderCellType.HasOA() then
                        let setter (oaOpt: OntologyAnnotation option) =
                            let oa = oaOpt |> Option.defaultValue (OntologyAnnotation())
                            Helperfuncs.updateAnnotation((fun anno -> 
                                {anno with Search.Key = oa}
                            ), a, annoState, setAnnoState)
                            //selectHeader ui setUi h |> dispatch
                        let input = annoState[a].Search.Key
                        Components.TermSearch.Input(setter, fullwidth=true, input=input, isjoin=true, ?portalTermSelectArea=element.current, classes="")
                        // elif model.HeaderCellType.HasIOType() then
                        //     Daisy.input [
                        //         prop.readOnly true
                        //         prop.valueOrDefault (
                        //             model.TryHeaderIO()
                        //             |> Option.get
                        //             |> _.ToString()
                        //         )
                        //     ]
                    ]
                ]
            ]
        ]

    [<ReactComponent>]
    let SearchElementBody ( a, annoState, setAnnoState) =
        let element = React.useElementRef()
        Html.div [
            prop.ref element
            prop.className "relative"
            prop.children [
                Daisy.join [
                    prop.className "w-full z-50"
                    prop.children [
                        TermOrUnitizedSwitch (a, annoState, setAnnoState)
                        // helper for setting the body cell type
                        let setter (oaOpt: OntologyAnnotation option) =
                            Helperfuncs.updateAnnotation((fun anno -> 
                                let oa = oaOpt |> Option.defaultValue (OntologyAnnotation()) 
                                let nextCell = anno.Search.Body.UpdateWithOA(oa)
                                {anno with Search.Body = nextCell}
                                
                            ), a, annoState, setAnnoState)

                        // let parent = model.TryHeaderOA()
                        let input = annoState[a].Search.Body.ToOA()
                        Components.TermSearch.Input(setter, fullwidth=true, input=input, parent=input, displayParent=false, ?portalTermSelectArea=element.current, isjoin=true, classes="")   
                    ]
                ]
            ]
        ]

[<AutoOpen>]

module private ARCtrlExtensions =
    type CompositeCell with
        member this.UpdateWithString(s: string) =
            match this with
            |CompositeCell.Unitized (_,oa) ->
                CompositeCell.Unitized (s,oa) 
            |_ -> this
    
    // let AddBuildingBlockButton (annoState: Annotation list, setState: Annotation list -> unit, a ) =
    //     // let state = model.AddBuildingBlockState
    //     Html.div [
    //         prop.className "flex justify-center"
    //         prop.children [
    //             Daisy.button.button  [
    //                 // let header = Helper.createCompositeHeaderFromState model
    //                 // let body = Helper.tryCreateCompositeCellFromState model
    //                 let isValid = if annoState.Head.Search.Key.ToString() <> "{Name = }" then true else false
    //                 log ("Key" + annoState.Head.Search.Key.ToString())
    //                 button.wide
    //                 if isValid then
    //                     button.info
    //                 else
    //                     // button.error
    //                     prop.disabled true
    //                 // prop.onClick ( 
    //                 //     fun _ -> 
    //                 //     let bodyCells =
    //                 //         if body.IsSome then // create as many body cells as there are rows in the active table
    //                 //             let rowCount = System.Math.Max(1, model.SpreadsheetModel.ActiveTable.RowCount)
    //                 //             Array.init rowCount (fun _ -> body.Value.Copy())
    //                 //         else
    //                 //             Array.empty
    //                 //     let column = CompositeColumn.create(header, bodyCells)
    //                 //     // let index = Spreadsheet.Controller.BuildingBlocks.SidebarControllerAux.getNextColumnIndex model.SpreadsheetModel
                        
    //                 //     Msg.AddAnnotationBlock column |> InterfaceMsg |> dispatch
    //                 // )
    //                 prop.onClick (fun e ->
    //                     (annoState |> List.mapi (fun i e ->
    //                         if i = a then {e with IsAdded = true}
    //                         else e
    //                     )) |> setState 
    //                 )
    //                 prop.text "Add Annotation"
    //             ]

    //         ]
    //     ]

type Components =
    
    [<ReactComponent>]
    static member AnnoBlockwithSwate() =
       
        let testAnno = Annotation.init(OntologyAnnotation("key1"),CompositeCell.Term(OntologyAnnotation"term1"))
        let testAnno2 = Annotation.init(OntologyAnnotation("key2"),CompositeCell.Term(OntologyAnnotation"term2"))
        let (ui: BuildingBlock.BuildingBlockUIState, setUi) = React.useState(BuildingBlock.BuildingBlockUIState.init)        
        let (annoState: Annotation list, setState) = React.useState ([testAnno;testAnno2])

               
        Html.div [
            for a in 0 .. annoState.Length - 1 do
                Bulma.block [
                    prop.className "m-30"
                    prop.children [
                    if annoState[a].IsOpen = false then 
                        Html.button [
                            Html.i [
                                prop.className "fa-solid fa-comment-dots"
                                prop.style [style.color "#ffe699"]
                                prop.onClick (fun e ->
                                    
                                    Helperfuncs.updateAnnotation ((fun e -> e.ToggleOpen()), a, annoState, setState)                            
                                )
                            ]
                        ] 
                    else
                        Html.div [
                            prop.className "bg-[#ffe699] p-3 text-black z-50 w-fit"
                            prop.children [
                                Bulma.columns [
                                    Bulma.column [
                                        column.is1
                                        prop.className "hover:bg-[#ffd966] cursor-pointer"
                                        prop.onClick (fun e -> Helperfuncs.updateAnnotation ((fun a -> a.ToggleOpen()), a, annoState, setState))
                                        prop.children [
                                            Html.span [
                                                Html.i [
                                                    prop.className "fa-solid fa-chevron-left"
                                                ]
                                            ]
                                        ]
                                    ]
                                    Bulma.column [
                                        prop.className "space-y-2"
                                        prop.children [
                                            Html.span [
                                                prop.className "delete float-right mt-0 mb-2"
                                                prop.onClick (fun _ -> 
                                                    let newAnnoList: Annotation list = annoState |> List.filter (fun x -> x = annoState[a] |> not)  
                                                    // List.removeAt (List.filter (fun x -> x = a) state) state
                                                    setState newAnnoList
                                                )
                                            ]
                                            Searchblock.SearchElementKey (ui, setUi,annoState, setState, a)
                                            if annoState[a].Search.KeyType.IsTermColumn() then
                                                Searchblock.SearchElementBody(a, annoState, setState)
                                                if annoState[a].Search.Body.isUnitized then
                                                    Daisy.formControl [
                                                        Daisy.join [
                                                            Daisy.input [
                                                                prop.autoFocus true
                                                                prop.placeholder "Value..."
                                                                prop.onChange (fun (s:string) ->
                                                                    Helperfuncs.updateAnnotation((fun anno -> 
                                                                        let nextCell = {anno with Search.Body = anno.Search.Body.UpdateWithString(s)}
                                                                        nextCell
                                                                    ),a,annoState, setState)
                                                                )
                                                            ]
                                                        ]
                                                    ]
                                        ]
                                    ]
                                ]
                            ]  
                        ] 
                    ]
                ]
            Html.div [
                prop.className "w-96 bg-white"
                prop.children [
                    Daisy.table [
                        prop.className "bg-white"
                        prop.children [
                            Html.thead [
                                Html.tr [
                                    Html.th [prop.text "Nooooooo.";prop.style [style.color.black]]
                                    Html.th [prop.text "Key";prop.style [style.color.black]]
                                    Html.th [prop.text "KeyType";prop.style [style.color.black]]
                                    Html.th [prop.text "Term";prop.style [style.color.black]]
                                    Html.th [prop.text "Value (if unitized)";prop.style [style.color.black]]
                                    Html.th [prop.text "";prop.style [style.color.black]]
                                ]
                            ]
                            Html.tbody [
                            for a in 0 .. annoState.Length - 1 do
                                let isBodyempty = 
                                    match annoState[a].Search.Body with
                                    | CompositeCell.Term oa -> System.String.IsNullOrWhiteSpace oa.NameText
                                    | CompositeCell.Unitized (v,oa) -> System.String.IsNullOrWhiteSpace v
                                    | _ -> true
                                    
                                let isKeyempty = System.String.IsNullOrWhiteSpace annoState[a].Search.Key.NameText

                                if isBodyempty && isKeyempty  then
                                    Html.tr []
                                else
                                    Html.tr [
                                        prop.children [
                                            Html.td [prop.text(a + 1); prop.style [style.color.black]]
                                            Html.td [prop.text (annoState[a].Search.Key.NameText); prop.style [style.color.black]]
                                            Html.td [prop.text (annoState[a].Search.KeyType.ToString()); prop.style [style.color.black]]
                                            match annoState[a].Search.Body with
                                            | (CompositeCell.Term oa) -> 
                                                Html.td [prop.text(oa.NameText); prop.style [style.color.black]]
                                                Html.td ""
                                            | (CompositeCell.Unitized (v,oa)) ->
                                                Html.td [prop.text(oa.NameText); prop.style [style.color.black]]
                                                Html.td [prop.text v; prop.style [style.color.black]]
                                            |_ -> ()
                                            Html.td [
                                                Html.button [
                                                    prop.className "text-black"
                                                    prop.onClick (fun _ -> 
                                                        let newAnnoList: Annotation list = annoState |> List.filter (fun x -> x = annoState[a] |> not)  
                                                        setState newAnnoList
                                                    )
                                                    prop.children [
                                                        Html.i [
                                                            prop.className "fa-regular fa-trash-can"
                                                        ]
                                                    ]
                                                ]
                                            ]
                                        ]
                                    ]
                            ]
                        ]
                    ]
                ]
            ]
        ]