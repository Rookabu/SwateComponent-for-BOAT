namespace App

open Feliz
open Feliz.Bulma
open ARCtrl
open Feliz.DaisyUI
open Shared
open Fable.Core
open Fable.Core.JsInterop

type Annotation = 
    {
        Key: OntologyAnnotation option
        Value: CompositeCell option
        IsOpen: bool
        IsAdded: bool
    } 
    static member init (?key,?value,?isOpen, ?isAdded) = 
        let isOpen = defaultArg isOpen true
        let isAdded = defaultArg isAdded false
        {
            Key= key
            Value= value
            IsOpen= isOpen
            IsAdded= isAdded

        }
    member this.ToggleOpen () = {this with IsOpen = not this.IsOpen}

module Searchblock =

    let private termOrUnitizedSwitch (model: BuildingBlock.Model, setModel) =
        React.fragment [
            Daisy.button.a [
                join.item
                let isActive = model.BodyCellType = CompositeCellDiscriminate.Term
                if isActive then button.info
                prop.onClick (fun _ -> 
                    let nextState = { model with BodyCellType = CompositeCellDiscriminate.Term }
                    setModel nextState
                    )
                prop.text "Term"
            ]
            Daisy.button.a [
                join.item
                let isActive = model.BodyCellType = CompositeCellDiscriminate.Unitized
                if isActive then button.info
                prop.onClick (fun _ -> 
                    let nextModel = { model with BodyCellType = CompositeCellDiscriminate.Unitized }
                    setModel nextModel
                    )
                prop.text "Unit"
            ]
        ]


    [<ReactComponent>]
    let SearchBuildingBlockHeaderElement (model: BuildingBlock.Model, setModel, oa,ui, setUi) = //missing ui and setui for dropdown
        let element = React.useElementRef()
        Html.div [
            prop.ref element // The ref must be place here, otherwise the portalled term select area will trigger daisy join syntax
            prop.style [style.position.relative]
            prop.children [
                Daisy.join [
                    prop.className "w-full"
                    prop.children [
                        // Choose building block type dropdown element
                        // Dropdown building block type choice
                        BuildingBlock.Dropdown.Main(ui, setUi, model, setModel)
                        // Term search field
                        if model.HeaderCellType.HasOA() then
                            let setter (oaOpt: OntologyAnnotation option) =
                                let case = oaOpt |> Option.map (fun oa -> !^oa)
                                // BuildingBlock.UpdateHeaderArg case |> BuildingBlockMsg |> dispatch
                                let nextModel = {model with HeaderArg = case}
                                setModel nextModel
                                //selectHeader ui setUi h |> dispatch
                            let input = model.TryHeaderOA()
                            Components.TermSearch.Input(setter, fullwidth=true, ?inputOa=oa, isjoin=true, ?portalTermSelectArea=element.current, classes="")
                        elif model.HeaderCellType.HasIOType() then
                            Daisy.input [
                                prop.readOnly true
                                prop.valueOrDefault (
                                    model.TryHeaderIO()
                                    |> Option.get
                                    |> _.ToString()
                                )
                            ]
                    ]
                ]
            ]
        ]

    [<ReactComponent>]
    let SearchBuildingBlockBodyElement (model: BuildingBlock.Model, setModel, cc) =
        let element = React.useElementRef()
        Html.div [
            prop.ref element
            prop.className "relative"
            prop.children [
                Daisy.join [
                    prop.className "w-full"
                    prop.children [
                        termOrUnitizedSwitch (model, setModel)
                        // helper for setting the body cell type
                        let setter (oaOpt: OntologyAnnotation option) =
                            let case = oaOpt |> Option.map (fun oa -> !^oa)
                            let nextModel = { model with BodyArg = case }
                            setModel nextModel
                        let parent = model.TryHeaderOA()
                        let input = model.TryBodyOA()
                        Components.TermSearch.Input(setter, fullwidth=true, ?inputCc=cc, ?parent=parent, displayParent=false, ?portalTermSelectArea=element.current, isjoin=true, classes="")
                    ]
                ]
            ]
        ]

    type Msg =
    | AddAnnotationBlock of CompositeColumn
    
    let AddBuildingBlockButton (model: BuildingBlock.Model, annoState: Annotation list, setState: Annotation list -> unit, revIndex ) =
        // let state = model.AddBuildingBlockState
        Html.div [
            prop.className "flex justify-center"
            prop.children [
                Daisy.button.button  [
                    // let header = Helper.createCompositeHeaderFromState model
                    // let body = Helper.tryCreateCompositeCellFromState model
                    let isValid = if annoState.Head.Key.ToString() <> "{Name = }" then true else false
                    log ("Key" + annoState.Head.Key.ToString())
                    button.wide
                    if isValid then
                        button.info
                    else
                        // button.error
                        prop.disabled true
                    // prop.onClick ( 
                    //     fun _ -> 
                    //     let bodyCells =
                    //         if body.IsSome then // create as many body cells as there are rows in the active table
                    //             let rowCount = System.Math.Max(1, model.SpreadsheetModel.ActiveTable.RowCount)
                    //             Array.init rowCount (fun _ -> body.Value.Copy())
                    //         else
                    //             Array.empty
                    //     let column = CompositeColumn.create(header, bodyCells)
                    //     // let index = Spreadsheet.Controller.BuildingBlocks.SidebarControllerAux.getNextColumnIndex model.SpreadsheetModel
                        
                    //     Msg.AddAnnotationBlock column |> InterfaceMsg |> dispatch
                    // )
                    prop.onClick (fun e ->
                        (annoState |> List.mapi (fun i e ->
                            if i = revIndex then {e with IsAdded = true}
                            else e
                        )) |> setState 
                    )
                    prop.text "Add Annotation"
                ]
            ]
        ]

type Components =
    
    [<ReactComponent>]
    static member annoBlockwithSwate() =
       
        let testAnno = Annotation.init(OntologyAnnotation("key1"), CompositeCell.createFreeText("value1"))
        let testAnno2 = Annotation.init(OntologyAnnotation("key2"), CompositeCell.createFreeText("value2"))
        let (model: BuildingBlock.Model, setModel) = React.useState(BuildingBlock.Model.init)
        let (ui: BuildingBlock.BuildingBlockUIState, setUi) = React.useState(BuildingBlock.BuildingBlockUIState.init)        
        let (annoState: Annotation list, setState) = React.useState ([testAnno;testAnno2])
        // let a = annoState.[0]

        let updateAnnotation (func:Annotation -> Annotation, indx: int) =
            let nextA = func annoState[indx]
            annoState |> List.mapi (fun i a ->
                if i = 0 then nextA else a 
            ) |> setState
        
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
                                    (annoState |> List.mapi (fun i e ->
                                        if i = 0 then e.ToggleOpen() 
                                        else {e with IsOpen = false}
                                    )) |> setState 
                                    // updateAnnotation (fun a -> a.ToggleOpen())                              
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
                                        prop.onClick (fun e -> updateAnnotation ((fun a -> a.ToggleOpen()), a))
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
                                            Html.span "Key: "
                                            Html.span [
                                                prop.className "delete float-right mt-0"
                                                prop.onClick (fun _ -> 
                                                    let newAnnoList: Annotation list = annoState |> List.filter (fun x -> x = annoState[a] |> not)  
                                                    // List.removeAt (List.filter (fun x -> x = a) state) state
                                                    setState newAnnoList
                                                )
                                            ]
                                            
                                            // Bulma.input.text [
                                            //     input.isSmall
                                            //     prop.value (a.Key|> Option.map (fun e -> e.Name.Value) |> Option.defaultValue "")
                                            //     prop.className ""
                                            //     prop.onChange (fun (x: string)-> 
                                            //         let updatetedAnno = 
                                            //             {a with Key = OntologyAnnotation(name = x) |> Some}

                                            //         let newAnnoList: Annotation list =
                                            //             annoState
                                            //             |> List.map (fun elem -> if elem = a then updatetedAnno else elem)

                                            //         setState newAnnoList
                                            //     )
                                            // ]
                                            Searchblock.SearchBuildingBlockHeaderElement (model, setModel, annoState[a].Key, ui, setUi)
                                            if model.HeaderCellType.IsTermColumn() then
                                                Html.p "Value: "
                                            // Bulma.input.text [
                                            //     input.isSmall
                                            //     prop.value (a.Value|> Option.map (fun e -> e.ToString()) |> Option.defaultValue "" )
                                            //     prop.className ""
                                            //     prop.onChange (fun (x:string) -> 
                                            //         let updatetedAnno = 
                                            //             {a with Value = CompositeCell.createFreeText(x) |> Some}
                                                        
                                            //         let newAnnoList: Annotation list =
                                            //             annoState
                                            //             |> List.map (fun elem -> if elem = a then updatetedAnno else elem)

                                            //         setState newAnnoList
                                            //     )
                                            // ]
                                                Searchblock.SearchBuildingBlockBodyElement(model, setModel, annoState[a].Value)
                                            Html.div [
                                                prop.className "mt-4"
                                                prop.children [
                                                    Searchblock.AddBuildingBlockButton(model, annoState, setState, a)
                                                    // Daisy.button.button [prop.text "Test button"]
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
                prop.className "w-96"
                prop.children [
                    Daisy.table [
                        Html.thead [Html.tr [Html.th "No.";Html.th "Key"; Html.th "Term"; Html.th "Unit"]]
                        Html.tbody [
                        for a in 0 .. annoState.Length - 1 do
                            if annoState.[a].IsAdded = true then
                                Html.tr [
                                    Html.td (a + 1)
                                    Html.td (annoState[a].Key|> Option.map (fun e -> e.Name.Value) |> Option.defaultValue "")
                                    Html.td ""
                                    Html.td (annoState[a].Value|> Option.map (fun e -> e.ToString()) |> Option.defaultValue "" )
                                    Html.td ""
                                ]
                        ]
                    ]
                ]
            ]
        ]

