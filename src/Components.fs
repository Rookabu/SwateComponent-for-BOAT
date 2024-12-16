namespace App

open Feliz
open Feliz.Bulma
open ARCtrl
open Feliz.DaisyUI

type Annotation = 
    {
        Key: OntologyAnnotation option
        Value: CompositeCell option
        IsOpen: bool 
    } 
    static member init (?key,?value,?isOpen) = 
        let isOpen = defaultArg isOpen true
        {
            Key= key
            Value= value
            IsOpen= isOpen

        }
    member this.ToggleOpen () = {this with IsOpen = not this.IsOpen}

type Model = {
    ///PageState
    // PageState                   : PageState
    // ///Data that needs to be persistent once loaded
    // PersistentStorageState      : PersistentStorageState
    // ///Error handling, Logging, etc.
    // DevState                    : DevState
    // ///States regarding term search
    // TermSearchState             : TermSearch.Model
    // ///Use this in the future to model excel stuff like table data
    // ExcelState                  : OfficeInterop.Model
    // ///States regarding File picker functionality
    // FilePickerState             : FilePicker.Model
    // ProtocolState               : Protocol.Model
    // ///Insert annotation columns
    AddBuildingBlockState       : BuildingBlock.Model
    // CytoscapeModel              : Cytoscape.Model
    // ///
    // DataAnnotatorModel          : DataAnnotator.Model
    // /// Contains all information about spreadsheet view
    // SpreadsheetModel            : Spreadsheet.Model
    // History                     : LocalHistory.Model
    // ModalState                  : ModalState
}

type Components =
    

    [<ReactComponent>]
    static member annoBlockwithSwate() =
       
        let testAnno = Annotation.init(OntologyAnnotation("key1"), CompositeCell.createFreeText("value1"))

        let (annoState: Annotation list, setState) = React.useState ([testAnno])
        let revIndex = 0
        let a = annoState.[revIndex]

        let (currentOnto: OntologyAnnotation option, ontoSetter) = React.useState(annoState[0].Key)


        let updateAnnotation (func:Annotation -> Annotation) =
            let nextA = func a
            annoState |> List.mapi (fun i a ->
                if i = revIndex then nextA else a 
            ) |> setState

        let SearchBuildingBlockHeaderElement (model: Model, dispatch) =
            let state = model.AddBuildingBlockState
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
                            // Dropdown.Main(ui, setUi, model, dispatch)
                            // Term search field
                            if state.HeaderCellType.HasOA() then
                                let setter (oaOpt: OntologyAnnotation option) =
                                    let case = oaOpt |> Option.map (fun oa -> !^oa)
                                    BuildingBlock.UpdateHeaderArg case |> BuildingBlockMsg |> dispatch
                                    //selectHeader ui setUi h |> dispatch
                                let input = model.AddBuildingBlockState.TryHeaderOA()
                                Components.TermSearch.Input(setter, fullwidth=true, ?input=input, isjoin=true, ?portalTermSelectArea=element.current, classes="border-current")
                            elif state.HeaderCellType.HasIOType() then
                                Daisy.input [
                                    prop.readOnly true
                                    prop.valueOrDefault (
                                        state.TryHeaderIO()
                                        |> Option.get
                                        |> _.ToString()
                                    )
                                ]
                        ]
                    ]
                ]
            ]

        Bulma.block [
            prop.className "m-30"
            prop.children [
            if a.IsOpen = false then 
                Html.button [
                    Html.i [
                        prop.className "fa-solid fa-comment-dots"
                        prop.style [style.color "#ffe699"]
                        prop.onClick (fun e ->
                            (annoState |> List.mapi (fun i e ->
                                if i = revIndex then e.ToggleOpen() 
                                else {e with IsOpen = false}
                            )) |> setState 
                            // updateAnnotation (fun a -> a.ToggleOpen())                              
                        )
                    ]
                ] 
            else
                Html.div [
                    prop.className "bg-[#ffe699] p-3 text-black z-50 max-w-96 mb-20"
                    
                    prop.children [
                        Bulma.columns [
                            Bulma.column [
                                column.is1
                                prop.className "hover:bg-[#ffd966] cursor-pointer"
                                prop.onClick (fun e -> updateAnnotation (fun a -> a.ToggleOpen())
                                
                                )
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
                                            let newAnnoList: Annotation list = annoState |> List.filter (fun x -> x = a |> not)  
                                            // List.removeAt (List.filter (fun x -> x = a) state) state
                                            setState newAnnoList
                                        )
                                    ]
                                    Bulma.input.text [
                                        input.isSmall
                                        prop.value (a.Key|> Option.map (fun e -> e.Name.Value) |> Option.defaultValue "")
                                        prop.className ""
                                        prop.onChange (fun (x: string)-> 
                                            let updatetedAnno = 
                                                {a with Key = OntologyAnnotation(name = x) |> Some}

                                            let newAnnoList: Annotation list =
                                                annoState
                                                |> List.map (fun elem -> if elem = a then updatetedAnno else elem)

                                            setState newAnnoList
                                        )
                                    ]
                                    Html.p "Value: "
                                    Bulma.input.text [
                                        input.isSmall
                                        prop.value (a.Value|> Option.map (fun e -> e.ToString()) |> Option.defaultValue "" )
                                        prop.className ""
                                        prop.onChange (fun (x:string) -> 
                                            let updatetedAnno = 
                                                {a with Value = CompositeCell.createFreeText(x) |> Some}
                                                
                                            let newAnnoList: Annotation list =
                                                annoState
                                                |> List.map (fun elem -> if elem = a then updatetedAnno else elem)

                                            setState newAnnoList
                                        )
                                    ]
                                    
                                ]
                            ]
                        ]
                    ]  
                ]
            ]
        ]
