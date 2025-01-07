module BuildingBlock.Dropdown

open Feliz
open Feliz.DaisyUI
open BuildingBlock
open ARCtrl
open Shared


[<ReactComponent>]
let FreeTextInputElement(onSubmit: string -> unit) =
    let inputS, setInput = React.useState ""
    Html.div [
        prop.className "flex flex-row gap-0 p-0"
        prop.children [
            Daisy.input [
                join.item
                input.sm
                prop.placeholder "..."
                prop.className "grow truncate"
                prop.onClick (fun e -> e.stopPropagation())
                prop.onChange (fun (v:string) -> setInput v)
                prop.onKeyDown(key.enter, fun e -> e.stopPropagation(); onSubmit inputS)
            ]
            Daisy.button.button [
                join.item
                button.accent
                button.sm
                prop.onClick (fun e -> e.stopPropagation(); onSubmit inputS)
                prop.children [
                    Html.i [prop.className "fa-solid fa-check"]
                ]
            ]
        ]
    ]

module private DropdownElements =

    let divider = Daisy.divider [prop.className "mx-2 my-0"]
    let private annotationsPrinciplesLink = Html.a [prop.href "https://nfdi4plants.github.io/AnnotationPrinciples/"; prop.target.blank; prop.className "ml-auto link-info"; prop.text "info"]

    let createSubBuildingBlockDropdownLink (state:BuildingBlockUIState) setState (subpage: DropdownPage) =
        Html.li [
            prop.onClick(fun e ->
                e.preventDefault()
                e.stopPropagation()
                setState {state with DropdownPage = subpage}
            )
            prop.children [
                Html.div [
                    prop.className "flex flex-row justify-between"
                    prop.children [
                        Html.span subpage.toString
                        Html.i [prop.className "fa-solid fa-arrow-right"]
                    ]
                ]
            ]
        ]

    /// Navigation element back to main page
    let DropdownContentInfoFooter setState (hasBack: bool) =
        Html.li [
            prop.className "flex flex-row justify-between pt-1"
            prop.onClick(fun e ->
                e.preventDefault()
                e.stopPropagation()
                setState {DropdownPage = DropdownPage.Main; DropdownIsActive = true}
            )
            prop.children [
                if hasBack then
                    Html.a [
                        prop.className "content-center"
                        prop.children [
                            Html.i [ prop.className "fa-solid fa-arrow-left" ]
                        ]
                    ]
                annotationsPrinciplesLink
            ]
        ]

    let isSameMajorCompositeHeaderDiscriminate (hct1: CompositeHeaderDiscriminate) (hct2: CompositeHeaderDiscriminate) =
        (hct1.IsTermColumn() = hct2.IsTermColumn())
        && (hct1.HasIOType() = hct2.HasIOType())

    let selectCompositeHeaderDiscriminate (hct: CompositeHeaderDiscriminate) setUiState close (model: Model) setModel =
        // BuildingBlock.UpdateHeaderCellType hct |> BuildingBlockMsg |> dispatch
        let nextState =
            if isSameMajorCompositeHeaderDiscriminate model.HeaderCellType hct then
                { model with
                    HeaderCellType = hct
                }
            else
                let nextBodyCellType = if hct.IsTermColumn() then CompositeCellDiscriminate.Term else CompositeCellDiscriminate.Text
                { model with
                    HeaderCellType = hct
                    BodyCellType = nextBodyCellType
                    HeaderArg = None
                    BodyArg = None
                }
        setModel nextState
        close()
        { DropdownPage = DropdownPage.Main; DropdownIsActive = false }|> setUiState

    let createBuildingBlockDropdownItem setUiState close setModel (model: Model)(headerType: CompositeHeaderDiscriminate)  =
        Html.li [Html.a [
            prop.onClick (fun e ->
                e.stopPropagation()
                selectCompositeHeaderDiscriminate headerType setUiState close model setModel 
            )
            prop.onKeyDown(fun k ->
                if (int k.which) = 13 then selectCompositeHeaderDiscriminate headerType setUiState close model setModel 
            )
            prop.text (headerType.ToString())
            prop.onBlur (fun e -> close())
            // prop.ref dropdownRef
        ]]

    let createIOTypeDropdownItem setUiState close (model:BuildingBlock.Model) setModel (headerType: CompositeHeaderDiscriminate)(iotype: IOType)  =
        let setIO (iotype) =
            { DropdownPage = DropdownPage.Main; DropdownIsActive = false } |> setUiState
            close()
            let nextState = {
                model with
                    HeaderCellType = headerType
                    HeaderArg = Some (Fable.Core.U2.Case2 iotype)
                    BodyArg = None
                    BodyCellType = CompositeCellDiscriminate.Text
            }
            setModel nextState
        Html.li [
            match iotype with
            | IOType.FreeText s ->
                let onSubmit = fun (v: string) ->
                    let header = IOType.FreeText v
                    setIO header
                prop.children [FreeTextInputElement onSubmit]
            | _ ->
                prop.onClick (fun e -> e.stopPropagation(); setIO iotype)
                prop.onKeyDown(fun k -> if (int k.which) = 13 then setIO iotype)
                prop.children [
                    Html.div [prop.text (iotype.ToString())]
                ]
        ]

    /// Main column types subpage for dropdown
    let dropdownContentMain state setState close (model:BuildingBlock.Model) (setModel: Model -> unit)=
            React.fragment [
                DropdownPage.IOTypes CompositeHeaderDiscriminate.Input |> createSubBuildingBlockDropdownLink state setState
                divider
                CompositeHeaderDiscriminate.Parameter      |> createBuildingBlockDropdownItem setState close setModel model 
                CompositeHeaderDiscriminate.Factor         |> createBuildingBlockDropdownItem setState close setModel model 
                CompositeHeaderDiscriminate.Characteristic |> createBuildingBlockDropdownItem setState close setModel model 
                CompositeHeaderDiscriminate.Component      |> createBuildingBlockDropdownItem setState close setModel model 
                DropdownPage.More       |> createSubBuildingBlockDropdownLink state setState
                divider
                DropdownPage.IOTypes CompositeHeaderDiscriminate.Output |> createSubBuildingBlockDropdownLink state setState
                DropdownContentInfoFooter setState false
            ]

        

    /// Protocol Type subpage for dropdown
    let dropdownContentProtocolTypeColumns setState close (model:Model) setModel  =
        React.fragment [
            CompositeHeaderDiscriminate.Date                |> createBuildingBlockDropdownItem setState close setModel model 
            CompositeHeaderDiscriminate.Performer           |> createBuildingBlockDropdownItem setState close setModel model 
            CompositeHeaderDiscriminate.ProtocolDescription |> createBuildingBlockDropdownItem setState close setModel model 
            CompositeHeaderDiscriminate.ProtocolType        |> createBuildingBlockDropdownItem setState close setModel model 
            CompositeHeaderDiscriminate.ProtocolUri         |> createBuildingBlockDropdownItem setState close setModel model 
            CompositeHeaderDiscriminate.ProtocolVersion     |> createBuildingBlockDropdownItem setState close setModel model 
            // Navigation element back to main page
            DropdownContentInfoFooter setState true
            
        ]

    /// Output columns subpage for dropdown
    let dropdownContentIOTypeColumns header setState close (model:BuildingBlock.Model) setModel  =
        React.fragment [
            IOType.Source           |> createIOTypeDropdownItem setState close model setModel header   
            IOType.Sample           |> createIOTypeDropdownItem setState close model setModel header 
            IOType.Material         |> createIOTypeDropdownItem setState close model setModel header 
            IOType.Data             |> createIOTypeDropdownItem setState close model setModel header 
            IOType.FreeText ""      |> createIOTypeDropdownItem setState close model setModel header 
            // Navigation element back to main page
            DropdownContentInfoFooter setState true
        ]

[<ReactComponent>]
let Main(state, setState, model: BuildingBlock.Model, setModel) =
    let isOpen, setOpen = React.useState false
    let close = fun _ -> setOpen false
    let dropdownRef:IRefValue<Browser.Types.HTMLElement option> = React.useRef(None)

    // Hook to handle clicks outside
    React.useEffect(fun () ->
        let handleClickOutside (event: Browser.Types.Event) =
            match dropdownRef.current with
            | Some element when element.contains(event.target :?> Browser.Types.Node) |> not ->
                close() //if clicked target is not the ref element, close the dropdown
            | _ -> ()
        Browser.Dom.document.addEventListener("click", handleClickOutside)
    )
        
    // Add event listener
    Components.BaseDropdown.Main(
        isOpen,
        setOpen,
        Daisy.button.div [
                prop.tabIndex 0
                button.info
                prop.onClick (fun _ -> setOpen (not isOpen))
                prop.role "button"
                join.item
                prop.className "flex-nowrap"
                prop.children [
                    Html.span (model.HeaderCellType.ToString())
                    Html.i [
                        prop.className "fa-solid fa-angle-down"
                    ]
                ]
                prop.ref dropdownRef
            ],
        [
            match state.DropdownPage with
            | DropdownPage.Main ->
                DropdownElements.dropdownContentMain state setState close model setModel 
            | DropdownPage.More ->
                DropdownElements.dropdownContentProtocolTypeColumns setState close model setModel 
            | DropdownPage.IOTypes iotype ->
                DropdownElements.dropdownContentIOTypeColumns iotype setState close model setModel
        ],
        style=Components.Style.init("join-item dropdown text-white", Map [
            "content", Components.Style.init("!min-w-64")
        ])
    )

 