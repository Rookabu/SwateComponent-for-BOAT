module BuildingBlock 

    open ARCtrl
    open ARCtrl.Helper
    open Shared
    open Fable.Core

    [<RequireQualifiedAccess>]
    type DropdownPage =
    | Main
    | More
    | IOTypes of CompositeHeaderDiscriminate

        member this.toString =
            match this with
            | Main -> "Main Page"
            | More -> "More"
            | IOTypes (t) -> t.ToString()

        member this.toTooltip =
            match this with
            | More -> "More"
            | IOTypes (t) -> $"Per table only one {t} is allowed. The value of this column must be a unique identifier."
            | _ -> ""

    type BuildingBlockUIState = {
        DropdownIsActive    : bool
        DropdownPage        : DropdownPage
    } with
        static member init() = {
            DropdownIsActive    = false
            DropdownPage        = DropdownPage.Main
        }

    // type Model = {

    //     HeaderCellType  : CompositeHeaderDiscriminate
    //     HeaderArg       : U2<OntologyAnnotation,IOType> option
    //     BodyCellType    : CompositeCellDiscriminate
    //     BodyArg         : U2<string, OntologyAnnotation> option

    // } with
    //     static member init () = {

    //         HeaderCellType      = CompositeHeaderDiscriminate.Parameter
    //         HeaderArg           = None
    //         BodyCellType        = CompositeCellDiscriminate.Term
    //         BodyArg             = None
    //     }

    //     member this.TryHeaderOA() =
    //         match this.HeaderArg with
    //             | Some (U2.Case1 oa) -> Some oa
    //             | _ -> None

    //     member this.TryHeaderIO() =
    //         match this.HeaderArg with
    //             | Some (U2.Case2 io) -> Some io
    //             | _ -> None

    //     member this.TryBodyOA() =
    //         match this.BodyArg with
    //             | Some (U2.Case2 oa) -> Some oa
    //             | _ -> None

    //     member this.TryBodyString() =
    //         match this.BodyArg with
    //             | Some (U2.Case1 s) -> Some s
    //             | _ -> None
