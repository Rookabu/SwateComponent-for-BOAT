namespace App

open Feliz
open Feliz.Bulma
open ARCtrl

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

type Components =
    [<ReactComponent>]
    static member annoBlockwithSwate() =
    
        let testAnno = Annotation.init(OntologyAnnotation("key1"), CompositeCell.createFreeText("value1"))
        let (annoState: Annotation list, setState) = React.useState ([testAnno])
        let revIndex = 0
        let a = annoState.[revIndex]

        let updateAnnotation (func:Annotation -> Annotation) =
            let nextA = func a
            annoState |> List.mapi (fun i a ->
                if i = revIndex then nextA else a 
            ) |> setState

        Bulma.block [
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

