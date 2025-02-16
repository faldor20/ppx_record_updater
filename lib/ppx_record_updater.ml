open Ppxlib
open Ast_helper

(* let rec expr_of_type typ = *)
(* let loc = typ.ptyp_loc in *)
(* match typ with *)
(* | Ptype_record labels-> *)
(* labels|> *)

let lid x =
  let loc = !default_loc in
  Ast_builder.Default.Located.lident ~loc x

let field_has_update_attr label =
  label.pld_attributes
  |> List.exists (fun x -> String.equal x.attr_name.txt "update")

let generate_update_function loc labels type_name =
  let og_typ = Typ.constr (lid type_name) [] in
  let fn_name name = Pat.var (Loc.make ~loc (name ^ "_apply_update")) in
  let record =
    let fields =
      labels
      |> List.map (fun label ->
             let field_lid = lid label.pld_name.txt in
             let update_access = Exp.field [%expr updater] field_lid in
             let og_access = Exp.field [%expr original] field_lid in
             match label.pld_type with
             | { ptyp_desc = Ptyp_constr (l_ident, _args); _ } ->
                 let this_name = l_ident.txt |> Longident.name in
                 let derives_updater = field_has_update_attr label in
                 if derives_updater then
                   let other_updater =
                     Exp.ident (lid (this_name ^ "_apply_update"))
                   in
                   (* let a=[%str type a={b:string[@hi];c:int}]in *)
                   let body =
                     [%expr
                       [%e update_access]
                       |> Option.map (fun x ->
                              [%e other_updater] x [%e og_access])
                       |> Option.value ~default:[%e og_access]]
                   in
                   (field_lid, body)
                 else
                   let body =
                     [%expr
                       [%e update_access]
                       |> Option.value ~default:[%e og_access]]
                   in
                   (field_lid, body)
             | _ ->
                 Location.raise_errorf ~loc
                   "Cannot derive anything for this type. record field type is \
                    not a valid type")
    in
    Exp.record fields None
  in

  [%stri
    let [%p fn_name type_name] =
     fun updater (original : [%t og_typ]) : [%t og_typ] -> [%e record]]

let generate_impl ~ctxt (_rec_flag, type_decls) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  type_decls
  |> List.concat_map @@ fun typ_decl ->
     match typ_decl with
     (*manifest is none for records*)
     | { ptype_name; ptype_kind = Ptype_record labels; _ } ->
         let newLabels =
           labels
           |> List.map (fun label ->
                  let typ =
                    if field_has_update_attr label then
                      match label.pld_type with
                      | { ptyp_desc = Ptyp_constr (l_ident, _args); _ } ->
                        
                          Typ.constr
                            ((l_ident.txt |> Longident.name) ^ "_update_t"
                            |> lid)
                            []
                      | _ ->
                          Location.raise_errorf ~loc
                            "Cannot derive anything for this type"
                    else label.pld_type
                  in
                  { label with pld_type = [%type: [%t typ] option] })
         in
         let name = Loc.make ~loc (ptype_name.txt ^ "_update_t") in
         let kind = Ptype_record newLabels in
         let newDecl =
           {
             typ_decl with
             ptype_kind = kind;
             ptype_name = name;
             ptype_loc = loc;
             ptype_attributes = [];
           }
         in
         (* [%stri type [%%e] = [%%t kind]] *)
         (* let oldDecl= {typ_decl with ptype_attributes= [];} in *)

         let item =
           { pstr_desc = Pstr_type (_rec_flag, [ newDecl ]); pstr_loc = loc }
         in
         let update_fn = generate_update_function loc labels ptype_name.txt in
         (* let b=[%stri type a= int ] in *)
         [
           [%stri (* module Updater = struct *) [%%i item]
                                                (* end *)]; update_fn;
         ]
     | _ -> Location.raise_errorf ~loc "Cannot derive anything for this type"

let impl_generator = Deriving.Generator.V2.make_noarg generate_impl
let stringify = Deriving.add "record_updater" ~str_type_decl:impl_generator
