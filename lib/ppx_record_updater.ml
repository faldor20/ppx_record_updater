open Ppxlib
open Ast_helper

(* Core helper for creating located identifiers *)
let lident name =
  let loc = !default_loc in
  Ast_builder.Default.Located.lident ~loc name

(* Determines if a field has the [@update] attribute *)
let has_update_attribute label =
  label.pld_attributes
  |> List.exists (fun x -> String.equal x.attr_name.txt "update")

(* Generates the update function body for a record field *)
let build_updated_field ~loc label =
  let field_lid = lident label.pld_name.txt in
  let update_access = Exp.field [%expr updater] field_lid in
  let original_access = Exp.field [%expr original] field_lid in
  let body =
    match label.pld_type with
    | { ptyp_desc = Ptyp_constr (l_ident, _); _ } ->
        let type_name = Longident.name l_ident.txt in
        if has_update_attribute label then
          let updater_fn = Exp.ident (lident (type_name ^ "_apply_update")) in
          [%expr
            [%e update_access]
            |> Option.map (fun x -> [%e updater_fn] x [%e original_access])
            |> Option.value ~default:[%e original_access]]
        else
          [%expr
            [%e update_access] |> Option.value ~default:[%e original_access]]
    | _ ->
        Location.raise_errorf ~loc:label.pld_loc
          "Record field type must be a named type to derive laterupdater"
  in
  (lident label.pld_name.txt, body)

(* Generates the complete update function for a record type *)
let generate_update_function loc type_name labels =
  let original_type = Typ.constr (lident type_name) [] in
  let record_body =
    Exp.record (labels |> List.map (build_updated_field ~loc)) None
  in

  [%stri
    let [%p Pat.var (Loc.make ~loc (type_name ^ "_apply_update"))] =
     fun updater (original : [%t original_type]) : [%t original_type] ->
      [%e record_body]]

(* Transforms a record field type to optional update type *)
let update_field_type ~loc label =
  let typ =
    if has_update_attribute label then
      match label.pld_type with
      | { ptyp_desc = Ptyp_constr (l_ident, _); _ } ->
          Typ.constr (lident (Longident.name l_ident.txt ^ "_update_t")) []
      | _ -> Location.raise_errorf ~loc:label.pld_loc "Invalid update type"
    else label.pld_type
  in
  [%type: [%t typ] option]

(* Creates the updated type declaration for the generated update type *)
let create_updated_type_decl loc type_decl labels =
  let updated_fields =
    labels
    |> List.map (fun label ->
           { label with pld_type = update_field_type ~loc label })
  in

  {
    type_decl with
    ptype_name = Loc.make ~loc (type_decl.ptype_name.txt ^ "_update_t");
    ptype_kind = Ptype_record updated_fields;
    ptype_loc = loc;
    ptype_attributes = [];
  }

(* Main generator implementation for the deriver *)
let generate_impl ~ctxt (_rec_flag, type_decls) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in

  type_decls
  |> List.concat_map @@ function
     | { ptype_name; ptype_kind = Ptype_record labels; _ } as decl ->
         let updated_type = create_updated_type_decl loc decl labels in
         let type_decl_item =
           {
             pstr_desc = Pstr_type (Recursive, [ updated_type ]);
             pstr_loc = loc;
           }
         in
         let update_fn = generate_update_function loc ptype_name.txt labels in
         [ type_decl_item; update_fn ]
     | _ ->
         Location.raise_errorf ~loc "Can only derive updater for record types"

(* Register the deriver *)
let _ =
  Deriving.add "record_updater"
    ~str_type_decl:(Deriving.Generator.V2.make_noarg generate_impl)
