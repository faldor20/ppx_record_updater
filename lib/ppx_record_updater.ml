open Ppxlib
open Ast_helper

(* Core helper for creating located identifiers *)
let lident name =
  let loc = !default_loc in
  Ast_builder.Default.Located.lident ~loc name

(** Determines if a field has the [@update] *)
let has_update_attribute label =
  label.pld_attributes
  |> List.exists (fun x -> String.equal x.attr_name.txt "updater")

(* Determines if a field has the [@no_update] attribute *)
let has_no_update_attribute label =
  label.pld_attributes
  |> List.exists (fun x -> String.equal x.attr_name.txt "no_update")

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

(** Just use the original field value *)
let build_no_update_field ~loc label =
  let field_lid = lident label.pld_name.txt in
  let original_access = Exp.field [%expr original] field_lid in
  (lident label.pld_name.txt, original_access)

(* Generates the complete update function for a record type *)
let generate_update_function loc type_name labels =
  let original_type = Typ.constr (lident type_name) [] in
  let record_body =
    Exp.record
      (labels
      |> List.map (fun label ->
             if has_no_update_attribute label then
               build_no_update_field ~loc label
             else build_updated_field ~loc label))
      (None)
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
let create_updated_type_decl loc type_decl labels derives =
  let updated_fields =
    labels
    |> List.filter (fun label -> not (has_no_update_attribute label))
    |> List.map (fun label ->
           { label with pld_type = update_field_type ~loc label })
  in

  {
    type_decl with
    ptype_name = Loc.make ~loc (type_decl.ptype_name.txt ^ "_update_t");
    ptype_kind = Ptype_record updated_fields;
    ptype_loc = loc;
    (* I can just use a tuple, also args are tuplesa*)
    ptype_attributes =
      derives|>Option.map(fun x->
        Attr.mk (Loc.make ~loc "deriving") (PStr [%str [%e x]]);
        )|>Option.to_list
  }

(* Main generator implementation for the deriver *)

let args () =
  (*
  This works by matching parts of an expression/pattern untill we get to an ___ at that point whatever would have gone in the ___'s place gets output.
  In this case our lident needs a string and so we get a string
  *)
  Deriving.Args.(
  (*We expect the user to supply args that we then directly pass into the deriving attribute on our generated class. We don't have to parse anything because the deriving attribute itself will parse*)
    empty +> arg "derive" (__))

let generate_impl ~ctxt (_rec_flag, type_decls) args =
  (* let args=Some []in *)
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in

  type_decls
  |> List.concat_map @@ function
     | { ptype_name; ptype_kind = Ptype_record labels; _ } as decl ->
           (* |> List.map (fun x -> x.txt |> Longident.name) *)
         let updated_type = create_updated_type_decl loc decl labels args in
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
    ~str_type_decl:(Deriving.Generator.V2.make (args ()) generate_impl)
