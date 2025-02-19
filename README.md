# PPX Record Updater

A PPX deriver for OCaml that generates record update functions for dynamically performing partial updates on records.

## Features
- Generates update functions for record types
- Supports nested record updates via `[@updater]` attribute
- Supports excluding fields from being updatable via `[@no_updater]` attribute
- Can specify what ppx the generated updater type should derive
  with `[@@deriving record_updater ~derive: show,yojson ]`

## Why would I want this?

### PATCH API
It's quite common to want to apply some kind of diff to a record.

Say you want to offer a patch API for some internal type. Usually you would have to manually make a copy of the type with all the fields optional, and then apply the patch manually.

This PPX deriver will generate the code so you can just call `my_type_apply_update` to apply the patch.

### Loading config

My motivation for creating this was loading a config file that will override a default config.

I load the update config, and then apply it to the default config.


## Installation

1. Install via OPAM/ add to your `dune-package` file:

```bash
opam install ppx_record_updater
```

2. Add to your `dune` file dependencies:

```lisp

(preprocess (pps ppx_record_updater ))

```

## Usage

Add the `record_updater` deriver to your type definition:

```ocaml
type person = {
age : int;
name : string;
} [@@deriving record_updater]
```

This generates:

- `person_apply_update` function
- `person_update_t` type
- `address_apply_update` function
- `address_update_t` type

### Basic Example

We can then use the `person_apply_update` function to update a `person` record with any fields that are Some in the `person_update_t` record.

```ocaml
let original = { age = 30; name = "Alice" }
let update   = { age = Some 31; name = None }
let updated = person_apply_update update original

updated == { age = 31; name = "Alice" }
```
### Nested Updates

Mark nested record fields with `[@updater]` to enable nested updating.
This will make the updating attempt to update the individual fields of the nested record rather than replacing it outright.

```ocaml
type inner = { value1 : int; value2 : int } [@@deriving record_updater]
type outer = { nested : inner [@update] } [@@deriving record_updater]
let updated = outer_apply_update
{ nested = Some { value1 = Some 42; value2 = None } }
{ nested = { value1 = 0; value2 = 0 } }

updated == { nested = { value1 = 42; value2 = 0 } }
```
### Excluding Fields

Use `[@no_updater]` to exclude a field from being updatable.

```ocaml
type person = { age : int; name : string [@no_updater] } [@@deriving record_updater]
let original = { age = 30; name = "Alice" }
(* We can't update the name field *)
let update   = { age = Some 31 }
let updated = person_apply_update update original

updated == { age = 30; name = "Alice" }
```
### Deriving for `update_t` type

You can specify what ppx you would like the updater to derive using the `~derive` parameter.

```ocaml
type person = { age : int } [@@deriving record_updater ~derive: show,yojson]
let updater={age= Some 10}
let _ = print_endline (show updater)
```


## Generated Code Explanation

For the example record `person`, the deriver generates:

1. **Update Type** (`person_update_t`):

```ocaml
type person_update_t = {
age: int option;
name: string option;
address: address_update_t option;
}
```

2. **Update Function** (`person_apply_update`):

```ocaml
let person_apply_update =
fun (updater : person_update_t) (original : person) ->
  { age = updater.age |> Option.value ~default:original.age
  ; name = updater.name |> Option.value ~default:original.name
  ; address = updater.address |>Option.map address_apply_update |>Option.value ~default:original.address
  }
```

## Limitations

- Requires named types for nested records
- Currently doesn't handle parametrized types
- Fields must be concrete types (no polymorphic fields)

## License

MIT - See [LICENSE](LICENSE) file for details
