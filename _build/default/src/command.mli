type command =
  | Roll
  | Grid of int
  | Player of string
  | Map
  | Quit
  | Buy
  | Sell of int
  | Upgrade
  | Continue

(** The type [command] represents a player command that is decomposed
    into a verb and possibly an object phrase. *)

exception Empty
(** Raised when an empty command is parsed. *)

exception Malformed
(** Raised when a malformed command is encountered. *)

val parse : string -> command
(** [parse command] parses users' inputs into commands that tell what
    actions to perform *)
