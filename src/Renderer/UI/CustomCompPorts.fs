module CustomCompPorts

(*
This module provides some functions that ensure consistency of instantiated custom components when changes are
made to ports in the underlying sheet. A dialog is presented which allows instantiated components ports to
be updated correctly, with best efforts attempt to keep existing connections to each instance where ports remain
the same or where it can safely be deduced how ports have been renamed.

The code potentially makes changes to every sheet in the project in the model, and writes out these changes to disk.
*)

open Fulma
open Fable.React
open Fable.React.Props

open Helpers
open JSHelpers
open ModelType
open ModelHelpers
open CommonTypes
open FilesIO
open Extractor
open PopupView
open FileMenuView





