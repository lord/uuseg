(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uuseg programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)


let unicode_version = Uucp.unicode_version

(* Segmenters *)

type 'a custom_inner = { name : string;
    create : unit -> 'a;
    copy : 'a -> 'a;
    mandatory : 'a -> bool;
    add : 'a -> [ `Uchar of Uchar.t | `Await | `End ] ->
      [ `Boundary | `Uchar of Uchar.t | `Await | `End ] }

type 'a segmenter =
  | Grapheme_cluster : Uuseg_grapheme_cluster.t segmenter
  | Word : Uuseg_word.t segmenter
  | Sentence : Uuseg_sentence.t segmenter
  | Line_break : Uuseg_line_break.t segmenter
  | Custom :'a custom_inner -> 'a segmenter

type custom = C : 'a custom_inner -> custom

type boundary =
  [ `Grapheme_cluster | `Word | `Sentence | `Line_break | `Custom of custom ]

let pp_boundary ppf b =
  match (b :> boundary) with
| `Grapheme_cluster -> Format.fprintf ppf "`Grapheme_cluster"
| `Word -> Format.fprintf ppf "`Word"
| `Sentence -> Format.fprintf ppf "`Sentence"
| `Line_break -> Format.fprintf ppf "`Line_break"
| `Custom (C s) -> Format.fprintf ppf "`Custom %s" s.name

(* Built-in segmenters *)

let mandatory_default _ = true

(* Generic segmenter inteface *)

type t = Seg : 'a * 'a segmenter -> t
type ret = Uuseg_base.ret

let create boundary =
  let boundary = (boundary :> boundary) in
  match boundary with
  | `Grapheme_cluster -> Seg (boundary, Uuseg_grapheme_cluster.create (), Grapheme_cluster)
  | `Word -> Seg (boundary, Uuseg_word.create (), Word)
  | `Sentence -> Seg (boundary, Uuseg_sentence.create (), Sentence)
  | `Line_break -> Seg (boundary, Uuseg_line_break.create (), Line_break)
  | `Custom (C c) -> Seg (boundary, c.create (), Custom c)

let boundary (Seg (_, seg)) =
  match seg with
  | Grapheme_cluster -> `Grapheme_cluster
  | Word -> `Word
  | Sentence -> `Sentence
  | Line_break -> `Line_break
  | Custom c -> `Custom c

let add (Seg (_, s, seg)) add =
  match seg with
  | Grapheme_cluster -> Uuseg_grapheme_cluster.add s add
  | Word -> Uuseg_word.add s add
  | Sentence -> Uuseg_sentence.add s add
  | Line_break -> Uuseg_line_break.add s add
  | Custom {add = f; _} -> f s add

let mandatory (Seg (_, s, seg)) =
 match seg with
  | Grapheme_cluster | Word | Sentence -> mandatory_default s
  | Line_break -> Uuseg_line_break.mandatory s
  | Custom {mandatory; _} -> mandatory s

let copy (Seg (b, s, seg)) =
  match seg with
  | Grapheme_cluster -> Seg (b, Uuseg_grapheme_cluster.copy s, seg)
  | Word -> Seg (b, Uuseg_word.copy s, seg)
  | Sentence -> Seg (b, Uuseg_sentence.copy s, seg)
  | Line_break -> Seg (b, Uuseg_line_break.copy s, seg)
  | Custom {copy; _} -> Seg (b, copy s, seg)

let pp_ret = Uuseg_base.pp_ret

(* Custom segmenters *)

let custom ?(mandatory = mandatory_default) ~name ~create ~copy ~add () =
  C { name; create; copy; mandatory; add }

let err_exp_await = Uuseg_base.err_exp_await
let err_ended = Uuseg_base.err_ended
let equal _ _ = raise (Invalid_argument "Uuseg.equal: not implemented")
