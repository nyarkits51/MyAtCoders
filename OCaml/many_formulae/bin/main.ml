(* Dir/File: many_formula/main.ml *)
(* Author: nyarkits *)
(* Theme: Bitwise full searching *)
(* 
    Introduction: 

    Get all formula patterns from data sets 
    with searching algorithm using bitwise operators.
    To create Bitwise Full Searching program, 
    OCaml Batteries. BitSet is implemented 
    extensive functions. Like a companion code.

*)

open Batteries;;

module FormlMap = Map.Make(String);;

module BitSet = struct
  include BitSet
  let false_index bit_set =
    let rec loop i bit_set =
      match not (BitSet.mem bit_set i) with
        true -> i
      | false ->
        if i = BitSet.capacity bit_set then
          failwith "No zero bit."
        else loop (i+1) bit_set in
    loop 0 bit_set
  let bit_succ bit_set =
    if bit_set =
       BitSet.create
         (BitSet.capacity bit_set) then
      BitSet.set bit_set 0
    else
    if BitSet.mem bit_set
        ((BitSet.capacity bit_set)-1)
    then
      BitSet.differentiate_sym
        (BitSet.create_full
           (BitSet.capacity bit_set))
        bit_set
    else
      BitSet.differentiate_sym bit_set
        (BitSet.create_full
           ((false_index bit_set)+1))
end
;;

let () =
  let s = Scanf.scanf "%s" (fun x -> x) in 
  let n = String.length s in
  let num_set = BitSet.create n in
  let formula =
    let rec loop i () formula =
      if i = (1 lsl n) then formula
      else
        let rec loop2 j s_tmp = 
          if j = n then
            loop (i+1) (BitSet.bit_succ num_set)
              (FormlMap.add s_tmp (BitSet.enum num_set) formula)
          else
          if (BitSet.mem num_set j) && (j <> n-1) then
            loop2 (j+1)
              (String.cat s_tmp
                 (String.cat (Char.escaped s.[j]) "+"))
          else 
            loop2 (j+1)
              (String.cat s_tmp (Char.escaped s.[j])) in
        loop2 0 String.empty in
    loop 0 () FormlMap.empty in
  let formlseq = FormlMap.to_seq formula in
  let num_list = Seq.fold_left (fun l fseq ->
      (String.split_on_char '+' (fst(fseq))) @ l) [] formlseq in  
  let total = ref (Int64.of_int 0) in
  let () =
    List.fold_left (fun () num_str ->
        total := Int64.add (!total) (Int64.of_string num_str)) () num_list in
  Stdlib.Printf.printf "%Ld\n" !total
;;
