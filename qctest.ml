open QCheck

let test_count = 10_000
  
(* A data type for symbolically representing calls to the Ptset API *)
type instr_tree =
  | Empty
  | Singleton of int
  | Add of int * instr_tree
  | Remove of int * instr_tree
  | Union of instr_tree * instr_tree
  | Inter of instr_tree * instr_tree

(*  to_string : instr_tree -> string  *)
let rec to_string a =
(*let int_to_string n = if n<0 then "(" ^ string_of_int n ^ ")" else string_of_int n in*)
  let int_to_string = string_of_int in
  match a with
  | Empty -> "Empty"
  | Singleton n -> "Singleton " ^ (int_to_string n)
  | Add (n,t) -> "Add (" ^ (int_to_string n) ^ ", " ^ (to_string t) ^ ")"
  | Remove (n,t) -> "Remove (" ^ (int_to_string n) ^ ", " ^ (to_string t) ^ ")"
  | Union (t,t') -> "Union (" ^ (to_string t) ^ ", " ^ (to_string t') ^ ")"
  | Inter (t,t') -> "Inter (" ^ (to_string t) ^ ", " ^ (to_string t') ^ ")"

(* interpreting calls over the Ptset module *)
let rec interpret t = match t with
  | Empty        -> Ptset.empty
  | Singleton n  -> Ptset.singleton n
  | Add (n,t)    -> Ptset.add n (interpret t)
  | Remove (n,t) -> Ptset.remove n (interpret t)
  | Union (t,t') ->
    let s  = interpret t in
    let s' = interpret t' in
    Ptset.union s s'
  | Inter (t,t') ->
    let s  = interpret t in
    let s' = interpret t' in
    Ptset.inter s s'


(** A recursive (size-bounded) generator of trees *)

(*  tree_gen : int Gen.t -> instr_tree Gen.t  *)
let tree_gen int_gen =
  Gen.sized
    (Gen.fix (fun recgen n -> match n with
      | 0 -> Gen.oneof [Gen.return Empty;
			Gen.map (fun i -> Singleton i) int_gen]
      | _ ->
	Gen.frequency
	  [ (1, Gen.return Empty);
	    (1, Gen.map (fun i -> Singleton i) int_gen);
	    (2, Gen.map2 (fun i t -> Add (i,t)) int_gen (recgen (n-1)));
	    (2, Gen.map2 (fun i t -> Remove (i,t)) int_gen (recgen (n-1)));
	    (2, Gen.map2 (fun l r -> Union (l,r)) (recgen (n/2)) (recgen (n/2)));
	    (2, Gen.map2 (fun l r -> Inter (l,r)) (recgen (n/2)) (recgen (n/2)));
	  ]))

let (<+>) = Iter.(<+>)
(*  tshrink : instr_tree -> instr_tree Iter.t  *)
let rec tshrink t = match t with
  | Empty -> Iter.empty
  | Singleton i ->
    (Iter.return Empty)
    <+> (Iter.map (fun i' -> Singleton i') (Shrink.int i))
  | Add (i,t) ->
    (Iter.of_list [Empty; t; Singleton i])
    <+> (Iter.map (fun t' -> Add (i,t')) (tshrink t))
    <+> (Iter.map (fun i' -> Add (i',t)) (Shrink.int i))
  | Remove (i,t) ->
    (Iter.of_list [Empty; t])
    <+> (Iter.map (fun t' -> Remove (i,t')) (tshrink t))
    <+> (Iter.map (fun i' -> Remove (i',t)) (Shrink.int i))
  | Union (t0,t1) ->
    (Iter.of_list [Empty;t0;t1])
    <+> (Iter.map (fun t0' -> Union (t0',t1)) (tshrink t0))
    <+> (Iter.map (fun t1' -> Union (t0,t1')) (tshrink t1))
  | Inter (t0,t1) ->
    (Iter.of_list [Empty;t0;t1])
    <+> (Iter.map (fun t0' -> Inter (t0',t1)) (tshrink t0))
    <+> (Iter.map (fun t1' -> Inter (t0,t1')) (tshrink t1))

(* An integer generator  *)
let arb_int =
  frequency
    [(5,small_signed_int);
     (3,int);
     (1, oneofl [min_int;max_int])]
 (* int *)
 (* small_signed_int *)

(*  arb_tree : instr_tree arbitrary *)
let arb_tree =
  make ~print:to_string ~shrink:tshrink
    (tree_gen arb_int.gen)


(** The model (identifiers are suffixed with _m) *)

let empty_m = []
let singleton_m i = [i]
let mem_m i s = List.mem i s
let add_m i s = if List.mem i s then s else List.sort compare (i::s)
let rec remove_m i s = match s with
  | [] -> []
  | j::s' -> if i=j then s' else j::(remove_m i s')
let rec union_m s s' = match s,s' with
  | [], _ -> s'
  | _, [] -> s
  | i::is,j::js -> if i<j then i::(union_m is s') else
                     if i>j then j::(union_m s js) else
		       i::(union_m is js)
let rec inter_m s s' = match s with
  | [] -> []
  | e::s -> if List.mem e s' then e::(inter_m s s') else inter_m s s'

(*let abstract s = Ptset.elements s*)
let abstract s = List.sort compare (Ptset.fold (fun i a -> i::a) s [])


(** A bunch of agreement properties *)

let test_empty =
  Test.make ~name:"empty" ~count:1
    unit
    (fun () ->
      let s = Ptset.empty in
      abstract s = empty_m)

let singleton_test =
  Test.make ~name:"singleton test" ~count:test_count
    arb_int
    (fun n ->
      abstract (Ptset.singleton n) = singleton_m n)

let mem_test =
  Test.make ~name:"mem test" ~count:test_count
    (pair arb_tree arb_int)
    (fun (t,n) ->
      let s = interpret t in
      Ptset.mem n s = mem_m n (abstract s))
    
let add_test =
  Test.make ~name:"add test" ~count:test_count
    (pair arb_tree arb_int)
    (fun (t,n) ->
      let s = interpret t in
      abstract (Ptset.add n s) = add_m n (abstract s))

let remove_test =
  Test.make ~name:"remove test" ~count:test_count
    (pair arb_tree arb_int)
    (fun (t,n) ->
      let s = interpret t in
      abstract (Ptset.remove n s) = remove_m n (abstract s))
   
let union_test =
  Test.make ~name:"union test" ~count:test_count
    (pair arb_tree arb_tree)
    (fun (t,t') ->
      let s  = interpret t in
      let s' = interpret t' in
      abstract (Ptset.union s s') = union_m (abstract s) (abstract s'))

let inter_test =
  Test.make ~name:"inter test" ~count:test_count
    (pair arb_tree arb_tree)
    (fun (t,t') ->
      let s  = interpret t in
      let s' = interpret t' in
      abstract (Ptset.inter s s') = inter_m (abstract s) (abstract s'))

;;
  QCheck_runner.run_tests(*_main*) ~verbose:true
    [ test_empty;
      singleton_test;
      mem_test;
      add_test;
      remove_test;
      union_test;
      inter_test;
    ]
