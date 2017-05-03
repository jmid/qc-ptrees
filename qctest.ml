open QCheck

let test_count = 2_500
  
(* different integer generators  *)

let intgen = frequency
              [(8,int);
	       (1, int_range (-1000) 1000);
	       (1, oneofl [min_int;-1;0;1;max_int])]
 
(* sign * pow(2, k) +/- 3 *)
let jespergen =
  make ~print:Print.int ~shrink:Shrink.int
    (Gen.map3
       (fun sign expo addition ->
	 let n = 1 lsl expo in
	 let n = if sign then -n else n in
	 if addition then n+3 else n-3)
       Gen.bool (Gen.int_range 60 63) Gen.bool) (*Force to large numbers *)

let arb_int =
  frequency
    [(5,small_int);
     (3,int);
     (1, oneofl [min_int;max_int])]

(* A data type for symbolically representing calls to the Ptset API *)
type instr_tree =
  | Empty
  | Singleton of int
  | Add of int * instr_tree
(*  | Mem of int * instr_tree  *)
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
(*  | Mem (n,t) -> "Mem (" ^ (int_to_string n) ^ ", " ^ (to_string t) ^ ")"  *)
  | Remove (n,t) -> "Remove (" ^ (int_to_string n) ^ ", " ^ (to_string t) ^ ")"
  | Union (t,t') -> "Union (" ^ (to_string t) ^ ", " ^ (to_string t') ^ ")"
  | Inter (t,t') -> "Inter (" ^ (to_string t) ^ ", " ^ (to_string t') ^ ")"

(* interpreting calls over the Ptset module *)
let rec interpret t = match t with
  | Empty        -> Ptset.empty
  | Singleton n  -> Ptset.singleton n
  | Add (n,t)    -> Ptset.add n (interpret t)
(*  | Mem (n,t)    ->
    let s = interpret t in
    let _ = Ptset.mem n s in
    s  *)
  | Remove (n,t) -> Ptset.remove n (interpret t)
  | Union (t,t') ->
    let s  = interpret t in
    let s' = interpret t' in
    Ptset.union s s'
  | Inter (t,t') ->
    let s  = interpret t in
    let s' = interpret t' in
    Ptset.inter s s'


(* A recursive (size-bounded) generator of trees *)

(*  tree_gen : instr_tree Gen.t  *)
let tree_gen int_gen =
  Gen.sized
    (Gen.fix (fun recgen n -> match n with
      | 0 -> Gen.oneof [Gen.return Empty;
			Gen.map (fun i -> Singleton i) int_gen]
      | _ ->
	Gen.frequency
	  ([ (1, Gen.return Empty);
	     (1, Gen.map (fun i -> Singleton i) int_gen);
	     (2, Gen.map2 (fun i t -> Add (i,t)) int_gen (recgen (n-1)));
          (* (2, Gen.map2 (fun i t -> Mem (i,t)) int_gen (recgen (n-1))); *)
	     (2, Gen.map2 (fun i t -> Remove (i,t)) int_gen (recgen (n-1)));
	     (2, Gen.map2 (fun l r -> Union (l,r)) (recgen (n/2)) (recgen (n/2)));
	     (2, Gen.map2 (fun l r -> Inter (l,r)) (recgen (n/2)) (recgen (n/2)));
	   ] )))

let (<+>) = Iter.(<+>)
let rec tshrink t = match t with
  | Empty -> Iter.empty
  | Singleton i ->
    (Iter.return Empty)
    <+> (Iter.map (fun i' -> Singleton i') (Shrink.int i))
  | Add (i,t) ->
    (Iter.of_list [Empty; t; Singleton i])
    <+> (Iter.map (fun t' -> Add (i,t')) (tshrink t))
    <+> (Iter.map (fun i' -> Add (i',t)) (Shrink.int i))
(*  | Mem (i,t) ->
    (Iter.of_list [Empty; t])
    <+> (Iter.map (fun t' -> Mem (i,t')) (tshrink t))
    <+> (Iter.map (fun i' -> Mem (i',t)) (Shrink.int i))  *)
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

(*  arb_tree : instr_tree arbitrary *)
let (>>=) = Gen.(>>=)
let arb_tree =
  make ~print:to_string ~shrink:tshrink
 (* (tree_gen Gen.int) *)
    (tree_gen arb_int.gen)
(*  (fun rs ->
      tree_gen (let is = Gen.generate ~rand:rs ~n:5 arb_int.gen in
		Gen.oneof [Gen.oneofl is; arb_int.gen]) rs) *)
    (* the following will generate the same list upfront before all tests:
    (tree_gen (let is = Gen.generate 5 arb_int.gen in
		Gen.oneof [Gen.oneofl is; arb_int.gen])) *)
    (* was: (tree_gen arb_int.gen) *)
(*    (Gen.generate oneof [tree_gen arb_int;])   *)
						

let empty_m = []
let singleton_m i = [i]
let mem_m i s = List.mem i s
let add_m i s = List.sort_uniq compare (i::s)
let rec remove_m i s = match s with
  | [] -> []
  | j::s' -> if i=j then s' else j::(remove_m i s')
let union_m s s' = ((*(),*) List.sort_uniq compare (s@s'))
let rec inter_m s s' = match s with
  | [] -> []
  | e::s -> if List.mem e s' then e::(inter_m s s') else inter_m s s'
let elements_m s = s

(*let abstract s = Ptset.elements s*)
let abstract s = List.sort Pervasives.compare (Ptset.fold (fun i a -> i::a) s [])

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
      abstract (Ptset.add n s) = add_m (n(*+1*)) (abstract s))

let remove_test =
  Test.make ~name:"remove test" ~count:test_count
    (pair arb_tree arb_int)
    (fun (t,n) ->
      let s = interpret t in
      abstract (Ptset.remove n s) = remove_m (n(*+1*)) (abstract s))
   
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

(* This will just test the identity fun. applied to interp t *)
(*
let elements_test =
  Test.make ~name:"elements test" ~count:test_count
    arb_tree
    (fun t ->
      let s  = interpret t in
      Ptset.elements s = elements_m (abstract s))
*)
    
;;
  QCheck_runner.run_tests(*_main*) ~verbose:true
    [ test_empty;
      singleton_test;
      mem_test;
      add_test;
      remove_test;
      union_test;
      inter_test;
   (* elements_test; *)
    ]
(*
;;    
let ls =
  Ptset.elements
    (Ptset.union
       (Ptset.add (-4611686018427387904) (Ptset.singleton 0))
       (Ptset.add (-4611686018427387904) (Ptset.singleton 1))) in
 print_endline (Print.list string_of_int ls)
*)
(*
;;
print_endline (to_string (Gen.generate1 arb_tree.gen))
*)
