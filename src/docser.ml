#directory "+threads"
#load "unix.cma"
#load "threads.cma"

open Event
open Thread
open Result

exception Invalid_operation of string

type document = string
type username = string
type password = string
type entry = { id : int; doc : document; owner : username; viewers : username list }
type account = { u : username; p : password }
type 'a response = ('a, string) result

(* diffrent types of requests 
   from User to Server *)
type request =
  | Publish of account * document * int response channel
  | ChangeOwner of account * int * username * unit response channel
  | View of account * int * string response channel
  | AddAccount of account * unit response channel
  | AddViewer of account * int * username * unit response channel
(* Diffrent types of answers 
   from Server To User *)
type answer =
  | Done 
  | Result int
  | Mistake string
  | Doc string

type t = request channel

(* Helper method, for sending requests to Server 
   also it sets up new Channel to receive answer *)
let op msg s =
  let c = new_channel () in
  sync (send s (msg c));
  match sync (receive c) with Ok v -> v | Error msg -> raise (Invalid_operation msg)

(* Diffrent methods for sending requests to Server *)
let publish user pass doc = op (fun c -> Publish ({ user; pass }, doc, c))
let change_owner user pass id owner = op (fun c -> ChangeOwner ({ user; pass }, id, owner, c))  
let add_account user pass = op (fun c -> AddAccount ({ user; pass }, c))
let add_viewer user pass id viewer = op (fun c -> AddViewer ({ user; pass }, id, viewer, c))
let view user pass id = op (fun c -> View ({ user; pass }, id, c))

(* Method for creating server 
   and starting endless loop 
   on server thread this loop 
   takes care of user requests *)

let document_server () =
  let c = new_channel () in

  let rec loop accs docs next_id =
    let check_account a c =
      if List.find_opt (( = ) a) accs = None then (
        sync (send c (Mistake "invalid login"));
        loop accs docs next_id)
    in

    (match sync (receive c) with

    | Publish (acc, doc, answer_c) ->
        check_account acc answer_c;
        sync (send answer_c (Result next_id));
        loop accs ({ id = next_id; doc; owner = acc.u; viewers = [] } :: docs) (next_id + 1)


    | ChangeOwner (acc, id, new_owner, answer_c) ->
        check_account acc answer_c;
        if List.find_opt (fun a -> a.u = new_owner) accs = None then
          sync (send answer_c (Mistake "incorrect owner Username"))
        else if List.find_opt (fun d -> d.id = id && d.owner = acc.u) docs = None then
          sync (send answer_c (Mistake "You do not have access to that"))
        else (
          sync (send answer_c (Done));
          let new_docs = List.map (fun e -> if e.id = id then { e with owner = new_owner } else e) docs in
          loop accs new_docs next_id)


    | View (acc, id, answer_c) -> (
        check_account acc answer_c;
        match List.find_opt (fun e -> e.id = id) docs with
        | None -> sync (send answer_c (Mistake "document does not exist"))
        | Some e ->
            if e.owner = acc.u || List.find_opt (( = ) acc.u) e.viewers <> None then sync (send answer_c (Doc e.doc))
            else sync (send answer_c (Mistake "You do not have permission to check document")))


    | AddViewer (acc, id, viewer, answer_c) -> (
        check_account acc answer_c;
        match (List.find_opt (fun e -> e.id = id) docs, List.find_opt (fun e -> e.u = viewer) accs) with
        | None, _ -> sync (send answer_c (Mistake "document does not exist"))
        | _, None -> sync (send answer_c (Mistake "viewer does not exist"))
        | Some e, _ when e.owner <> acc.u -> sync (send answer_c (Mistake "You do not have permission to add Users"))
        | _ ->
            sync (send answer_c (Done));
            let new_docs = List.map (fun e -> if e.id = id then { e with viewers = viewer :: e.viewers } else e) docs in
            loop accs new_docs next_id)


    | AddAccount (acc, answer_c) ->
        if List.find_opt (fun a -> a.u = acc.u) accs <> None then sync (send answer_c (Mistake "account already exists"))
        else (
          sync (send answer_c (Done));
          loop (acc :: accs) docs next_id));
    loop accs docs next_id;
    failwith "endless loop"

    
  in
  let _ = create (loop [] []) 0 in

  c




