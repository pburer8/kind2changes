(* This file is part of the Kind 2 model checker.

   Copyright (c) 2015 by the Board of Trustees of the University of Iowa

   Licensed under the Apache License, Version 2.0 (the "License"); you
   may not use this file except in compliance with the License.  You
   may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0 

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
   implied. See the License for the specific language governing
   permissions and limitations under the License. 

*)

(* #load "threads.cma" *) (* might be necessary if testing in toplevel *)

open Lib


(******************************)
(*  Types                     *)
(******************************)


exception SocketConnectFailure
exception SocketBindFailure
exception BadMessage
exception InvalidProcessName  
exception NotInitialized


(* Pretty-print ZMQ message frames *)
let rec pp_print_zmsg_frames ppf = function
  | [] -> ()
  | x :: xs ->
      Format.fprintf ppf "%s" (String.escaped x);
      if xs != [] then Format.fprintf ppf ";@ ";
      pp_print_zmsg_frames ppf xs

(* Pretty-print a ZMQ message *)
let pp_print_zmsg ppf zmsg =
  (* Copy message and print all frames *)
  Format.fprintf ppf "@[<hv 1>{%a}@]" pp_print_zmsg_frames zmsg


(* Message and conversions *)
module type RelayMessage = 
sig

  (* A message to be relayed to other processes *)
  type t

  (** ZMQ's representation of the message *)
  type zmsg = string list

  (* Convert a message to a strings for message frames *)
  val message_of_strings : zmsg -> t

  (* Convert string from message frames to a message *)
  val strings_of_message : t -> zmsg

  (* Pretty-print a message *)
  val pp_print_message : Format.formatter -> t -> unit
  
end


(* Output signature of functor *)
module type S =
sig

  type relay_message 

  (** A message to be output to the user *)
  type output_message = 
    | Log of int * string
    | Stat of string
    | Progress of int

  (** A message internal to the messaging system *)
  type control_message = 
    | Ready
    | Ping
    | Terminate
    | Resend of int

  (** A message *)
  type message = 
    | OutputMessage of output_message
    | ControlMessage of control_message
    | RelayMessage of int * relay_message

  (** Thread *)
  type thread

  (** Handle to the invariant manager's publisher socket *)
  type im_socket
  val path_of_im : im_socket -> string
  val im_socket_of_path : string -> im_socket

  (** Handle to a worker's subscriber socket *)
  type worker_socket

  (** Create the publisher socket for the invariant manager. *)
  val init_im : unit -> im_socket

  (** Create a subscriber socket for a worker process, subscribed
      to the invariant manager's publisher. *)
  val init_worker : Lib.kind_module -> im_socket -> worker_socket

  (** Start the background thread for the invariant manager *)
  val run_im : im_socket -> (int * Lib.kind_module) list -> (exn -> unit) -> unit

  (** Start the background thread for a worker process *)
  val run_worker : worker_socket -> Lib.kind_module -> (exn -> unit) -> thread

  val send_relay_message : relay_message -> unit
  val send_output_message : output_message -> unit
  val send_term_message : unit -> unit
  val recv : unit -> (Lib.kind_module * message) list
  val update_child_processes_list : (int * Lib.kind_module) list -> unit

  (** Purge the invariant manager mailbox *)
  val purge_im_mailbox : im_socket -> unit

  val check_termination : unit -> bool
  val exit : thread -> unit 

end


(* Functor to instantiate the messaging system with a type of messages *)
module Make (T: RelayMessage) : S with type relay_message = T.t =
struct

  (* Background thread *)
  type thread = Thread.t

  (* Message to be broadcast *)
  type relay_message = T.t


  (* Message to be output to the user *)
  type output_message = 

    (* Log message with level *)
    | Log of int * string

    (* Statistics *)
    | Stat of string

    (* Progress *)
    | Progress of int
        
  
  (* Message internal to the messaging system *)
  type control_message = 

    (* Process is ready *)
    | Ready

    (* Request reply from process *)
    | Ping

    (* Request termination of process *)
    | Terminate

    (* Request resending of relay message *)
    | Resend of int


  (* Message *)
  type message = 

    (* Output to user *)
    | OutputMessage of output_message

    (* Message internal to the messaging system *)
    | ControlMessage of control_message

    (* Message to be broadcast to worker processes *)
    | RelayMessage of int * relay_message


  (* Pretty-print a message *)
  let pp_print_message ppf = function 
    | OutputMessage (Log (l, s)) -> 
      Format.fprintf ppf "@[<hv>LOG %d@ %s@]" l s
                               
    | OutputMessage (Stat _) -> 
      Format.fprintf ppf "@[<v>STAT@,@]"
        
    | OutputMessage (Progress k) -> 
      Format.fprintf ppf "@[<h>PROGRESS %d@]" k
                                    
    | ControlMessage Ready -> 
      Format.fprintf ppf "Ready"
                                
    | ControlMessage Ping -> 
      Format.fprintf ppf "Ping"
                               
    | ControlMessage Terminate -> 
      Format.fprintf ppf "Terminate"

    | ControlMessage (Resend i) -> 
      Format.fprintf ppf "Resend %d" i

    | RelayMessage (i, m) -> 
      Format.fprintf ppf "@[<hv>Relay %d@ %a@]" i T.pp_print_message m


  (* ******************************************************************** *)
  (* Conversions                                                          *)
  (* ******************************************************************** *)

  (* Return a list of strings of a message *)
  let strings_of_output_message = function 
    | Log (i, s) -> ["LOG"; string_of_int i; s]
    | Stat s -> ["STAT"; s]
    | Progress i -> ["PROGRESS"; string_of_int i]


  (* Return a message of a list of strings *)
  let output_message_of_strings = function
    | "LOG" :: i :: s :: _ -> (try Log (int_of_string i, s) with
        | Invalid_argument _ ->
          raise (Invalid_argument "output_message_of_strings a"))
    | "STAT" :: s :: _ -> Stat s
    | "PROGRESS" :: i :: _ -> (try Progress (int_of_string i) with 
        | Invalid_argument _ -> 
          raise (Invalid_argument "output_message_of_strings b"))
    | x -> ignore (Debug.messaging "MSG:%s" (String.concat "" x)); raise (Invalid_argument "output_message_of_strings c")


  (* Return a list of strings of a message *)
  let strings_of_control_message = function 
    | Ready -> ["READY"]
    | Ping -> ["PING"]
    | Terminate -> ["TERM"]
    | Resend i -> ["RESEND"; string_of_int i]


  (* Return a message of a list of strings *)
  let control_message_of_strings = function
    | "READY" :: _ -> Ready
    | "PING" :: _ -> Ping
    | "TERM" :: _ -> Terminate
    | "RESEND" :: i :: _ -> (try Resend (int_of_string i) with 
        | Invalid_argument _ -> 
          raise (Invalid_argument "control_message_of_strings"))
    | _ -> raise (Invalid_argument "control_message_of_strings")


  (* Return unique tag for message type *)
  let tag_of_message = function
    | OutputMessage _ -> "OUTPUT"
    | ControlMessage _ -> "CONTROL"
    | RelayMessage _ -> "RELAY"


  (* Return a message from strings *)
  let message_of_strings payload = function
    | "OUTPUT" -> OutputMessage (output_message_of_strings payload)
    | "CONTROL" -> ControlMessage (control_message_of_strings payload)
    | "RELAY" -> (
        let i = List.hd payload in
        try
          RelayMessage (int_of_string i, T.message_of_strings (List.tl payload))
        with Invalid_argument _ -> raise BadMessage)
    | _ -> raise BadMessage


  (*        zmsg representation of a message:              *)
  (* top of stack                                          *)
  (* ----------------------------------------------------- *)
  (*  MSG TYPE | SENDER | PAYLOAD | (PAYLOAD) | (PAYLOAD)  *)
  (* ----------------------------------------------------- *)
      
  (* We want the type of the message first, so that workers can
     subscribe to the relevant messages only *)

  (* Create a ZeroMQ message *)
  let zmsg_of_msg msg =
    (* Use the PID of the process as sender *)
    let sender = string_of_int (Unix.getpid ()) in
    let zmsg =
      tag_of_message msg :: sender
      ::
      ( match msg with
      | OutputMessage m -> strings_of_output_message m
      | ControlMessage m -> strings_of_control_message m
      | RelayMessage (i, m) -> string_of_int i :: T.strings_of_message m )
    in
    Debug.messaging "@[<hv>zmsg_of_msg:@ %a@]" pp_print_zmsg zmsg;
    (* Return message *)
    zmsg


  (* Return a message of a ZeroMQ message *)
  let msg_of_zmsg = function
    | tag :: sender :: payload ->
        Debug.messaging "@[<hv>msg_of_zmsg:@ %a@]" pp_print_zmsg
          (tag :: sender :: payload);
        (int_of_string sender, message_of_strings payload tag)
    | _ -> raise BadMessage

  (* ******************************************************************** *)
  (* Socket Module                                                        *)
  (* ******************************************************************** *)

  (* Send a buffer with a fixed-length header*)
  let send_frame flow buf =
    let len = Cstruct.length buf in
    let header = Cstruct.create 4 in
    Cstruct.BE.set_uint32 header 0 (Int32.of_int len);
    Eio.Flow.write flow [header; buf]

  (* Receive a buffer *)
  let recv_frame flow =
    let header = Cstruct.create 4 in
    Eio.Flow.read_exact flow header;
    let len = Int32.to_int (Cstruct.BE.get_uint32 header 0) in
    let max_frame = 16 * 1024 * 1024 in  (* 16 MB ceiling *)
    if len < 0 || len > max_frame then raise (Invalid_argument (Printf.sprintf "frame too large: %d" len));
    let body = Cstruct.create len in
    Eio.Flow.read_exact flow body;
    body

  (* Dynamically resolve the true OS temporary directory once at startup *)
  let temp_dir = 
    let raw_dir = try Sys.getenv "TMPDIR" with Not_found -> "/tmp" in
    try Unix.realpath raw_dir with Unix.Unix_error _ -> raw_dir
  
  (* Publisher module for invariant manager*)
  module Publisher : sig
    type t = {
      mutex : Eio.Mutex.t;
      path : string;
      mutable subscribers : string list;
      stream : string list Eio.Stream.t
    }

    val create : string -> t
    val path : t -> string
    val listen : t -> Eio_unix.Stdenv.base -> unit
    val recv_all : t -> string list option
    val send_all : t -> string list -> Eio_unix.Stdenv.base -> unit
  end = 
  struct
    type t = {
      mutex : Eio.Mutex.t;
      path : string;
      mutable subscribers : string list;
      stream : string list Eio.Stream.t
    }

    let create p =
      (* Drop a stale socket file *)
      (try Unix.unlink p with Unix.Unix_error _ -> ());
      {
        mutex = Eio.Mutex.create (); path = p; subscribers = []; stream = Eio.Stream.create max_int
      }

    let path pub = pub.path

    let snapshot_subscribers pub =
      Eio.Mutex.lock pub.mutex;
      let subs = pub.subscribers in
      Eio.Mutex.unlock pub.mutex;
      subs
    
    let recv pub sw server =
      (* Accept a connection from a client *)
      Eio.Net.accept_fork server ~sw ~on_error:raise
        (fun conn _addr ->
          try
            while true do
              (* Receive frame *)
              let frame = recv_frame conn in

              (* Reconstruct zmsg *)
              let str = Cstruct.to_string frame in
              let parts = Marshal.from_string str 0 in

              (* If this is the first message from this subscriber, add it to our list *)
              let path = temp_dir ^ "/worker" ^ (List.nth parts 1) ^ ".sock" in
              Eio.Mutex.lock pub.mutex;
              if not (List.mem path pub.subscribers) then pub.subscribers <- path :: pub.subscribers;
              Eio.Mutex.unlock pub.mutex;

              (* Add zmsg to queue *)
              Eio.Stream.add pub.stream parts
            done
          with End_of_file -> ()
        )
      
    (* Persistently accept connections from clients *)
    let listen pub env =
      Eio.Switch.run @@ fun sw ->
        let net = Eio.Stdenv.net env in
        let server = Eio.Net.listen net ~sw ~reuse_addr:true ~backlog:5 (`Unix pub.path) in
        while true do
          recv pub sw server
        done

    (* Take messages from the queue *)
    let recv_all pub =
      Eio.Stream.take_nonblocking pub.stream

    (* Send to all connections *)
    let send connections zmsg = 
      List.iter (fun conn -> send_frame conn (Cstruct.of_string (Marshal.to_string zmsg []))) connections

    (* Get rid of old connections *)
    let remove_dead_connections pub env =
      Eio.Mutex.lock pub.mutex;
      let dead_connections = ref [] in
      Eio.Switch.run @@ fun sw ->
        let net = Eio.Stdenv.net env in

        (* Try connecting to each subscriber. If it causes an error, it's dead*)
        List.iter(fun subscriber ->
          try
            (let _ = Eio.Net.connect ~sw net (`Unix subscriber) in
            ())
          with _ -> dead_connections := subscriber :: !dead_connections
        ) pub.subscribers;
      
      (* Filter out dead subscribers *)
      pub.subscribers <- List.filter (fun sub -> not (List.mem sub !dead_connections)) pub.subscribers;
      Eio.Mutex.unlock pub.mutex
    
    let send_all pub zmsg env =
      Eio.Switch.run @@ fun sw ->
        let net = Eio.Stdenv.net env in

        (* Remove dead connections first *)
        remove_dead_connections pub env;

        (* Connect to all subscribers *)
        let connections = List.map (fun path -> Eio.Net.connect ~sw net (`Unix (path))) (snapshot_subscribers pub) in
        Eio.Mutex.lock pub.mutex;
        send connections zmsg;
        Eio.Mutex.unlock pub.mutex
  end

  (* Subscriber module for workers *)
  module Subscriber : sig 
    type t = {
      mutex : Eio.Mutex.t;
      path : string;
      publisher_path : string;
      mutable topics : string list;
      stream : string list Eio.Stream.t
    }

    val create : string -> t
    val subscribe : t -> string -> unit
    val listen : t -> Eio_unix.Stdenv.base -> unit
    val recv_all : t -> string list option
    val send_all : t -> string list -> Eio_unix.Stdenv.base -> unit

  end =
  struct
    type t = {
      mutex : Eio.Mutex.t;
      path : string;
      publisher_path : string;
      mutable topics : string list;
      stream : string list Eio.Stream.t
    }

    let create publisher_path =
      (* PID-based path *)
      let p = temp_dir ^ Printf.sprintf ("/worker_%d.sock") (Unix.getpid ()) in
      (try Unix.unlink p with Unix.Unix_error _ -> ());
      {
        mutex = Eio.Mutex.create (); path = p; publisher_path; topics = []; stream = Eio.Stream.create max_int
      }

    (* Add to topic filtering *)
    let subscribe sub topic =
      Eio.Mutex.lock sub.mutex;
      sub.topics <- sub.topics @ [topic];
      Eio.Mutex.unlock sub.mutex


    let recv sub sw server =
      Eio.Net.accept_fork server ~sw ~on_error:raise
        (fun conn _addr ->
          try 
            while true do 
              let frame = recv_frame conn in
              let str = Cstruct.to_string frame in
              let parts = Marshal.from_string str 0 in
              if List.mem (List.hd parts) sub.topics then
                Eio.Stream.add sub.stream parts;
            done
          with End_of_file -> ()
        )

    let listen sub env =
      Eio.Switch.run @@ fun sw ->
        let net = Eio.Stdenv.net env in
        let server = Eio.Net.listen net ~sw ~reuse_addr:true ~backlog:5 (`Unix sub.path) in
        Eio.Fiber.fork ~sw
          (fun () ->
            while true do
              recv sub sw server
            done)
          
    let recv_all sub =
      Eio.Stream.take_nonblocking sub.stream
    
    let send out_conn zmsg =
      send_frame out_conn (Cstruct.of_string (Marshal.to_string zmsg []))
    
    let send_all sub zmsg env =
      Eio.Switch.run @@ fun sw ->
        let net = Eio.Stdenv.net env in

        (* Connect to publisher *)
        let out_conn = Eio.Net.connect ~sw net (`Unix sub.publisher_path) in
        Eio.Mutex.lock sub.mutex;
        send out_conn zmsg;
        Eio.Mutex.unlock sub.mutex
  end

  type im_socket = Publisher.t
  type worker_socket = Subscriber.t

  let path_of_im im = Publisher.path im
  (* Inside the Make functor in messaging.ml *)

  let im_socket_of_path p =
    { Publisher.mutex = Eio.Mutex.create (); 
      Publisher.path = p; 
      Publisher.subscribers = [];
      Publisher.stream = Eio.Stream.create max_int }

  (* ******************************************************************** *)
  (* Threadsafe list option                                               *)
  (* ******************************************************************** *)

  type 'a locking_list_option =
      { lock : Mutex.t ; mutable l_opt : 'a list option }

  let new_locking_list_option () =
    { lock = Mutex.create () ; l_opt = None }

  (* ******************************************************************** *)
  (* Threadsafe locking queue                                             *)
  (* ******************************************************************** *)
        
  type 'a locking_queue = { lock : Mutex.t ; mutable q : 'a list }


  let new_locking_queue () =
    { lock = Mutex.create (); q = [] }
    
  
  let enqueue entry queue =
    
    (* insert at back of queue *)
    Mutex.lock queue.lock;
    
    queue.q <- queue.q @ [entry]; 
    
    (* a tail-recursive append would be more efficient, depending on
       how big queue gets *)
    Mutex.unlock queue.lock


  (*
  let push_front entry queue = 
    
    (* push to front of queue *)
    Mutex.lock queue.lock;
    
    queue.q <- entry :: queue.q;
    
    Mutex.unlock queue.lock
  *)
      
  
  let dequeue queue =
    
    Mutex.lock queue.lock;
    
    let entry =
      match queue.q with 
        | [] -> None
        | h::t -> 
          queue.q <- t; 
          Some(h)
    in
    
    Mutex.unlock queue.lock;
    
    entry
    
  
  (* Return all elements in queue in order, and empty the queue *)
  let empty_list queue = 
    
    Mutex.lock queue.lock;
    
    let res = queue.q in
    
    queue.q <- [];
    
    Mutex.unlock queue.lock;
    
    res
    
  
  (* Checks if a message in 'queue' is such that f. Does not modify
     'queue'. *)
  let queue_exists f queue = 
    
    Mutex.lock queue.lock;

    let res = List.exists f queue.q in
    
    Mutex.unlock queue.lock;
    
    res
    
  (* ******************************************************************** *)
  (*  Globals                                                             *)
  (* ******************************************************************** *)

  (* Fresh incoming messages

     Keep messages in the order received, first message at the head of
     the list *)
  let incoming = new_locking_queue ()

  (* Optional list of new child processes. Used to tell the background
     thread we restarted with new child processes. *)
  let new_workers_option = new_locking_list_option ()

  (* Messages to be sent

     Keep messages in the order received *)
  let outgoing = new_locking_queue ()

  (* Messages to be delivered to worker process

     Keep messages in the order received *)
  let incoming_handled = new_locking_queue ()

  (* messages to receive iteration of the background thread loop *)
  let message_burst_size = 100

  (* how often (in seconds) must workers check in with Invariant
     Manager? *)
  let worker_time_threshold = (1.0 *. 60.)

  (* how soon (in seconds) must invariants be confirmed before workers
     resend them? *)
  let worker_invariant_confirmation_threshold = (0.3 *. 60.)

  (* currently initialized process *)
  let initialized_process = ref None
      
  (* debugging/testing? *)
  let debug_mode = ref false
      
  (* Exit requested? *)
  let exit_flag = ref false
      
  (* ******************************************************************** *)
  (*  Thread Helpers                                                      *)
  (* ******************************************************************** *)

  let im_handle_messages workers worker_status invariant_id invariants = 

    let rec handle_all = function

      | msg :: t ->  

        (* *)
        let sender, payload = msg in

        Debug.messaging
          "Invariant manager received message %a from %d"
          pp_print_message payload 
          sender;

        if List.mem_assoc sender workers then begin

        (match payload with 

          | OutputMessage _ -> 

            enqueue 
              ((List.assoc sender workers), payload) 
              incoming_handled

          | ControlMessage m -> 

            (match m with
              | Ready -> ()
              | Ping -> enqueue (ControlMessage(Ready)) outgoing
              | Terminate -> enqueue (ControlMessage(Terminate)) outgoing

              | Resend n -> 

                try 
                  enqueue (Hashtbl.find invariants n) outgoing
                with 
                  | Not_found -> ()

            )

          | RelayMessage (_, m) -> 

            let identified_msg = 
              RelayMessage (!invariant_id, m)
            in

            Hashtbl.add invariants !invariant_id identified_msg;

            enqueue identified_msg outgoing;

            invariant_id := !invariant_id + 1;

            enqueue
              ((List.assoc sender workers), payload) 
              incoming_handled
        );

        (* update the status of the sender *)
        Hashtbl.replace worker_status sender (Unix.time ())
        end ;

        handle_all t;

      | []  -> ()

    in

    let msgs = (empty_list incoming) in

    handle_all msgs
      
  
  let rec worker_request_missing_invariants 
      last_received_invariant_id 
      current_invariant_id =
    
    (* request all invariants between [last_received_invariant_id] and
       [current_invariant_id] *)
    if 
      
      ((!last_received_invariant_id) + 1) >= current_invariant_id 
      
    then
      
      ()
      
    else 
      
      (
        
        last_received_invariant_id := !last_received_invariant_id + 1;

        enqueue
          (ControlMessage (Resend (!last_received_invariant_id))) 
          outgoing;

        worker_request_missing_invariants 
          last_received_invariant_id 
          current_invariant_id

    )


  let worker_handle_messages 
      unconfirmed_invariants 
      confirmed_invariants 
      last_received_invariant_id = 

    (* handle messages in incoming queue of worker process *)

    (* it might be worth looking into efficiency of dealing with
       unconfirmed invariant list *)
    let rec handle_all = function

      | msg :: t ->  

        let sender, payload = msg in

        Debug.messaging
          "Worker received message %a from %d"
          pp_print_message payload 
          sender;

        (match payload with 

          | OutputMessage _ -> ()

          | ControlMessage m  -> 

            (match m with

              | Ready -> ()

              | Ping -> enqueue (ControlMessage Ready) outgoing

              | Terminate -> 

                enqueue
                  (`Supervisor, payload) 
                  incoming_handled

              (* Workers do not resend messages *)
              | Resend _ -> ()

            )


          | RelayMessage (i, m) ->

            (* Remove sequence number from message *)
            let payload' = RelayMessage (0, m) in 

            if 

              (* Message is ours and had not been confirmed? *)
              Hashtbl.mem 
                unconfirmed_invariants 
                payload'

            then 

              (

                (* Message is no longer unconfirmed *)
                Hashtbl.remove 
                  unconfirmed_invariants 
                  payload';

                (* Message is confirmed *)
                Hashtbl.add confirmed_invariants i msg

              ) 

            else 

              (

                (* Skip if message has received before *)
                if Hashtbl.mem confirmed_invariants i then () else 

                  (

                    (* Accept message *)
                    enqueue 
                      (`Supervisor, payload) 
                      incoming_handled;

                    (* Store message *)
                    Hashtbl.add confirmed_invariants i msg;

                    if 

                      (* Gap in sequence detected? *)
                      i > ((!last_received_invariant_id) + 1) 

                    then 

                      (

                        (* we've missed at least one invariant,
                           request any not received *)
                        worker_request_missing_invariants 
                          last_received_invariant_id 
                          i

                      );

                    (* Keep sequence for next iteration *)
                    last_received_invariant_id := i

                  )

              )

        );

        handle_all t;

      | [] -> ()

    in 

    handle_all (empty_list incoming)

  let purge_messages sock =
  let rec recv_iter = function
    | Some _ -> recv_iter (Publisher.recv_all sock)
    | None -> ()
  in
  recv_iter (Publisher.recv_all sock)


  let im_recv_messages (sock : Publisher.t) =
    (* receive up to 'message_burst_size' messages from sock *)
    let rec recv_iter i zmsg =
      if i < message_burst_size then (
        match zmsg with
        | Some m -> enqueue (msg_of_zmsg m) incoming ;
          recv_iter (i + 1) (Publisher.recv_all sock)
        | None -> ())
    in

    recv_iter 0 (Publisher.recv_all sock)

  let worker_recv_messages (sock : Subscriber.t) =
    (* receive up to 'message_burst_size' messages from sock *)
    let rec recv_iter i zmsg =
      if i < message_burst_size then (
        match zmsg with
        | Some m -> ( if not !debug_mode then
          enqueue (msg_of_zmsg m) incoming
        else
          let _, message = msg_of_zmsg m in
          enqueue (`Supervisor, message) incoming_handled );
        recv_iter (i + 1) (Subscriber.recv_all sock)
        | None -> ())
    in

    recv_iter 0 (Subscriber.recv_all sock)

  let im_send_messages sock env =
    (* send up to 'message_burst_size' messages in invariant manager's
       outgoing message queue *)
    let rec send_iter i outgoing_msg =
      if i < message_burst_size && outgoing_msg != None then (
        let message = get outgoing_msg in
        let zm = zmsg_of_msg message in
        Publisher.send_all sock zm env;

        send_iter (i + 1) (dequeue outgoing) )
    in

    send_iter 0 (dequeue outgoing)


  let worker_send_messages sock unconfirmed_invariants env =
    (* send up to 'message_burst_size' messages in worker's outgoing
       message queue *)
    let rec send_iter i outgoing_msg =
      if i < message_burst_size && outgoing_msg != None then (
        let message = get outgoing_msg in

        Debug.messaging "Worker %d sending message %a" (Unix.getpid ())
          pp_print_message message;

        Subscriber.send_all sock (zmsg_of_msg message) env;

        (* if this message is a relay message, place it in
           unconfirmed list with current timestamp *)
        ( match message with
        | RelayMessage (_, m) ->
            Hashtbl.add unconfirmed_invariants
              (RelayMessage (0, m))
              (Unix.time ())
        | _ -> () );

        send_iter (i + 1) (dequeue outgoing) )
    in

    send_iter 0 (dequeue outgoing)


  let worker_resend_invariants unconfirmed_invariants =
    (* resend unconfirmed invariants *)
    let resend_if_needed invariant timestamp =
      if Unix.time () -. timestamp > worker_invariant_confirmation_threshold
      then (
        enqueue invariant outgoing;

        (* a missed invariant is only resent once *)
        match invariant with
        | RelayMessage (_, m) ->
            Hashtbl.remove unconfirmed_invariants (RelayMessage (0, m))
        | _ -> () )
    in

    Hashtbl.iter resend_if_needed unconfirmed_invariants


  let update_worker_status workers worker_status =
    (* update timestamp of worker status *)
    for i = 0 to ((List.length workers) - 1) do
      Hashtbl.add (worker_status) (List.nth workers i) (Unix.time ());
    done

  (*
  let wait_for_workers workers worker_status pub_sock pull_sock =
    (* wait for ready from all workers *)
    let rec wait_iter = function
      (* No more workers to wait for *)
      | [] -> ()
      (* List of workers to wait for is not empty *)
      | workers_remaining -> (
          Debug.messaging "Sending PING to workers";

          (* let workers know invariant manager is ready *)
          Zmq.Socket.send_all pub_sock (zmsg_of_msg (ControlMessage Ping));

          (* Receive message on PULL socket *)
          try
            let msg = Zmq.Socket.recv_all ~block:false pull_sock in

            let sender, payload = msg_of_zmsg msg in

            if payload = ControlMessage Ready then (
              Debug.messaging
                "Received a READY message from %d while waiting for workers"
                sender;

              wait_iter (List.filter (( <> ) sender) workers_remaining) )
            else (
              Debug.messaging
                "Received message from %d while waiting for workers: %a" sender
                pp_print_message payload;

              wait_iter (List.filter (( <> ) sender) workers_remaining) )
          with Unix.Unix_error (Unix.EAGAIN, _, _) ->
            Debug.messaging "No message received, still waiting for workers";

            minisleep 0.1;
            wait_iter workers_remaining )
    in

    wait_iter workers;
    update_worker_status workers worker_status
    *)


  let im_check_workers_status workers worker_status =
    (* ensure that all workers have checked in within
       worker_time_threshold seconds *)
    let rec check_status workers need_ping =
      match workers with
      | h :: t ->
          let last_seen = 
            try Hashtbl.find worker_status h 
            with Not_found -> 0.0 (* Default to 0 if the worker hasn't registered a status yet *)
          in
          if
            Unix.time () -. last_seen > worker_time_threshold
          then (
            (* at least one worker has not communicated recently *)
            Hashtbl.replace worker_status h (Unix.time ());

            check_status t true )
          else check_status t need_ping
      | [] -> need_ping
    in

    (* if a worker hasn't communicated in a while, broadcast a ping *)
    if check_status workers false then enqueue (ControlMessage Ping) outgoing


  (* ******************************************************************** *)
  (*  Threads                                                             *)
  (* ******************************************************************** *)

  let im_thread (im : Publisher.t) workers on_exit =

    try 
      Eio_main.run @@ fun env ->
        Eio.Switch.run @@ fun sw ->

        Eio.Fiber.fork_daemon ~sw (fun () ->
          Publisher.listen im env;
          `Stop_daemon); 
        let invariant_id = ref 1 in

        let rec init_and_run workers =
          (* List of PIDs only. *)
          let worker_pids = List.map fst workers in

          (* Hashtable to store time each worker was last seen. *)
          let worker_status =
            (Hashtbl.create (List.length worker_pids))
          in

          (* Rewrite this code or remove it.
             Messages are discarded while waiting for workers.

          Debug.messaging
            "Waiting for workers (%a) to become ready."
            (pp_print_list Format.pp_print_int ",@")
            worker_pids;

          (* Waiting for all workers to be ready. *)
          wait_for_workers
            worker_pids worker_status pub_sock pull_sock ;

          Debug.messaging "All workers are ready.";*)

          update_worker_status worker_pids worker_status ;

          (* Unique invariant identifier and invariants hash table. *)
          invariant_id := 1 ;
          let invariants = (Hashtbl.create 1000) in

          (* Running with the workers pids, the time hashtable, and the
             invariants. *)
          run workers worker_pids worker_status invariants

        and run workers worker_pids worker_status invariants =

          (* We take the lock to avoid race conditions during restarts,
          especially we want to avoid messages from the previous analysis to be received *)
          Mutex.lock new_workers_option.lock ;

          (* Check for new workers, indicating a restart of the supervisor. *)
          let res = new_workers_option.l_opt in
          new_workers_option.l_opt <- None ;
          match res with
          | Some new_workers -> (
            (* We do not need the lock here
            because init_and_run does not reads the messages *)
            Mutex.unlock new_workers_option.lock ;
            Debug.messaging
              "Child processes update, \
                setting things up and resume running.";
            init_and_run new_workers
          )
          | None -> (
            (* No worker means that the reception of messages is disabled *)
            if worker_pids <> []
            then (
              (* Check on the workers. *)
              im_check_workers_status worker_pids worker_status ;

              (* Get any messages from workers. *)
              im_recv_messages im;

              (* Relay messages. *)
              im_handle_messages
                workers worker_status invariant_id invariants ;

              (* Send any messages in outgoing queue. *)
              im_send_messages im env
            ) ;
              
            (* We free the lock *)
            Mutex.unlock new_workers_option.lock ;

            Eio.Time.sleep (Eio.Stdenv.clock env) 0.01 ;

            run workers worker_pids worker_status invariants
          )

        in

        init_and_run workers
      
    with e -> on_exit e
                

  let worker_thread worker on_exit =
    try
      Eio_main.run @@ fun env -> 
        Eio.Switch.run @@ fun sw ->
        (*let rc =
          zmsg_send 
            (zmsg_of_msg 
               (ControlMessage Ready)) 
            push_sock
        in

        assert (rc = 0);

        (* wait for a message from the IM before sending anything *)
        Debug.messaging
          "Waiting for message from invariant manager in %d" (Unix.getpid ());

        ignore(zmsg_recv sub_sock);*)

          Debug.messaging "Worker is ready to send messages";

          let confirmed_invariants = (Hashtbl.create 1000) in
          let unconfirmed_invariants = (Hashtbl.create 100) in
          let last_received_invariant_id = ref 0 in

          Eio.Fiber.fork_daemon ~sw (fun () ->
            Subscriber.listen worker env;
            `Stop_daemon);
          let rec process_loop () =
            if !exit_flag then
              (* flush anything still queued before this fiber ends *)
              worker_send_messages worker unconfirmed_invariants env
            else begin
              worker_recv_messages worker ;

              if not !debug_mode then
                worker_handle_messages
                  unconfirmed_invariants
                  confirmed_invariants
                  last_received_invariant_id ;

              worker_send_messages worker unconfirmed_invariants env ;

              worker_resend_invariants unconfirmed_invariants ;

              Eio.Time.sleep (Eio.Stdenv.clock env) 0.01 ;
              process_loop ()
            end
          in  
        
          process_loop ()

    with e -> on_exit e


(* ******************************************************************** *)
(*  Public Interface                                                    *)
(* ******************************************************************** *)

  let init_im () =
    let im = Publisher.create (temp_dir ^ "/im.sock") in
    Debug.messaging "PUB socket is at %s/im.sock" temp_dir;
    im

  let init_worker proc im =
    let worker = Subscriber.create (Publisher.path im) in
    Subscriber.subscribe worker "CONTROL";
    Subscriber.subscribe worker "RELAY";
    Debug.messaging "SUB port for %a is %s" pp_print_kind_module proc worker.path;
    worker


  let run_im im workers on_exit =
    try
      let p =
        Thread.create
          (im_thread im workers)
          (fun exn ->
            on_exit exn)
      in

      initialized_process := Some `Supervisor;

      (* thread identifier, might come in handy *)
      ignore p
    with SocketBindFailure -> raise SocketBindFailure


  let run_worker worker proc on_exit =
    try
      let p =
        Thread.create
          (worker_thread worker )
          (fun exn ->
            on_exit exn)
      in

      initialized_process := Some proc;

      p
    with (* | Terminate -> raise Terminate *)
    | SocketConnectFailure ->
      raise SocketConnectFailure


  let send msg = 
    if !initialized_process = None then raise NotInitialized else
      ( (* minisleep otherwise some messages get lost *)
        minisleep 0.001;
        enqueue msg outgoing)


  let send_term_message () = send (ControlMessage Terminate)


  let send_output_message msg = send (OutputMessage msg)


  let send_relay_message msg = send (RelayMessage (0, msg))


  let recv () = 
    if !initialized_process = None then raise NotInitialized else
      (empty_list incoming_handled)


  let update_child_processes_list ps =
    Debug.messaging
      "Updating child process list in background thread.";
    if !initialized_process = None
    then raise NotInitialized
    else (
      (* Taking a lock on the list option. *)
      Mutex.lock new_workers_option.lock ;
      (* Setting the new value of the list option. *)
      new_workers_option.l_opt <- Some ps ;
      (* Releasing lock. *)
      Mutex.unlock new_workers_option.lock
    )


  let purge_im_mailbox im =
    if !initialized_process = None
    then raise NotInitialized
    else (
      (* Purging the messages because they refer to the old child processes *)
      purge_messages im;
      empty_list incoming |> ignore ;
      empty_list incoming_handled |> ignore ;
      empty_list outgoing |> ignore
    )

  let check_termination () =

    if !initialized_process = None
    then false
    else
      queue_exists
        ( fun msg ->
          match snd msg with
          | ControlMessage Terminate -> true
          | _ -> false )
        incoming_handled


  let exit t = 
    exit_flag := true; 
    Thread.join t

end


(*

(******************************)
(*  Tests                     *)
(******************************)

let messaging_selftest () = 

  let minisleep (sec: float) =
    (* sleep for sec seconds *)
    ignore (Unix.select [] [] [] sec)
  in

  let on_exit e = 
    print_endline (Printexc.to_string e)
  in

  let write_to_file file line = 
    let oc = open_out_gen [Open_creat; Open_text; Open_append; Open_nonblock] 0o640 file in
    output_string oc (line ^ "\n");
    close_out oc;
  in

  let worker_selftest worker_name = 
    let get_messages invariants_received outfile =
      let msgs = recv () in
      let write_msg msg = 
        write_to_file outfile ( worker_name ^ " received " ^ (string_of_msg msg));
      in 
      List.iter write_msg msgs;
      invariants_received := !invariants_received + List.length msgs;
    in
    let send_messages invariants_sent outfile =
      for i = 0 to 20 do
        let outmsg = (InvariantMessage(INVAR((worker_name ^ " invariant " ^ (string_of_int (!invariants_sent))), 0))) in
        send outmsg;
        invariants_sent := !invariants_sent + 1;
        let outmsg = (CounterexampleMessage(COUNTEREXAMPLE((!invariants_sent)))) in
        send outmsg;
        invariants_sent := !invariants_sent + 1;
        (* test BMC messages *)
        let outmsg = (InductionMessage(BMCSTATE(!invariants_sent, [worker_name; (worker_name ^ "x"); ""; "end of list"]))) in
        send outmsg;
        invariants_sent := !invariants_sent + 1;
      done;
    in
    (* overwrite file if it exists *)
    let outfile = ("selftest_out.txt") in
    let oc = open_out outfile in
    output_string oc "Self Test Results\n-----------------\n\n";
    close_out oc;
    write_to_file outfile (worker_name ^ " starting");
    ignore(init (kind_module_of_string worker_name) on_exit);
    let invariants_sent = ref 0 in 
    let invariants_received = ref 0 in
    for i = 0 to 10 do    
      send_messages invariants_sent outfile;
      get_messages invariants_received outfile;
      minisleep 0.01;
    done;
    Unix.sleep 1;
    get_messages invariants_received outfile;
    let resultsfile = "selftest_out.txt" in
    print_endline (worker_name ^ " results:");
    write_to_file resultsfile (worker_name ^ " results:\n" ^ "\t" ^ (string_of_int !invariants_sent) 
                              ^ " messages sent and " 
                              ^ (string_of_int !invariants_received)
                              ^ " messages received."
                              ^ (string_of_float (Sys.time ()))
                              ^ " seconds.");
    print_endline ("\t" ^ (string_of_int !invariants_sent) 
                  ^ " messages sent and " 
                  ^ (string_of_int !invariants_received)
                  ^ " messages received in "
                  ^ (string_of_float (Sys.time ()))
                  ^ " seconds.");
  in

  let im_selftest workers = 
    let outfile = "selftest_out.txt" in
    write_to_file outfile ("InvariantManager starting");
    ignore(init (InvariantManager workers) on_exit);
    while true do
      minisleep 0.01
    done;
  in

  (* begin test *)
  debug_mode := true;
  
  (* for each worker and the IM, spawn a process which will send and receive so many messages *)
  let pids = ref [] in
  (*
  let a = Unix.fork () in
  (match a with
    0   ->  im_selftest (); exit ExitCodes.success;
  | _   ->  pids := a::(!pids)
  );
*)
  let spawn_worker worker_name = 
    let a = Unix.fork () in
    (match a with
      0   ->  worker_selftest worker_name; exit ExitCodes.success; 
    | _   ->  pids := a::(!pids))
  in
  print_endline "Spawning workers and sending messages..";
  List.iter spawn_worker ["BMC"];
  im_selftest !pids; 
  (* wait for first child to exit *)
  let p, stat = Unix.wait () in
  ignore(p);
  ignore(stat); (* useful if you want the reason why the child terminated *)
  Unix.sleep 3;
  (* kill any remaining children *)
  let killprocess signal pid =
    try Unix.kill pid signal with Unix.Unix_error(Unix.ESRCH, "kill", "") -> ();
  in
  List.iter (killprocess 9) !pids;
  print_endline "Self test complete.";
  debug_mode := false;


(* messaging_selftest () *)

*)

(* 
   Local Variables:
   compile-command: "make -C .. -k"
   tuareg-interactive-program: "./kind2.top -I ./_build -I ./_build/SExpr"
   indent-tabs-mode: nil
   End: 
*)
