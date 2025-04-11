open! Base
open! Stdio

let rec interpret stack code codePtr ptr mem =
  match code.(codePtr) with
  | '>' -> interpret stack code (codePtr + 1) (ptr + 1) mem
  | '<' -> interpret stack code (codePtr + 1) (ptr - 1) mem
  | '+' ->
    mem.(ptr) <- mem.(ptr) + 1;
    interpret stack code (codePtr + 1) ptr mem
  | '-' ->
    mem.(ptr) <- mem.(ptr) - 1;
    interpret stack code (codePtr + 1) ptr mem
  | '.' ->
    printf "%c" (Char.of_int mem.(ptr) |> Option.value_exn);
    interpret stack code (codePtr + 1) ptr mem
  | ',' ->
    let c = In_channel.input_char In_channel.stdin |> Option.value_exn in
    mem.(ptr) <- Char.to_int c;
    interpret stack code (codePtr + 1) ptr mem
  | '[' ->
    if mem.(ptr) = 0
    then jump_to_endloop stack code codePtr ptr mem
    else (
      Stack.push stack (codePtr + 1);
      interpret stack code (codePtr + 1) ptr mem)
  | ']' ->
    if mem.(ptr) = 0
    then (
      Stack.pop stack |> ignore;
      interpret stack code (codePtr + 1) ptr mem)
    else interpret stack code (Stack.top_exn stack) ptr mem
  | _ -> 0

and jump_to_endloop stack code codePtr ptr mem =
  let rec find_end code codePtr counter = function
    | '[' -> find_end code (codePtr + 1) (counter + 1) code.(codePtr + 1)
    | ']' when counter = 1 -> codePtr + 1
    | ']' -> find_end code (codePtr + 1) (counter - 1) code.(codePtr + 1)
    | _ -> find_end code (codePtr + 1) counter code.(codePtr + 1)
  in
  let newCodePtr = find_end code (codePtr + 1) 1 code.(codePtr + 1) in
  interpret stack code newCodePtr ptr mem

let () =
  let args = Sys.get_argv () in
  let filename = args.(1) in
  let hello_world = In_channel.read_all filename |> String.to_array in
  let mem = Array.create ~len:10000000 0 in
  let stack = Stack.create () in
  interpret stack hello_world 0 0 mem |> ignore;
  printf "\n"
