(****************************************************************************)
(*                                                                           *)
(*   Vincent Jacquier                                     :::      ::::::::  *)
(*   Jean-Jacques MOIROUX                               :+:      :+:    :+:  *)
(*                                                    +:+ +:+         +:+    *)
(*   By: vjacquie <vjacquie@student.42.fr>          +#+  +:+       +#+       *)
(*   By: jmoiroux <jjmoiroux@gmail.com>           +#+#+#+#+#+   +#+          *)
(*                                                     #+#    #+#            *)
(*   Created: 2015/18/15 by vjacquie jmoiroux         ###   ########.fr      *)
(*                                                                           *)
(*****************************************************************************)

type z = int * int
type t = string * string * string * string (* why ? *)

let get_x ((a, b):z) = a
let get_y ((a, b):z) = b

let isInt charCheck =
	if charCheck >= '0' && charCheck <= '9' then true
	else false

let string_to_coord data =
	if String.length data <> 3 then ((-1), (-1))
	else if isInt data.[0] <> true || isInt data.[2] <> true || data.[1] <> ' ' then ((-1), (-1))
	else (((int_of_char(data.[0])) - 48), ((int_of_char(data.[2])) - 48))

let parse ((a, b):z) =
	if a < 1 || a > 9 then false
	else if b < 1 || b > 9 then false
	else true

let adj_cood_x x = (x - 1) mod 3

let adj_cood_y y =
	if y > 6 then 2
	else if y > 3 then 1
	else 0

let rec read_loop () =
	let line = read_line () in
	let test = string_to_coord line in
	if parse test = false then begin
		print_endline "Incorrect format.";
		read_loop ()
	end
	else string_to_coord line

let winner c = print_endline (c ^ " wins the game!")

let rec run play status playerNbr =
	let coord = read_loop () in
	let case = Mmap.getcase (adj_cood_x (get_x coord)) (adj_cood_y (get_x coord)) play in
	let ret = Case.putchar ((get_y coord) - 1) case playerNbr in
	if ret = false then begin print_endline "Illegal move." ; run play status playerNbr end
	else print_string "";
	ignore(Mmap.print_map1 play);
	print_char '\n';
	let is_win = Mmap.check play in
	if is_win = "0" then begin
		if playerNbr = '1' then begin print_endline "O 's turn to play" ; run play status '2' end
		else begin print_endline "X 's turn to play" ; run play status '1' end
	end
	else winner is_win

let main () =
	let play = DataSet.getNewMap () in
	ignore(Mmap.print_map1 play);
	print_char '\n';
	print_endline "X 's turn to play";
	run play true '1'


(*****************************************************************************)
let () = main ()
