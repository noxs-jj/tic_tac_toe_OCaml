(********************************************************************************)
(*                                                                              *)
(*    Vincent Jacquier                                     :::      ::::::::    *)
(*    Jean-Jacques MOIROUX                               :+:      :+:    :+:    *)
(*                                                     +:+ +:+         +:+      *)
(*    By: vjacquie <vjacquie@student.42.fr>          +#+  +:+       +#+         *)
(*    By: jmoiroux <jjmoiroux@gmail.com>           +#+#+#+#+#+   +#+            *)
(*                                                      #+#    #+#              *)
(*    Created: 2015/18/15 by vjacquie jmoiroux         ###   ########.fr        *)
(*                                                                              *)
(********************************************************************************)

type z = int * int

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
	if a < 0 || a > 2 then false
	else if b < 0 || b > 2 then false
	else true

let rec read_loop () =
	let line = read_line () in
	let test = string_to_coord line in
(* 	print_int (get_x test);
	print_char '\n';
	print_int (get_y test);
	print_char '\n'; *)
	if parse test = false then read_loop ()
	else string_to_coord line
	(* if (parse (string_to_coord line)) = false then read_loop
	else string_to_coord line *)

let run play status =
	let coord = read_loop () in
	let case = Map.getcase ((get_x coord) / 3) ((get_y coord) / 3) play in
	Case.putchar ((get_x coord) mod 3) ((get_y coord) mod 3) case '1';
	ignore(Map.print_map1 play);
	print_char '\n'
	(* if status = true then run play status;  *)



let main () =
	let play = Map.createMap in
(* 	ignore(Map.print_map1 play);
	print_char '\n'; *)
	run play true

let () = main ()