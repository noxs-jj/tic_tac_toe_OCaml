(*****************************************************************************)
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

(* 
type t_line = [Case.t]
type t_map = [t_line] *)

let rec getcase (x:int) (y:int) map = match map with
	| [] -> invalid_arg "Error y (getcase)"
	| h1::tail -> begin
		if y > 0 then getcase x (y - 1) tail
		else begin
			let rec loop1 x elem = match elem with
			| [] -> invalid_arg "Error x (getcase)"
			| e1::tail ->
			begin
				if x > 0 then loop1 (x - 1) tail
				else e1
			end
			in
			loop1 x h1
		end
	end

let createMap = 
	let lst = [Case.newCase ; Case.newCase ; Case.newCase] in
	[lst ; lst ; lst]

let get_Case_status (a, b, c, d) = d

let do_check x y z =
	if x = y && x = z then true
	else false

let check map =
	let c1 = (get_Case_status (List.nth (List.nth map 1) 1)) in
	let c2 = (get_Case_status (List.nth (List.nth map 1) 2)) in
	let c3 = (get_Case_status (List.nth (List.nth map 1) 3)) in
	let c4 = (get_Case_status (List.nth (List.nth map 1) 1)) in
	let c5 = (get_Case_status (List.nth (List.nth map 1) 2)) in
	let c6 = (get_Case_status (List.nth (List.nth map 1) 3)) in
	let c7 = (get_Case_status (List.nth (List.nth map 1) 1)) in
	let c8 = (get_Case_status (List.nth (List.nth map 1) 2)) in
	let c9 = (get_Case_status (List.nth (List.nth map 1) 3)) in
	if (do_check c1 c2 c3) = true then c1 
	else if (do_check c4 c5 c6) = true then c4
	else if (do_check c7 c8 c9) = true then c7
	else if (do_check c1 c4 c7) = true then c1
	else if (do_check c2 c5 c8) = true then c1 
	else if (do_check c3 c6 c9) = true then c1 
	else if (do_check c1 c5 c9) = true then c1
	else if (do_check c3 c5 c9) = true then c1
	else 0

let rec print_map1 map = match map with
	| [] -> print_string ""
	| h1::[] ->
	begin
		let rec loop elem line = match elem with
			| [] -> print_string ""
			| e1::[]-> Case.line_print e1 line
			| e1::tail ->
			begin
			 	Case.line_print e1 line;
			 	print_string " |";
			 	loop tail line
			 end
		in
		loop h1 0;
		print_char '\n';
		loop h1 1;
		print_char '\n';
		loop h1 2;
	end
	| h1::tail ->
	begin
		let rec loop elem line = match elem with
			| [] -> print_string ""
			| e1::[]-> Case.line_print e1 line
			| e1::tail ->
			begin
			 	Case.line_print e1 line;
			 	print_string " |";
			 	loop tail line
			 end
		in
		loop h1 0;
		print_char '\n';
		loop h1 1;
		print_char '\n';
		loop h1 2;
		print_char '\n';
		print_endline "-----------------------";
		print_map1 tail
	end
