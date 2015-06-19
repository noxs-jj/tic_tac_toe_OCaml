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

include Case

type t_line = [Case.t] 
type t_map = [t_line]

let rec getcase x y map = match map with
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

let rec init map = match map with
	| [] -> print_string ""
	| h1::tail -> begin
		let rec loop elem = match elem with
			| [] -> print_string ""
			| e1::tail -> Case.init e1; loop tail
		in
		loop h1;
		init tail
	end

let get_Case_status (_, _, _, d) = d

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