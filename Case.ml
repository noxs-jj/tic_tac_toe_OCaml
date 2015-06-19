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

(* module Case =
struct *)
	
	type t = string * string * string * string

	let newCase = (" - - -", " - - -", " - - -", "0")

	let whatPlayerFromInt player =
		if player = 1 then 'X'
		else if player = 2 then 'O'
		else '-'

	let whatPlayerFromChar charCheck =
		if charCheck = 'X' then '1'
		else if charCheck = 'O' then '2'
		else '0'

	let putchar x y case player = 
		let char_to_case = whatPlayerFromInt player in
		match case with
			| (line0, line1, line2, status) -> begin
				if y = 0 then String.set line0 (x * 2 + 1) char_to_case
				else if y = 1 then String.set line1 (x * 2 + 1) char_to_case
				else if y = 2 then String.set line2 (x * 2 + 1) char_to_case
				else invalid_arg "Case::putchar case wrong y"
			end

	let full_cercle case = match case with
		| (line0, line1, line2, status) -> begin
			String.set line0 1 '/'; String.set line0 3 '_'; String.set line0 5 '\\';
			String.set line1 1 '|'; String.set line1 3 ' '; String.set line1 5 '|';
			String.set line2 1 '\\'; String.set line2 3 '_'; String.set line2 5 '/'
		end

	let full_cross case = match case with
		| (line0, line1, line2, status) -> begin
			String.set line0 1 '\\'; String.set line0 3 ' '; String.set line0 5 '/';
			String.set line1 1 ' '; String.set line1 3 'X'; String.set line1 5 ' ';
			String.set line2 1 '/'; String.set line2 3 ' '; String.set line2 5 '\\'
		end

	let check_hori case = match case with
		| (line0, line1, line2, status) -> begin
			if (line0.[1] = line0.[3]) && (line0.[1] = line0.[5]) then whatPlayerFromChar line0.[1]
			else if (line1.[1] = line1.[3]) && (line1.[1] = line1.[5]) then whatPlayerFromChar line1.[1]
			else if (line2.[1] = line2.[3]) && (line2.[1] = line2.[5]) then whatPlayerFromChar line2.[1]
			else '0'
		end

	let check_vert case = match case with
		| (line0, line1, line2, status) -> begin
			if (line0.[1] = line1.[1]) && (line0.[1] = line2.[1]) then whatPlayerFromChar line0.[1]
			else if (line0.[3] = line1.[3]) && (line0.[3] = line2.[3]) then whatPlayerFromChar line0.[3]
			else if (line0.[5] = line1.[5]) && (line0.[5] = line2.[5]) then whatPlayerFromChar line0.[5]
			else '0'
		end

	let check_diag case = match case with
		| (line0, line1, line2, status) -> begin
			if (line0.[1] = line1.[3]) && (line0.[1] = line2.[5]) then whatPlayerFromChar line0.[1]
			else if (line2.[1] = line1.[3]) && (line2.[1] = line0.[5]) then whatPlayerFromChar line2.[1]
			else '0'
		end



	let check (case:t) = match case with
		| (line0, line1, line2, status) -> begin
				if check_hori case != '0' then status.[0] <- check_hori case
				else if check_vert case != '0' then status.[0] <- check_vert case
				else if check_diag case != '0' then status.[0] <- check_diag case
				else status.[0] <- '0'
		end

(* end *)
