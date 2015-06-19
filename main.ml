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

let main () =
	let play = Map.createMap in
	ignore(Map.print_map1 play);
	print_char '\n'


let () = main ()