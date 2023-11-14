(* 1.1.1 Draw me a line *)
let build_line n str =
  if n < 0 then
    (* ici on définit un cas d'exception où n est négatif *)
    invalid_arg "The line has to be set with a positive number of string"
  else let rec chap_build_line n str = match str with
           str when n <> 0 -> str ^ (chap_build_line (n-1) str)
          (* la  concaténation de n éléments de str en récurrence *)
          |_ -> ""
                  (* le cas d'arrêt de la récurrence *)
       in chap_build_line n str ;;
(* tous les programmes présentés par la suite sont aussi en récurrence 
et ont toujours un cas d'arrêt *)

(* 1.1.2 Draw me a square *)
(* on recrée build_line car on veut un output qui est unit et pas string *)
let build_line_print n str =
  if n < 0 then
    invalid_arg "The line has to be set with a positive number of string"
  else let rec chap_build_line_print n str = match str with
           str when n <> 0  -> print_string str; chap_build_line_print (n-1) str
          |_ -> ()
       in chap_build_line_print n str;;

let square n str =
  if n < 0 then
    invalid_arg "The square has to be set with a positive number of string"
  else let rec chap_square x = match x with
           x when x <> 0 ->build_line_print n str; print_newline();
                           chap_square (x-1)
          |_ -> ()
       in chap_square n;;

(* 1.1.3 Draw me a square - bis *)
let square2 n (a,b) =
  if n < 0 then
    invalid_arg "The square has to be set with a positive number of string"
  else let rec chap_square2 x = match x with
           x when x <> 0 && x mod 2 = 0 -> build_line_print n (a^b);
                            print_newline();
                            chap_square2 (x-1)
          |x when x <> 0 && x mod 2 = 1 -> build_line_print n (b^a);
                            print_newline();
                            chap_square2 (x-1)
          |_ -> ()
       in chap_square2 n;;

(* 1.1.4 Draw me a triangle *)
let triangle n str =
  if n < 0 then
    invalid_arg "The triangle has to be set with a positive number of string"
  else let rec chap_triangle x  = match x with
           x when x <>n ->build_line_print (x+1) str; print_newline();
                           chap_triangle (x+1)
          |_ -> ()
       in chap_triangle 0 ;;

(* 1.2.1 Draw me a pyramide *)
(*  on recrée build_line_print car on veut afficher dans l'ordre décroissant,
 de 0 à n fois str *)
let build_line_print0 n str =
  if n < 0 then
    invalid_arg "The line has to be set with a positive number of string"

  else let rec chap_build_line_print0 x str = match str with
           str when x <> n  -> print_string str;
                               chap_build_line_print0 (x+1) str
          |_ -> ()
       in chap_build_line_print0 0 str;;

let pyramid n (a,b) =
   if n < 2 then
    invalid_arg "The pyramid has to be set with at least n > 1 or it will be a line"
   else let rec chap_pyramid x = match x with
          |x when x <> 1  -> build_line_print (x-1) a;
                             build_line_print0 ((n-x+1)*2) b;
                             build_line_print(x-1) a;print_newline() ;
                             chap_pyramid (x-1);
          |_ -> print_endline((build_line (n*2) b))
        in chap_pyramid n;;

(* 1.2.2 Draw me a cross *)
(* on recrée la fonction pyramide, puisqu'on veut une pyramide vide 
pointée vers le bas, et une autre pyramide vide pointée vers le haut *)
(* les deux doivent avoir à leurs pointes, 2 fois le string b,
et (n-1) fois le string a *)
(* et la ligne entre les deux va etre print quand n = 0, 
donc quand la récurrence est au cas d'arrêt *)
let empty_pyramid_down n (a,b) =
   if n < 2 then
    invalid_arg "The empty down  pyramid has to be set with at least n > 1 or it will print nothing"
  else let rec chap_empty_pyramid_down x = match x with
           x when x <> 1  -> build_line_print (x-2) a; print_string b;
                             build_line_print0 ((n-x+1)*2-1) a ; print_string b;
                             build_line_print(x-2) a; print_newline();
                             chap_empty_pyramid_down (x-1);
          |_ -> ()
                  in chap_empty_pyramid_down n;;

let empty_pyramid_up n (a,b) =
   if n < 2 then
    invalid_arg "The empty down pyramid has to be set with at least n > 1 or it will print nothing"
  else let rec chap_empty_pyramid_up x = match x with
           x when x <> n-1  ->build_line_print0 (x) a; print_string b;
                              build_line_print ((n-x)*2-3) a ; print_string b;
                              build_line_print0 (x) a; print_newline();
                              chap_empty_pyramid_up (x+1);
          |_ -> ()
       in chap_empty_pyramid_up 0;;

let cross n (a,b) =
   if n < 2 then
    invalid_arg "The cross has to be set with at least n > 1 or it will print nothing"
  else let chap_cross x = match x with
           x when x <> 1  -> empty_pyramid_up x (a,b); build_line_print (n-1) a;
                             print_string b; build_line_print (n-1) a;
                             print_newline(); empty_pyramid_down x (a,b)
          |_ -> ()
       in chap_cross n;;

