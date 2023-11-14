#use "topfind" ;;
#require "graphics" ;;
open Graphics ;;
open_graph " 1200x800" ;;

let test (x, y) (z, t) =
  clear_graph () ;
  set_color red ;
  moveto x y ;
  lineto z t ;
  set_color blue ;
  rmoveto x y ;
  rlineto z t ;;

open_graph " 1200x800" ;;
test (50,50) (50,150) ;;
test (100,100) (200,100) ;;



let draw_line (x, y) (z, t) =
  moveto x y ;
  lineto z t ;;

let draw_line_float (x, y) (z, t) =
  moveto (int_of_float(x)) (int_of_float(y));
  lineto (int_of_float(z)) (int_of_float(t));;


(* toutes les fonctions présentés ont des calculs en float 
pour etre plus précis  *)
(* 2.1.1 Mountain *)
let mountain n (x, y) (z, t) =
  if n < 0 then
    invalid_arg "n has to be at least bigger then 0 !"
  else
    let rec chap_mountain a w (x, y) (z, t)  = match a with
        a when a >= 0 -> chap_mountain (a-1) ((y+t)/2 + Random.int (10*(a+1))-40)
                           (x,y) (((x+z)/2),w);
                         chap_mountain (a-1) ((y+t)/2 + Random.int (10*(a+1))-40)
                           (((x+z)/2),w) (z, t)
       |_ ->  draw_line (x,y) (z,t)
    in clear_graph() ; chap_mountain n ((y+t)/2+Random.int (10*(n+1)))(x, y)(z,t);;

mountain 9 (200, 200) (1000, 800);;

(* 2.1.2 Dragon *)
let dragon n (x, y) (z, t) =
  if n < 0 then
    invalid_arg "n has to be at least bigger then 0 !"
  else
    let rec chap_dragon a (x, y) (z, t) = match a with
        a when a >= 0-> chap_dragon (a-1) (x, y)
                           ((x+.z)/.2.+.(t-.y)/.2., (y+.t)/.2.-.(z-.x)/.2.);
                         chap_dragon (a-1) (z, t)
                           ((x+.z)/.2.+.(t-.y)/.2., (y+.t)/.2.-.(z-.x)/.2.)
       |_ -> draw_line_float (x,y) (z, t)
    in clear_graph() ; chap_dragon n (float_of_int(x),float_of_int(y))
                         (float_of_int(z),float_of_int(t));;
                         
dragon 20 (300, 400) (600, 600) ;; 
dragon 19 (150,150) (350, 350);;

(* 2.2.1 Sierpinski carpet *)
let draw_sponge n (x, y) (w,h)  = if n < 0 then
                  invalid_arg "the sponge has to be at least bigger then 0 !"
                else let rec chap_draw_sponge n (x, y) (w, h) = match n with
                       |n when n >= 0 -> set_color (rgb 255 127 0);
                         fill_rect (int_of_float(x)) (int_of_float(y))
                           (int_of_float(w)) (int_of_float(h)) ;
                         set_color (rgb 255 255 255);
                         fill_rect (int_of_float(x+.w/.3.))
                           (int_of_float(y+.w/.3.))
                           (int_of_float(w/.3.)) (int_of_float(h/.3.));
                         set_color (rgb 255 127 0);
                         chap_draw_sponge (n-1) (x, y) (w/.3., h/.3.);
                         chap_draw_sponge (n-1) (x, (y+.h/.3.)) (w/.3., h/.3.);
                         chap_draw_sponge (n-1) (x, y+.2.*.(w/.3.))
                           (w/.3., h/.3.);
                         chap_draw_sponge (n-1) (x+.w/.3., y+.2.*.(w/.3.))
                           (w/.3., h/.3.);
                         chap_draw_sponge (n-1) (x+.2.*.(w/.3.), y+.2.*.(w/.3.))
                           (w/.3., h/.3.);
                         chap_draw_sponge (n-1) (x+.2.*.(w/.3.), y+.w/.3.)
                           (w/.3., h/.3.) ;
                         chap_draw_sponge (n-1) (x+.2.*.(w/.3.), y)
                           (w/.3., h/.3.) ;
                         chap_draw_sponge (n-1) (x+.w/.3., y) (w/.3., h/.3.)
                       |_ -> draw_line_float (x, y) (x, y)
                     in clear_graph();
                        chap_draw_sponge n (float_of_int(x), float_of_int(y))
                          (float_of_int(w), float_of_int(h));
                        set_color blue;;
                        
draw_sponge 7 (400, 300) (500, 500);;

(* 2.2.2 Sierpinski triangle *)
let draw_triangle n (x,y) w  =
  if n < 0 then
    invalid_arg "n has to be at least bigger then 0 !"
  else let rec chap_draw_triangle n (x,y) w = match n with
         |n when n >= 0 -> draw_line_float (x, y) ((x+.w),y);
                draw_line_float (x, y) ((x+.w/.2.), (y+.(sqrt(3.)/.2.*.w)));
                draw_line_float ((x+.w/.2.), (y+.(sqrt(3.)/.2.*.w))) ((x+.w), y);
                chap_draw_triangle (n-1) (x, y) (w/.2.);
                chap_draw_triangle (n-1) ((x+.w/.2.), y) (w/.2.);
                chap_draw_triangle (n-1) ((x+.w/.4.), (y+.((sqrt(3.)/.2.*.w))/.2.)) (w/.2.)
         |_ -> draw_line_float (x, y) (x, y)
       in clear_graph(); chap_draw_triangle n
                           ((float_of_int(x)),(float_of_int(y)))
                           (float_of_int(w));;

draw_triangle 12 (400, 300) 500;;

(* 2.3.1 Cercle *)
let draw_circle_float x y r = draw_circle (int_of_float(x)) (int_of_float(y))
                                (int_of_float(r)) ;;
let draw_cercle n (x, y) r =
  if n < 0 then
    invalid_arg "n has to be at least bigger then 0 !"
  else let rec chap_draw_cercle n (x, y) r = match n with
         |n when n >=  0 -> draw_circle_float x y r;
                           chap_draw_cercle (n-1) ((x-.r/.2.), y) (r/.2.);
                           chap_draw_cercle (n-1) ((x+.r/.2.), y) (r/.2.)
         |_ -> draw_line_float (x, y) (x, y)
       in clear_graph(); chap_draw_cercle n
                           ((float_of_int(x)), float_of_int(y))
                           (float_of_int(r)) ;;

draw_cercle 18 (800, 450) 400;;
