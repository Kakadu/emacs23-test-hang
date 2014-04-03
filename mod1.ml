open Js
open Dom_html
open Firebug

let map_width = 10
let map_height = 6

class type _player = object
  method x : float prop
  method y : float prop
end

class type g = object
  method name: Js.js_string Js.t writeonly_prop
  method camlInitMap: _player Js.t -> unit Js.js_array Js.t meth
end
class type unitDesc = object
  method sort: js_string t prop
  method hp: float prop
end

let make_archer hp =
  let archer1 : unitDesc Js.t = Js.Unsafe.obj [||] in
  archer1##sort <- Js.string "Archer";
  archer1##hp <- hp;
  archer1

let make_warrior hp =
  let archer1 : unitDesc Js.t = Js.Unsafe.obj [||] in
  archer1##sort <- Js.string "Warrior";
  archer1##hp <- hp;
  archer1

class type cellDesc = object
  method color: js_string t writeonly_prop
  method city:  js_string t writeonly_prop
  method unit:  unitDesc t prop
end

let initMap player =
  console##log (Js.string "initMap");
  let n = map_width * map_height in
  let arr : cellDesc Js.t array = Array.init n (fun _ ->
      let ans = Js.Unsafe.obj [||] in
      ans##color <- Js.string "#abcdef";
      ans
    ) in

  let at x y = arr.(y*map_width + x) in

  let warrior1 = make_warrior 20. in

  let archer1 = make_archer 20. in

  (at 0 1)##city <- Js.string "5";
  (at 0 2)##unit <- warrior1;
  (at 0 0)##unit <- archer1;
  Js.array arr

let repaint (ctx: canvasRenderingContext2D Js.t)  =
  ()

let () =
  console##log (Js.string "testing logging");
  let g: g Js.t = Js.Unsafe.variable "Global.global" in
  let mapData: _ Js.t = Js.Unsafe.variable  "MapData" in
  mapData##mapWidth  <- Js.string @@ string_of_int map_width;
  mapData##mapHeight <- Js.string @@ string_of_int map_height;
  g##name <- Js.string "";

  (Js.Unsafe.coerce g)##camlInitMap <- Js.wrap_callback initMap;
  (Js.Unsafe.coerce g)##repaint <- Js.wrap_callback repaint;
  ()
