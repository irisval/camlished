(* TODO: Based on https://flafla2.github.io/2014/08/09/perlinnoise.html *)

(**[lerp a b x] is linear interpolation. *)
let lerp a b x = a +. x*.(b-.a)

let perm = Array.of_list [
    151;160;137;91;90;15;            
    131;13;201;95;96;53;194;233;7;225;140;36;103;30;69;142;8;99;37;240;21;10;23;
    190; 6;148;247;120;234;75;0;26;197;62;94;252;219;203;117;35;11;32;57;177;33;
    88;237;149;56;87;174;20;125;136;171;168; 68;175;74;165;71;134;139;48;27;166;
    77;146;158;231;83;111;229;122;60;211;133;230;220;105;92;41;55;46;245;40;244;
    102;143;54; 65;25;63;161; 1;216;80;73;209;76;132;187;208; 89;18;169;200;196;
    135;130;116;188;159;86;164;100;109;198;173;186; 3;64;52;217;226;250;124;123;
    5;202;38;147;118;126;255;82;85;212;207;206;59;227;47;16;58;17;182;189;28;42;
    223;183;170;213;119;248;152; 2;44;154;163; 70;221;153;101;155;167; 43;172;9;
    129;22;39;253; 19;98;108;110;79;113;224;232;178;185; 112;104;218;246;97;228;
    251;34;242;193;238;210;144;12;191;179;162;241; 81;51;145;235;249;14;239;107;
    49;192;214; 31;181;199;106;157;184; 84;204;176;115;121;50;45;127; 4;150;254;
    138;236;205;93;222;114;67;29;24;72;243;141;128;195;78;66;215;61;156;180]
let p = Array.append perm perm

let fade t = t *. t *. t *. (t *. (t *. 6. -. 15.) +. 10.)

let inc i = i+1 mod 255

let grad hash x y = match hash mod 4 with 
  | 0 -> x
  | 1 -> -.x
  | 2 -> y
  | 3 -> -.y
  | _ -> failwith ("impossible "^(hash mod 4 |> string_of_int))

let perlin x y =
  let x =  mod_float x 255. in
  let y = mod_float y 255. in
  let xi = (int_of_float x) mod 255 in
  let yi = (int_of_float y) mod 255 in
  let xf = x-.(float_of_int (int_of_float x)) in
  let yf = y-.(float_of_int (int_of_float y)) in
  let u = fade xf in
  let v = fade yf in

  let aa = (xi |> Array.get p)+yi |> Array.get p in
  let ba = (inc xi |> Array.get p)+yi |> Array.get p in
  let ab = (xi |> Array.get p)+(inc yi) |> Array.get p in
  let bb = (inc xi |> Array.get p)+(inc yi) |> Array.get p in

  let y1 = lerp (grad aa xf yf) (grad ba (xf-. 1.) yf) u in
  let y2 = lerp (grad ab xf (yf-.1.)) (grad bb (xf-. 1.) (yf-. 1.)) u in
  ((lerp y1 y2 v)+. 1.) /. 2.