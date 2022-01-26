open Ocamlbuild_plugin
 
(*
   Let subdirectories see modules declared in other subdirectories

   For example, 
   
   | After_rules ->
      Pathname.define_context "src/B" ["src/A"]
      Pathname.define_context "src/C" ["src/A" ; "src/B"];

   
   lets B see modules from A and C see modules from A and B
*)
let () =
  dispatch begin function
  | After_rules ->
     Pathname.define_context "" [] ; (* Your modules go here *)
  | _ -> ()
  end