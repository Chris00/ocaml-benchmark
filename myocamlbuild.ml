open Ocamlbuild_plugin
open Command
;;

dispatch begin function
| After_rules ->
    ocaml_lib "benchmark";

    flag ["compile"; "ocaml"; "use_pcre"] (S[A"-I"; A"+pcre"]);
    flag ["link"; "program"; "ocaml"; "byte"; "use_pcre"] (S[A"pcre.cma"]);
    flag ["link"; "program"; "ocaml"; "native"; "use_pcre"] (S[A"pcre.cmxa"]);

    let examples =
      Array.fold_right begin fun f acc ->
        if Pathname.get_extension f = "ml" then
          ("examples" / Pathname.update_extension "native" f) :: acc
        else
          acc
      end (Pathname.readdir "examples") [] in
    rule "All examples"
      ~prod:"examples.otarget"
      ~deps:examples
      (fun _ _ -> Nop);

| _ -> ()
end
