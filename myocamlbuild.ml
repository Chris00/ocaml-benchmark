open Ocamlbuild_plugin
open Command
;;

Options.make_links := false;;

dispatch begin function
| After_rules ->
    ocaml_lib "benchmark";

    flag ["compile"; "ocaml"; "use_pcre"] (S[A"-I"; A"+pcre"]);
    flag ["link"; "program"; "ocaml"; "byte"; "use_pcre"] (S[A"pcre.cma"]);
    flag ["link"; "program"; "ocaml"; "native"; "use_pcre"] (S[A"pcre.cmxa"]);

    let examples_rule ext =
      let examples =
        Array.fold_right begin fun f acc ->
          if Pathname.get_extension f = "ml" then
            ("examples" / Pathname.update_extension ext f) :: acc
          else
            acc
        end (Pathname.readdir "examples") [] in
      rule ("All examples " ^ ext)
        ~prod:("examples." ^ ext)
        ~deps:examples
        (fun _ _ -> Nop) in

    examples_rule "byte";
    examples_rule "native";

| _ -> ()
end
