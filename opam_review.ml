(* Copyright 2026 Hannes Mehnert, all rights reserved *)

(* goals and non-goals:
   - be fast!
   - (strictly) follow the opam-repository governance policies
   - easy to extend
   - separation between "blocker" and "warning"
   - ability to output suggestions for use in GitHub PRs
   - do not compile and test code (no need to be aware of various platforms)
*)

(* checks that could be useful (from policy)
   - 0. meet the criteria for inclusion
   - 1. removal of packages should be avoided
   - 2. packages should be installable (here, dependencies should be present)
   - 3. provide some sort of utility for users (unclear)
   - 4. namesquatting (unclear)
   - 5. strict dependencies should be avoided (=, >, upper bounds)
        -> maybe = apart from = version
   - (done) 6. version numbers should not have "v" prefix
     -- but if all other versions have a 'v' prefix, that's fine
   - (done) 7. new packages should avoid the "ocaml" prefix/suffix (also, msys2-, arch-, host-arch, host-system, system-)
   - 8. development packages (pointing to a git branch) are to be avoided
   - 9. packages should only depend on packages they need for build or runtime (unclear)
   - 10. binary packages should be avoided (unclear how to test)
   - 11. changes to a package's source archive are prohibited
   - (done) 12. extra-files are prohibited
   - (done) 13. weak hash algorithms are prohibited

   bug reports / feature wishes:
   - (done) ensure hex-encoded checksums are actually hex - and the right length
   - (done) x-maintenance-intent field is defined (if not warning) and well-formed (if not, error) -- also report if it changed (or got removed) from an earlier version
   - (done) run opam lint (OpamFileTools.lint_file) - with check_upstream = true
   - download archive(s) and check checksum -- use the "mirrors" as well
   - if license/url is changed (taking an earlier version of package), warn
   - if the package is new, and a package with the same name exists in the archive repository, warn!
   - (done) pre-release should have the avoid-version flag; while normal releases shouldn't
   - check repo file #26065 / https://opam.ocaml.org/doc/Manual.html#repo
   - warn if ocaml lower bound is less than supported ocaml version (indicate that versions aren't checked by CI)
   - insane dependencies (2 = 1, 1 & false, etc.)
   - ensure other packages are installable after this change [esp. for renamed and removed]
   - (done) ensure a license which complies with SPDX
   - (done) ensure appropriate line endings (#30223)
   - avoid duplicate fields (such as "available")
   - check deps for "v" version prefix (see jan midgaards PRs recently)

   done:
   - ensure there's no pin-depends
   - ensure there's no "name" field (if present, warn, if different from the packages/NAME/, error)
   - ensure there's no "version" field (if present, warn, if different from the packages/NAME/NAME.VERSION, error)
*)

let ( let* ) = Result.bind

type t =
  | Added of { filename : string ; content : string ; package : string ; version : string ; versions : string list }
  | Modified of { filename : string ; old_content : string ; new_content : string ; package : string ; version : string ; versions : string list }
  | Deleted of { filename : string ; package : string ; version : string ; old_content : string }
  | Renamed of { old_filename : string ; new_filename : string ; content : string ; new_package : string ; new_version : string ; versions : string list }

let get_file commit name =
  let version = commit ^ ":" ^ name in
  let cmd = Bos.Cmd.(v "git" % "show" % version) in
  Bos.OS.Cmd.(run_out ~err:err_null cmd |> out_string |> success)

let versions commit package =
  match get_file commit ("packages/" ^ package) with
  | Error _ -> []
  | Ok data ->
    (* output is "tree <>"\n\nfiles *)
    match String.split_on_char '\n' data with
    | _tree :: packages ->
      let plen = String.length package in
      List.fold_left (fun acc v ->
          if String.length v = 0 then
            acc
          else if String.starts_with ~prefix:package v && String.length v > plen then
            String.sub v (plen + 1) (String.length v - plen - 2) :: acc
          else begin
            Logs.warn (fun m -> m "package %s has a strange directory %s (during git show %s:packages/%s)"
                          package v commit package);
            acc
          end)
        []
        packages
    | _ ->
      Logs.warn (fun m -> m "couldn't parse git show output for %s: %s" package data);
      []

let check_path data =
  match String.split_on_char '/' data with
  | [ "packages" ; pkg ; pkg_ver ; "opam" ] ->
    let pkg' = List.hd (String.split_on_char '.' pkg_ver) in
    if String.equal pkg pkg' then
      let ver = String.sub pkg_ver (String.length pkg + 1) (String.length pkg_ver - String.length pkg - 1) in
      Ok (pkg, ver)
    else begin
      Logs.err (fun m -> m "path violation, package name (%S) is not a prefix of package name with version (%S) - or the latter doesn't include a ."
                   pkg pkg_ver);
      Error ()
    end
  | _ ->
    Logs.err (fun m -> m "path violation, opam files should reside in packages/<pkg-name>/<pkg-name.version>/opam, but the path is %S" data);
    Error ()

let hex_size = function
  | `MD5 -> 32
  | `SHA256 -> 64
  | `SHA512 -> 128

let check_hash_checksum package url =
  let check_one_hash url hash =
    let content = OpamHash.contents hash in
    (try ignore (Ohex.decode content) with
     | Invalid_argument a ->
       Logs.err (fun m -> m "package %s url %s invalid hex (%s) for checksum %s" package url a content));
    let kind = OpamHash.kind hash in
    (if String.length content <> hex_size kind then
       Logs.err (fun m -> m "package %s url %s has an invalid hex size %u, expected %u"
                    package url (String.length content) (hex_size kind)));
    kind = `MD5
  in
  let url_str = OpamUrl.to_string (OpamFile.URL.url url) in
  match OpamFile.URL.checksum url with
  | [] -> Logs.err (fun m -> m "package %s missing checksum for %s" package url_str)
  | xs ->
    let weak =
      List.fold_left (fun weak hash ->
          let weak' = check_one_hash url_str hash in
          weak && weak')
        false xs
    in
    if weak then
      Logs.err (fun m -> m "weak hash (md5) for %s" url_str)

let decode_intent s =
  let is_numeric s =
    let n = function '0'..'9' -> true | _ -> false in
    String.for_all n s
  and base_good = function
    | "(any)" | "(none)" | "(latest)" -> true
    | s when String.starts_with ~prefix:"(latest-" s -> true
    | _ -> false
  in
  match s with
  | s when base_good s -> true
  | s ->
    match String.split_on_char '.' s with
    (* first may be vX, others being X only *)
    | [] -> assert false
    | v0 :: versions ->
      (is_numeric v0 ||
       (if String.starts_with ~prefix:"v" v0 then is_numeric (String.sub v0 1 (String.length v0 - 1)) else false) ||
       base_good v0) &&
      List.for_all (fun v -> is_numeric v || base_good v) versions

let may_avoid_version package version versions =
  if String.equal "ocaml-variants" package then
    String.ends_with ~suffix:"+trunk" version
  else if String.equal "ocaml-system" package || String.equal "ocaml" package then
    begin
      (* version is latest patch *)
      try
        let prefix, patch =
          match String.split_on_char '.' version with
          | [ major ; minor ; patch ] -> major ^ "." ^ minor ^ ".", int_of_string patch
          | _ -> failwith "expected major.minor.patch"
        in
        let other_versions =
          List.filter (String.starts_with ~prefix) versions
        in
        let other_patch =
          List.map (fun v ->
              let patch =
                let last_dot = String.rindex v '.' in
                String.sub v (last_dot + 1) (String.length v - last_dot - 1)
              in
              int_of_string patch)
            other_versions
        in
        List.for_all (fun p -> Int.compare patch p = 1) other_patch
      with
        Failure f ->
        Logs.warn (fun m -> m "package %s version %s parse error %s" package version f);
        false
    end
  else
    false

let check_opam_data filename pkg version versions opam =
  if OpamFile.OPAM.pin_depends opam <> [] then
    Logs.err (fun m -> m "opam file %s contains 'pin-depends'" filename);
  (match OpamFile.OPAM.name_opt opam with
   | None -> ()
   | Some name ->
     let pkg = List.nth (String.split_on_char '/' filename) 1 in
     let name' = OpamPackage.Name.to_string name in
     if String.equal pkg name' then
       Logs.debug (fun m -> m "opam file %s contains a 'name' field %s" filename name')
     else
       Logs.err (fun m -> m "opam file %s contains a 'name' field %s, and this does not match the path %s" filename name' pkg));
  (match OpamFile.OPAM.version_opt opam with
   | None -> ()
   | Some ver ->
     let ver' = OpamPackage.Version.to_string ver in
     let ver_pkg =
       String.concat "." (List.tl (String.split_on_char '.' (List.nth (String.split_on_char '/' filename) 2))) in
     if String.equal ver_pkg ver' then
       Logs.debug (fun m -> m "opam file %s contains a 'version' field %s" filename ver')
     else
       Logs.err (fun m -> m "opam file %s contains a 'version' field %s, and this does not match the path %s" filename ver' ver_pkg));
  (match OpamFile.OPAM.extra_files opam with
   | None -> ()
   | Some _ -> Logs.err (fun m -> m "opam file %s contains extra_files" filename));
  (match OpamFile.OPAM.url opam with
   | None -> ()
   | Some u -> check_hash_checksum filename u);
  (let extras = OpamFile.OPAM.extra_sources opam in
   List.iter (fun (_, url) -> check_hash_checksum filename url) extras);
  (match OpamFile.OPAM.extended opam "x-maintenance-intent" Fun.id with
   | None -> ()
   | Some { pelem = List { pelem = xs ; _ } ; _ } ->
     let extract_string = function
       | { OpamParserTypes.FullPos.pelem = OpamParserTypes.FullPos.String s ; _ } ->
         if not (decode_intent s) then
           Logs.err (fun m -> m "opam file %s couldn't decode x-maintenance-intent %S"
                        filename s)
       | x ->
         Logs.warn (fun m -> m "opam file %s x-maintenance-intent failure: expected a string, got %s"
                       filename (OpamPrinter.FullPos.value x))
     in
     List.iter extract_string xs
   | Some x ->
     Logs.warn (fun m -> m "opam file %s x-maintenance-intent failure: expected a list of strings, got %s"
                   filename (OpamPrinter.FullPos.value x));
     ());
  (match OpamFile.OPAM.license opam with
   | [] -> Logs.err (fun m -> m "opam file %s missing license" filename)
   | ls ->
     List.iter (fun l ->
         match Spdx_licenses.parse l with
         | Ok _ -> ()
         | Error `InvalidLicenseID id ->
           Logs.err (fun m -> m "opam file %s invalid license ID %S" filename id)
         | Error `InvalidExceptionID id ->
           Logs.err (fun m -> m "opam file %s invalid exception ID %S" filename id)
         | Error `ParseError ->
           Logs.err (fun m -> m "opam file %s license parse error %S" filename l))
       ls);
  (if List.mem OpamTypes.Pkgflag_AvoidVersion (OpamFile.OPAM.flags opam) then
     if may_avoid_version pkg version versions then () else
     match String.index_opt version '~' with
     | None -> Logs.warn (fun m -> m "opam file %s with avoid version, but version does not include ~"
                             filename)
     | _ -> ())


let parse_and_check_opam dirname file pkg version versions data =
  (match String.index_opt data '\r' with
   | None -> ()
   | Some _ -> Logs.err (fun m -> m "opam file %s contains carriage return" file));
  let base = OpamFilename.Base.of_string file in
  let filename = OpamFile.make (OpamFilename.create dirname base) in
  match OpamFile.OPAM.read_from_string ~filename data with
  | exception e ->
    Logs.err (fun m -> m "error while parsing opam file %s: %s" file
                 (Printexc.to_string e))
  | opam ->
    List.iter (function
        | (25|35|36|37|46|47|62|68), _, _ -> () (* ignore missing authors, homepage, bug-reports, dev-repo, 'conf flagged', synopsis (start, end), SPDX-license, license *)
        | id, `Error, err -> Logs.err (fun m -> m "opam file %s lint error %u: %s" file id err)
        | id, `Warning, err -> Logs.warn (fun m -> m "opam file %s lint warning %u: %s" file id err))
      (OpamFileTools.lint opam);
    check_opam_data file pkg version versions opam

let find_changed_opams commit head =
  let cmd = Bos.Cmd.(v "git" % "diff" % "--name-status" % commit % head) in
  let* pkgs = Bos.OS.Cmd.(run_out cmd |> out_lines |> success) in
  let pkgs =
    List.fold_left (fun acc pkg ->
        let action, pkg_name =
          String.get pkg 0,
          String.concat "" (List.tl (String.split_on_char '\t' pkg))
        in
        if String.ends_with ~suffix:"opam" pkg_name then
          match
            match action with
            | 'A' ->
              let* pkg, ver = check_path pkg_name in
              Ok (`Added (pkg_name, pkg, ver))
            | 'M' ->
              let* pkg, ver = check_path pkg_name in
              Ok (`Modified (pkg_name, pkg, ver))
            | 'D' ->
              let* pkg, ver = check_path pkg_name in
              Ok (`Deleted (pkg_name, pkg, ver))
            | 'R' ->
              begin match List.rev (String.split_on_char '\t' pkg) with
                | new_name :: old_name :: _ ->
                  let* pkg, ver = check_path old_name in
                  let* pkg', ver' = check_path new_name in
                  Ok (`Renamed ((old_name, pkg, ver), (new_name, pkg', ver')))
                | _ ->
                  Logs.warn (fun m -> m "cannot figure out renamed package %s" pkg);
                  Error ()
              end
            | x ->
              Logs.app (fun m -> m "unsupported action %c on file %s" x pkg_name);
              Error ()
          with
          | Ok x -> x :: acc
          | Error () -> acc
        else if String.starts_with ~prefix:"packages" pkg_name then begin
          Logs.err (fun m -> m "changed file %s is not an opam file, so it shouldn't be in the packages directory" pkg_name);
          acc
        end else begin
          Logs.app (fun m -> m "ignoring changed file %s (not an opam file)" pkg_name);
          acc
        end)
      [] pkgs
  in
  List.fold_left (fun acc d ->
      let* acc in
      match d with
      | `Added (filename, package, version) ->
        let* content = get_file head filename in
        let versions = versions commit package in
        Ok (Added { filename ; content ; package ; version ; versions } :: acc)
      | `Modified (filename, package, version) ->
        let* new_content = get_file head filename in
        let* old_content = get_file commit filename in
        let versions = versions commit package in
        Ok (Modified { filename ; new_content ; old_content ; package ; version ; versions } :: acc)
      | `Deleted (filename, package, version) ->
        let* old_content = get_file commit filename in
        Ok (Deleted { filename ; old_content ; package ; version } :: acc)
      | `Renamed ((old_filename, _old_package, _old_version), (new_filename, new_package, new_version)) ->
        let* content = get_file head new_filename in
        let versions = versions commit new_package in
        Ok (Renamed { old_filename ; new_filename ; content ; new_package ; new_version ; versions } :: acc))
    (Ok []) pkgs

let check_versions filename version existing_ones =
  if List.exists (fun v -> String.get v 0 = 'v') existing_ones then
    if not (String.get version 0 = 'v') then
      Logs.err (fun m -> m "opam file %s does not have a 'v' prefix in the version %s, other releases have"
                   filename version)
    else
      ()
  else
    if String.get version 0 = 'v' then
      Logs.err (fun m -> m "opam file %s does have a 'v' prefix in the version %s, other releases have no such prefix"
                   filename version)

let check_name filename pkg =
  List.iter (fun prefix ->
      if String.starts_with ~prefix pkg then
        Logs.warn (fun m -> m "NEW opam file %s should avoid %s prefix" filename prefix))
    [  "ocaml" ; "msys2" ; "arch" ; "host-arch" ; "host-system" ; "system" ];
  if String.ends_with ~suffix:"ocaml" pkg then
    Logs.warn (fun m -> m "NEW opam file %s should avoid ocaml suffix" filename)


let jump () opam_repo commit head =
  let* () = Bos.OS.Dir.set_current (Fpath.v opam_repo) in
  let* opams = find_changed_opams commit head in
  let opam_repo = OpamFilename.Dir.of_string opam_repo in
  List.iter (function
      | Added { filename ; package ; content ; version ; versions ; _ } ->
        Logs.info (fun m -> m "added %s (version %s, versions %a)" filename version Fmt.(list ~sep:(any ", ") string) versions);
        parse_and_check_opam opam_repo filename package version versions content;
        check_versions filename version versions;
        if versions = [] then check_name filenam package
      | Modified { filename ; new_content ; package ; version ; versions ; _ (* old_content *) } ->
        Logs.info (fun m -> m "modified %s" filename);
        parse_and_check_opam opam_repo filename package version versions new_content;
      | Deleted { filename ; _ } (* old_content *) ->
        Logs.info (fun m -> m "deleted %s" filename);
      | Renamed { new_filename ; content ; old_filename ; new_package ; new_version ; versions } ->
        Logs.info (fun m -> m "renamed %s -> %s" old_filename new_filename);
        parse_and_check_opam opam_repo new_filename new_package new_version versions content;
        check_versions new_filename new_version versions;
        if versions = [] then check_name new_filename new_package)
    opams;
(*  let all_opams =
    lazy
      (OpamPackage.keys
         (OpamRepositoryState.load_opams_from_dir
            (OpamRepositoryName.of_string "temporary")
            (OpamFilename.Dir.of_string opam_repository)))
  in
*)
  Ok ()

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ~dst:Format.std_formatter ())

open Cmdliner

let setup_log =
  Term.(const setup_log
        $ Fmt_cli.style_renderer ()
        $ Logs_cli.level ())

let opam_repository =
  let doc = "Opam repository directory to work on" in
  Arg.(value & opt dir "." & info ~doc ["opam-repository"])

let commit =
  let doc = "Handle the given commit." in
  Arg.(value & opt string "HEAD^1" & info ~doc ["commit"])

let head =
  let doc = "Compare against the commit." in
  Arg.(value & opt string "HEAD" & info ~doc ["head"])

let cmd =
  let info = Cmd.info "opam-review" ~version:"%%VERSION_NUM%%"
  and term =
    Term.(term_result (const jump $ setup_log $ opam_repository $ commit $ head))
  in
  Cmd.v info term

let () = exit (Cmd.eval cmd)
