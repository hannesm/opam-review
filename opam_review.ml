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
   - (done) 11. changes to a package's source archive are prohibited
     - also compare that build/install/remove didn't change
   - (done) 12. extra-files are prohibited
   - (done) 13. weak hash algorithms are prohibited

   bug reports / feature wishes:
   - (done) ensure hex-encoded checksums are actually hex - and the right length (#30118)
   - (done) x-maintenance-intent field is defined (if not warning) and well-formed (if not, error)
   - report if x-maintenance-intent changed (or got removed) from an earlier version
   - (done) run opam lint (OpamFileTools.lint_file) - with check_upstream = true
   - download archive(s) and check checksum -- use the "mirrors" as well
   - if license/url is changed (taking an earlier version of package), warn
   - if the package is new, and a package with the same name exists in the archive repository, warn!
   - (done) pre-release should have the avoid-version flag; while normal releases shouldn't
   - (done) check repo file #26065 / https://opam.ocaml.org/doc/Manual.html#repo
   - warn if ocaml lower bound is less than supported ocaml version (indicate that versions aren't checked by CI)
   - ensure other packages are installable after this change [esp. for renamed and removed]
   - (done) ensure a license which complies with SPDX
   - (done) ensure appropriate line endings (#30223)
   - avoid duplicate fields (such as "available")
   - dependencies:
     - insane dependencies (2 = 1, 1 & false, etc.)
     - check deps for "v" version prefix (see jan midgaards PRs recently - such a version must exist (i.e. bin_prot >= "0.17.0" does not exist)
     - check for duplicate depends (i.e. "dune" {...} "dune" {...})
     - check for duplicate bounds in depends (i.e. "dune" {>= .. & >= ..})

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
  | Repo of { content : string }

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

let check_hash_checksum url =
  let check_one_hash url hash =
    let content = OpamHash.contents hash in
    let acc =
      try ignore (Ohex.decode content); [] with
      | Invalid_argument a ->
        [ `Error, Fmt.str "Invalid hex (error: %s) for checksum %s" a content ]
    in
    let kind = OpamHash.kind hash in
    let acc =
      if String.length content <> hex_size kind then
        (`Error, Fmt.str "Invalid hex length for %s, expected %u got %u" url (hex_size kind) (String.length content)) :: acc
      else
        acc
    in
    kind = `MD5, acc
  in
  let url_str = OpamUrl.to_string (OpamFile.URL.url url) in
  match OpamFile.URL.checksum url with
  | [] -> [ `Error, Fmt.str "missing checksum for %s" url_str ]
  | xs ->
    let weak, acc =
      List.fold_left (fun (weak, acc) hash ->
          let weak', w = check_one_hash url_str hash in
          weak && weak', w @ acc)
        (false, []) xs
    in
    if weak then
      (`Error, Fmt.str "Only a weak hash was provided for %s" url_str) :: acc
    else
      acc

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
  let acc =
    if OpamFile.OPAM.pin_depends opam <> [] then
      [ `Error, "Contains pin-depends" ]
    else
      []
  in
  let acc =
    match OpamFile.OPAM.name_opt opam with
    | None -> acc
    | Some name ->
      let pkg = List.nth (String.split_on_char '/' filename) 1 in
      let name' = OpamPackage.Name.to_string name in
      if String.equal pkg name' then
        acc
      else
        (`Error, Fmt.str "'name: %s' doesn't match path %s" name' pkg) :: acc
  in
  let acc =
    match OpamFile.OPAM.version_opt opam with
    | None -> acc
    | Some ver ->
      let ver' = OpamPackage.Version.to_string ver in
      let ver_pkg =
        String.concat "." (List.tl (String.split_on_char '.' (List.nth (String.split_on_char '/' filename) 2))) in
      if String.equal ver_pkg ver' then
        acc
      else
        (`Error, Fmt.str "'version: %s' doesn't match path %s" ver' ver_pkg) :: acc
  in
  let acc =
    match OpamFile.OPAM.extra_files opam with
    | None -> acc
    | Some _ -> (`Error, "Contains extra-files") :: acc
  in
  let acc =
    match OpamFile.OPAM.url opam with
    | None -> acc
    | Some u -> check_hash_checksum u @ acc
  in
  let acc =
    let extras = OpamFile.OPAM.extra_sources opam in
    List.fold_left (fun acc (_, url) -> check_hash_checksum url @ acc) acc extras
  in
  let acc =
    match OpamFile.OPAM.extended opam "x-maintenance-intent" Fun.id with
    | None -> (`Warning, "Missing x-maintenance-intent field") :: acc
    | Some { pelem = List { pelem = xs ; _ } ; _ } ->
      let extract_string = function
        | { OpamParserTypes.FullPos.pelem = OpamParserTypes.FullPos.String s ; _ } ->
          if decode_intent s then
            None
          else
            Some (`Error, Fmt.str "Decoding x-maintenance-intent %s failed" s)
        | x ->
          Some (`Error, Fmt.str "Expected a string, found %s while decoding x-maintenance-intent" (OpamPrinter.FullPos.value x))
      in
      (List.filter_map extract_string xs) @ acc
    | Some x ->
      (`Error, Fmt.str "expected a list of strings, got %s" (OpamPrinter.FullPos.value x)) :: acc
  in
  let acc =
    match OpamFile.OPAM.license opam with
    | [] -> (`Error, "Missing license") :: acc
    | ls ->
      List.filter_map (fun l ->
          match Spdx_licenses.parse l with
          | Ok _ -> None
          | Error `InvalidLicenseID id ->
            Some (`Error, Fmt.str "Invalid license ID %S" id)
          | Error `InvalidExceptionID id ->
            Some (`Error, Fmt.str "Invalid exception ID %S" id)
          | Error `ParseError ->
            Some (`Error, Fmt.str "License parse error %S" l))
        ls @ acc
  in
  if List.mem OpamTypes.Pkgflag_AvoidVersion (OpamFile.OPAM.flags opam) then
    if may_avoid_version pkg version versions then
      acc
    else
      match String.index_opt version '~' with
      | None -> (`Warning, "Contains 'avoid-version', but version does not include '~'") :: acc
      | _ -> acc
  else
    acc

let parse_and_check_opam dirname file pkg version versions data =
  let acc =
    match String.find_first ~sub:"\r\n" data with
    | None -> []
    | Some pos -> [ `Error, Fmt.str "Contains a carriage return newline at offset %u" pos ]
  in
  let base = OpamFilename.Base.of_string file in
  let filename = OpamFile.make (OpamFilename.create dirname base) in
  match OpamFile.OPAM.read_from_string ~filename data with
  | exception e -> [ `Error, Fmt.str "Parse error: %s" (Printexc.to_string e) ]
  | opam ->
    let acc' = check_opam_data file pkg version versions opam in
    acc @ acc' @
    List.filter_map (function
        | (25|35|36|37|46|47|62|68), _, _ -> None (* ignore missing authors, homepage, bug-reports, dev-repo, 'conf flagged', synopsis (start, end), SPDX-license, license *)
        | id, `Error, err -> Some (`Error, Fmt.str "opam lint error: %u %s" id err)
        | id, `Warning, err -> Some (`Warning, Fmt.str "opam lint warning: %u %s" id err))
      (OpamFileTools.lint opam)

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
        end else if String.equal pkg_name "repo" then
          `Repo :: acc
        else begin
          Logs.warn (fun m -> m "ignoring changed file %s (not an opam file)" pkg_name);
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
        Ok (Renamed { old_filename ; new_filename ; content ; new_package ; new_version ; versions } :: acc)
      | `Repo ->
        let* content = get_file head "repo" in
        Ok (Repo { content } :: acc)
    )
    (Ok []) pkgs

let check_versions version existing_ones =
  if List.exists (fun v -> String.get v 0 = 'v') existing_ones then
    if not (String.get version 0 = 'v') then
      [ `Error, Fmt.str "Version does not have a 'v' prefix %s, while other releases have" version ]
    else
      []
  else
    if String.get version 0 = 'v' then
      [ `Error, Fmt.str "Version does have a 'v' prefix %s, while other releases have no such prefix" version ]
    else
      []

let check_name pkg =
  let acc =
    List.filter_map (fun prefix ->
        if String.starts_with ~prefix pkg then
          Some (`Warning, Fmt.str "Should avoid %s prefix" prefix)
        else
          None)
      [ "ocaml" ; "msys2" ; "arch" ; "host-arch" ; "host-system" ; "system" ]
  in
  if String.ends_with ~suffix:"ocaml" pkg then
    (`Warning, "Should avoid ocaml suffix") :: acc
  else
    acc

let changes_are_fine dirname file old_content new_content =
  let check_url old_url new_url =
    let new_checksums_are_superset =
      List.for_all (fun hash ->
          List.exists (fun h -> OpamHash.equal hash h) (OpamFile.URL.checksum new_url))
        (OpamFile.URL.checksum old_url)
    in
    if new_checksums_are_superset then
      []
    else
      [ `Error, Fmt.str "Not all checksums of %s are contained in the modified file" (OpamUrl.to_string (OpamFile.URL.url new_url)) ]
  in
  let base = OpamFilename.Base.of_string file in
  let filename = OpamFile.make (OpamFilename.create dirname base) in
  match OpamFile.OPAM.read_from_string ~filename old_content,
        OpamFile.OPAM.read_from_string ~filename new_content with
  | exception _ -> []
  | old_opam, new_opam ->
    let acc =
      match OpamFile.OPAM.url old_opam, OpamFile.OPAM.url new_opam with
      | Some old_url, Some new_url -> check_url old_url new_url
      | _ -> []
    in
    let old_extra, new_extra = OpamFile.OPAM.extra_sources old_opam, OpamFile.OPAM.extra_sources new_opam in
    let acc' =
      if List.length old_extra = List.length new_extra then
        List.fold_left (fun acc (name, url) ->
            let exists, acc =
              List.fold_left (fun (exists, acc) (name', url') ->
                  if OpamFilename.Base.equal name name' then
                    (true, (check_url url url') @ acc)
                  else
                    (exists, acc))
                (false, acc) new_extra
            in
            if exists then
              acc
            else
              (`Error, Fmt.str "extra-sources %s does not exist anymore"
                 (OpamFilename.Base.to_string name)) :: acc)
          [] old_extra
      else
          [ `Error, Fmt.str "extra-sources have been altered" ]
    in
    acc @ acc'

let check_repo dirname content =
  let base = OpamFilename.Base.of_string "repo" in
  let filename = OpamFile.make (OpamFilename.create dirname base) in
  match OpamFile.Repo.read_from_string ~filename content with
  | exception e -> [ `Error, Fmt.str "Failed to parse: %s" (Printexc.to_string e) ]
  | _ -> []

let jump () opam_repo commit head =
  let* () = Bos.OS.Dir.set_current (Fpath.v opam_repo) in
  let* opams = find_changed_opams commit head in
  let opam_repo = OpamFilename.Dir.of_string opam_repo in
  let warnings =
    List.map (function
        | Added { filename ; package ; content ; version ; versions ; _ } ->
          let acc = parse_and_check_opam opam_repo filename package version versions content in
          let acc = check_versions version versions @ acc in
          let acc = if versions = [] then check_name package @ acc else acc in
          (filename, acc)
        | Modified { filename ; new_content ; package ; version ; versions ; old_content } ->
          let acc = parse_and_check_opam opam_repo filename package version versions new_content in
          let acc = changes_are_fine opam_repo filename old_content new_content @ acc in
          (filename, acc)
        | Deleted { filename ; _ } (* old_content *) ->
          (filename, [ `Warning, "Deleted" ])
        | Renamed { new_filename ; content ; new_package ; new_version ; versions ; _ } ->
          let acc = parse_and_check_opam opam_repo new_filename new_package new_version versions content in
          let acc = check_versions new_version versions @ acc in
          let acc = if versions = [] then check_name new_package @ acc else acc in
          (new_filename, acc)
        | Repo { content } ->
          let acc = check_repo opam_repo content in
          ("repo", acc))
      opams
  in
  List.iter (fun (filename, warnings) ->
      if warnings = [] then
        Fmt.pr "✅ %s@.@." filename
      else
        let warnings = List.sort (fun (lvl, msg) (lvl', msg') ->
            match lvl, lvl' with
            | `Warning, `Warning | `Error, `Error -> String.compare msg msg'
            | `Error, `Warning -> -1 | `Warning, `Error -> 1)
            warnings
        in
        Fmt.pr "%s:@." filename;
        List.iter (fun (lvl, msg) ->
            let symbol = match lvl with
              | `Warning -> "⚠️"
              | `Error -> "❌"
            in
            Fmt.pr "- %s %s@." symbol msg)
          warnings;
        Fmt.pr "@.")
      warnings;
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
