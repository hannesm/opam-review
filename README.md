# opam-review

Reviews a submitted opam file, and takes the [opam-repository policies](https://github.com/ocaml/opam-repository/tree/master/governance/policies) into account.

The goal is to be precise and picky about the opam-repository policies. At the same time, years of observing the opam-repository and what goes wrong, fuel the design of this application. The approach taken is to rely on a git repository to be able to see the repository in an earlier state (i.e. when modifying an opam file, or renaming) to evaluate some best practises. The "what changed" tracking is implemented by taking a commit and a HEAD commit into account, and manually parsing the output of "git diff --name-status".

The checks implemented are not complete. Also, "OpamFileTools.lint_file" is used (while some warnings and errors are ignored) to complete the verdict.

## License

It is licensed under the AGPL-3.0 or later.

## AI

Contributions involving AI must comply with the rules set out [here](./AI.md).
If they do not comply, we reserve the right to reject your contribution.
