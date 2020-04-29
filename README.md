[![](https://github.com/jeremyschlatter/chime/workflows/CI/badge.svg)](https://github.com/jeremyschlatter/chime/actions?query=workflow%3ACI)

# chime

An interpreter for Paul Graham's [Bel](http://paulgraham.com/bel.html) programming language.

[Try it out in your browser](https://bel-repl.com).

To run it locally, download a binary from the [release](https://github.com/jeremyschlatter/bel/releases/tag/v0.3.0) page.

Or to build from source, install [Stack](https://docs.haskellstack.org/en/stable/README/#how-to-install) and run `stack run` from the repository root.

## status

Good enough for learning and playing with Bel, not yet good enough to use [in anger](https://news.ycombinator.com/item?id=10806244).

Everything in the spec is implemented except for threads and syscalls.

Every REPL example in the spec works as shown except for a few where the example output is slightly incorrect in the spec (eg the spec shows a truthy value as `t` when actually the value contains more info than that).

Some optimization work has been done, but code still runs very slowly.

The interpreter has had very little usage so far, and there are surely many improvements that one would wish for. Probably some incorrect behavior, as well.
