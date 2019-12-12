module RawStringsQQ where
import Prelude
import Language.Haskell.TH
import Language.Haskell.TH.Quote

{-

For some reason I am unable to import the raw-strings-qq package using the
normal mechanism, so here is a copy of the implementation of `r` from that
package.

-}

normaliseNewlines :: String -> String
normaliseNewlines []             = []
normaliseNewlines ('\r':'\n':cs) = '\n':normaliseNewlines cs
normaliseNewlines (c:cs)         = c:normaliseNewlines cs

r :: QuasiQuoter
r = QuasiQuoter {
    -- Extracted from dead-simple-json.
    quoteExp  = return . LitE . StringL . normaliseNewlines,

    quotePat  = \_ -> fail "illegal raw string QuasiQuote \
                           \(allowed as expression only, used as a pattern)",
    quoteType = \_ -> fail "illegal raw string QuasiQuote \
                           \(allowed as expression only, used as a type)",
    quoteDec  = \_ -> fail "illegal raw string QuasiQuote \
                           \(allowed as expression only, used as a declaration)"
}
