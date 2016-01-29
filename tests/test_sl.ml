
let sl = "
(seq
  (defmacro uconv (dst string) (iconv UTF-8 dst string))
  (defmacro rusmask (regexp)
    (or (bodymask () regexp (uconv cp1251 regexp) (uconv koi8-r regexp))))
  (defmacro irusmask (regexp)
    (or (bodymask (i) regexp (uconv cp1251 regexp) (uconv koi8-r regexp))))

  (if (rusmask маска) (notify \"Совпадение в одной из кодировок\"))
  (if (filemask () \"goodmask\") (notify \"test message2\"))
  (if (filemask () \"goodmask\") (notify \"test message3\"))
  (if (filemask () \"goodmask\") (notify \"test message4\"))
  (if (filemask () \"goodmask\") (notify \"test message5\"))
  (if (bodymask () \"goodbody\") (notify \"test message6\"))
)"

let domain = SexpLoc.File "/some/config"

let ast =
  let sl = SlParser.of_string domain sl in
  let sl = SlMacro.replace sl in
  SlParser.t_to_ast sl

let optimized = ASTOptimized.Parser.of_ast ast

let main =
  let (context_info, optimized) = optimized in
  let filename = "tests/matchedfile" in
  ASTOptimized.Sample.apply optimized context_info filename filename
