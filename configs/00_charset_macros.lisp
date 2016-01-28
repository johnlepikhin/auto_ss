(seq

 (defmacro uconv (dst string) (iconv UTF-8 dst string))

 (defmacro rusmask (regexp)
   (bodymask () regexp (uconv cp1251 regexp) (uconv koi8-r regexp)))

 (defmacro irusmask (regexp)
   (bodymask (i) regexp (uconv cp1251 regexp) (uconv koi8-r regexp)))

)
