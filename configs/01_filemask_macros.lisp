(seq
 (defmacro html () (filemask (i) \.html?$))

 (defmacro php () (filemask (i) \.php[345678]?$))

 (defmacro asp () (filemask (i) \.aspx?$))

 (defmacro perl () (filemask (i) \.pl$))

 (defmacro script () (or php asp perl))

)
