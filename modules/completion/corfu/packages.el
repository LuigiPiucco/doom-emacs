;; -*- no-byte-compile: t; -*-
;;; completion/corfu/packages.el

(package! corfu :recipe (:files ("*.el" "extensions/*.el")) :pin "b2b9a2312f58117514724e729fda43efc4433ecd")
(package! cape :pin "bee13e568d42ffec9010fbf795e8f942973da174")
(when (modulep! +icons)
  (package! nerd-icons-completion :pin "c2db8557a3c1a9588d111f8c8e91cae96ee85010")
  (package! nerd-icons-corfu :pin "5082a22dfbdea092fcc475db3484cb3ff812b730"))
(when (modulep! +orderless)
  (package! orderless :pin "d6b402a89e234d0e6166247ed6025f9acc8b4d9a"))
(when (modulep! :os tty)
  (package! corfu-terminal :pin "501548c3d51f926c687e8cd838c5865ec45d03cc"))
(when (modulep! :editor snippets)
  (package! yasnippet-capf :pin "40654214db7a44db3a99321447632b43a10fae57"))
