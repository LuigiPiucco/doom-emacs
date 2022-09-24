;; -*- no-byte-compile: t; -*-
;;; completion/corfu/packages.el

(package! corfu :recipe (:files ("*.el" "extensions/*.el")))
(package! cape)
(when (modulep! +icons)
  (package! nerd-icons-completion))
(when (modulep! +orderless)
  (package! orderless))
(when (modulep! :os tty)
  (package! corfu-terminal))
