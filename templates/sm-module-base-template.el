;;;; This module should contain your basic configuration, that will be
;;;; shared by all the profiles - if they include the base module, of
;;;; course!

(sm-module "base"
           ;; add the packages required by your basic configuration here
           :require-packages nil
           ;; set this to t if you want to manage this module yourself
           ;; instead of using the builtin package loading infrastructure
           :unmanaged-p nil)


;;;; Remove these 2 blocks if the module is unmanaged
(sm-module-pre (base)
  ;; TODO Write the code that should be executed BEFORE the packages are initialized
  )

(sm-module-post (base)
  ;; TODO Write the code that should be executed AFTER the packages are initialized
  )

(sm-provide :module base)
;;;; End base module
