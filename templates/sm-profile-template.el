;;; smotitah: profile PROFILE-NAME

;;; Uncomment this if this profile should not use smotitah's
;;; module-loading capabilities.
;; (setq sm-unmanaged-profile t)


;;; Delete this block if the profile is unmanaged
(sm-profile-pre (PROFILE-NAME)
  ;; TODO write the code to be executed BEFORE the modules are loaded
  )

;;; Delete this block if the profile is unmanaged
;; TODO: add the modules you want to load here
(sm-require-modules "base")

;;; Delete this block if the profile is unmanaged
(sm-profile-post (PROFILE-NAME)
  ;; TODO write the code to be executed AFTER the modules are loaded
  )

;;;; sm-base-profile end.
