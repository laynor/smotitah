;; Super-modular
;; Probably a preloading of certain modules is necessary for some profiles
;; consider giving a hook for module loading

(require 'cl)
(package-initialize)
(package-refresh-contents)
(when (featurep 'el-get)
  (el-get 'sync))
;;;; ------------------------------------- Variables -------------------------------------

;;; Profile loading related variables

(defvar sm-debug nil
  "Whether or not to write debug messages")

(defvar sm-profile nil
  "The name of the currently loaded profile")

(defvar sm-unmanaged-profile nil
  "If this is set to T, just load the profile file")

(defvar sm-profile-functions-format "sm-profile-%s-%s"
  "Format string used to calculate the names of the init and post functions for a profile.
  - init function: sm-profile-<profile-name>-init
  - post function: sm-profile-<profile-name>-post")


;;; Module related variables

(defvar sm-active-modules nil
  "List of the modules to be activated by the profile.")

(defvar sm-module-table (make-hash-table :test 'equal)
  "Table of loaded modules. Each module is represented as a property list.")

(defvar  sm-module-functions-format "sm-module-%s-%s"
  "Format string used to calculate the names of the init and post functions for a module.
  - init function: sm-module-<module name>-pre
  - post function: sm-module-<module name>-post")


;;; Directories

(defvar sm-profiles-dir (concat user-emacs-directory "profiles/")
  "Profiles directory.")

(defvar sm-modules-dir (concat user-emacs-directory "modules/")
  "Modules directory.")

(defvar sm-packages-dir (concat user-emacs-directory "packages/")
  "Packages directory")

(defvar sm-directory (file-name-directory load-file-name)
  "Smotitah installation directory.")

(defvar sm-template-dir (concat sm-directory "templates/"))



(defvar sm-base-module-file-name (concat sm-modules-dir "sm-module-base.el")
  "Base profile file name.")

(defvar sm-profile-module-integration-file-name-format "%s-%s-%s.el"
  "Format string used to calculate the name of the integration
  files relative to a given module and profile.")


;;; Template files
(defvar sm-template-profile (concat sm-template-dir "sm-profile-template.el"))
(defvar sm-template-module (concat sm-template-dir "sm-module-template.el"))
(defvar sm-template-package (concat sm-template-dir "sm-package-template.el"))
(defvar sm-template-module-integration (concat sm-template-dir "sm-integration-template.el"))

;;; Packages and package managers

(defvar sm-supported-package-managers '("el-get" "package")
  "Supported package managers.")

(defvar sm-package-table (make-hash-table :test 'equal)
  "Table of loaded packages.")

;;; TODO integrate package managers in a generic way
(defvar sm-package-installation-function-alist '((el-get . el-get-install) (package . package-install)))

;;;; ------------------------------------- Utilities -------------------------------------

(defun sm-debug-msg (format-string &rest args)
  (when sm-debug 
    (apply 'message (concat "smotitah: " format-string) args)))

(defun sm-load-file-if-exists (filename)
  "Loads a file if exists."
  (when (file-exists-p filename)
    (load filename)))

(defun sm-require-if-file-exists (feature filename)
  "Requires a feature if filename exists."
  (when (file-exists-p filename)
    (require feature filename)))

(defun sm-as-string (obj)
  "Converts a symbol or a keyword into a string, or returns OBJ
if it is a string"
  (etypecase obj
    (string obj)
    (symbol (let ((name (symbol-name obj)))
	      (if (= (elt name 0) ?:)
		  (subseq name 1)
		name)))))

(defun sm-as-symbol (obj)
  "Converts a string to a symbol, or returns OBJ if ir is a
symbol."
  (etypecase obj
    (string (intern obj))
    (symbol obj)))


;;;; --------------------------------- Name -> Filename ----------------------------------

(defun sm-profile-filename (profile-name)
  "Returns the file path for a profile named PROFILE-NAME."
  (concat sm-profiles-dir (sm-as-string profile-name) ".el"))

(defun sm-module-filename (module-name)
  "Returns the file path for a module named MODULE-NAME."
  (concat sm-modules-dir "sm-module-" (sm-as-string  module-name) ".el"))

(defun sm-package-filename (package-name)
  "Returns the file path for a package named PACKAGE-NAME."
  (concat sm-packages-dir "sm-package-" (sm-as-string package-name) ".el"))

(defun sm-profile-module-integration-file (profile-name module-name stage)
  "Returns the file path for the integration file that integrates
the module named MODULE-NAME in the profile named
PROFILE-NAME. STAGE can be either :pre, for scripts to be loaded
before loading the module, or :post, for scripts to be loaded
after loading the module."
  (concat sm-profiles-dir profile-name "/" (format sm-profile-module-integration-file-name-format
                                                   (sm-as-string profile-name)
                                                   (sm-as-string module-name)
                                                   (sm-as-string stage))))

;;;; ------------------------------- Features for require --------------------------------

(defun sm-module-feature (module-name)
  "Returns a symbol to be used as a feature when requiring
a module named MODULE-NAME."
  (intern (concat "sm-module-" (sm-as-string module-name))))

(defun sm-profile-module-integration-feature (profile-name module-name stage)
  "Returns a symbol to be used as a feature when requiring a
module integration file."
  (intern (format "sm-pmi-%s-%s-%s-integration"
                  (sm-as-string profile-name)
                  (sm-as-string module-name) (sm-as-string stage))))

(defun sm-package-feature (package-name)
  "Returns a symbol that will be used as feature when requiring a
package named PACKAGE-NAME."
  (intern (concat "sm-package-" (sm-as-string package-name))))

(defmacro sm-provide (kind name &optional <- module-name)
  (assert (ecase kind
            ((:integration-pre :integration-post) (and module-name (eq <- '<-)))
            ((:module :package) (every 'null (list <- module-name))))
          nil
          "Wrong sm-provide statement")
  `(provide
    ',(case kind
        (:module (sm-module-feature name))
        (:package (sm-package-feature name))
        (:integration-pre (sm-profile-module-integration-feature name module-name :pre))
        (:integration-post (sm-profile-module-integration-feature name module-name :post)))))


;;;; -------------------------------------- Profile --------------------------------------

(defun sm-profile-init-fn (profile-name)
  "Returns a symbol whose name is the name of the intialiation
function for a profile named PROFILE-NAME."
  (intern (format sm-profile-functions-format profile-name "init")))

(defun sm-profile-post-fn (profile-name)
  "Returns a symbol whose name is the name of the post-module-loading
function for a profile named PROFILE-NAME."
  (intern (format sm-profile-functions-format profile-name "post")))

(defun sm-load-profile (profile-name)
  "Loads the profile. This function is part of the framework
startup, and is not meant to be called directly by the user."
  ;; Load profile file
  (sm-debug-msg "Loading profile file: %s" profile-name)
  (load (sm-profile-filename profile-name))
  ;; Do not do anything if the profile is unmanaged
  (unless sm-unmanaged-profile
    ;; Try to call the profile's init function
    (condition-case nil
	(progn 
	  (sm-debug-msg "Calling profile init fn")
	  (funcall (sm-profile-init-fn profile-name)))
      (error (message "smotitah: no profile initialization function")))
    ;; Load the modules
    (sm-debug-msg "Loading modules")
    (sm-activate-modules sm-active-modules)
    ;; Try to call the profile's post module loading function
    (condition-case nil
	(progn
	  (sm-debug-msg "Calling profile post fn")
	  (funcall (sm-profile-post-fn profile-name)))
      (error (message "smotitah: no profile post-module-loading function"))))
  (sm-debug-msg "Profile loading: end."))

(defun sm-profile-list ()
  "Returns a list of profile files."
  (mapcar 'file-name-sans-extension (directory-files sm-profiles-dir nil "^\\w.*\\.el$")))

(defun sm-profile-integrate-module (stage module-name)
  "Loads the integration file for the module named MODULE-NAME
  for the current profile, if found."
  (sm-require-if-file-exists (sm-profile-module-integration-feature sm-profile module-name stage)
                             (sm-profile-module-integration-file sm-profile
                                                              module-name
                                                              stage)))

(defun sm-require-modules (&rest module-names)
  "Add a module dependency. This is meant to be used in a profile file."
  (setf sm-active-modules (append sm-active-modules module-names)))

(defmacro* sm-profile-pre ((profile-name) &body body)
  (declare (indent 1))
  `(defun ,(sm-profile-init-fn profile-name) ()
     ,@body))

(defmacro* sm-profile-post ((profile-name) &body body)
  (declare (indent 1))
  `(defun ,(sm-profile-post-fn profile-name) ()
     ,@body))

;;;; -------------------------------------- Modules --------------------------------------

(defmacro* sm-module (module-name &key unmanaged-p require-packages)
  "Declares a module. This is meant to be called in the module file,
which must be located in your .emacs.d/modules directory, and must be named
sm-module-MODULE-NAME.el."
  (declare (indent 1))
  (let ((mname (sm-as-string module-name)))
    `(progn
       (setf (sm-get-module ,mname) nil)
       (setf (sm-unmanaged-module-p ,mname) ,unmanaged-p)
       (setf (sm-module-packages ,mname) ,require-packages))))

(defmacro sm-get-module (module-name)
  "Returns the property list for the module named MODULE-NAME,
which must be loaded."
  `(gethash ,module-name sm-module-table))

(defmacro sm-unmanaged-module-p (module-name)
  "Returns true if the module is unmanaged."
  `(getf (sm-get-module ,module-name) :unmanaged-p))

(defmacro sm-module-packages (module-name)
  "Returns the list of packages the module named MODULE-NAME
depends on."
  `(getf (sm-get-module ,module-name) :packages))

(defun sm-activate-modules (modules)
  (sm-debug-msg "Initializing modules %S" modules)
  (sm-initialize-modules modules)
  (sm-debug-msg "Running integration scripts, stage: pre.")
  (sm-integrate-modules modules :pre)
  (sm-debug-msg "Calling modules pre fns.")
  (sm-call-modules-fn modules :pre)
  (sm-debug-msg "Requiring packages")
  (dolist (m modules)
    (sm-require-module-packages m))
  (sm-debug-msg "Calling modules post fns.")
  (sm-call-modules-fn modules :post)
  (sm-debug-msg "Running integration scripts, stage: post.")
  (sm-integrate-modules modules :post))

(defun sm-initialize-modules (modules)
  (dolist (m modules)
    (sm-debug-msg "Requiring module file for %s" m)
    (sm-require-module-base m)
    (sm-debug-msg "   Module %S: %S" m (sm-get-module m))))

(defun sm-integrate-modules (modules stage)
  (dolist (m modules)
    (unless (sm-unmanaged-module-p m)
      (sm-debug-msg "Requiring integration for %s, %s" m stage)
      (sm-profile-integrate-module stage m))))

(defun sm-call-modules-fn (modules stage)
  (dolist (m modules)
    (unless (sm-unmanaged-module-p m)
      (sm-debug-msg "Calling %s's %s fn." m stage)
      (funcall (case stage
		 (:pre (sm-module-pre-fn m))
		 (:post (sm-module-post-fn m)))))))

(defun sm-require-module-packages (module)
  (unless (sm-unmanaged-module-p module)
    (let ((packages (sm-module-packages module)))
      (sm-debug-msg "Requiring packages for %S: %S" module (sm-module-packages module))
      (dolist (p packages)
	(sm-package-initialize p)))))
   
  
;; (defun sm-load-module (module-name)
;;   "Loads the module named MODULE-NAME."
;;   (interactive "sModule: ")
;;   (sm-profile-integrate-module :pre module-name)
;;   (sm-do-load-module module-name)
;;   (sm-profile-integrate-module :post module-name))

(defun sm-module-pre-fn (module-name)
  "Returns a symbol named like the initialization function of a
module named MODULE-NAME."
  (intern (format sm-module-functions-format module-name "pre")))

(defun sm-module-post-fn (module-name)
  "Returns a symbol named like the post-module-loading function
of a module named MODULE-NAME."
  (intern (format sm-module-functions-format module-name "post")))

(defun sm-require-module-base (module-name)
  "Requires a module file. Not intended to be used directly."
  (require (sm-module-feature module-name)
           (sm-module-filename (sm-as-string module-name))))

;; (defun sm-do-load-module (module-name)
;;   "Loads the module, the packages it depends on and the related
;; integration scripts."
;;   (interactive "sModule: ")
;;   (sm-require-module-base (sm-module-filename module-name))
;;   (unless (sm-unmanaged-module-p module-name)
;;     (funcall (sm-module-pre-fn module-name))
;;     (dolist (package-name (sm-module-packages module-name))
;;       (sm-package-initialize package-name module-name))
;;     (funcall (sm-module-post-fn module-name))))

(defun sm-module-list ()
  "Returns the list of modules in .emacs.d/modules"
  (mapcar (lambda (module-file)
            (substring (file-name-sans-extension module-file) (length "sm-module-")))
          (directory-files sm-modules-dir nil "^sm-module-.*\\.el$")))

(defmacro* sm-module-pre ((module-name) &body body)
  (declare (indent 1))
  `(defun ,(sm-module-pre-fn module-name) ()
     ,@body))

(defmacro* sm-module-post ((module-name) &body body)
  (declare (indent 1))
  `(defun ,(sm-module-post-fn module-name) ()
     ,@body))

;;;; ------------------------------------- Packages --------------------------------------

(defun sm-xor (x y)
  (not (eq (and x t)
           (and y t))))

(defmacro sm-get-package (name)
  `(gethash ,name sm-package-table))

(defun sm-package-install-with (package-name package-manager)
  (funcall (cdr (assoc (sm-as-symbol package-manager) sm-package-installation-function-alist))
           (sm-as-symbol package-name)))

(defmacro* sm-package (name &key package-manager unmanaged-p)
  "Declares a package named NAME. Only one of PACKAGE-MANAGER and UNMANAGED-P must be true.
PACKAGE-MANAGER, if provided, must be one of the package managers
supported by smotitah - see `sm-supported-package-managers'."
  (declare (indent 1))
  `(progn
     (assert (sm-xor ,package-manager ,unmanaged-p) nil
	     "Error in package '%s' declaration: one (and only one)
              of PACKAGE-MANAGER and UNMANAGED-P must be non-nil" name)
     (setf (sm-get-package ,(sm-as-string name)) (list :package ,(sm-as-string name) :package-manager ,package-manager :unmanaged-p ,unmanaged-p))
     (unless (or ,unmanaged-p (sm-package-installed-p ,(sm-as-string name)))
       (sm-package-install-with ,(sm-as-string name) ,package-manager)
       (assert (sm-package-installed-p ,(sm-as-string name)) nil
	       "smotitah: Cannot install package %s with package manager %s."
	       ,name ,package-manager))))


(defun sm-package-installed-packages ()
  "Returns the list of packages installed with package.el."
  ;; (package-initialize) where to put this?
  (mapcar #'car package-alist))

(defun sm-el-get-installed-packages ()
  "Returns the list of packages installed with el-get."
  (if (fboundp 'el-get-package-is-installed)
      (remove-if-not 'el-get-package-is-installed (el-get-read-all-recipe-names))
    nil))

(defun sm-all-installed-packages ()
  "Returns all the packages installed with the package managers
listed in SM-SUPPORTED-PACKAGE-MANAGERS"
  (remove-duplicates (append (sm-package-installed-packages)
			     (sm-el-get-installed-packages))
		     :test 'equal))

(defun sm-package-installed-p (package-name)
  "Returns t if the package named PACKAGE-NAME is installed with
any of the package managers listed in
SM-SUPPORTED-PACKAGE-MANAGERS, nil otherwise."
  (member (sm-as-symbol package-name) (sm-all-installed-packages)))

(defun* sm-package-initialize (package-name)
  "Initializes the package named PACKAGE-NAME. If MODULE-NAME is provided,
it also loads the related integration scripts.  If user-managed-p
is t, just load the package file found in .emacs.d/packages."
  (unless (featurep (sm-package-feature package-name))
    (sm-debug-msg "Requiring package %s." package-name))
  (sm-require-if-file-exists (sm-package-feature package-name)
                             (sm-package-filename package-name)))

(defun sm-package-list ()
  (mapcar (lambda (package-file)
            (substring (file-name-sans-extension package-file) (length "sm-package-")))
          (directory-files sm-packages-dir nil "^sm-package-.*\\.el$")))

;;;; ---------------------------------- Initialization -----------------------------------
(defun sm-create-directories-if-needed ()
  "Creates the profiles, modules and packages directories in your
user emacs dir if not present"
  (ignore-errors (make-directory sm-profiles-dir))
  (ignore-errors (make-directory sm-modules-dir))
  (ignore-errors (make-directory sm-packages-dir))
  (assert (every 'file-exists-p (list sm-profiles-dir sm-modules-dir sm-packages-dir))
          nil "smotitah: Cannot create profiles, modules, packages directories in %s"
          user-emacs-directory))

(defun sm-create-base-module-if-needed ()
  "Creates a stub for the profile-shared base module."
  (unless (file-exists-p sm-base-module-file-name)
    (copy-file (concat sm-template-dir "sm-module-base-template.el")
               sm-base-module-file-name)))

(defun sm-initialize ()
  "Initializes the smotitah configuration framework."
  (interactive)
  (let ((profile-list (sm-profile-list)))
    (sm-create-base-module-if-needed)
    (sm-create-directories-if-needed)
    (setq sm-profile (getenv "EMACS_PROFILE"))
    (when (and (null sm-profile) profile-list)
      (setq sm-profile (sm-select-profile-interactively)))
    (cond ((and (null profile-list) (yes-or-no-p "No profiles found. Do you want to create one now?"))
	   (let ((profile-name (read-from-minibuffer "Profile name: ")))
	     (sm-find-file-or-fill-template (sm-profile-filename profile-name)
					    sm-template-profile `(("PROFILE-NAME" . ,profile-name)))))
      
	  (sm-profile
	    (sm-load-profile sm-profile)))))


(defun sm-select-profile-interactively ()
  "Prompts the user to select a profile to load."
  (interactive)
  (ido-completing-read "Load Profile: " (sm-profile-list)))

;;;; ----------------------------------- User commands -----------------------------------

(defun sm-fill-template-and-save (template-filename destination-file substitution-alist)
  (let ((buf (find-file-noselect template-filename)))
    (with-current-buffer buf
      (dolist (sub substitution-alist)
        (goto-char (point-min))
        (while (search-forward (car sub) nil t)
          (replace-match (cdr sub) t t)))
      (write-file destination-file))
    (kill-buffer buf))
  (setq inhibit-splash-screen t)
  (find-file destination-file))

;;; TODO use templates
(defun sm-find-file-or-fill-template (filename template-filename substitutions)
  (if (file-exists-p filename)
      (find-file filename)
    (sm-fill-template-and-save template-filename filename substitutions)))

(defun sm-integrate-module (profile-name module-name)
  "Opens the integration files that integrate the module named
MODULE-NAME in the profile named PROFILE-NAME."
  (interactive (let ((pname (ido-completing-read "Integrate in profile: " (sm-profile-list) nil t sm-profile))
                     (mname (ido-completing-read "Integrate module: " (sm-module-list) nil nil)))
                 (list pname mname)))
  (let ((integration-pre-filename (sm-profile-module-integration-file profile-name module-name :pre))
        (integration-post-filename (sm-profile-module-integration-file profile-name module-name :post)))
    (sm-find-file-or-fill-template integration-post-filename sm-template-module-integration
                                   `(("PROFILE-NAME" . ,profile-name) ("MODULE-NAME" . ,module-name)
                                     ("STAGE" . "post")))
    (sm-find-file-or-fill-template integration-pre-filename
				   sm-template-module-integration
                                   `(("PROFILE-NAME" . ,profile-name) ("MODULE-NAME" . ,module-name)
                                     ("STAGE" . "pre")))))


(defun sm-edit-profile (profile-name)
  "Opens the profile file for the profile named PROFILE-NAME."
  (interactive (list (ido-completing-read "Edit Profile: " (sm-profile-list) nil nil sm-profile)))
  (sm-find-file-or-fill-template (sm-profile-filename profile-name)
                                 sm-template-profile
                                 `(("PROFILE-NAME" . ,profile-name))))

(defun sm-edit-module (module-name)
  "Opens the module file for the module named MODULE-NAME."
  (interactive (list (ido-completing-read "Edit Module: " (sm-module-list))))
  (sm-find-file-or-fill-template (sm-module-filename module-name)
                                 sm-template-module
                                 `(("MODULE-NAME" . ,module-name))))

(defun sm-edit-package (package-name)
  "Opens the package file for the package named PACKAGE-NAME."
  (interactive (list (ido-completing-read "Edit Package: " (sm-package-list))))
  (sm-find-file-or-fill-template (sm-package-filename package-name)
                                 sm-template-package
                                 `(("PACKAGE-NAME" . ,package-name))))


(defadvice package-install (after sm-offer-package-file-creation (name) activate)
  (unless (file-exists-p (sm-package-filename (sm-as-string name)))
    (sm-edit-package (sm-as-string name))))


(provide 'smotitah)
