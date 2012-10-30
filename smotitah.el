;; Super-modular
;; Probably a preloading of certain modules is necessary for some profiles
;; consider giving a hook for module loading

(require 'cl)

;;;; ------------------------------------- Variables -------------------------------------

(defvar sm-profile nil
  "The name of the currently loaded profile")

(defvar sm-unmanaged-profile nil
  "If this is set to T, just load the profile file")

(defvar sm-profile-functions-format "sm-profile-%s-%s"
  "Format string used to calculate the names of the init and post functions for a profile.
  - init function: sm-profile-<profile-name>-init
  - post function: sm-profile-<profile-name>-post")

(defvar sm-active-modules nil
  "List of the modules to be activated by the profile.")

(defvar sm-module-table (make-hash-table :test 'equal)
  "Table of loaded modules. Each module is represented as a property list.")

(defvar  sm-module-functions-format "sm-module-%s-%s"
  "Format string used to calculate the names of the init and post functions for a module.
  - init function: sm-profile-<module name>-init
  - post function: sm-profile-<module name>-post")

(defvar sm-profile-module-integration-file-name-format "%s-%s-%s.el"
  "Format string used to calculate the name of the integration
  files relative to a given module and profile.")

(defvar sm-supported-package-managers '("el-get" "package")
  "Supported package managers.")

(defvar sm-profiles-dir (concat user-emacs-directory "profiles/")
  "Profiles directory.")

(defvar sm-modules-dir (concat user-emacs-directory "modules/")
  "Modules directory.")

(defvar sm-packages-dir (concat user-emacs-directory "packages/")
  "Packages directory")

(defvar sm-directory (file-name-directory load-file-name)
  "Smotitah installation directory.")

(defvar sm-base-profile-file-name (concat sm-profiles-dir "sm-base-profile.el")
  "Base profile file name.")


;;;; ------------------------------------- Utilities -------------------------------------

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

(defun sm-module-package-integration-file (package-name module-name)
  "Returns the file path for the integration file for a package
named PACKAGE-NAME in a module named MODULE-NAME."
  (format "%s/%s/%s-integration.el"
          sm-modules-dir
	  (sm-as-string module-name)
          (sm-as-string package-name)))

(defun sm-profile-module-integration-file (profile-name module-name stage)
  "Returns the file path for the integration file that integrates
the module named MODULE-NAME in the profile named
PROFILE-NAME. STAGE can be either :init, for scripts to be loaded
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
  (intern (concat "sm-package" (sm-as-string package-name))))

(defun sm-module-package-integration-feature (module-name package-name)
  "Returns a symbol to be used as a feature when requiring a
package integration file."
  (intern (format "sm-mpi-%s-%s-integration" (sm-as-string module-name) (sm-as-string package-name))))

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
  ;; Use the profile file as custom file
  (setq custom-file (sm-profile-filename profile-name))

  ;; Load profile file
  (unless (equal (sm-profile-filename profile-name) sm-base-profile-file-name)
    (load (sm-profile-filename profile-name)))

  ;; Do not do anything if the profile is unmanaged
  (unless sm-unmanaged-profile
    ;; Load the base profile
    (load sm-base-profile-file-name)
    ;; Try to call the profile's init function
    (condition-case nil
	(funcall (sm-profile-init-fn profile-name))
      (error (message "smotitah: no profile initialization function")))
    ;; Load the modules
    (dolist (module sm-active-modules)
      (sm-load-module module))
    ;; Try to call the profile's post module loading function
    (condition-case nil
	(funcall (sm-profile-post-fn profile-name))
      (error (message "smotitah: no profile post-module-loading function")))))

(defun sm-profile-list ()
  "Returns a list of profile files."
  (mapcar 'file-name-sans-extension (directory-files sm-profiles-dir nil ".*.el")))

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


;;;; -------------------------------------- Modules --------------------------------------

(defun* sm-module (module-name &key unmanaged-p require-packages)
  "Declares a module. This is meant to be called in the module file,
which must be located in your .emacs.d/modules directory, and must be named
sm-module-MODULE-NAME.el."
  (setf (sm-unmanaged-module-p module-name) unmanaged-p)
  (setf (sm-module-packages module-name) require-packages))

(defmacro sm-get-module (module-name)
  "Returns the property list for the module named MODULE-NAME,
which must be loaded."
  `(gethash ,module-name sm-module-table))

(defmacro sm-unmanaged-module-p (module-name)
  "Returns true if the module is unmanaged."
  `(getf (sm-get-module ,module-name) 'unmanaged-p))

(defun  sm-load-module (module-name)
  "Loads the module named MODULE-NAME."
  (interactive "sModule: ")
  (sm-profile-integrate-module :init module-name)
  (sm-do-load-module module-name)
  (sm-profile-integrate-module :post module-name))

(defun sm-module-init-fn (module-name)
  "Returns a symbol named like the initialization function of a
module named MODULE-NAME."
  (intern (format sm-module-functions-format module-name "init")))

(defun sm-module-post-fn (module-name)
  "Returns a symbol named like the post-module-loading function
of a module named MODULE-NAME."
  (intern (format sm-module-functions-format module-name "post")))
                                                   
(defun sm-require-module-base (module-name)
  "Requires a module file. Not intended to be used directly."
  (require (sm-module-feature module-name)
           (sm-module-filename (sm-as-string module-name))))

(defun sm-do-load-module (module-name)
  "Loads the module, the packages it depends on and the related
integration scripts."
  (interactive "sModule: ")
  (sm-require-module-base (sm-module-filename module-name))
  (unless (sm-unmanaged-module-p module-name)
    (funcall (sm-module-init-fn module-name))
    (dolist (package-name (sm-module-packages module-name))
      (sm-package-initialize package-name module-name))
    (funcall (sm-module-post-fn module-name))))

(defmacro sm-module-packages (module-name)
  "Returns the list of packages the module named MODULE-NAME
depends on."
  `(getf (sm-get-module ,module-name) :packages))

(defun sm-module-integrate-package (package-name module-name)
  "Loads the integration files needed to integrate the package
named PACKAGE-NAME in the module named MODULE-NAME."
  (sm-require-if-file-exists (sm-module-package-integration-feature module-name package-name)
                             (sm-module-package-integration-file package-name module-name)))

(defun sm-module-list ()
  "Returns the list of modules in .emacs.d/modules"
  (mapcar (lambda (module-file)
            (substring (file-name-sans-extension module-file) (length "sm-module-")))
          (directory-files sm-modules-dir nil ".*.el")))


;;;; ------------------------------------- Packages --------------------------------------

(defun sm-package-installed-packages ()
  "Returns the list of packages installed with package.el."
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

(defun* sm-package-initialize (package-name &optional module-name user-managed-p)
  "Initializes the package named PACKAGE-NAME. If MODULE-NAME is provided,
it also loads the related integration scripts.  If user-managed-p
is t, just load the package file found in .emacs.d/packages."
  (unless user-managed-p
    (sm-install-package-if-needed package-name))
  (sm-require-if-file-exists (sm-package-feature package-name)
                             (sm-package-filename package-name))
  (when module-name
    (sm-module-integrate-package package-name module-name)))


(defun sm-install-package-if-needed (package-name)
  "Checks if package-name has been installed with any of the
supported package managers, and interactively installs it if not
present."
  (interactive "sPackage-name: ")
  (while (not (sm-package-installed-p package-name))
    (let ((package-manager (sm-as-symbol
			    (ido-completing-read (format "Install %s with: "
							 (sm-as-string package-name))
						 sm-supported-package-managers))))

      (case package-manager
	(el-get (el-get-install package-name))
	(package (package-install package-name))))))

;; (defun sm-load-package-file-if-exists (package-file-name)
;;   "Loads a package file if present."
;;   (sm-load-file-if-exists
;;    package-file-name))

(defun sm-package-list ()
  (mapcar (lambda (module-file)
            (substring (file-name-sans-extension module-file) (length "sm-module-")))
          (directory-files sm-modules-dir nil ".*.el")))

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

(defun sm-create-base-profile-file-if-needed ()
  "Creates the file sm-base-profile.el in your profiles directory
if not present."
  (unless (file-exists-p sm-base-profile-file-name)
    (copy-file (concat sm-directory "sm-base-profile-template.el")
               sm-base-profile-file-name)))

(defun sm-initialize ()
  "Initializes the smotitah configuration framework."
  (interactive)
  (sm-create-directories-if-needed)
  (sm-create-base-profile-file-if-needed)
  (setq sm-profile (getenv "EMACS_PROFILE"))
  (unless sm-profile
    (setq sm-profile (sm-select-profile-interactively)))
  (sm-load-profile sm-profile))


(defun sm-select-profile-interactively ()
  "Prompts the user to select a profile to load."
  (interactive)
  (ido-completing-read "Load Profile: " (sm-profile-list)))

;;;; ----------------------------------- User commands -----------------------------------

(defun sm-integrate-module (profile-name module-name)
  "Opens the integration files that integrate the module named
MODULE-NAME in the profile named PROFILE-NAME."
  (interactive (let ((pname (ido-completing-read "Integrate in profile: " (sm-profile-list) nil t sm-profile))
                     (mname (ido-completing-read "Integrate module: " (sm-module-list) nil nil)))
                 (list pname mname)))
  (let ((integration-init-filename (sm-profile-module-integration-file profile-name module-name :init))
        (integration-post-filename (sm-profile-module-integration-file profile-name module-name :post)))
    (find-file integration-post-filename)
    (find-file integration-init-filename)))

(defun sm-integrate-package (module-name package-name)
  "Opens the integration files that integrate the package named
PACKAGE-NAME in the module named MODULE-NAME."
  (interactive (let ((mname (ido-completing-read "Integrate in profile: " (sm-module-list) nil t))
                     (pname (ido-completing-read "Integrate package: " (sm-package-list) nil nil)))
                 (list pname mname)))
  (let ((integration-filename (sm-module-package-integration-file package-name module-name)))
    (find-file integration-filename)))

(defun sm-edit-profile (profile-name)
  "Opens the profile file for the profile named PROFILE-NAME."
  (interactive (list (ido-completing-read "Edit Profile: " (sm-profile-list) nil nil sm-profile)))
  (find-file (sm-profile-filename profile-name)))

(defun sm-edit-module (module-name)
  "Opens the module file for the module named MODULE-NAME."
  (interactive (list (ido-completing-read "Edit Module: " (sm-module-list))))
  (find-file (sm-module-filename module-name)))

(defun sm-edit-package (package-name)
  "Opens the package file for the package named PACKAGE-NAME."
  (interactive (list (ido-completing-read "Edit Package: " (sm-package-list))))
  (find-file (sm-package-filename package-name)))



(provide 'smotitah)
