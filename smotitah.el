;;; smotitah.el --- Modular configuration framework for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2013  Alessandro Piras

;; Author: Alessandro Piras <laynor@gmail.com>
;; Keywords: configuration

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; CONCEPTS
;; ========
;;
;; This package is meant to modularize the Emacs user configuration,
;; and simplify the maintenance of multiple Emacs configurations,
;; reducing code duplication.
;; It also offers support for automatic package installation for packages
;; installed with package.el.
;;
;; The configuration is expressed as a hierarchical structure.
;; The concepts provided by this library are:
;; - profiles
;; - modules
;; - packages
;;
;; A profile is a given Emacs configuration. For example, I, the
;; author, have 3 profiles: job, home-windows, home-linux.
;;
;;
;; A profile can require (and customize as needed in that profile) a
;; set of modules.  Modules can be seen as activities, like C
;; programming, elisp programming, or LaTeX authoring.
;; Modules can require a set of packages, and customize them as needed
;; by the given module.
;;
;; For example, the package flymake could need different customizations for
;; C and Python. These customizations can be written in the respective C and
;; Python modules, while the common base configuration can be written in
;; the flymake package configuration file.
;;
;; Smotitah packages are customization units for Emacs packages.
;; Packages can be either managed or unmanaged. A managed package is a package
;; that has been installed with a package manager, while an unmanaged one is
;; a package that is manually installed and updated by the user.
;; At the moment, package.el is the only package manager supported by Smotitah,
;; but there are plans on supporting el-get too.
;;
;;
;; CONFIGURATION LAYOUT
;; ====================
;;
;; Smotitah structures the Emacs configuration as follows:
;;
;;   .emacs.d/
;;      + profiles/
;;      + modules/
;;      + packages/
;;
;; A profile file has the following structure:
;; ----------------------------------------------------------------------
;; (sm-profile-pre (profile-name)
;;   ;; code to be executed *before* loading the modules
;;   )
;;
;; ;;; Modules to activate,
;; (sm-require-modules "C" "Python")
;;
;; (sm-profile-post (job)
;;  ;; Code to be executed *after* loading the modules
;;   )
;; ----------------------------------------------------------------------
;;
;; A module file has the following structure (example: C programming):
;; ----------------------------------------------------------------------
;; (sm-module C
;;            :unmanaged-p nil
;;            ;; Required packages list
;;            :require-packages '(yasnippet auto-complete-clang c-eldoc))
;;
;; (sm-module-pre (C)
;;   ;; Code to be executed *before* loading and configuring the packages
;;   )
;;
;; (sm-module-post (C)
;;   ;; Code to be executed *after* loading and configuring the packages
;;   )
;;
;; (sm-provide :module C)
;; ----------------------------------------------------------------------
;;
;; A package file has the following structure (example: yasnippet):
;; ----------------------------------------------------------------------
;; (sm-package yasnippet
;;             :package-manager "package"
;;             :unmanaged-p nil)
;;
;; ;; Put the package's base configuration here.
;; (require 'yasnippet)
;;
;; (defun add-yasnippet-ac-sources ()
;;   (add-to-list 'ac-sources 'ac-source-yasnippet))

;; (sm-provide :package yasnippet)
;; ----------------------------------------------------------------------
;;
;; EXECUTING CODE AFTER A PACKAGE OR MODULE HAS BEEN LOADED
;; ========================================================
;; Sometimes you can need to execute code after a given package or module
;; has been loaded. For example, I need to integrate the package Evil
;; (a vim-like editing package) with lots of other packages.
;; Taking care of the loading order of packages is pretty annoying.

;; The macro `sm-integrate-with' takes care of this.
;; It works like `eval-after-load' (but with no need to quote the block of
;; code to be executed, and with an implicit progn) if a string or symbol
;; is given as its first argument, but it can also take a list of the form
;;
;;   (:package package-name)
;; or
;;   (:module module-name)
;;
;; A example from my evil package configuration:
;;
;;   (sm-integrate-with (:package direx)
;;     (evil-global-set-key 'normal (kbd "C-d") 'popwin:direx))
;;
;;   (sm-integrate-with (:package ipa)
;;     (evil-global-set-key 'normal (kbd "M-i M-i") 'ipa-toggle)
;;     (evil-global-set-key 'normal (kbd "M-i i") 'ipa-insert)
;;     (evil-global-set-key 'normal (kbd "M-i e") 'ipa-edit)
;;     (evil-global-set-key 'normal (kbd "M-i m") 'ipa-move))
;;
;;
;; USAGE
;; =====
;;
;; Smotitah can be loaded from .emacs.d/init.el or your .emacs file.
;; The procedure to activate Smotitah is different whether you manually downloaded
;; its source code or you installed it with package.el.

;; Put this in your .emacs or init.el file if you manually downloaded the sources:

;;     (add-to-list 'load-path user-emacs-directory)
;;
;;     (setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")))
;;
;;     (add-to-list 'load-path "path/to/smotitah")
;;     (require 'smotitah)
;;     (sm-initialize)

;; Put this in your .emacs or init.el file if you installed smotitah with package.el.

;;     (add-to-list 'load-path user-emacs-directory)
;;     (setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")))

;;     (let ((package-enable-at-startup nil))
;;       (package-initialize t)
;;       (package-activate 'smotitah (package-desc-vers (cdr (assoc 'smotitah package-alist)))))
;;     (sm-initialize)

;;
;; Smotitah profile, module and package files stubs can be created from template
;; using some facilities provided by the library:
;;
;; To create a profile file, M-x sm-edit-profile RET profile-name RET
;; To create a module file, M-x sm-edit-module RET module-name RET
;; To create a package file, M-x sm-edit-package RET package-name RET
;;
;; If a profile, module or package with the given name is already present,
;; these functions will just open the file for editing.
;;
;; Please note that when installing a package with package-install or
;; from the list-packages menu a smotitah package file is
;; automatically created in the .emacs.d/packages directory.
;;
;; To load a given profile, set the EMACS_PROFILE environment variable
;; to the profile name to load:
;;
;; Example: use profile-1
;;    $ EMACS_PROFILE="profile-1" emacs
;;
;; or alternatively put this in your initialization file (init.el or .emacs):
;;
;; (setq sm-profile "profile-1")
;;
;; If no profile has been set in any of these two ways, smotitah will ask
;; the user to choose which profile to load at startup.
;;
;; To load only a given set of modules, set the EMACS_MODULES environment
;; variable to a comma-separated list of module names:
;;
;; Example: load C and python modules:
;;    $ EMACS_MODULES="C, python" emacs
;;
;;
;;; Code:

;; Probably a preloading of certain modules is necessary for some profiles
;; consider giving a hook for module loading

(require 'cl)

;;; KLUDGE KLUDGE KLUDGE KLUDGE KLUDGE KLUDGE KLUDGE KLUDGE KLUDGE KLUDGE
;;; KLUDGE KLUDGE KLUDGE KLUDGE KLUDGE KLUDGE KLUDGE KLUDGE KLUDGE KLUDGE
;;; KLUDGE KLUDGE KLUDGE KLUDGE KLUDGE KLUDGE KLUDGE KLUDGE KLUDGE KLUDGE
;;; KLUDGE KLUDGE KLUDGE KLUDGE KLUDGE KLUDGE KLUDGE KLUDGE KLUDGE KLUDGE
;;; the function `command-line' seems to execute the user scripts,
;;; and then it runs `package-initialize' if `package-enable-at-startup'
;;; is not nil. Since this happens _after_ loading the user scripts,
;;; we cannot disable package-enable-at-startup before the package-initialize
;;; and then re-enable it to make `package-install' work correctly.
(require 'package)
(setf (symbol-function 'smotitah-package-initialize) (symbol-function 'package-initialize))
(defadvice package-compute-transaction (before smotitah-activate-installed-requirements (package-list requirements) activate)
  (mapc (lambda (req)
          (when (package-installed-p (car req))
            (package-activate (car req) (cddr req))))
        requirements))
(defun package-initialize (&optional no-activate)
  (smotitah-package-initialize t))

;;;; ------------------------------------- Variables -------------------------------------

;;; Profile loading related variables

(defvar sm-debug nil
  "Whether or not to write debug messages")

(defvar sm-profile nil
  "The name of the currently loaded profile")

(defvar sm-unmanaged-profile nil
  "If this is set to T, just load the profile file")

(defvar sm--profile-functions-format "sm-profile-%s-%s"
  "Format string used to calculate the names of the init and post functions for a profile.
  - init function: sm-profile-<profile-name>-init
  - post function: sm-profile-<profile-name>-post")


;;; Module related variables

(defvar sm--active-modules nil
  "List of the modules to be activated by the profile.")

(defvar sm--module-table (make-hash-table :test 'equal)
  "Table of loaded modules. Each module is represented as a property list.")

(defvar  sm--module-functions-format "sm-module-%s-%s"
  "Format string used to calculate the names of the init and post functions for a module.
  - init function: sm-module-<module name>-pre
  - post function: sm-module-<module name>-post")


;;; Directories

(defvar sm--profiles-dir (concat user-emacs-directory "profiles/")
  "Profiles directory.")

(defvar sm--modules-dir (concat user-emacs-directory "modules/")
  "Modules directory.")

(defvar sm--packages-dir (concat user-emacs-directory "packages/")
  "Packages directory")

(defvar sm--directory (file-name-directory load-file-name)
  "Smotitah installation directory.")

(defvar sm--template-dir (concat sm--directory "templates/"))



(defvar sm--base-module-file-name (concat sm--modules-dir "sm-module-base.el")
  "Base profile file name.")

(defvar sm--profile-module-integration-file-name-format "%s-%s-%s"
  "Format string used to calculate the name of the integration
  files relative to a given module and profile.")


;;; Template files
(defvar sm--template-profile (concat sm--template-dir "sm-profile-template.el"))
(defvar sm--template-module (concat sm--template-dir "sm-module-template.el"))
(defvar sm--template-package (concat sm--template-dir "sm-package-template.el"))
(defvar sm--template-module-integration (concat sm--template-dir "sm-integration-template.el"))

;;; Packages and package managers

(defvar  sm-packages-to-preload nil
  "This variable contains a list of packages that should be
loaded BEFORE initializing the package managers. These packages
MUST BE UNMANAGED packages.

The only situation in which someone would need to use this is
when the user wants to override some package used by smotitah or
one of the supported package managers.

We - the developers - only use this to preload our version of
CEDET, because package.el indirectly uses CEDET (eieio) and we
need to load it before package.el is loaded.

DO NOT add managed packages to this list - it won't work. If you
feel you need to preload a managed package, please file an issue
on github (https://github.com/laynor/smotitah/issues) and we will
try to work around it.")

(defvar sm--supported-package-managers '("el-get" "package")
  "Supported package managers.")

(defvar sm--package-table (make-hash-table :test 'equal)
  "Table of loaded packages.")

(defvar sm--package-refreshed-p nil
  "Whether or not the package list for package.el has been refreshed.")
;;; TODO integrate package managers in a generic way
(defvar sm--package-installation-function-alist '((el-get . el-get-install) (package . sm--package-install)))
(defvar sm--package-activation-function-alist '((package . sm--package-activate-package) (el-get . sm--el-get-activate-package)))
(defvar sm--package-list-function-alist '((package . sm--package-installed-packages)
					  (el-get . sm--el-get-installed-packages)))

;;;; ------------------------------------- Utilities -------------------------------------


(defun sm--package-install (package-name)
  (package-install package-name))


(defun sm-debug-msg (format-string &rest args)
  "Debug message utility."
  (when sm-debug
    (apply 'message (concat "smotitah: " format-string) args)))

(defun sm-load-file-if-exists (filename)
  "Loads a file if exists."
  (when (file-exists-p filename)
    (load filename)))

(defun sm--script-exists-p (filename)
  (some (lambda (suffix)
          (file-exists-p (concat filename suffix)))
        (get-load-suffixes)))

(defun sm-require-if-file-exists (feature filename)
  "Requires a feature if filename exists."
  (when (sm--script-exists-p filename)
    (sm-debug-msg "smotitah: Requiring feature %S from %S." feature filename)
    (require feature filename)))

(defun sm--as-string (obj)
  "Internal. Converts a symbol or a keyword into a string, or returns OBJ
if it is a string"
  (etypecase obj
    (string obj)
    (symbol (let ((name (symbol-name obj)))
              (if (= (elt name 0) ?:)
                  (subseq name 1)
                name)))))

(defun sm--as-symbol (obj)
  "Internal. Converts a string to a symbol, or returns OBJ if ir is a
symbol."
  (etypecase obj
    (string (intern obj))
    (symbol obj)))

(defun sm--xor (x y)
  "Internal. Boolean XOR."
  (not (eq (and x t)
           (and y t))))


;;;; --------------------------------- Name -> Filename ----------------------------------

(defun sm--profile-filename (profile-name)
  "Internal. Returns the file path for a profile named PROFILE-NAME."
  (concat sm--profiles-dir (sm--as-string profile-name)))

(defun sm--module-filename (module-name)
  "Returns the file path for a module named MODULE-NAME."
  (concat sm--modules-dir "sm-module-" (sm--as-string  module-name)))

(defun sm--package-filename (package-name)
  "Returns the file path for a package named PACKAGE-NAME."
  (concat sm--packages-dir "sm-package-" (sm--as-string package-name)))

(defun sm--profile-module-integration-file (profile-name module-name stage)
  "Returns the file path for the integration file that integrates
the module named MODULE-NAME in the profile named
PROFILE-NAME. STAGE can be either :pre, for scripts to be loaded
before loading the module, or :post, for scripts to be loaded
after loading the module."
  (concat sm--profiles-dir profile-name "/" (format sm--profile-module-integration-file-name-format
                                                    (sm--as-string profile-name)
                                                    (sm--as-string module-name)
                                                    (sm--as-string stage))))

;;;; ------------------------------- Features for require --------------------------------

(defun sm--module-feature (module-name)
  "Returns a symbol to be used as a feature when requiring
a module named MODULE-NAME."
  (intern (concat "sm-module-" (sm--as-string module-name))))

(defun sm--profile-module-integration-feature (profile-name module-name stage)
  "Returns a symbol to be used as a feature when requiring a
module integration file."
  (intern (format "sm-pmi-%s-%s-%s-integration"
                  (sm--as-string profile-name)
                  (sm--as-string module-name) (sm--as-string stage))))

(defun sm--package-feature (package-name)
  "Returns a symbol that will be used as feature when requiring a
package named PACKAGE-NAME."
  (intern (concat "sm-package-" (sm--as-string package-name))))

(defmacro sm-provide (kind name &optional <- module-name)
  (assert (ecase kind
            ((:integration-pre :integration-post) (and module-name (eq <- '<-)))
            ((:module :package) (every 'null (list <- module-name))))
          nil
          "Wrong sm-provide statement")
  `(provide
    ',(case kind
        (:module (sm--module-feature name))
        (:package (sm--package-feature name))
        (:integration-pre (sm--profile-module-integration-feature name module-name :pre))
        (:integration-post (sm--profile-module-integration-feature name module-name :post)))))


;;;; -------------------------------------- Profile --------------------------------------

(defun sm--profile-init-fn (profile-name)
  "Returns a symbol whose name is the name of the intialiation
function for a profile named PROFILE-NAME."
  (intern (format sm--profile-functions-format profile-name "init")))

(defun sm--profile-post-fn (profile-name)
  "Returns a symbol whose name is the name of the post-module-loading
function for a profile named PROFILE-NAME."
  (intern (format sm--profile-functions-format profile-name "post")))

(defun sm--load-profile (profile-name)
  "Loads the profile. This function is part of the framework
startup, and is not meant to be called directly by the user."
  ;; Load profile file
  (sm-debug-msg "Loading profile file: %s" profile-name)
  (load (sm--profile-filename profile-name))
  ;; Do not do anything if the profile is unmanaged
  (unless sm-unmanaged-profile
    ;; Try to call the profile's init function
    (condition-case nil
        (progn
          (sm-debug-msg "Calling profile init fn")
          (funcall (sm--profile-init-fn profile-name)))
      (error (message "smotitah: no profile initialization function")))
    ;; Load the modules
    (sm-debug-msg "Loading modules")
    (sm--activate-modules sm--active-modules)
    ;; Try to call the profile's post module loading function
    (cond ((fboundp (sm--profile-post-fn profile-name))
	   (condition-case err
	       (progn
		 (sm-debug-msg "Calling profile post fn")
		 (funcall (sm--profile-post-fn profile-name)))
	     (error (error "smotitah: error when calling profile %S post module loading function: %s"
			   profile-name (error-message-string err)))))
	  (t (message "smotitah: no profile post-module-loading function"))))
  (sm-debug-msg "Profile loading: end."))

(defun sm-profile-list ()
  "Returns a list of profile files."
  (mapcar 'file-name-sans-extension (directory-files sm--profiles-dir nil "^\\w.*\\.el$")))

(defun sm--profile-integrate-module (stage module-name)
  "Loads the integration file for the module named MODULE-NAME
  for the current profile, if found."
  (sm-require-if-file-exists (sm--profile-module-integration-feature sm-profile module-name stage)
                             (sm--profile-module-integration-file sm-profile
                                                                  module-name
                                                                  stage)))

(defun sm-require-modules (&rest module-names)
  "Add a module dependency. This is meant to be used in a profile file."
  (setf sm--active-modules (append sm--active-modules module-names)))

(defmacro* sm-profile-pre ((profile-name) &body body)
  (declare (indent 1))
  `(defun ,(sm--profile-init-fn profile-name) ()
     ,@body))

(defmacro* sm-profile-post ((profile-name) &body body)
  (declare (indent 1))
  `(defun ,(sm--profile-post-fn profile-name) ()
     ,@body))

;;;; -------------------------------------- Modules --------------------------------------

(defmacro* sm-module (module-name &key unmanaged-p require-packages)
  "Declares a module. This is meant to be called in the module file,
which must be located in your .emacs.d/modules directory, and must be named
sm-module-MODULE-NAME.el."
  (declare (indent 1))
  (let ((mname (sm--as-string module-name)))
    `(progn
       (setf (sm--get-module ,mname) nil)
       (setf (sm--unmanaged-module-p ,mname) ,unmanaged-p)
       (setf (sm--module-packages ,mname) ,require-packages))))

(defmacro sm--get-module (module-name)
  "Returns the property list for the module named MODULE-NAME,
which must be loaded."
  `(gethash ,module-name sm--module-table))

(defmacro sm--unmanaged-module-p (module-name)
  "Returns true if the module is unmanaged."
  `(getf (sm--get-module ,module-name) :unmanaged-p))

(defmacro sm--module-packages (module-name)
  "Returns the list of packages the module named MODULE-NAME
depends on."
  `(getf (sm--get-module ,module-name) :packages))

(defun sm--module-pre-fn (module-name)
  "Internal. Returns a symbol named like the initialization function of a
module named MODULE-NAME."
  (intern (format sm--module-functions-format module-name "pre")))

(defun sm--module-post-fn (module-name)
  "Internal. Returns a symbol named like the post-module-loading function
of a module named MODULE-NAME."
  (intern (format sm--module-functions-format module-name "post")))

(defun sm--require-module-base (module-name)
  "Internal. Requires a module file."
  (require (sm--module-feature module-name)
           (sm--module-filename (sm--as-string module-name))))

(defun sm--initialize-modules (modules)
  "Internal. Loads the module files."
  (dolist (m modules)
    (sm-debug-msg "Requiring module file for %s" m)
    (sm--require-module-base m)
    (sm-debug-msg "   Module %S: %S" m (sm--get-module m))))

(defun sm--integrate-modules (modules stage)
  "Internal. Runs the integration scripts for MODULES in the
current profile, before (:pre) or after (:post) the module loading happens, as
specified in STAGE."
  (dolist (m modules)
    (unless (sm--unmanaged-module-p m)
      (sm-debug-msg "Requiring integration for %s, %s" m stage)
      (sm--profile-integrate-module stage m))))

(defun sm--call-modules-fn (modules stage)
  "Internal. Calls the pre or post functions for MODULES, as specified by
STAGE."
  (dolist (m modules)
    (unless (sm--unmanaged-module-p m)
      (sm-debug-msg "Calling %s's %s fn." m stage)
      (funcall (case stage
                 (:pre (sm--module-pre-fn m))
                 (:post (sm--module-post-fn m)))))))

(defun sm--require-module-packages (module)
  "Internal. Requires a module file."
  (unless (sm--unmanaged-module-p module)
    (let ((packages (sm--module-packages module)))
      (sm-debug-msg "Requiring packages for %S: %S" module (sm--module-packages module))
      (dolist (p packages)
        (sm--package-initialize p)))))

(defun sm--activate-modules (modules)
  "Loads every module listed in MODULES, loading and activating
their package dependencies."
  (sm-debug-msg "Initializing modules %S" modules)
  (sm--initialize-modules modules)
  (sm-debug-msg "Running integration scripts, stage: pre.")
  (sm--integrate-modules modules :pre)
  (sm-debug-msg "Calling modules pre fns.")
  (sm--call-modules-fn modules :pre)
  (sm-debug-msg "Requiring packages")
  (dolist (m modules)
    (sm--require-module-packages m))
  (sm-debug-msg "Calling modules post fns.")
  (sm--call-modules-fn modules :post)
  (sm-debug-msg "Running integration scripts, stage: post.")
  (sm--integrate-modules modules :post))



;; (defun sm-load-module (module-name)
;;   "Loads the module named MODULE-NAME."
;;   (interactive "sModule: ")
;;   (sm--profile-integrate-module :pre module-name)
;;   (sm-do-load-module module-name)
;;   (sm--profile-integrate-module :post module-name))


;; (defun sm-do-load-module (module-name)
;;   "Loads the module, the packages it depends on and the related
;; integration scripts."
;;   (interactive "sModule: ")
;;   (sm--require-module-base (sm--module-filename module-name))
;;   (unless (sm--unmanaged-module-p module-name)
;;     (funcall (sm--module-pre-fn module-name))
;;     (dolist (package-name (sm--module-packages module-name))
;;       (sm--package-initialize package-name module-name))
;;     (funcall (sm--module-post-fn module-name))))

(defun sm-module-list ()
  "Returns the list of modules in .emacs.d/modules"
  (mapcar (lambda (module-file)
            (substring (file-name-sans-extension module-file) (length "sm-module-")))
          (directory-files sm--modules-dir nil "^sm-module-.*\\.el$")))

(defmacro* sm-module-pre ((module-name) &body body)
  "Use this to surround the statements that should be executed
BEFORE the packages are loaded in a module file."
  (declare (indent 1))
  `(defun ,(sm--module-pre-fn module-name) ()
     ,@body))

(defmacro* sm-module-post ((module-name) &body body)
  "Use this to surround the statements that should be executed
AFTER the packages are loaded in a module file."
  (declare (indent 1))
  `(defun ,(sm--module-post-fn module-name) ()
     ,@body))

;;;; ------------------------------------- Packages --------------------------------------

(defmacro sm--get-package (name)
  "Internal. Gets the package description for the package named NAME."
  `(gethash ,name sm--package-table))

(defun sm--package-install-with (package-name package-manager)
  "Installs PACKAGE-NAME with PACKAGE-MANAGER."
  (unless sm--package-refreshed-p
    (setf sm--package-refreshed-p t)
    (package-refresh-contents))
  (funcall (cdr (assoc (sm--as-symbol package-manager) sm--package-installation-function-alist))
           (sm--as-symbol package-name)))

(defun sm--package-activate-with (package-name package-manager)
  "Activates PACKAGE-NAME with PACKAGE-MANAGER."
  (funcall (cdr (assoc (sm--as-symbol package-manager) sm--package-activation-function-alist))
           (sm--as-symbol package-name)))

(defmacro* sm-package (name &key package-manager unmanaged-p)
  "Declares a package named NAME. Only one of PACKAGE-MANAGER and UNMANAGED-P must be true.
PACKAGE-MANAGER, if provided, must be one of the package managers
supported by smotitah - see `sm--supported-package-managers'."
  (declare (indent 1))
  (let ((unmanaged-p-1 (gensym)))
    `(progn
       (assert (sm--xor ,package-manager ,unmanaged-p) nil
               "Error in package '%s' declaration: one (and only one)
              of PACKAGE-MANAGER and UNMANAGED-P must be non-nil" name)
       (let ((,unmanaged-p-1 (or ,unmanaged-p (equal (sm--as-string ,package-manager) "builtin"))))
         (setf (sm--get-package ,(sm--as-string name)) (list :package ,(sm--as-string name) :package-manager ,package-manager :unmanaged-p ,unmanaged-p-1))
         (unless (or ,unmanaged-p-1 (sm--package-installed-p ,(sm--as-string name)))
           (sm--package-install-with ,(sm--as-string name) ,package-manager)
           (assert (sm--package-installed-p ,(sm--as-string name)) nil
                   "smotitah: Cannot install package %s with package manager %s."
                   ,name ,package-manager))
         (unless ,unmanaged-p-1
           (sm--package-activate-with ,(sm--as-string name) ,package-manager))))))


(defun sm--package-installed-packages ()
  "Internal. Returns the list of packages installed with package.el."
  ;; (package-initialize) where to put this?
  (mapcar #'car package-alist))

(defun sm--el-get-installed-packages ()
  "Internal. Returns the list of packages installed with el-get."
  (when (featurep 'el-get)
    (let ((p-s-alist (el-get-read-status-file)))
      (append (el-get-filter-package-alist-with-status p-s-alist "required")
	      (el-get-filter-package-alist-with-status p-s-alist "installed")))))

(defun sm--all-installed-packages ()
  "Internal. Returns all the packages installed with the package managers
listed in SM--SUPPORTED-PACKAGE-MANAGERS"
  (remove-duplicates (append (sm--package-installed-packages)
                             (sm--el-get-installed-packages))
                     :test 'equal))

(defun sm--package-package-installed-p (package-name)
  (member (sm--as-symbol package-name) 
	  (sm--package-installed-packages)))

(defun sm--el-get-package-installed-p (package-name)
  (let ((p-s-alist (el-get-read-status-file)))
    (member (sm--as-string package-name)
	    (el-get-filter-package-alist-with-status p-s-alist "installed"))))

(defun sm--package-installed-p (package-name)
  "Internal. Returns t if the package named PACKAGE-NAME is installed with
any of the package managers listed in
SM--SUPPORTED-PACKAGE-MANAGERS, nil otherwise."
  (or (sm--package-package-installed-p package-name)
      (sm--el-get-package-installed-p package-name)))

(defun* sm--package-initialize (package-name)
  "Internal. Initializes the package named PACKAGE-NAME. If MODULE-NAME is provided,
it also loads the related integration scripts.  If user-managed-p
is t, just load the package file found in .emacs.d/packages."
  (unless (featurep (sm--package-feature package-name))
    (sm-debug-msg "Requiring package %s." package-name))
  (sm-require-if-file-exists (sm--package-feature package-name)
                             (sm--package-filename package-name)))

(defun sm-package-list ()
  "Returns the list of packages managed by smotitah."
  (mapcar (lambda (package-file)
            (substring (file-name-sans-extension package-file) (length "sm-package-")))
          (directory-files sm--packages-dir nil "^sm-package-.*\\.el$")))

(defun sm-package-activated-p (package-name)
  "Checks if a package with this name is active.
This can be useful to enable or disable some code conditionally.
For example, I have a module 'evil', that integrates evil
with various other packages, and is loaded at the end of my
profile.
This module consists of a series of
  (when (sm-package-activated-p 'some-package)
     ;; some-package configuration
     )
  ...
  (when (sm-package-activated-p 'some-other-package)
     ;; some-other-package configuration
     )"
  (let ((pn (sm--as-symbol package-name)))
    (featurep (sm--package-feature package-name))))

(defmacro* sm-integrate-with (thing &body body)
  "Delimits a block of code meant to integrate the current
  package with THING.  If THING is a string or a symbol, the
  behavior is the same as `eval-after-load'.
  If THING is a list of the form
   (:module module-name) or
   (:package package-name)
  BODY will be executed after the module named module-name or
  package named package-name has been loaded."
  (declare (indent defun))
  `(eval-after-load (etypecase ',thing
                      ((or string symbol)
                       ',thing)
                      (list
                       (pcase ',thing
                         (`(:module ,module-name)
                          (sm--module-feature module-name))
                         (`(:package ,package-name)
                          (sm--package-feature package-name))
                         (_  (error "Wrong list format %s" ',thing)))))
     '(progn ,@body)))

(defun sm--package-get-version (package)
  (let ((version (cdr (assoc package package-alist))))
    (cond ((fboundp 'package-desc-vers)
	   (package-desc-vers version))
	  ((fboundp 'package-desc-version)
           (when (and (not (package-desc-p version))
                      (listp version))
             (setq version (car version)))
	   (package-desc-version version))
	  (t nil))))

(defun sm--el-get-activate-package (package-name)
  nil)

(defun sm--package-activate-package (package-name)
  "Activates a package with package.el"
  (let ((pn (sm--as-symbol package-name)))
    (package-activate pn (sm--package-get-version pn))))

;;;; ---------------------------------- Initialization -----------------------------------
(defun sm--create-directories-if-needed ()
  "Internal. Creates the profiles, modules and packages directories in your
user emacs dir if not present"
  (ignore-errors (make-directory sm--profiles-dir))
  (ignore-errors (make-directory sm--modules-dir))
  (ignore-errors (make-directory sm--packages-dir))
  (assert (every 'file-exists-p (list sm--profiles-dir sm--modules-dir sm--packages-dir))
          nil "smotitah: Cannot create profiles, modules, packages directories in %s"
          user-emacs-directory))

(defun sm--create-base-module-if-needed ()
  "Internal. Creates a stub for the profile-shared base module."
  (unless (file-exists-p sm--base-module-file-name)
    (copy-file (concat sm--template-dir "sm-module-base-template.el")
               sm--base-module-file-name)))

(defun sm--select-profile-interactively ()
  "Internal. Prompts the user to select a profile to load."
  (interactive)
  (ido-completing-read "Load Profile: " (sm-profile-list)))

;;;###autoload
(defun sm-initialize ()
  "Initializes the smotitah configuration framework. Call this in
your init file."
  (interactive)

  (when sm-packages-to-preload
    (let ((pp (split-string sm-packages-to-preload)))
      (mapc 'sm--package-initialize pp)))

  (package-initialize t)
  (when (featurep 'el-get)
    (el-get 'sync))

  (let ((profile-list (sm-profile-list))
        (modules-to-activate (getenv "EMACS_MODULES")))
    (sm--create-base-module-if-needed)
    (sm--create-directories-if-needed)
    (cond ((null modules-to-activate)
           (setq sm-profile (getenv "EMACS_PROFILE"))
           ;; Interactively prompt for profile if profiles are present
           (when (and (null sm-profile) profile-list)
             (setq sm-profile (sm--select-profile-interactively)))
           (cond ((and (null profile-list) (yes-or-no-p "No profiles found. Do you want to create one now?"))
                  ;; Create profile interactively
                  (let ((profile-name (read-from-minibuffer "Profile name: ")))
                    (sm--find-file-or-fill-template (sm--profile-filename profile-name)
                                                    sm--template-profile `(("PROFILE-NAME" . ,profile-name)))))

                 (sm-profile
                  ;; Load profile
                  (sm--load-profile sm-profile))))

          (t (sm-debug-msg "Loading modules %S." modules-to-activate)
             (let ((mm (split-string modules-to-activate "\\s-*,\\s-*" t)))
               (sm--activate-modules mm))))))

;;;; -------------------------------- Template Subsystem ---------------------------------

(defun* sm--fill-template-and-save (template-filename destination-file substitution-alist &optional (visit t))
  "Internal. This function implements a simple and limited
template system.
Saves TEMPLATE-FILENAME as DESTINATION-FILE,performing the
substitutions specified in SUBSTITUTION-ALIST.
The substitutions are represented as cons cells,
 (MATCH-STRING . REPLACE). If visit is non-nil, visit
DESTINATION-FILE (default)."
  (let ((buf (find-file-noselect template-filename)))
    (with-current-buffer buf
      (dolist (sub substitution-alist)
        (goto-char (point-min))
        (while (search-forward (car sub) nil t)
          (replace-match (cdr sub) t t)))
      (write-file destination-file))
    (kill-buffer buf))
  (when visit
    (setq inhibit-splash-screen t)
    (find-file destination-file)))

(defun sm--find-file-or-fill-template (filename template-filename substitutions)
  "If the file pointed by FILENAME does not exist, fills
TEMPLATE-FILENAME as specified by SUBSTITUTIONS - see
`sm--fill-template-and-save' - and saves the results to
FILENAME. Visits FILENAME."
  (if (file-exists-p filename)
      (find-file filename)
    (sm--fill-template-and-save template-filename filename substitutions)))


;;;; ----------------------------------- User commands -----------------------------------

(defun sm-integrate-module (profile-name module-name)
  "Opens the integration files that integrate the module named
MODULE-NAME in the profile named PROFILE-NAME."
  (interactive (let ((pname (ido-completing-read "Integrate in profile: " (sm-profile-list) nil t sm-profile))
                     (mname (ido-completing-read "Integrate module: " (sm-module-list) nil nil)))
                 (list pname mname)))
  (let ((integration-pre-filename (concat (sm--profile-module-integration-file profile-name module-name :pre) ".el"))
        (integration-post-filename (concat (sm--profile-module-integration-file profile-name module-name :post) ".el")))
    (sm--find-file-or-fill-template integration-post-filename sm--template-module-integration
                                    `(("PROFILE-NAME" . ,profile-name) ("MODULE-NAME" . ,module-name)
                                      ("STAGE" . "post")))
    (sm--find-file-or-fill-template integration-pre-filename
                                    sm--template-module-integration
                                    `(("PROFILE-NAME" . ,profile-name) ("MODULE-NAME" . ,module-name)
                                      ("STAGE" . "pre")))))


;;;###autoload
(defun sm-edit-profile (profile-name)
  "Opens the profile file for the profile named PROFILE-NAME."
  (interactive (list (ido-completing-read "Edit Profile: " (sm-profile-list) nil nil sm-profile)))
  (sm--find-file-or-fill-template (concat (sm--profile-filename profile-name) ".el")
                                  sm--template-profile
                                  `(("PROFILE-NAME" . ,profile-name))))

;;;###autoload
(defun sm-edit-module (module-name)
  "Opens the module file for the module named MODULE-NAME."
  (interactive (list (ido-completing-read "Edit Module: " (sm-module-list))))
  (sm--find-file-or-fill-template (concat (sm--module-filename module-name) ".el")
                                  sm--template-module
                                  `(("MODULE-NAME" . ,module-name))))

;;;###autoload
(defun sm-edit-package (package-name)
  "Opens the package file for the package named PACKAGE-NAME."
  (interactive (list (ido-completing-read "Edit Package: " (sm-package-list))))
  (sm--find-file-or-fill-template (concat (sm--package-filename package-name) ".el")
                                  sm--template-package
                                  `(("PACKAGE-NAME" . ,package-name) ("PACKAGEMANAGER" . "nil") ("UNMANAGEDP" . "t"))))


;;; This advices hooks after package-install creating a clean package file
;;; for all the packages that do not still have one.

(defun sm--ensure-package-files-exist (package-manager)
  (let ((package-manager-installed-packages
	 (funcall (cdr (assoc package-manager
			      sm--package-list-function-alist )))))
    (dolist (p package-manager-installed-packages)
      (let* ((package-name (sm--as-string p))
	     (sm-package-file (concat (sm--package-filename package-name) ".el")))
	(unless (file-exists-p sm-package-file)
	  (sm--fill-template-and-save sm--template-package
				      sm-package-file
				      `(("PACKAGE-NAME" . ,package-name)
					("PACKAGEMANAGER" . ,(format "%S" (sm--as-string package-manager)))
					("UNMANAGEDP" . "nil"))
				      nil))))))

(defadvice package-install (after sm-offer-package-file-creation (package-or-name) activate)
  (sm--ensure-package-files-exist 'package))

(defadvice el-get-do-install (after sm-el-get-offer-package-file-creation (package-name) activate)
  (sm--ensure-package-files-exist 'el-get))

;;;; ------------------------------------ Compilation ------------------------------------

(defun sm-recompile-all ()
  (interactive)
  (byte-recompile-directory sm--profiles-dir 0 t)
  (byte-recompile-directory sm--modules-dir 0 t)
  (byte-recompile-directory sm--packages-dir 0 t)
  (byte-recompile-file (concat sm--directory "smotitah.el") t 0))

;;;; ---------------------- Indentation kludges for macros ------------------------
(put 'sm-profile-pre 'lisp-indent-function 1)
(put 'sm-module-pre 'lisp-indent-function 1)
(put 'sm-profile-post 'lisp-indent-function 1)
(put 'sm-module-post 'lisp-indent-function 1)

(provide 'smotitah)
;;; smotitah.el ends here
