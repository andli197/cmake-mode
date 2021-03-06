';;; cmake-mode --- Toolkit for building projects using cmake

;; Author: Andreas Lindblad
;; Keywords: extensions, tools, cmake
;; URL: http://github.com/andli197/cmake-mode
;; Emacs: GNU Emacs 24 (or later)
;; Package-Requires:
;; Version: 0.1.0

(defgroup cmake-mode nil
  "cmake buildsystem mode"
  :group 'tools
  :tag "cmake mode")

;;;###autoload
(define-minor-mode cmake-mode
  "Small cmake project mode."
  :lighter " cmake"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-c c") 'cmake-mode-add-run-cmake)
            (define-key map (kbd "C-c C-c b") 'cmake-mode-add-run-custom-build)
            (define-key map (kbd "C-c C-c t") 'cmake-mode-add-selected-test)
            (define-key map (kbd "C-c C-c T") 'cmake-mode-add-run-tests)
            (define-key map (kbd "C-c C-c x") 'cmake-mode-add-selected-executable)
            map)
  :group 'cmake-mode)

;;;###autoload
(add-hook 'c++-mode-hook 'cmake-mode)

(defvar cmake-mode--cmake-build-types-list
  '(("Debug" "debug" "DEBUG")
    ("Release" "release" "RELEASE")
    ("Relase with debug info" "release_with_debug_info" "RELWITHDEBINFO")
    ("Minimal size release" "minimal_size_release" "MINSIZEREL"))
  "Types of builds supported by cmake.")

(defvar cmake-mode-build-type (car cmake-mode--cmake-build-types-list))
(defvar cmake-mode-operating_system "")
(defvar cmake-mode-selected-test-to-run "")
(defvar cmake-mode--extra-arguments-history '("{command}"))
(defvar cmake-mode--execute-command-history '())
(defvar cmake-mode--execute-argument-history '())
(defvar cmake-mode-execute-prefix-commands '()
  "Hook to generate commands to be added before the executable")
 
(defcustom cmake-mode-available-operating-system
  '("linux" "centos6" "centos7")
  "Types of operating systems to build for."
  :type 'list
  :group 'cmake-mode)

(defcustom cmake-mode-build-folder-name
  "build"
  "Folder name to build in."
  :type 'string
  :group 'cmake-mode)
 
(defcustom cmake-mode-install-folder-name
  "install"
  "Folder name to install to."
  :type 'string
  :group 'cmake-mode)
 
(defcustom cmake-mode-generator
  "Unix Makefiles"
  "Generator to use with the cmake-mode"
  :type 'string
  :group 'cmake-mode)
 
(defcustom cmake-mode-build-settings-file-name
  ".cmake-mode.el"
  "File to read and write the current selection to."
  :type 'string
  :group 'cmake-mode)

(defun user/completing-read-item (prompt choises display-fn &optional predicate require-match initial-input history def inherit-input-method)
  "Call `ido-completing-read' with PROMPT and a transofmration of CHOISES with DISPLAY-FN along with PROMPT, PREDICATE, REQUIRE-MATCH, INITIAL-INPUT, HISTORY, DEF and INHERIT-INPUT-METHOD.  Based on selection the method use the DISPLAY-FN to deduce the item from CHOISES and return the whole item.  The DISPLAY-FN must be forcing an unique string from the CHOISES list."
  (interactive)
  (let* ((display-items (seq-map display-fn choises))
         (display-selection
	  (ido-completing-read
	   prompt display-items predicate require-match initial-input history
	   def inherit-input-method)))
    (seq-find
     (lambda (item) (string= (funcall display-fn item) display-selection))
     choises)))
 
(defun cmake-mode-add-run-cmake ()
  "Add cmake command to execution model."
  (interactive)
  (cmake-mode-read-build-file)
  (cmake-mode-compile
   (concat "{(cmake-mode-generate-cd-build-path)} && "
	   (cmake-mode-generate-cmake-command))))
 
(defun cmake-mode-add-run-custom-build ()
  "Add build command to execution model."
  (interactive)
  (cmake-mode-read-build-file)
  (let ((target (ido-completing-read "target: " (cmake-mode--target-read-all))))
    (cmake-mode-compile
     (concat "{(cmake-mode-generate-cd-build-path)} && "
	     (cmake-mode-generate-build-target target)))))

(defun cmake-mode-add-run-tests ()
  "Add unittests command to execution model."
  (interactive)
  (cmake-mode-read-build-file)
  (cmake-mode-compile
   (concat "{(cmake-mode-generate-cd-build-path)} && "
	   cmake-mode--generate-test-command)))

(defun cmake-mode-add-selected-test ()
  "Select unittest from target and add tp execution model."
  (interactive)
  (cmake-mode-write-build-file)
  (cmake-mode-add-to-fluent-with-extra (cmake-mode-select-test)))
 
(defun cmake-mode-read-with-history (prompt history)
  (read-string prompt (or (car (eval history)) "") history))

(defun cmake-mode-add-selected-executable ()
  "Propmpt user for command and arguments for executable to execute."
  (interactive)
  (cmake-mode-read-build-file)
  (let* ((command (cmake-mode-read-with-history
                   "command: "
                   'cmake-mode--execute-command-history))
        (arguments (cmake-mode-read-with-history
                    "arguments: "
                    'cmake-mode--execute-argument-history))
        (commands-prefix (seq-map (lambda (c) (funcall c))
                                  (reverse cmake-mode-execute-prefix-commands)))
        (execution-command (list "{(cmake-mode-generate-cd-install-path)}"
                                 (concat command " " arguments)))
        (commands (seq-concatenate 'list commands-prefix execution-command)))
    (cmake-mode-add-to-fluent-with-extra
     (mapconcat 'identity commands " && "))))
 
(defun cmake-mode-add-to-fluent-with-extra (command)
  "Add COMMAND to execution and give the user possibility to add extra to the command."
  (interactive)
  (let ((new-command
	 (read-string "additional: "
		      (car cmake-mode--extra-arguments-history)
		      'cmake-mode--extra-arguments-history)))
    (fluent-add
     (replace-regexp-in-string
      (regexp-quote "{command}")
      command
      new-command))))
 
(defun cmake-mode-compile (command)
  "Add COMMAND to fluent execution."
  (interactive)
  (if (bound-and-true-p fluent-mode)
      (fluent-add command)
    (compile (mapconcat 'identity command " && "))))

(defun cmake-mode--cmake-build-types-get-human-readable-name (item)
  "Return human readable name from ITEM."
  (car item))
 
(defun cmake-mode--get-foldername-from-selected-build-type ()
  "Return folder name from ITEM."
  (cadr cmake-mode-build-type))
 
(defun cmake-mode--get-selected-build-type ()
  "Return cmake build type from ITEM."
  (car (cddr cmake-mode-build-type)))
 
(defun cmake-mode-select-build-and-os ()
  "Read current settings and prompt the user for which os and build type to use then write back the selections."
  (interactive)
  (cmake-mode-read-build-file)
  (setq cmake-mode-operating_system
	(ido-completing-read "Select OS:" cmake-mode-available-operating-system))
  (setq cmake-mode-build-type
	(user/completing-read-item
	 "Select build type: "
	 cmake-mode--cmake-build-types-list
	 'cmake-mode--cmake-build-types-get-human-readable-name))
  (cmake-mode-write-build-file))
 
(defun cmake-mode-get-sub-path ()
  "Return the subpath specified by selected operating-system and build-type."
  (file-name-as-directory
   (concat (file-name-as-directory cmake-mode-operating_system)
           (cmake-mode--get-foldername-from-selected-build-type))))
 
(defun cmake-mode-get-sibling-folder-name (base-path directory)
  "Return a sibling directory to BASE-PATH named DIRECTORY."
  (file-name-as-directory
   (concat (file-name-directory (directory-file-name base-path)) directory)))

(defun cmake-mode-get-build-path ()
  "Return the path where to execute buid."
  (let ((build-path (concat (cmake-mode-get-build-root)
			    (cmake-mode-get-sub-path))))
    (make-directory build-path :parents)
    build-path))
 
(defun cmake-mode-get-install-path ()
  "Return the path where to install."
  (concat (cmake-mode-get-install-root) (cmake-mode-get-sub-path)))
 
(defun cmake-mode-get-code-path ()
  "Return the path where the project is located."
  (file-name-as-directory (or (getenv "CWD") (projectile-project-root))))
 
(defun cmake-mode-generate-cmake-command ()
  "Command to execute cmake with."
  (concat
   "cmake"
   " -DCMAKE_BUILD_TYPE="
   (cmake-mode--get-selected-build-type)
   " -DCMAKE_INSTALL_PREFIX="
   (cmake-mode-get-install-path)
   " -DCMAKE_EXPORT_COMPILE_COMMANDS=ON"
   " -G\"{cmake-mode-generator}\""
   " " (cmake-mode-get-code-path)
   ))
 

(defun cmake-mode-generate-build-file-content ()
  "Content to write to build file."
  (concat
   "(setq\n"
   " cmake-mode-operating_system \"" cmake-mode-operating_system "\"\n"
   " cmake-mode-build-type '" (format "%s" (mapcar (lambda (e) (concat "\"" e "\"")) cmake-mode-build-type)) "\n"
   " cmake-mode-build-folder-name \"" cmake-mode-build-folder-name "\"\n"
   " cmake-mode-install-folder-name \"" cmake-mode-install-folder-name "\"\n"
   " )\n"
   ))
 
(defun cmake-mode-read-build-file ()
  "Read the settings from build-file."
  (interactive)
  (let ((cmake-mode-build-file
	 (concat (cmake-mode-get-code-path)
		 cmake-mode-build-settings-file-name)))
    (when (file-exists-p cmake-mode-build-file)
      (load cmake-mode-build-file)
      (message (concat cmake-mode-build-file " read")))))
 
(defun cmake-mode-write-build-file ()
  "Write the settings to build-file."
  (interactive)
  (let ((cmake-mode-build-file
         (concat (cmake-mode-get-code-path)
                 cmake-mode-build-settings-file-name)))
    (with-temp-file
	cmake-mode-build-file
      (insert (cmake-mode-generate-build-file-content)))
    (message (concat cmake-mode-build-file " written"))))

;; Command generating section
(defun cmake-mode-generate-cd-build-path ()
  "Generate code for changing directory to the build path."
  (concat "cd " (cmake-mode-get-build-path)))
 
(defun cmake-mode-generate-cd-install-path ()
  "Generate code for changing directory to the install path."
  (concat "cd " (cmake-mode-get-install-path)))
 
(defun cmake-mode-generate-build-target (target)
  "Command for generating TARGET in build."
  (concat "cmake --build . --target " target))
 
(defvar cmake-mode--generate-test-command
  "ctest -j8 --output-on-failure"
  "Command for running all test.")
 
(defun re-seq (regexp string)
  "Fetch all match in the REGEXP applied to the STRING and return it as a list."
  (save-match-data
    (let ((pos 0) matches)
      (while (string-match regexp string pos)
        (push (match-string 0 string) matches)
        (setq pos (match-end 0)))
      matches)))
  
(defvar cmake-mode--testcase-extract-regexp
  "^[0-9]+: Test command: \\([-_.a-zA-Z0-9\\/\\:]+\\).*\n[ ]+Test[ ]+#[0-9]+: \\(.+\\)"
  "Regexp for extracting test names and executables from verbose ctest output.")
 
(defvar cmake-mode--testcase-list-all-command
  "ctest -N -V"
  "Command for reading all tests.")

(defvar cmake-mode-pre-cmake-commands
  '()
  "A set of commands to set up the shell before running cmake commands.")

(defun cmake-mode--build-pre-cmake-commands ()
  "Concatinate all cmake-mode-pre-cmake-commands with double ampersands and combine into a single string."
  (mapconcat 'identity
             (seq-map (lambda (cmd) (cond ((stringp cmd) cmd)
                                             ((functionp cmd) (funcall cmd))
                                             (t '())))
                         cmake-mode-pre-cmake-commands)
             " && "))

(defun cmake-mode--testcase-read-all ()
  "Read all test defined in the cmake project and return list of pairs as (NAME . COMMAND)."
  (let*  ((command (cmake-mode--build-pre-cmake-commands)
                   (cmake-mode-generate-cd-build-path)
                   cmake-mode--testcase-list-all-command))
          (ctest-output (shell-command-to-string
                         (mapconcat 'identity command " && ")))
          (match-seq (reverse
                      (re-seq cmake-mode--testcase-extract-regexp
                              ctest-output))))
    (mapcar 'cmake-mode--testcase-extract-name-and-command match-seq))

(defun cmake-mode--testcase-extract-name-and-command (match)
  "Parse a MATCH to (NAME . COMMAND) pair."
  (save-match-data
    (string-match cmake-mode--testcase-extract-regexp match)
    (cons (match-string 2 match) (replace-regexp-in-string (regexp-quote "\\") "/" (match-string 1 match)))))
 
(defvar cmake-mode--target-extract-regexp
  "\\(.+\\): .+"
  "Regexp for extracting targets from the cmake project help target.")
 
(defvar cmake-mode--target-list-all-command
  "cmake --build . --target help"
  "Command for how to list all targets.")
 
(defun cmake-mode--target-read-all ()
  "Read all target from cmake file and return all targets listed as phony."
  (let* ((command (list (cmake-mode--build-pre-cmake-commands)
                        (cmake-mode-generate-cd-build-path)
                        cmake-mode--target-list-all-command))
         (targets-output (shell-command-to-string
                          (mapconcat 'identity command " && ")))
         (match-seq (reverse
                     (re-seq cmake-mode--target-extract-regexp
                             targets-output))))
    (cons "all" (mapcar 'cmake-mode--target-get-name match-seq))))
 
(defun cmake-mode--target-get-name (match)
  "Parse a MATCH to target name to return."
  (save-match-data
    (string-match cmake-mode--target-extract-regexp match)
    (match-string 1 match)))
 
(defvar cmake-mode--selected-test-item (cons '() '()))
(defun cmake-mode-select-test ()
  "Select test to be executed."
  (interactive)
  (setq cmake-mode--selected-test-item
        (user/completing-read-item
         "Select test: "
         (cmake-mode--testcase-read-all)
         'cmake-mode--get-test-name-from-item))
  (setq cmake-mode-selected-test-to-run
        (cmake-mode--get-test-command-from-item cmake-mode--selected-test-item)))
 
(defun cmake-mode--get-test-name-from-item (item)
  "Return the name of the test from the ITEM."
  (car item))
 
(defun cmake-mode--get-test-command-from-item (item)
  "Return the command for the test from the test ITEM."
  (cdr item))
 
(defun cmake-mode-list-available-tests ()
  "List the names of all available test."
  (mapcar 'cmake-mode--get-test-name-from-item (cmake-mode--testcase-read-all)))
 
(defun cmake-mode--get-test-item-from-name (test-name)
  "Extract the test command from a given TEST-NAME."
  (seq-find
   (lambda (pair) (string= (car pair) test-name))
   (cmake-mode--testcase-read-all)))
 
(defun cmake-mode-get-build-root ()
  "Return the deduced build root."
  (cmake-mode-get-sibling-folder-name
   (cmake-mode-get-code-path)
   cmake-mode-build-folder-name))
 
(defun cmake-mode-get-install-root ()
  "Return the deduced install root."
  (cmake-mode-get-sibling-folder-name
   (cmake-mode-get-code-path)
   cmake-mode-install-folder-name))

(provide 'cmake-mode)
;;; cmake-mode ends here
