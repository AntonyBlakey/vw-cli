;;; package --- summary -*- lexical-binding: t; -*-
;;; COMMENTARY:
;;; CODE:

(require 'use-package)

(use-package cl-lib :ensure t)
(use-package seq :ensure t)
(use-package pcomplete :ensure t)
(use-package eshell :ensure t)
(use-package f :ensure t)
(use-package ansi-color :ensure t)

;;;
;;;
;;;

(defconst *vw-os-root-dirs*
  (list (cons nil "/Users/antony/Development/Cincom/Visualworks/")
        '(osx . "/Visualworks/")
        '(windows . "Z:/-Visualworks/")
        '(linux . "/Visualworks/")
        '(solaris . "/Visualworks/")))

(defconst *vw-root-dir* (cl-rest (cl-assoc nil *vw-os-root-dirs*)))
(defconst *vw-scripts-dir* (concat *vw-root-dir* "scripts/"))
(defconst *vw-vagrant-machines-dir* (concat *vw-root-dir* "vagrant-machines/"))
(defconst *vw-temp-dir* (concat *vw-root-dir* "temp/"))
(defconst *vw-releases-dir* (concat *vw-root-dir* "releases/"))
(defconst *vw-images-dir* (concat *vw-root-dir* "images/"))
(defconst *vw-backup-images-dir* (concat *vw-root-dir* "backup-images/"))

(defun vw-convert-file-name-prefix-for-os (file-name os)
  "Replace the local path prefix in FILE-NAME with one suitable for a vagrant machine running OS."
  (concat (cl-rest (cl-assoc os *vw-os-root-dirs*)) (substring file-name (length *vw-root-dir*))))

(defun vw-convert-file-name-for-os (file-name os)
  "Replace the local path prefix in FILE-NAME with one suitable for a vagrant machine running OS, and translate path separators if required."
  (let ((path (vw-convert-file-name-prefix-for-os file-name os)))
    (if (eq os 'windows) (replace-regexp-in-string "/" "\\" path nil t) path)))

(defun vw-directory-leaf-name (dir-name)
  "Return the leaf name of DIR-NAME."
  (file-name-nondirectory (directory-file-name dir-name)))

(defun vw-directory-symlink-target (dir-name)
  "Return the target of directory symlink DIR-NAME or nil if it's not a symlink."
  (file-symlink-p (directory-file-name dir-name)))

(defun vw-file-normal-directory-p (filename)
  "Return true if the FILENAME is a directory, but not '.' or '..'."
  (and (file-directory-p filename)
       (not (member (file-name-nondirectory filename) '("." "..")))))

(defun vw-directory-subdirectories (dir)
  "Return all the subdirectories of the given DIR, excluding . and .."
  (mapcar 'file-name-as-directory (mapcar 'expand-file-name (seq-filter 'vw-file-normal-directory-p (directory-files dir t)))))

(defconst *vw-single-quote* "\"'\"")

;;;
;;;
;;;

(cl-defstruct (vw-system (:conc-name vw-system.)) images releases)
(cl-defstruct (vw-release (:conc-name vw-release.)) dev-cycle name tags images)
(cl-defstruct (vw-image (:conc-name vw-image.)) name release width)


;;;
;;; vw-system
;;;

(defun vw-system.make ()
  "Return a configured vw-system."
  (let ((system (make-vw-system)))

    ;; Add all the discovered releases
    (dolist (dev-cycle-dir (vw-directory-subdirectories *vw-releases-dir*))
      (let* ((dev-cycle (vw-directory-leaf-name dev-cycle-dir))
             (release-dirs (vw-directory-subdirectories dev-cycle-dir))
             (grouped-release-dirs (seq-group-by 'vw-directory-symlink-target release-dirs)))
        (dolist (release-dir (cl-rest (cl-assoc nil grouped-release-dirs)))
          (let* ((name (vw-directory-leaf-name release-dir))
                 (tag-dirs (cl-rest (cl-assoc name grouped-release-dirs :test 'string-equal)))
                 (tags (mapcar 'vw-directory-leaf-name tag-dirs))
                 (release (make-vw-release :dev-cycle dev-cycle :name name :tags tags)))
            (push release (vw-system.releases system))))))

    ;; Sort the releases
    (setf (vw-system.releases system) (seq-sort 'vw-release.< (vw-system.releases system)))

    ;; Add all the discovered images
    (dolist (image-dir (vw-directory-subdirectories *vw-images-dir*))
      (let* ((name (vw-directory-leaf-name image-dir))
             (width (if (file-exists-p (concat image-dir "visual32.im")) 32 64))
             (release-dir (file-chase-links (concat image-dir "release")))
             (release (vw-system.find-release-with-directory system release-dir))
             (image (make-vw-image :name name :release release :width width)))
        (push image (vw-system.images system))
        (when release (push image (vw-release.images release)))))

    ;; Sort the images
    (setf (vw-system.images system) (seq-sort 'vw-image.< (vw-system.images system)))

    ;; Sort the list of images in each release
    (dolist (release (vw-system.releases system))
      (setf (vw-release.images release) (seq-sort 'vw-image.< (vw-release.images release))))

    ;; Return the system
    system))

(defun vw-system.image-names (system)
  "Return a list of all the images in SYSTEM."
  (mapcar 'vw-image.name (vw-system.images system)))

(defun vw-system.find-image-with-name (system name)
  "Get the image from SYSTEM named NAME."
  (seq-find (lambda (image) (string-equal name (vw-image.name image)))
            (vw-system.images system)))

(defun vw-system.release-directories (system)
  "Return a list of all directories for all the releases in SYSTEM, including tag directories."
  (seq-sort 'string< (apply 'append (mapcar 'vw-release.directories-including-tags (vw-system.releases system)))))

(defun vw-system.find-release-with-directory (system directory)
  "Get the release from SYSTEM that has directory DIRECTORY, including tag directories."
  (let ((absolute-directory (file-name-as-directory (expand-file-name directory))))
    (seq-find (lambda (release) (seq-contains (vw-release.directories-including-tags release) absolute-directory))
              (vw-system.releases system))))

(defun vw-system.vagrant-machine-names ()
  (mapcar 'file-name-nondirectory (seq-filter 'vw-file-normal-directory-p (directory-files *vw-vagrant-machines-dir* t))))


;;;
;;; vw-release
;;;

(defun vw-release.< (a b)
  "This predicate orders two releases A and B."
  (string< (concat (vw-release.dev-cycle a) "/" (vw-release.name a))
           (concat (vw-release.dev-cycle b) "/" (vw-release.name b))))

(defun vw-release.directory (release)
  "Return the directory containing RELEASE, for os context OS."
  (concat *vw-releases-dir* (vw-release.dev-cycle release) "/" (vw-release.name release) "/"))

(defun vw-release.imagefile-name (release width)
  "Return the name of the imagefile for RELEASE with word size width."
  (concat (vw-release.directory release) "image/visual" (if (= width 32) "" "64") ".im"))

(defun vw-release.executablefile-name (release width os)
  "Return the executable used in RELEASE using to run images of WIDTH on OS."
  (concat (vw-release.directory release)
          (cond
           ((and (eq os 'windows) (= 32 width)) "bin/win/visual.exe")
           ((and (eq os 'windows) (= 64 width)) "bin/win64/visual.exe")
           ((and (eq os 'linux) (= 32 width)) "bin/linux86/visual")
           ((and (eq os 'linux) (= 64 width)) "bin/linuxx86_64/visual")
           ((and (eq os 'solaris) (= 32 width)) "bin/solaris/visual")
           ((and (eq os 'solaris) (= 64 width)) "bin/solaris64/visual")
           (t "bin/macx/visual.app/Contents/MacOS/visual"))))

(defun vw-release.directories-including-tags (release)
  "Return a list of directories associated with the RELEASE."
  (let* ((directory (vw-release.directory release))
         (result (list directory))
         (parent-directory (file-name-directory (directory-file-name directory))))
    (dolist (tag (vw-release.tags release) result)
      (push (file-name-as-directory (concat parent-directory tag)) result))))

(defun vw-release.description (release &optional column-width)
  "Return a string describing RELEASE, optionally padded to COLUMN-WIDTH characters."
  (vw-pad-string (concat (vw-release.dev-cycle release) "/" (vw-release.name release)
                         (if (vw-release.tags release)
                             (concat " (" (mapconcat 'identity (vw-release.tags release) ", ") ")")
                           ""))
                 (or column-width 0)))

(defun vw-release.create-image (release name width)
  "Create a new image from RELEASE named NAME with word size width."
  (let* ((image (make-vw-image :name name :release release :width width)))
    (make-directory (vw-image.directory image))
    (f-symlink (f-relative (vw-release.directory release) (vw-image.directory image)) (vw-image.releasesymlinkfile-name image))
    (copy-file (vw-release.imagefile-name release width) (vw-image.imagefile-name image))
    (set-file-modes (vw-image.imagefile-name image) #o644)))


;;;
;;; vw-image
;;;

(defun vw-image.< (a b)
  "This predicate orders two images A and B."
  (string< (vw-image.name a) (vw-image.name b)))

(defun vw-image.directory (image)
  "Return the directory containing IMAGE."
  (concat *vw-images-dir* (vw-image.name image) "/"))

(defun vw-image.backups-directory (image)
  "Return the directory containing IMAGE."
  (concat (vw-image.directory image) "backups/"))

(defun vw-image.imagefile-leaf-name (image)
  "Return the directory containing the image data."
  (format "visual%s.im" (vw-image.width image)))

(defun vw-image.imagefile-name (image)
  "Return the directory containing the image data."
  (concat (vw-image.directory image) (vw-image.imagefile-leaf-name image)))

(defun vw-image.changesfile-leaf-name (image)
  "Return the directory containing the image data."
  (format "visual%s.cha" (vw-image.width image)))

(defun vw-image.changesfile-name (image)
  "Return the directory containing the image data."
  (concat (vw-image.directory image) (vw-image.changesfile-leaf-name image)))

(defun vw-image.releasesymlinkfile-name (image)
  "Return the name of the release symlink file for IMAGE."
  (concat (vw-image.directory image) "release"))

(defun vw-image.executablefile-name (image os)
  "Return the file name of the executable used to run IMAGE on OS."
  (vw-release.executablefile-name (vw-image.release image) (vw-image.width image) os))

(defun vw-image.description (image &optional column-width)
  "Return a string describing IMAGE, optionally padded to COLUMN-WIDTH characters for the name."
  (format "%s | %s" (vw-image.width image) (vw-pad-string (vw-image.name image) (or column-width 0))))

(defun vw-image.execute (image vagrant-machine-name script-file-name buffer-name)
  "Execute IMAGE on VAGRANT-MACHINE-NAME processing SCRIPT-FILE-NAME."
  (let* ((release (vw-image.release image))
         (os (vw-vagrant-machine-os vagrant-machine-name))
         (temporary-file-directory *vw-temp-dir*)

         (user-script-file-name (when script-file-name
                                  (let ((file-name (make-temp-file "user-" nil ".st"))
                                        (full-script-file-name (concat *vw-scripts-dir* script-file-name)))
                                    (with-temp-file file-name
                                      (insert-file-contents full-script-file-name)
                                      (vw-replace-string "{{SCRIPT.ORIGINAL_FILENAME}}" (vw-convert-file-name-for-os full-script-file-name os))
                                      (vw-replace-string "{{SCRIPT.FILENAME}}" (vw-convert-file-name-for-os file-name os))
                                      (vw-replace-string "{{SYSTEM.ROOT-DIRECTORY}}" (vw-convert-file-name-for-os *vw-root-dir* os))
                                      (vw-replace-string "{{SYSTEM.SCRIPTS-DIRECTORY}}" (vw-convert-file-name-for-os *vw-scripts-dir* os))
                                      (vw-replace-string "{{RELEASE.DIRECTORY}}" (vw-convert-file-name-for-os (vw-release.directory release) os))
                                      (vw-replace-string "{{RELEASE.DEV-CYCLE}}" (vw-release.dev-cycle release))
                                      (vw-replace-string "{{RELEASE.NAME}}" (vw-release.name release))
                                      (vw-replace-string "{{IMAGE.DIRECTORY}}" (vw-convert-file-name-for-os (vw-image.directory image) os))
                                      (vw-replace-string "{{IMAGE.NAME}}" (vw-image.name image))
                                      (vw-replace-string "{{IMAGE.WIDTH}}" (vw-image.width image)))
                                    file-name)))

         (inner-script-file-name (let ((file-name (make-temp-file "inner-" nil ".sh"))
                                       (executablefile-name (vw-release.executablefile-name release (vw-image.width image) os))
                                       (imagefile-name (vw-image.imagefile-name image)))
                                   (with-temp-file file-name
                                     (when script-file-name
                                       (insert "echo 'Script  : " (f-relative (concat *vw-scripts-dir* script-file-name) *vw-root-dir*) "'\n"))
                                     (insert "echo 'VM      : " (f-relative executablefile-name *vw-root-dir*) "'\n"
                                             "echo 'VI      : " (f-relative (vw-image.imagefile-name image) *vw-root-dir*) "'\n"
                                             "echo 'Date    : " (current-time-string) " " (cl-second (current-time-zone)) "'\n"
                                             "echo '---------------------------------------------------------------------------------------'\n"
                                             "export DISPLAY=:0\n" ;; just for X11, doesn't affect any other platform
                                             "export VISUALWORKS='" (vw-convert-file-name-for-os (vw-release.directory release) os) "'\n"
                                             "'" (vw-convert-file-name-prefix-for-os executablefile-name os) "' "
                                             "'" (vw-convert-file-name-for-os imagefile-name os) "' ")
                                     (when user-script-file-name
                                       (insert "-doit 'Compiler evaluate: '"
                                               *vw-single-quote*
                                               "'" (vw-convert-file-name-for-os user-script-file-name os) "'"
                                               *vw-single-quote*
                                               "' asFilename contentsOfEntireFile. '"
                                               *vw-single-quote* *vw-single-quote*))
                                     (insert "\n"))
                                   file-name))

         (outer-script-file-name (when vagrant-machine-name
                                   (let ((file-name (make-temp-file "outer-" nil ".sh"))
                                         (vagrant-machine-dir (concat *vw-vagrant-machines-dir* vagrant-machine-name)))
                                     (with-temp-file file-name
                                       (insert "echo 'Machine : " (f-relative vagrant-machine-dir *vw-root-dir*) "'\n")
                                       (insert "cd " vagrant-machine-dir "\n")
                                       (insert "vagrant up\n")
                                       (insert "vagrant ssh -- "
                                               *vw-single-quote*
                                               "'" (vw-convert-file-name-prefix-for-os inner-script-file-name os) "'"
                                               *vw-single-quote*
                                               "\n"))
                                     file-name))))

    (let* ((process (start-process "VisualWorks" nil "/bin/bash" (or outer-script-file-name inner-script-file-name)))
           (buffer (get-buffer-create (or buffer-name (process-name process))))
           (sentinel (lambda (process event)
                       (when (not (eq 'run (process-status process)))
                         (when user-script-file-name (f-delete user-script-file-name))
                         (when outer-script-file-name (f-delete outer-script-file-name))
                         (f-delete inner-script-file-name))
                       (when (buffer-live-p (process-buffer process))
                         (with-current-buffer (process-buffer process)
                           (goto-char (point-max))
                           (insert "\n\n---------------------------------------------------------------------------------------\n")
                           (insert "Process " (process-name process) " " event))))))
      (set-process-sentinel process sentinel)
      (buffer-disable-undo buffer)
      (save-selected-window
        (split-window-below-and-focus)
        (save-current-buffer
          (switch-to-buffer buffer t)
          (erase-buffer)
          (set-process-buffer process buffer)
          (let ((output-filter (lambda (process string)
                                 (let ((char-filter (lambda (c) (= c ?\r))))
                                   (when (buffer-live-p (process-buffer process))
                                     (with-current-buffer (process-buffer process)
                                       (let ((point-was-at-end (= (point) (point-max))))
                                         (save-excursion
                                           (goto-char (point-max))
                                           (let ((start-point (point)))
                                             (insert (concat (seq-remove char-filter string)))
                                             (ansi-color-apply-on-region start-point (point))))
                                         (when point-was-at-end (goto-char (point-max))))))))))
            (set-process-filter process output-filter))
          (goto-char (point-max))
          (process-send-eof process))))))


;;;
;;; Utility functions
;;;

(defun vw-vagrant-machine-os (vagrant-machine-name)
  "Return the operating system of VAGRANT-MACHINE-NAME, which is one of 'windows, 'osx, 'solaris, 'linux."
  (and vagrant-machine-name
       (with-temp-buffer
         (insert-file-contents (concat (file-name-as-directory (concat *vw-vagrant-machines-dir* vagrant-machine-name)) "Vagrantfile"))
         (re-search-forward "config.vm.box[ \t]* =[ \t]*\"\\([^\"]*\\)\"")
         (let ((box (match-string 1)))
           (cond
            ((string-prefix-p "win" box) 'windows)
            ((string-prefix-p "osx" box) 'osx)
            ((string-prefix-p "solaris" box) 'solaris)
            (t 'linux))))))

(defun vw-replace-string (from to)
  "Replace string FROM with string TO in the current buffer.  Normal elisp 'replace-string' is interactive only."
  (goto-char (point-min))
  (while (search-forward from nil t)
    (replace-match to t t)))

(defun vw-pad-string (string width)
  "Pad STRING on the right with spaces so that the string is at least WIDTH characters long."
  (concat string (make-string (max 0 (- width (length string))) ?\s)))

;; (defun vw-extract-version-from-image-file (image)
;;   "Extract the version signature from a visualworks IMAGE file."
;;   (with-temp-buffer
;;     (insert-file-contents-literally image nil 68 72)
;;     (vconcat (buffer-string))))


;;;
;;; EShell commands and pcompleters
;;;

(defun eshell/vw-init (release-dir width image-name)
  "Create new visualworks image from RELEASE-DIR, word size WIDTH, saved as IMAGE-NAME."
  (let ((release (vw-system.find-release-with-directory (vw-system.make) release-dir)))
    (vw-release.create-image release image-name width)))

(defun pcomplete/vw-init ()
  "PCompleter for the vw-init command."
  (pcomplete-here (vw-system.release-directories (vw-system.make)))
  (pcomplete-here '("32" "64")))

(defun eshell/vw-list-images ()
  "List the visualworks images."
  (eshell-flush -1)
  (let* ((images (vw-system.images (vw-system.make)))
         (column-width (apply 'max (mapcar 'length (mapcar 'vw-image.name images)))))
    (dolist (image images)
      (eshell-buffered-print (vw-image.description image column-width)
                             " | "
                             (let ((release (vw-image.release image)))
                               (and release (vw-release.description (vw-image.release image))))
                             "\n")))
  (eshell-flush))

(defun eshell/vw-list-releases ()
  "List the visualworks releases."
  (eshell-flush -1)
  (let* ((releases (vw-system.releases (vw-system.make)))
         (column-width (apply 'max (mapcar 'length (mapcar 'vw-release.description releases)))))
    (dolist (release releases)
      (let (done-first-line)
        (eshell-buffered-print (vw-release.description release column-width))
        (dolist (image (vw-release.images release))
          (when done-first-line
            (eshell-buffered-print (make-string column-width ?\s)))
          (eshell-buffered-print " | " (vw-image.description image) "\n")
          (setq done-first-line t))
        (unless done-first-line
          (eshell-buffered-print "\n")))))
  (eshell-flush))

(defun eshell/vw-execute (image-name &optional vagrant-machine-name script-file-name buffer-name)
  "Run the visualworks image named IMAGE-NAME on VAGRANT-MACHINE-NAME, optionally executing script SCRIPT-FILE-NAME and putting output into BUFFER-NAME."
  ;; TODO: specify VM
  (vw-image.execute (vw-system.find-image-with-name (vw-system.make) image-name)
                    (unless (string= "localhost" vagrant-machine-name) vagrant-machine-name)
                    script-file-name
                    buffer-name))

(defun pcomplete/vw-execute ()
  "PCompleter for the vw-execute command."
  (pcomplete-here (vw-system.image-names (vw-system.make)))
  (pcomplete-here (cons "localhost" (vw-system.vagrant-machine-names)))
  (pcomplete-here (mapcar 'file-name-nondirectory (file-expand-wildcards (concat *vw-scripts-dir* "*.st")))))

(defun eshell/vw-backup (image-name)
  "Backup the visualworks image name IMAGE-NAME."
  (let* ((image (vw-system.find-image-with-name (vw-system.make) image-name))
         (timestamp (format-time-string "%Y-%m-%d-%H-%M-%S-%3N"))
         (dest (concat (vw-image.backups-directory image) timestamp "/")))
    (make-directory dest t)
    (copy-file (vw-image.imagefile-name image) (concat dest (vw-image.imagefile-leaf-name image)))
    (copy-file (vw-image.changesfile-name image) (concat dest (vw-image.changesfile-leaf-name image)))))

(defun pcomplete/vw-backup ()
  "PCompleter for the vw-backup command."
  (pcomplete-here (vw-system.image-names (vw-system.make))))

(defun eshell/vw-vagrant (vagrant-machine-name &rest args)
  "Execute a vagrant command on VAGRANT-MACHINE-NAME, as 'vagrant {ARGS}'."
  (let ((default-directory (file-name-as-directory (concat *vw-vagrant-machines-dir* vagrant-machine-name)))
        (command (concat "vagrant " (mapconcat 'identity args " "))))
    (eshell-flush -1)
    (eshell-wait-for-process (eshell-gather-process-output "/bin/bash" (list "-c" command)))
    (eshell-flush)))

(defun pcomplete/vw-vagrant ()
  "PCompleter for the vw-vagrant command."
  (pcomplete-here (vw-system.vagrant-machine-names)))


;;;
;;;
;;;

(provide 'vw)

;;; vw.el ends here
