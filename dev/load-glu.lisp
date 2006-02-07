(in-package :COMMON-LISP-USER)

;;; ---------------------------------------------------------------------------

#-EKSL-GENERIC-LOAD-UTILS
(let (#+MCL (*warn-if-redefine* nil))
  (defun current-load-pathname ()
    #+lucid lcl:*source-pathname*
    #+allegro excl:*source-pathname*
    #+(or Genera Explorer) sys:fdefine-file-pathname
    #+MCL (if *load-truename* 
            *load-truename*
            ;; This makes it work in a fred buffer...
            *loading-file-source-file*)
    #-(or lucid allegro Genera Explorer MCL)
    *load-truename*)
  
  ;;; ---------------------------------------------------------------------------
  
  (setf (logical-pathname-translations "GLU")
        (list (list "GLU:ROOT;**;*.*.*" 
                    (directory-namestring 
                     (make-pathname
                      :directory (append
                                  (pathname-directory (current-load-pathname))
                                  (list :wild-inferiors)))))))
  
  ;;; ---------------------------------------------------------------------------
  
  (defun eksl-load-if-exists (filespec &rest args &key (verbose t) &allow-other-keys)
    (when (and filespec (probe-file filespec))
      (apply #'load filespec :verbose verbose args)
      (values t)))
  
  ;;; ---------------------------------------------------------------------------
  
  (defun load-sibling (name &rest args &key (verbose t) &allow-other-keys)
    "Load the file named 'name' that lives in the same folder as THIS file."
    (apply #'eksl-load-if-exists
           (merge-pathnames name (current-load-pathname))
           :verbose verbose
           args))
  
  ;;; ---------------------------------------------------------------------------
  
  (defun canonical-glu-file ()
    (let ((current-directory (and (current-load-pathname)
                                  (pathname-directory (current-load-pathname)))))
      (when current-directory
        (make-pathname 
         :directory (append
                     (butlast current-directory 2)
                     (list "GENERIC-LOAD-UTILITIES" "DEV"))
         :name "generic-load-utils"
         :type "lisp"))))
  
  ;;; ---------------------------------------------------------------------------
  
  (defun load-in-canonical-place ()
    (eksl-load-if-exists (canonical-glu-file))) 
  
  
  ;;; ---------------------------------------------------------------------------
  
  (defun load-glu ()
    "Attempt to load generic-load-utilities.lisp"
    (or 
     ;; Try the 'canonical' one
     (load-in-canonical-place)
     ;; try right here
     (load-sibling "generic-load-utils")
     ;; give up
     (warn "Unable to load generic-load-utilities. Please load it by hand before attempting to load or compile an EKSL load system.")))
  
  
  ;;; ---------------------------------------------------------------------------
  ;;; try to load generic utilities
  ;;; ---------------------------------------------------------------------------
  
  (load-glu))

;;; ***************************************************************************
;;; *                              End of File                                *
;;; ***************************************************************************