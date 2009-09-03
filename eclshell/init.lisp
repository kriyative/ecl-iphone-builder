(in-package :cl-user)

(setq *print-case* :downcase)           ; I like lowercase better
(let ((home (translate-logical-pathname "home:")))
  (setq *default-pathname-defaults* home
        *default-directory* home))

(defun sysinfo (&optional (out *standard-output*))
  "Print the current environment to a stream."
  (declare (stream out))
  (format out "~&~%~75~~%~75,,,'-:@<<{[ The current environment ]}>~>~%~
Implementation:~20t~a~%~7tversion:~20t~a~%Machine:  type:~20t~a
~7tversion:~20t~a~%~6tinstance:~20t~a~%Opeating System:~19t"
          (lisp-implementation-type) (lisp-implementation-version)
          (machine-type) (machine-version) (machine-instance))
  #+darwin (princ " Darwin")
  #+unix (princ " Unix")
  (format out "~%Software: type:~20t~a~%~7tversion:~20t~a~%Site:~20t~a (~a)
User home:~20t~a~%Current directory:~20t~a~%Default pathname:~20t~a~%"
          (software-type) (software-version) (long-site-name)
          (short-site-name) (user-homedir-pathname) *default-directory*
          *default-pathname-defaults*)
  (format out "Features: ~s.
Modules:~s.~%
Current package:~s~%"
           *features* *modules* *package*)
  (flet ((exdi (fl) (integer-length (nth-value 1 (decode-float fl)))))
    (format out "Fixnum length:~25t~3d bits
Short Floats:~25t~3d bits exponent, ~3d bits significand (mantissa)
Single Floats:~25t~3d bits exponent, ~3d bits significand (mantissa)
Double Floats:~25t~3d bits exponent, ~3d bits significand (mantissa)
Long Floats:~25t~3d bits exponent, ~3d bits significand (mantissa)~%"
            (integer-length most-positive-fixnum)
            (exdi most-positive-short-float)
            (float-digits most-positive-short-float)
            (exdi most-positive-single-float)
            (float-digits most-positive-single-float)
            (exdi most-positive-double-float)
            (float-digits most-positive-double-float)
            (exdi most-positive-long-float)
            (float-digits most-positive-long-float)))
  (dolist (sy '(array-total-size-limit array-rank-limit array-dimension-limit
                lambda-parameters-limit call-arguments-limit
                multiple-values-limit char-code-limit))
    (format out " ~a:~30t~15:d~%" sy (symbol-value sy)))
  (format out "lambda-list-keywords:~s~%"
          lambda-list-keywords)
  (format out "Internal time unit:~25t~f sec~%*gensym-counter*:~25t~:d
Current time:~25t" (/ internal-time-units-per-second) *gensym-counter*)
  (format out "~a" (get-universal-time))
  (format out "~%~75~~%") (room) (values))

(defun str (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun pathname-parent (pathname)
  (make-pathname :directory (or (pathname-directory pathname)
                                (list :relative))
                 :name nil
                 :type nil
                 :defaults pathname))

(defun swank-load (h)
  (load (str h "slime/swank-loader.lisp") :verbose t)
  (funcall (read-from-string "swank-loader:init")))

(swank-load (pathname-parent *load-pathname*))

(defun safe-substr (str start &optional length)
  (subseq str 0 (if length (min (length str) length))))

(defvar *label*)
(setq *label* (eclffi:make-label "" :x 10.0 :y 50.0 :width 300.0 :height 25.0))

(mp:process-run-function
 "SLIME-listener"
 (lambda ()
   (cond
     (setq *print-case* :downcase)
     ((string-equal (safe-substr (machine-type) 0 6) "iPhone")
      (let* ((ip-vec (sb-bsd-sockets:host-ent-address
                      (sb-bsd-sockets:get-host-by-name
                       (str (machine-instance) ".local"))))
             (swank::*loopback-interface* (format nil "~d.~d.~d.~d"
                                                  (aref ip-vec 0)
                                                  (aref ip-vec 1)
                                                  (aref ip-vec 2)
                                                  (aref ip-vec 3))))
        (eclffi:set-label-text *label*
                               (format nil "SLIME: ~a~%" swank::*loopback-interface*))
        (swank:create-server :port 4005 :dont-close t)))
     (t (swank:create-server :port 4005 :dont-close t)))))

;; A silly test to randomly move the label around
#+test
(let ((x 50) (y 50))
  (dotimes (i 1000)
    (if (> (random 4) 2)
        (incf x (random 2))
        (incf y (random 2)))
    (eclffi:set-label-frame *label* :x x :y y :width 160.0 :height 25.0)
    (eclffi:set-label-text *label* (format nil "~d,~d" x y))))
