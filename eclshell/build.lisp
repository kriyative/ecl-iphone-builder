(require 'cmp)

(setq *print-case* :downcase)

(defun compile-if-old (destdir sources &rest options)
  (unless (probe-file destdir)
    (si::mkdir destdir #o0777))
  (mapcar #'(lambda (source)
	      (let ((object (merge-pathnames destdir (compile-file-pathname source :type :object))))
		(unless (and (probe-file object)
			     (>= (file-write-date object) (file-write-date source)))
		  (format t "~&(compile-file ~S :output-file ~S~{ ~S~})~%"
			  source object options)
		  (apply #'compile-file source :output-file object options))
		object))
	  sources))

(defun str (&rest args)
  (with-output-to-string (*standard-output*)
    (dolist (s args) (princ s))))

(defun join (seq sep)
  "Concatenate the strings in `seq' delimited by the separator `sep'."
  (format nil (concatenate 'string "~{~a~^" sep "~}") seq))

(defun safe-delete-file (f)
  (ignore-errors (delete-file f)))

(defun clean (target)
  (dolist (f (list (str target ".c")
                   (str target ".h")
                   (str target ".o")
                   (str "lib" target ".a")))
    (safe-delete-file f)))

(defun build (target source-files &key ecl-include-dir cflags sdk sysroot)
  (let* ((compiler::*ecl-include-directory* ecl-include-dir)
         (compiler::*cc* (format nil "~a/usr/bin/gcc-4.2" sdk))
         (compiler::*cc-flags* (join (list* "-g"
                                            "-x objective-c"
                                            "-D__IPHONE_OS_VERSION_MIN_REQUIRED=30000"
                                            "-O2 -fPIC -fno-common -D_THREAD_SAFE -Ddarwin -ObjC"
                                            (format nil "-isysroot ~a" sysroot)
                                            cflags)
                                     " "))
         (lisp-files (compile-if-old #p"" source-files :system-p t :c-file t :data-file t :h-file t)))
    (compiler:build-static-library target :lisp-files lisp-files)))

(defun build-simulator (target source-files)
  (let* ((sdk "/Developer/Platforms/iPhoneSimulator.platform/Developer")
         (sdk-ver "3.0"))
    (build target
           source-files
           :ecl-include-dir "/opt/iphone/simulator/include/"
           :cflags '("-arch i386")
           :sdk sdk
           :sysroot (format nil "~a/SDKs/iPhoneSimulator~a.sdk" sdk sdk-ver)))
  (let ((lib (str "lib" target "_simulator.a")))
    (safe-delete-file lib)
    (rename-file (str "lib" target ".a") lib)))

(defun build-device (target source-files)
  (let* ((sdk "/Developer/Platforms/iPhoneOS.platform/Developer")
         (sdk-ver "3.0")
         (sysroot (format nil "~a/SDKs/iPhoneOS~a.sdk" sdk sdk-ver)))
    (build target
           source-files
           :ecl-include-dir "/opt/iphone/device/include/"
           :cflags '("-arch armv6")
           :sdk "/Developer/Platforms/iPhoneOS.platform/Developer"
           :sysroot sysroot))
  (let ((lib (str "lib" target "_device.a")))
    (safe-delete-file lib)
    (rename-file (str "lib" target ".a") lib)))

(defun lipo (target &key (sdk "/Developer/Platforms/iPhoneOS.platform/Developer"))
  (system:system (join (list
                        (str sdk "/usr/bin/lipo")
                        "-arch arm"
                        (str "lib" target "_device.a")
                        "-arch i386"
                        (str "lib" target "_simulator.a")
                        "-create"
                        "-output" (str "lib" target ".a"))
                       " ")))

(clean "eclffi")
(build-simulator "eclffi" '("eclffi.lisp"))
(clean "eclffi")
(build-device "eclffi" '("eclffi.lisp"))
(lipo "eclffi")
