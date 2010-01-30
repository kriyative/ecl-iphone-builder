(defpackage :eclffi
  (:use :cl :ffi :uffi)
  (:export
   :alloc
   :make-label
   :ns-log
   :redraw
   :release
   :set-frame
   :set-text
   :with-autorelease-pool))

(in-package :eclffi)

(clines
 "#import <CoreGraphics/CoreGraphics.h>"
 "#import <Foundation/Foundation.h>"
 "#import <UIKit/UIKit.h>"
 "#import \"ecl_boot.h\""
 )

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro c-fficall (arg-specs return-type body &rest args)
    `(ffi:c-inline ,(map 'list #'first arg-specs)
                   ,(map 'list #'second arg-specs)
                   ,return-type
                   ,body
                   ,@args))
  )

(defun ns-log (msg)
  (c-fficall ((msg :cstring)) :void
    "NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
     NSLog([NSString stringWithCString: #0]);
     [pool release];"))

(defun make-ns-string (string)
  (c-fficall ((string :cstring)) :pointer-void "[NSString stringWithCString: #0]" :one-liner t))

(defun alloc (class-name &key init)
  (let ((obj (c-fficall ((class-name :cstring))
                 :pointer-void
               "[NSClassFromString([NSString stringWithCString: #0]) alloc]"
               :one-liner t)))
    (if init
        (c-fficall ((obj :pointer-void)) :pointer-void "[#0 init]" :one-liner t)
        obj)))

(defun release (obj)
  (c-fficall ((obj :pointer-void)) :void "[#0 release];"))

(defvar *autorelease-pool* nil)
(defvar *autorelease-level* 0)

(defun make-autorelease-pool ()
  (c-fficall () :pointer-void "[[NSAutoreleasePool alloc] init]" :one-liner t))

(defmacro with-autorelease-pool (() &body body)
  `(let ((*autorelease-pool* (or *autorelease-pool* (make-autorelease-pool))))
     (unwind-protect
          (let ((*autorelease-level* (1+ *autorelease-level*)))
            ,@body)
       (when (zerop *autorelease-level*)
         (release *autorelease-pool*)))))

(defun make-label (text &key (frame '(10 10 100.0 25.0)) (font-size 24.0))
  (with-autorelease-pool ()
    (destructuring-bind (x y width height) frame
      (c-fficall ((text :cstring)
                  (x :float)
                  (y :float)
                  (width :float)
                  (height :float)
                  (font-size :float))
          :pointer-void
        "
	UILabel *tmp = [[UILabel alloc] initWithFrame: CGRectMake(#1, #2, #3, #4)];
	[tmp setText: [NSString stringWithCString: #0]];
	[tmp setFont: [UIFont systemFontOfSize: #5]];
	[tmp setTextColor: [UIColor blackColor]];
	@(return) = tmp;"))))

(defun key-window ()
  (c-fficall () :pointer-void
    "[[UIApplication sharedApplication] keyWindow]"
    :one-liner t))

(defun redraw (view)
  (c-fficall ((view :pointer-void)) :void
    "[#0 performSelectorOnMainThread: NSSelectorFromString([NSString stringWithCString: \"setNeedsDisplay\"])
	 withObject: nil
	 waitUntilDone: YES];"))

(defun add-subview (view subview)
  (with-autorelease-pool ()
    (c-fficall ((view :pointer-void)
                (subview :pointer-void))
        :void
      "[((id) #0) addSubview: #1];")))

(defun set-text (label text)
  (with-autorelease-pool ()
    (c-fficall ((label :pointer-void)
                (text :cstring))
        :void
      "[#0 setText: [NSString stringWithCString: #1]];")))

(defun set-frame (label frame)
  (with-autorelease-pool ()
    (destructuring-bind (x y width height) frame
      (c-fficall ((label :pointer-void)
                  (x :float)
                  (y :float)
                  (width :float)
                  (height :float))
          :void
        "[#0 setFrame: CGRectMake(#1,#2,#3,#4)];"))))
