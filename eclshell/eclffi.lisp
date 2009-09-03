(defpackage :eclffi
  (:use :cl :ffi :uffi)
  (:export :show
           :make-label
           :set-label-text
           :set-label-frame))

(in-package :eclffi)

(clines
 "#import <CoreGraphics/CoreGraphics.h>"
 "#import <Foundation/Foundation.h>"
 "#import <UIKit/UIKit.h>"
 "#import \"ecl_boot.h\""
 )

(defun show (msg)
  (c-inline (msg)
            (:cstring)
            :int
            "{NSString *fmt = [NSString stringWithCString: #0];
              NSLog(fmt);
              @(return) = 0;}"
            :side-effects t))

(defun make-label (text &key (x 10.0) (y 10.0) (width 100.0) (height 25.0))
  (c-inline (text x y width height)
            (:cstring :float :float :float :float)
            :pointer-void
            "{NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
              UILabel *tmp = [[UILabel alloc] initWithFrame: CGRectMake(#1, #2, #3, #4)];
              tmp.text = [NSString stringWithCString: #0];
              tmp.font = [UIFont systemFontOfSize: 24.0f];
              tmp.textColor = [UIColor blackColor];
              UIWindow *win = [[UIApplication sharedApplication] keyWindow];
              [win performSelectorOnMainThread: NSSelectorFromString([NSString stringWithCString: \"addSubview:\"]) withObject: tmp waitUntilDone: YES];
              [tmp release];
              [pool release];
              @(return) = tmp;}"
            :side-effects t))

(defun set-label-text (label text)
  (c-inline (label text)
            (:pointer-void :cstring)
            :int
            "{NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
              UILabel *tmp = (UILabel *) #0;
              [tmp performSelectorOnMainThread: NSSelectorFromString([NSString stringWithCString: \"setText:\"]) withObject: [NSString stringWithCString: #1] waitUntilDone: YES];
              [pool release];
              @(return) = 0;}"
            :side-effects t))

(defun set-label-frame (label &key (x 0.0) (y 0.0) (width 160.0) (height 25.0))
  (c-inline (label x y width height)
            (:pointer-void :float :float :float :float)
            :int
            "{NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
              UILabel *tmp = (UILabel *) #0;
              CGRect frame = CGRectMake(#1,#2,#3,#4);
              tmp.frame = frame;
              [tmp performSelectorOnMainThread: NSSelectorFromString([NSString stringWithCString: \"setNeedsDisplay\"]) withObject: nil waitUntilDone: YES];
              [pool release];
              @(return) = 0;}"
            :side-effects t))
