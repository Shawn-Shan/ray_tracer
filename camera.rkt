#lang typed/racket

;; CMSC15100 Winter 2017
;; Project 2 -- camera.rkt module
;; <YOUR NAME>

;; load testing infrastructure
;;
(require typed/test-engine/racket-tests)

;; load custom definitions
;;
(require "../include/uchicago151.rkt")

;; load image library definitions
;;
(require "../include/uc151image.rkt")

;; load vector module
;;
(require "vec3.rkt")
(require "objects.rkt")
(require "scene.rkt")

;; load material module
;;
(require "material.rkt")

;; A (Camera w h z flen) specifies the size of the view image and
;; the viewing parameters
(define-struct Camera
  [(wid : Natural)           ;; width of view image
   (ht : Natural)            ;; height of view image
   (z-pos : Real)            ;; z-coordinate of camera
   (focal-len : Real)])      ;; distance to image plane



(: center-position : Camera Integer Integer -> Vec3)
;; return the center position of the pixel
(define (center-position cam row col)
  (match cam
    [(Camera wid ht z-pos flen)
     (local {
             (: pw : Real)
             (define pw (/ 2 wid))
             (: x0 : Real)
             (define x0 (- (/ pw 2) 1))
             (: y0 : Real)
             (define y0 (- (/ ht wid) (/ pw 2)))
             (: z : Real)
             (define z (+ z-pos flen))}
       (Vec3 (+ x0 (* col pw)) (- y0 (* row pw)) z))]))

(check-within (center-position (Camera 10 10 10 10) 3 3) (Vec3 -0.3 0.3 20) 0.001)
(check-within (center-position (Camera 2 2 2 10) 3 3) (Vec3 2.5 -2.5 12) 0.001)


(: ray-for-pixel : Camera -> (Integer Integer -> Ray))
;; takes a camera and returns a function
;; for generating rays from a row and column
(define (ray-for-pixel cam)
  (lambda ([row : Integer]
           [col : Integer])
    (make-ray (center-position cam row col)
              (vec3- (center-position cam row col)
                     (Vec3 0 0 (Camera-z-pos cam))))))

(check-within ((ray-for-pixel (Camera 10 10 10 10)) 5 5)
              (Ray (Vec3 0.1 -0.1 20) (Vec3 0.0096 -0.009 0.99))
              0.01)




(: foreach-pixel : Camera (Ray -> RGB) -> Image)
;; iterates over the pixels in the image rectangle
(define (foreach-pixel cam f)
  (local
    {(: foreach-row : Integer (Listof Color) -> (Listof Color))
     (define (foreach-row row res)
       (local
         {(: foreach-col : Integer (Listof Color) -> (Listof Color))
          (define (foreach-col col result)
            (if (< col 0)
                result
                (foreach-col (- col 1)
                             (cons (rgb->color (f
                                                ((ray-for-pixel cam) row col))) result))))}
         (if (< row 0)
             res
             (foreach-row (- row 1)
                          (foreach-col (- (Camera-wid cam) 1)
                                       res)))))}
    (color-list->bitmap
     (foreach-row (- (Camera-ht cam) 1) '())
     (Camera-wid cam)
     (Camera-ht cam))))



;; Exports
;;;;;;;;;;

(provide (struct-out Camera))

(provide foreach-pixel)