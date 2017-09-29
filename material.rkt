#lang typed/racket

;; CMSC15100 Winter 2017
;; Project 2 -- Material module
;; <YOUR NAME>

;; load testing infrastructure
;;
(require typed/test-engine/racket-tests)

;; load custom definitions
;;
(require "../include/uchicago151.rkt")

(require "vec3.rkt")
;; load image library definitions
;;
(require "../include/uc151image.rkt")

;; Construct a RGB type
(define-struct RGB
  ([red : Real]
   [green : Real]
   [blue : Real]))

(: rgb->vec3 : RGB -> Vec3)
;; Convert RGB to Vec3
(define (rgb->vec3 rgb)
  (match rgb
    [(RGB r g b)
     (Vec3 r g b)]))
(check-expect (rgb->vec3 (RGB 1 2 3)) (Vec3 1 2 3))

(: vec3->rbg : Vec3 -> RGB)
;;Convert Vec back to rgb
(define (vec3->rbg v)
  (match v
    [(Vec3 r g b)
     (RGB r g b)]))
(check-expect (vec3->rbg (Vec3 1 2 3)) (RGB 1 2 3))

(: rgb+ : RGB RGB -> RGB)
;; Add two RGB values
(define (rgb+ rgb1 rgb2)
  (vec3->rbg (vec3+ (rgb->vec3 rgb1) (rgb->vec3 rgb2))))
(check-expect (rgb+ (RGB 1 2 3) (RGB 1 2 3)) (RGB 2 4 6))
(check-expect (rgb+ (RGB 1 2 0) (RGB 0 10 3)) (RGB 1 12 3))

(: rgb* : RGB RGB -> RGB)
;; Pointwise multiplication of two RGB values
(define (rgb* a b)
  (match* (a b)
    [((RGB x y z) (RGB a b c))
     (RGB (* a x)
          (* b y)
          (* c z))]))
(check-expect (rgb* (RGB 1 2 3) (RGB 1 2 3)) (RGB 1 4 9))
(check-expect (rgb* (RGB 1 2 0) (RGB 0 10 3)) (RGB 0 20 0))


(: rgb-scale : Real RGB -> RGB)
;; Multiply a scalar and an RGB value
(define (rgb-scale r rgb)
  (vec3->rbg (vec3-scale r (rgb->vec3 rgb))))
(check-expect (rgb-scale 10 (RGB 0 10 3)) (RGB 0 100 30))
(check-expect (rgb-scale 2 (RGB 4 1 3)) (RGB 8 2 6))


(: rgb->color : RGB -> Color)
;; convert an RGB value to an image color value
(define (rgb->color rgb)
  (local
    {(: ->byte : Real -> Byte)
     (define (->byte x)
       (local
         {(define b (exact-floor (* 255.99 x)))}
         (cond
           [(< b 0) 0]
           [(< 255 b) 255]
           [else b])))}
    (match rgb
      [(RGB r g b) (color (->byte r) (->byte g) (->byte b))])))

(check-expect (rgb->color (RGB 0 0 0)) (color 0 0 0))
(check-expect (rgb->color (RGB 1 0 0)) (color 255 0 0))
(check-expect (rgb->color (RGB 0 1 0)) (color 0 255 0))
(check-expect (rgb->color (RGB 0 0 1)) (color 0 0 255))



(define-struct Material
  ([diffuse : RGB]
   [specular : RGB]
   [sharpness : Real]))

(: make-diffuse : RGB -> Material)
;; make a diffuse material
(define (make-diffuse rgb)
  (Material rgb (RGB 0 0 0) 0))

(check-expect (make-diffuse (RGB 0 0 0))
              (Material (RGB 0 0 0) (RGB 0 0 0) 0))
(check-expect (make-diffuse (RGB 0 3 0))
              (Material (RGB 0 3 0) (RGB 0 0 0) 0))

(: specular? : Material -> Boolean)
;;  returns true when the sharpness field is greater than zero
(define (specular? mat)
  (match mat
    [(Material dif spe shar)
     (> shar 0)]))

(check-expect (specular? (Material (RGB 0 0 0) (RGB 0 0 0) 2)) #t)
(check-expect (specular? (Material (RGB 0 0 0) (RGB 0 0 0) -2)) #f)

;; Exports
;;;;;;;;;;

(provide (struct-out RGB)
         (struct-out Material))
;; Project 2 Exports
;;;;;;;;;;;;;;;;;;;;

(provide make-diffuse
         specular?)
(provide rgb+
         rgb*
         rgb-scale
         rgb->color)