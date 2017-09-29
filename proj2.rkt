#lang typed/racket
;; CMSC15100 Winter 2017
;; Project 2 -- Scene module
;; Shawn Shan

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
(require "scene.rkt")
(require "camera.rkt")
;; load material module
;;
(require "material.rkt")

;; load objects module
;;
(require "objects.rkt")


(: render : Camera Scene Natural -> Image)
;; Define the render function to produce images
(define (render cam scene depth)
  (foreach-pixel cam (trace-ray scene depth)))



(define mat-green (make-diffuse (RGB 0 1 0)))
(define mat-orange (make-diffuse (RGB 1 11/17 0)))

(define scene-1
  (make-scene
   (RGB 169/255 169/255 169/255)
   (list
    (make-plane (Vec3 0 1 0) -2 mat-green)
    (make-sphere (Vec3 0 0 3) 1 mat-orange))
   (RGB 0.2 0.2 0.2)
   (list (make-dir-light (Vec3 -1 1 -1) (RGB 1 1 1)))))

;(render (Camera 320 256 -8 4) scene-1 1)

(define scene-2
  (make-scene
   (RGB 169/255 169/255 169/255)
   (list
    (make-plane (Vec3 0 1 0) -1 mat-green))
   (RGB 0.2 0.2 0.2)
   (list (make-point-light (Vec3 0 1 3) (RGB 1 1 1) 0.05))))

(render (Camera 320 256 -8 4) scene-2 1)

(: make-test-scene : Real -> Scene)
(define (make-test-scene sharpness)
  (make-scene
   (RGB 169/255 169/255 169/255)
   (list
    (make-sphere (Vec3 0 -2 1) 1 (Material (RGB 0.5 0.1 0.5) (RGB 1 1 1) sharpness)))
   (RGB 0.3 0.2 0.3)
   (list
    (make-dir-light (Vec3 0 1 6) (RGB 1 1 1)))))

(: specular-test : Real -> Image)
(define (specular-test sharpness)
  (render (Camera 320 256 -5 2) (make-test-scene sharpness) 0))

(specular-test 1)

(specular-test 10)

(specular-test 20)



(define mat-dodgerblue (make-diffuse (RGB 2/17 48/85 1)))
(define mat-mirror (Material (RGB 0.1 0.1 0.1) (RGB 1 1 1) 10))

(define flat-mirror-scene
  (make-scene
   (RGB 169/255 169/255 169/255)
   (list
    (make-plane (Vec3 0 0 -1) -10 mat-mirror)
    (make-plane (Vec3 0 1 0) -1 mat-mirror)
    (make-sphere (Vec3 -1.5 0.5 5) 1/2 mat-orange)
    (make-sphere (Vec3 1.5 0.5 5) 1/2 mat-dodgerblue))
   (RGB 1 1 1)
   (list)))

(: flat-mirror-test : Natural -> Image)
(define (flat-mirror-test depth-limit)
  (render (Camera 320 256 -8 4) flat-mirror-scene depth-limit))

(flat-mirror-test 0)
(flat-mirror-test 1)
(flat-mirror-test 2)


(define mirror-scene
  (make-scene
   (RGB 169/255 169/255 169/255)
   (list
    (make-plane (Vec3 0 0 -1) -10 mat-mirror)
    (make-plane (Vec3 0 1 0) -1 mat-mirror)
    (make-sphere (Vec3 -1.5 0.5 5) 1/2 mat-orange)
    (make-sphere (Vec3 1.5 0.5 5) 1/2 mat-dodgerblue))
   (RGB 0.2 0.2 0.2)
   (list (make-dir-light (Vec3 -1 1 -1) (RGB 0.8 0.8 0.8)))))







