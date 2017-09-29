#lang typed/racket

;; CMSC15100 Winter 2017
;; Project 2 -- Vector module
;; Shawn Shan

;; load testing infrastructure
;;
(require typed/test-engine/racket-tests)

;; load custom definitions
;;
(require "../include/uchicago151.rkt")

;; Vectors
;;;;;;;;;;

;; load image library definitions
;;
(require "../include/uc151image.rkt")

;; Define a struct to represent vectors in 3-space
(define-struct Vec3
  ([x : Real]
   [y : Real]
   [z : Real]))

(: vec3-zero : Vec3)
;; Define zero vector
(define vec3-zero (Vec3 0 0 0))

(: vec3-negate : Vec3 -> Vec3)
;; Define vec3-negate to negates all components
(define (vec3-negate x)
  (match x
    [(Vec3 x y z) (Vec3 (* -1 x)
                        (* -1 y)
                        (* -1 z))]))
(check-expect (vec3-negate (Vec3 1 2 3)) (Vec3 -1 -2 -3))
(check-expect (vec3-negate (Vec3 312 -2 0)) (Vec3 -312 2 0))

(: vec3+ : Vec3 Vec3 -> Vec3)
;; Function to adds two vectors, yielding a new vector
(define (vec3+ a b)
  (Vec3 (+ (Vec3-x a)
           (Vec3-x b))
        (+ (Vec3-y a)
           (Vec3-y b))
        (+ (Vec3-z a)
           (Vec3-z b))))
(check-expect (vec3+ (Vec3 1 -2 0) (Vec3 2 -2 0)) (Vec3 3 -4 0))
(check-expect (vec3+ (Vec3 0 -2 0) (Vec3 2 -2 0)) (Vec3 2 -4 0))

(: vec3- : Vec3 Vec3 -> Vec3)
;; Function to subtracts the second vector
;; from the first, yielding a new vector
(define (vec3- a b)
  (Vec3 (- (Vec3-x a)
           (Vec3-x b))
        (- (Vec3-y a)
           (Vec3-y b))
        (- (Vec3-z a)
           (Vec3-z b))))
(check-expect (vec3- (Vec3 1 -2 0) (Vec3 2 -2 0)) (Vec3 -1 0 0))
(check-expect (vec3- (Vec3 0 -2 0) (Vec3 0 -2 0)) (Vec3 0 0 0))

(: vec3-scale : Real Vec3 -> Vec3)
;; Function to takes a scalar s (Real) and a vector v and returns a
;; new vector with all of the components of v multiplied by s.
(define (vec3-scale n v)
  (match v
    [(Vec3 a b c) (Vec3 (* n a)
                        (* n b)
                        (* n c))]))
(check-expect (vec3-scale 3 (Vec3 1 1 1)) (Vec3 3 3 3))
(check-expect (vec3-scale 3 (Vec3 1 1 2)) (Vec3 3 3 6))

(: vec3-dot : Vec3 Vec3 -> Real)
;; computes the dot product of two vectors, which is
;; the sum of the componentwise products.
(define (vec3-dot a b)
  (match a
    [(Vec3 x y z) (+ (* x (Vec3-x b))
                     (* y (Vec3-y b))
                     (* z (Vec3-z b)))]))
(check-expect (vec3-dot (Vec3 1 1 1) (Vec3 1 1 1)) 3)
(check-expect (vec3-dot (Vec3 2 1 1) (Vec3 1 1 1)) 4)

(: vec3-length : Vec3 -> Real)
;; computes the length (magnitude) of a vector,
;; which is the square root of the vector dotted with itself.
(define (vec3-length v)
  (sqrt (vec3-dot v v)))
(check-expect (vec3-length (Vec3 0 0 0)) 0)
(check-expect (vec3-length (Vec3 1 2 2)) 3)

(: vec3-normalize : Vec3 -> Vec3)
;; Function to normalizes the vector
(define (vec3-normalize v)
  (if (< (vec3-length v) 0.0001)
      vec3-zero
      (local
        {(: ratio : Real)
         (define ratio (/ 1.0 (vec3-length v)))}
        (match v
          [(Vec3 x y z) (Vec3 (* x ratio)
                              (* y ratio)
                              (* z ratio))]))))

(check-expect (vec3-normalize (Vec3 0 0 0)) (Vec3 0 0 0))
(check-within (vec3-normalize (Vec3 0 0 1.0)) (Vec3 0 0 1.0) 0.001)

;; Rays
;;;;;;;;;;

;; A (Ray origin dir) represents a ray in 3D space, where origin
;; is the origin of the ray and dir is a unit vector specifying
;; the direction.
(define-struct Ray
  ([origin : Vec3]  ;; the origin of the ray
   [dir : Vec3]))   ;; the direction of the ray (unit vector)

(: make-ray : Vec3 Vec3 -> Ray)
;; make a ray with the given origin and direction vector.
;; This function normalizes the direction to a unit vector
(define (make-ray pt dir)
  (Ray pt (vec3-normalize dir)))

(: ray-point-at : Ray Real -> Vec3)
;; return the point on the ray at the given distance
;; from the origin
(define (ray-point-at ray t)
  (match ray
    [(Ray origin dir) (vec3+ origin (vec3-scale t dir))]))

(: ray-offset : Ray Real -> Ray)
;; offset the origin of the ray by the given distance
(define (ray-offset ray t)
  (Ray (ray-point-at ray t) (Ray-dir ray)))



(: vec3-reflect : Vec3 Vec3 -> Vec3)
;; given a vector v and a unit surface normal n, return
;; the reflection of v off of the surface.
(define (vec3-reflect v n)
  (vec3- v (vec3-scale (* 2 (vec3-dot v n)) n)))

(check-expect (vec3-reflect (Vec3 1 1 1) (Vec3 0 1 0)) (Vec3 1 -1 1))
(check-expect (vec3-reflect (Vec3 -1 -1 0) (Vec3 0 1 0))
              (Vec3 -1 1 0))

(: vec3-halfway : Vec3 Vec3 -> Vec3)
;; that computes the halfway vector between two unit vectors
(define (vec3-halfway v1 v2)
  (vec3-normalize (vec3+ v1 v2)))
(check-within (vec3-halfway (Vec3 -1 0 0) (Vec3 0 1 0)) (Vec3 -0.7 0.7 0) 0.1)


;; Exports
;;;;;;;;;;

(provide (struct-out Vec3)
         (struct-out Ray))
;; Project 2 Exports
;;;;;;;;;;;;;;;;;;;;

(provide vec3-reflect
         vec3-halfway)

(provide
 vec3-zero
 vec3-negate
 vec3+
 vec3-
 vec3-scale
 vec3-dot
 vec3-length
 vec3-normalize
 make-ray
 ray-point-at
 ray-offset)