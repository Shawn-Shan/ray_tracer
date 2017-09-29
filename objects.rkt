#lang typed/racket

;; CMSC15100 Winter 2017
;; Project 2 -- Objects module
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

;; load material module
;;
(require "material.rkt")

;; For now, we only have spheres
(define-type Object (U Plane Sphere))

(define-struct Sphere
  ([c : Vec3]
   [r : Real]
   [r^2 : Real]
   [mat : Material]))


(: make-sphere : Vec3 Real Material -> Object)
;; This function takes a position, radius,
;; and material are returns a sphere
(define (make-sphere c r mat)
  (Sphere c
          r
          (* r r)
          mat))

;; A (Hit t obj) represents an intersection between a ray R
;; and the object obj at point R(t).
(define-struct Hit
  ([t : Real]
   [obj : Object]))

;; A Maybe-Hit is either a Hit or 'miss
(define-type Maybe-Hit (U Hit 'miss))


(: ray-sphere-intersect : Ray -> (Sphere -> Maybe-Hit))
;; Test whether a ray will hit a sphere
(define (ray-sphere-intersect ray)
  (lambda ([sphere : Sphere])
    (local
      {(: C : Vec3)
       (define C (Sphere-c sphere))
       (: o : Vec3)
       (define o (Ray-origin ray))
       (: q : Vec3)
       (define q (vec3- o C))
       (: d : Vec3)
       (define d (Ray-dir ray))
       (: a : Real)
       (define a 1)
       (: b : Real)
       (define b (* 2 (vec3-dot q d)))
       (: c : Real)
       (define c (- (vec3-dot q q) (Sphere-r^2 sphere)))
       (: D : Real)
       (define D (- (* b b) (* 4 a c)))
       }
      (if
       (or [<= (vec3-dot q q) (Sphere-r^2 sphere)]
           [< D 0]
           [< (/ (- (- b) (sqrt D))
                 (* 2 a)) 0])
       'miss
       (Hit (/ (- (- b) (sqrt D))
               (* 2 a))
            sphere)))))
               
(check-within ((ray-sphere-intersect (make-ray (Vec3 0 0 0) (Vec3 1 0 0)))
               (Sphere (Vec3 1 0 0) 0.5 0.25 (Material (RGB 0 1 1) (RGB 0 1 1) 2)))
              (Hit 0.5 (Sphere (Vec3 1 0 0) 0.5 0.25 (Material (RGB 0 1 1) (RGB 0 1 1) 2)))
              0.1)
(check-within ((ray-sphere-intersect (make-ray (Vec3 0 0 0) (Vec3 1 0 0)))
               (Sphere (Vec3 3 0 0) 2 4 (Material (RGB 0 1 1) (RGB 0 1 1) 2)))
              (Hit 1.0 (Sphere (Vec3 3 0 0) 2 4 (Material (RGB 0 1 1) (RGB 0 1 1) 2)))
              0.1)
(check-expect ((ray-sphere-intersect (make-ray (Vec3 0 0 0) (Vec3 0 1 0)))
               (Sphere (Vec3 3 0 0) 2 4 (Material (RGB 0 1 1) (RGB 0 1 1) 2)))
              'miss)


(: sphere-normal : Sphere Vec3 -> Vec3)
;; returns the surface-normal vector for
;; the given sphere and intersection point
(define (sphere-normal sphere pt)
  (vec3-normalize (vec3- pt (Sphere-c sphere))))


(check-within (sphere-normal (Sphere (Vec3 1 0 0) 1 1
                                     (Material (RGB 0 1 1) (RGB 0 1 1) 2))
                             (Vec3 0 0 0))
              (Vec3 -1 0 0) 0.001)         

(define-struct Plane
  ([normal : Vec3]
   [s : Real]
   [mat : Material]))
             

(: make-plane : Vec3 Real Material -> Object)
;; represents a plane using representation
(define (make-plane normal s mat)
  (Plane (vec3-normalize normal)
         s
         mat))

(check-within (make-plane (Vec3 0 0 1) 3 (Material (RGB 0 0 0) (RGB 1 1 1) 1))
              (Plane (Vec3 0 0 1) 3 (Material (RGB 0 0 0) (RGB 1 1 1) 1)) 0.001)

(check-within (make-plane (Vec3 0 0 1) -2 (Material (RGB 0 0 0) (RGB 1 1 1) 1))
              (Plane (Vec3 0 0 1) -2 (Material (RGB 0 0 0) (RGB 1 1 1) 1)) 0.001)

(: ray-plane-intersect : Ray -> Plane -> Maybe-Hit)
;; tests a ray against a plane for intersection and returns the Hit
;; if there is an intersection or returns 'miss if there is no intersection
(define (ray-plane-intersect ray)
  (lambda ([plane : Plane])
    (match* (ray plane)
      [((Ray o d) (Plane n s mat))
       (local {(define c : Real (vec3-dot n d))
               (define t : Real (/ (- s (vec3-dot n o)) c))}
         (if (or (> c -0.00001) (<= t 0))
             'miss
             (Hit t plane)))])))

(check-within ((ray-plane-intersect (make-ray (Vec3 0 0 0) (Vec3 0 0 1)))
               (Plane (Vec3 0 0 -1) -3 (Material (RGB 0 0 0) (RGB 1 1 1) 1)))
              (Hit 3.0 (Plane (Vec3 0 0 -1) -3 (Material (RGB 0 0 0) (RGB 1 1 1) 1))) 0.01)
(check-expect ((ray-plane-intersect (make-ray (Vec3 0 0 4) (Vec3 0 0 -1)))
               (Plane (Vec3 0 0 -1) -3 (Material (RGB 0 0 0) (RGB 1 1 1) 1)))
              'miss)
              
(: ray-object-intersect : Ray -> (Object -> Maybe-Hit))
;; test for a ray-object intersection
(define (ray-object-intersect ray)
  (lambda ([object : Object])
    (match object
      [(Sphere _ _ _ _) ((ray-sphere-intersect ray) object)]
      [(Plane _ _ _) ((ray-plane-intersect ray) object)])))

(: object-normal : Object Vec3 -> Vec3)
;; return the unit surface normal for the given point on the object
(define (object-normal object v)
  (match object
    [(Sphere _ _ _ _) (sphere-normal object v)]
    [(Plane _ _ _) (Plane-normal object)]))

(: object-material : Object -> Material)
;; return the object's surface material
(define (object-material object)
  (match object
    [(Sphere _ _ _ _) (Sphere-mat object)]
    [(Plane _ _ _) (Plane-mat object)]))


;; Exports
;;;;;;;;;;

(provide
 Object
 (struct-out Hit)
 Maybe-Hit)
(provide make-plane)
(provide make-sphere
         ray-object-intersect
         object-normal
         object-material)