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

;; load material module
;;
(require "material.rkt")

;; load objects module
;;
(require "objects.rkt")

;; a directional light source
(define-struct Dir-Light
  ([dir : Vec3]         ;; unit vector pointing toward the light
   [intensity : RGB]))  ;; the light's intensity

(define-type Light (U Point-Light Dir-Light))

(: make-dir-light : Vec3 RGB -> Light)
;; given a vector pointing toward the light and an intensity,
;; make a directional light source
(define (make-dir-light dir rgb)
  (Dir-Light (vec3-normalize dir) rgb))

(define-struct Point-Light
  ([pos : Vec3]
   [intensity : RGB]
   [atten : Real]))

(: make-point-light : Vec3 RGB Real -> Light)
(define make-point-light Point-Light)




(define-struct Scene
  ([background : RGB]
   [objects : (Listof Object)]
   [ambient : RGB]
   [lights : (Listof Light)]))

(: make-scene : RGB (Listof Object) RGB (Listof Light) -> Scene)
;; create a Scene; the arguments are background color, the list of
;; objects in the scene, the ambient light, and the list of light
;; sources in the scene.
(define (make-scene background objects ambient lights)
  (Scene background
         objects
         ambient
         lights))

(: closest-hit : Scene Ray -> Maybe-Hit)
;; returns the hit for the first object that the ray intersects
(define (closest-hit scene ray)
  (local
    {(: compare-maybe-hit : Maybe-Hit Maybe-Hit -> Maybe-Hit)
     (define (compare-maybe-hit maybe1 maybe2)
       (match* (maybe1 maybe2)
         [('miss _) maybe2]
         [(_ 'miss) maybe1]
         [((Hit t1 _)(Hit t2 _))
          (if (<= t1 t2)
              maybe1
              maybe2)]))}
    ((inst foldl Object Maybe-Hit)
     (lambda
         ([object : Object]
          [acc : Maybe-Hit])
       (compare-maybe-hit ((ray-object-intersect ray) object) acc))
     'miss
     (Scene-objects scene))))

(check-within (closest-hit (Scene (RGB 0 0 1)
                                  (list (make-sphere (Vec3 0 0 1) 0.5 (Material (RGB 0 1 1) (RGB 0 1 1) 2))
                                        (make-sphere (Vec3 0 1 0) 0.5 (Material (RGB 0 1 1) (RGB 0 1 1) 2))
                                        (make-sphere (Vec3 1 0 0) 0.5 (Material (RGB 0 1 1) (RGB 0 1 1) 2))
                                        (make-sphere (Vec3 1 1 1) 0.5 (Material (RGB 0 1 1) (RGB 0 1 1) 2)))
                                  (RGB 1 1 1)
                                  '())
                           (make-ray (Vec3 0 0 0) (Vec3 1 0 0)))
              (Hit 0.5 (make-sphere (Vec3 1 0 0) 0.5 (Material (RGB 0 1 1) (RGB 0 1 1) 2))) 0.01)
                                  
(check-within (closest-hit (Scene (RGB 0 0 1)
                                  (list (make-sphere (Vec3 0 0 1) 0.5 (Material (RGB 0 1 1) (RGB 0 1 1) 2))
                                        (make-sphere (Vec3 0 0 2) 0.5 (Material (RGB 0 1 1) (RGB 0 1 1) 2))
                                        (make-sphere (Vec3 0 0 3) 0.5 (Material (RGB 0 1 1) (RGB 0 1 1) 2))
                                        (make-sphere (Vec3 0 0 4) 0.5 (Material (RGB 0 1 1) (RGB 0 1 1) 2)))
                                  (RGB 1 1 1)
                                  '())
                           (make-ray (Vec3 0 0 0) (Vec3 0 0 1)))
              (Hit 0.5 (make-sphere (Vec3 0 0 1) 0.5 (Material (RGB 0 1 1) (RGB 0 1 1) 2))) 0.01)

(check-within (closest-hit (Scene (RGB 0 0 1)
                                  (list (make-sphere (Vec3 1 2 1) 0.5 (Material (RGB 0 1 1) (RGB 0 1 1) 2))
                                        (make-sphere (Vec3 0 0 3) 0.5 (Material (RGB 0 1 1) (RGB 0 1 1) 2))
                                        (make-sphere (Vec3 0 0 6) 0.5 (Material (RGB 0 1 1) (RGB 0 1 1) 2))
                                        (make-sphere (Vec3 0 0 4) 0.5 (Material (RGB 0 1 1) (RGB 0 1 1) 2)))
                                  (RGB 1 1 1)
                                  '())
                           (make-ray (Vec3 0 0 0) (Vec3 0 0 1)))
              (Hit 2.5 (make-sphere (Vec3 0 0 3) 0.5 (Material (RGB 0 1 1) (RGB 0 1 1) 2))) 0.01)

(: any-hit? : Scene Ray Real -> Boolean)
;; special version of the hit test that
;; returns #t on the first Hit and #f if there are no hits
(define (any-hit? scene ray max)
  ((inst ormap Object)
   (lambda
       ([object : Object])
     (match ((ray-object-intersect ray) object)
       ['miss #f]
       [(Hit t _)
        (if (>= t max)
            #f
            #t)]))
   (Scene-objects scene)))

(check-expect (any-hit? (Scene (RGB 0 0 1)
                               (list (make-sphere (Vec3 1 2 1) 0.5 (Material (RGB 0 1 1) (RGB 0 1 1) 2))
                                     (make-sphere (Vec3 0 0 3) 0.5 (Material (RGB 0 1 1) (RGB 0 1 1) 2))
                                     (make-sphere (Vec3 0 0 6) 0.5 (Material (RGB 0 1 1) (RGB 0 1 1) 2))
                                     (make-sphere (Vec3 0 0 4) 0.5 (Material (RGB 0 1 1) (RGB 0 1 1) 2)))
                               (RGB 1 1 1)
                               '())
                        (make-ray (Vec3 0 0 0) (Vec3 0 0 1))
                        4)
              #t)
(check-expect (any-hit? (Scene (RGB 0 0 1)
                               (list (make-sphere (Vec3 0 0 1) 0.5 (Material (RGB 0 1 1) (RGB 0 1 1) 2))
                                     (make-sphere (Vec3 0 0 3) 0.5 (Material (RGB 0 1 1) (RGB 0 1 1) 2))
                                     (make-sphere (Vec3 0 0 6) 0.5 (Material (RGB 0 1 1) (RGB 0 1 1) 2))
                                     (make-sphere (Vec3 0 0 4) 0.5 (Material (RGB 0 1 1) (RGB 0 1 1) 2)))
                               (RGB 1 1 1)
                               '())
                        (make-ray (Vec3 0 0 0) (Vec3 0 1 1))
                        3)
              #f)
(check-expect (any-hit? (Scene (RGB 0 0 1)
                               (list (make-sphere (Vec3 1 1 1) 3 (Material (RGB 0 1 1) (RGB 0 1 1) 2))
                                     (make-sphere (Vec3 0 0 3) 0.5 (Material (RGB 0 1 1) (RGB 0 1 1) 2))
                                     (make-sphere (Vec3 0 0 6) 0.5 (Material (RGB 0 1 1) (RGB 0 1 1) 2))
                                     (make-sphere (Vec3 0 0 4) 0.5 (Material (RGB 0 1 1) (RGB 0 1 1) 2)))
                               (RGB 1 1 1)
                               '())
                        (make-ray (Vec3 0 0 0) (Vec3 0 1 1))
                        3)
              #f)

;; An (Illum dir rgb) represents a source of light, where dir is a unit
;; vector pointing toward the light and rgb is the intensity of the light
;; at the point of illumination
(define-struct Illum
  ([dir : Vec3]
   [intensity : RGB]))







(: get-illumination : Scene Vec3 -> (Listof Illum))
;; takes a scene and a point in 3D space and returns a list of Illum
;; values for the lights that cast illumination on the point.
(define (get-illumination scene pt)
  ((inst foldr Light (Listof Illum))
   (lambda
       ([light : Light]
        [illum : (Listof Illum)])
     (match light
       [(Dir-Light dir intensity)
        (if (any-hit? scene
                      (ray-offset (make-ray pt dir) 0.001)
                      +inf.0)
            illum
            (cons (Illum dir intensity) illum))]
       [(Point-Light pos intensity atten)
        (local
          {(define d : Vec3 (vec3- pos pt))}
          (if (any-hit? scene
                        (Ray pt (vec3-normalize d))
                        (vec3-length d))
              illum
              (cons (Illum (vec3-normalize d)
                           (rgb-scale
                            (/ 1 (+ 1 (* atten (vec3-length d) (vec3-length d))))
                            intensity))
                    illum)))]))
   '() (Scene-lights scene)))


(: diffuse-rgb : Scene Material Vec3 (Listof Illum) -> RGB)
;; given the surface material, surface normal, and list of light sources,
;; compute the ambient and diffuse components of the surface color
;;
(define (diffuse-rgb scene surf nvec illums)
  (local
    {(define diffuse (Material-diffuse surf))
     (: do-illum : Illum RGB -> RGB)
     ;; add the diffuse light from one source
     (define (do-illum illum rgb)
       (rgb+ (rgb-scale
              (max 0 (vec3-dot (Illum-dir illum) nvec))
              (Illum-intensity illum))
             rgb))}
    (rgb* diffuse (foldl do-illum (Scene-ambient scene) illums))))


(: specular-rgb : Material Vec3 Vec3 (Listof Illum) -> RGB)
;; surface material, the surface-normal vector,
;; the unit vector pointing from the point toward the viewer,
;; and the list of light sources, and return the specular-lighting
;; component of the illumination equation
;; 
(define (specular-rgb mat v-normal v-viewer list)
  (local
    {(define specular (Material-specular mat))
     (: do-illum : Illum RGB -> RGB)
     ;; add the diffuse light from one source
     (define (do-illum illum rgb)
       (rgb+ (rgb-scale
              (expt (max 0 (vec3-dot (vec3-halfway (Illum-dir illum) v-viewer) v-normal)) (Material-sharpness mat))
              (Illum-intensity illum))
             rgb))}
    (rgb* specular (foldl do-illum (RGB 0 0 0) list))))


(: trace : Scene Ray Integer -> RGB)
;; trace a ray in the scene, where the third argument specifies
;; the recursion-depth limit.  If it is zero, then return the
;; background color.  Otherwise check for a hit and compute
;; the ray's color if there is a hit
;;
(define (trace scene ray depth-limit)
  (local {(: reflect-ray : Ray Object -> Ray)
          (define (reflect-ray ray object)
            (match ((ray-object-intersect ray) object)
              ['miss ray]
              [(Hit t obj)
               (local {(: hit-pos : Vec3)
                       (define hit-pos (ray-point-at ray t))}
                 (ray-offset (Ray hit-pos
                                  (vec3-reflect (Ray-dir ray)
                                                (object-normal object hit-pos))) 0.0001))]))}
    (match scene
      [(Scene background objects ambient lights)
       (match (closest-hit scene ray)
         ['miss background]
         [(Hit t obj)
          (local {(: hit-pos : Vec3)
                  (define hit-pos (ray-point-at ray t))
                  (: normal : Vec3)
                  (define normal (object-normal obj hit-pos))
                  (: viewer : Vec3)
                  (define viewer (vec3-normalize (vec3-negate (Ray-dir ray))))
                  (: illist : (Listof Illum))
                  (define illist (get-illumination scene hit-pos))
                  (: mat : Material)
                  (define mat (object-material obj))}
            (rgb+ (diffuse-rgb scene mat normal illist)
                  (rgb+ (specular-rgb mat normal viewer illist)
                        (rgb* (Material-specular mat)
                              (if (= depth-limit 0)
                                  (RGB 0 0 0)
                                  (trace scene (reflect-ray ray obj) (- depth-limit 1)))))))])])))

            
                            
(check-within (trace (Scene (RGB 1 2 3)
                            (list (make-sphere (Vec3 0 0 3) 2 (Material (RGB 1 1 1) (RGB 1 4 2) 2))
                                  (make-sphere (Vec3 1 0 -3) 2 (Material (RGB 1 1 1) (RGB 1 4 2) 2)))
                            (RGB 2 3 4)
                            '())
                     (make-ray (Vec3 0 0 0) (Vec3 0 0 1))
                     10)
              (RGB 5 47 24) 0.001)




(: trace-ray : Scene Natural -> Ray -> RGB)
;; takes a Scene and the initial ray that defines
;; a pixel and traces the ray through
;; the scene and computes the color of the light
;; that flows backwards along the ray’s path
(define (trace-ray scene depth)
  (lambda ([ray : Ray])
    (trace scene ray depth)))




(check-expect ((trace-ray (Scene (RGB 0 0 1)
                                 (list (make-sphere (Vec3 1 2 1) 0.5 (Material (RGB 0 1 1) (RGB 0 1 1) 2))
                                       (make-sphere (Vec3 0 0 3) 0.5 (Material (RGB 0 1 1) (RGB 0 1 1) 2))
                                       (make-sphere (Vec3 0 0 6) 0.5 (Material (RGB 0 1 1) (RGB 0 1 1) 2))
                                       (make-sphere (Vec3 0 0 4) 0.5 (Material (RGB 0 1 1) (RGB 0 1 1) 2)))
                                 (RGB 1 1 1)
                                 '()) 10)
               (make-ray (Vec3 0 0 0) (Vec3 0 0 1)))
              (RGB 0 1 2))

(check-expect ((trace-ray (Scene (RGB 0 0 1)
                                 (list (make-sphere (Vec3 1 1 1) 0.5 (Material (RGB 0 1 1) (RGB 0 1 1) 2)))
                                 (RGB 1 1 1)
                                 (list (make-dir-light (Vec3 1 1 1) (RGB 28 18 28)))) 10)
               (make-ray (Vec3 0 0 0) (Vec3 0 0 1)))
              (RGB 0 0 1))



;; Exports
;;;;;;;;;;

(provide Light
         Scene)

(provide make-dir-light
         make-scene
         trace-ray)
(provide make-point-light)


(test)