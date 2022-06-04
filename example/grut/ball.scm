(use file.util)
(use gauche.parseopt)
(use graviton)
(use graviton.grut)
(use graviton.misc)
(use text.console)

(import-js ("https://cdn.skypack.dev/three" :as THREE))

(define-jsvar camera)
(define-jsvar scene)
(define-jsvar ballMesh)
(define-jsvar renderer)

(bind-url-path "/ball_texture.png" (build-path (sys-dirname (current-load-path)) "ball_texture.png"))
(bind-url-path "/bounce1.wav" (build-path (sys-dirname (current-load-path)) "bounce1.wav"))
(bind-url-path "/bounce2.wav" (build-path (sys-dirname (current-load-path)) "bounce2.wav"))

(define field-left -0.6)
(define field-right 0.6)
(define field-bottom -0.35)

(define g-accel -0.5e-3)

(define-class <ball> ()
  ((angle :init-value 0)
   (angular-velocity :init-value 5e-2)
   (x :init-value -0.2)
   (y :init-value 0.35)
   (vx :init-value 0.4e-2)
   (vy :init-value 0)
   (floor-bounce-sound :init-form (load-audio "/bounce1.wav"))
   (side-bounce-sound :init-form (load-audio "/bounce2.wav"))))

(define *ball* (make-window-parameter #f))

(define (update-ball-position!)
  (with-slots (angle angular-velocity x y vx vy floor-bounce-sound side-bounce-sound) (*ball*)
    (let1 nx (+ x vx)
      (cond
        ((<= field-left nx field-right)
         (set! x nx))
        (else
         (set! vx (* -1 vx))
         (set! angular-velocity (* -1 angular-velocity))
         (side-bounce-sound'play))))
    (inc! vy g-accel)
    (let1 ny (+ y vy)
      (cond
        ((<= field-bottom ny)
         (set! y ny))
        (else
         (set! vy (* -1 vy))
         (floor-bounce-sound'play))))
    (inc! angle angular-velocity)))

(define (draw-ball)
  (jslet ((angle (~ (*ball*)'angle))
          (x (~ (*ball*)'x))
          (y (~ (*ball*)'y)))
    (ballMesh.quaternion.setFromEuler (new THREE.Euler 0 angle (- (/ Math.PI 6)) "ZYX"))
    (set! ballMesh.position.x x)
    (set! ballMesh.position.y y)
    (renderer.render scene camera)))

(define (setup-camera aspect)
  (jslet ((aspect aspect))
    (set! camera (new THREE.PerspectiveCamera 70 aspect 0.01 10))
    (set! camera.position.z 1)))

(define (setup-scene)
  (jslet ()
    (set! scene (new THREE.Scene)))

  (scene-add-light!)
  (scene-add-field!)
  (scene-add-ball!))

(define (scene-add-light!)
  (jslet ()
    (scene.add (new THREE.AmbientLight #xffffff 0.3))
    (let1 directionalLight (new THREE.DirectionalLight #xffffff 1.0)
      (directionalLight.position.set -1 0 1.7)
      (set! directionalLight.castShadow #t)
      (scene.add directionalLight))))

(define (scene-add-field!)
  (jslet ()
    ;; Add background plane.
    (let ((plane (new THREE.PlaneGeometry 10 10))
          (material (new THREE.MeshLambertMaterial)))
      (set! material.color (new THREE.Color #xaaaaaa))

      (let ((mesh (new THREE.Mesh plane material)))
        (set! mesh.position.z -0.3)
        (set! mesh.receiveShadow #t)
        (scene.add mesh)))

    ;; Add grid.
    (let ((points #())
          (grid (/ 2.25 15))
          (geometry (new THREE.BufferGeometry)))
      (let1 x -1.125
        (dotimes (i 16)
          (points.push (new THREE.Vector3 x -0.75 -0.3))
          (points.push (new THREE.Vector3 x 0.75 -0.3))
          (points.push (new THREE.Vector3 x -0.75 -0.3))
          (points.push (new THREE.Vector3 x -0.75 -0.1))
          (inc! x grid)))
      (let1 y 0.75
        (while (<= -0.75 y)
          (points.push (new THREE.Vector3 -1.125 y -0.3))
          (points.push (new THREE.Vector3 1.125 y -0.3))
          (dec! y grid)))
      (let1 z -0.3
        (while (<= z -0.1)
          (points.push (new THREE.Vector3 -1.125 -0.75 z))
          (points.push (new THREE.Vector3 1.125 -0.75 z))
          (inc! z grid)))
      (geometry.setFromPoints points)

      (let* ((material (new THREE.LineBasicMaterial (object ("color" . #xff00ff) ("linewidth" . 1))))
             (lineSegments (new THREE.LineSegments geometry material)))
        (scene.add lineSegments)))))

(define (scene-add-ball!)
  (jslet ()
    (let* ((loader (new THREE.TextureLoader))
           (texture (loader.load "/ball_texture.png"))
           (geometry (new THREE.SphereGeometry 0.25))
           (material (new THREE.MeshLambertMaterial (object ("map" . texture)))))
      (set! ballMesh (new THREE.Mesh geometry material))
      (set! ballMesh.castShadow #t)
      (scene.add ballMesh))))

(define (setup-renderer proc)
  (jslet ((proc proc))
    (set! renderer (new THREE.WebGLRenderer (object ("antialias" . #t) ("canvas" . canvas))))
    (set! renderer.shadowMap.enabled #t)
    (renderer.setAnimationLoop proc)))

(define (update-text text-console)
  (clear-screen text-console)
  (when (equal? (~ audio-context'state) "suspended")
    (putstr text-console "Hit any key to enable audio.")))

(define (main args)
  (let-args (cdr args)
      ((force-player? "player" #f)
       (force-browser? "browser" #f)
       (force-server? "server" #f))
    (grv-config :mode (cond
                        (force-player? 'player)
                        (force-browser? 'browser)
                        (force-server? 'server)
                        (else #f)))
    (with-window (grut-text+canvas-window 1024 768 :font-size "24px" :padding "5px")
        (text-console canvas)
      (on-jsevent window "keyup" (key)
        (when (equal? key "Escape")
          (close-window)))

      (on-jsevent audio-context "statechange" ()
        (update-text text-console))

      (*ball* (make <ball>))

      (setup-camera (/. (~ canvas'width) (~ canvas'height)))
      (setup-scene)
      (setup-renderer (lambda (t)
                        (update-ball-position!)
                        (draw-ball)))

      (update-text text-console))))

