(ns dfy.nanoscopic
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

;; By Erik Svedäng, Nov 2014

(defn init-text-attrs []
  (->
    (q/create-font "Arial" 16 true)
    (q/text-font 16)))

(defn pulse [low high rate]
  (let [diff (- high low)
        half (/ diff 2)
        mid (+ low half)
        s (/ (q/millis) 1000.0)
        x (q/sin (* s (/ 1.0 rate)))]
    (+ mid (* x half))))

(defn rand-between [low high]
  (let [diff (- high low)]
    (+ low (rand diff))))

(defn rand-coord [size]
  [(rand-between (- size) size)
   (rand-between (- size) size)])

(defn translate-v2 [[x y] [dx dy]]
  [(+ x dx) (+ y dy)])

(defn draw-axis? [entity]
  (:draw-axis entity))

(defn draw-axis
  ([]
   (draw-axis 100))
  ([length]
    ; x red
   (q/with-stroke [255 40 40]
                  (q/line 0 0 length 0))
    ; y green
   (q/with-stroke [40 255 40]
                  (q/line 0 0 0 length))))

(defn render-ship0 [ship]
  (q/stroke 0 0)                                            ; no outline
  ; Magnetoplasmadynamic thruster jet
  (let [speed (:speed ship)
        len (* -40 speed)]
    (when (pos? speed)
      (q/fill 0 180 250)
      (q/triangle 0 -5 0 5 len 0)
      (when (< 0.5 speed)
        (q/fill 220 220 250)
        (q/triangle 0 -2 0 2 -15 0))))
  ; ship
  (q/fill 50 80 50)
  (q/rect -2 0 5 14)
  (q/fill 150 180 150)
  (q/triangle 0 -10 25 0 0 10)
  (q/fill 30 100 30)
  (q/ellipse 8 0 8 8))

(defn render-ship [ship]
  (render-ship0 ship))

(defn create-ship []
  {:pos        [-1000.0 1000.0]
   :velocity   [0.0 0.0]
   :dir        -0.4
   :dir-change 0.0
   :speed      0.0
   :z          1.0
   :draw-axis  true
   :render-fn  render-ship})

(defn render-star [star]
  (let [size (:size star)]
    (q/fill 255)
    (q/rect 0 0 size size)))

(defn create-star [pos]
  {:pos       pos
   :dir       (rand q/TWO-PI)
   :size      (+ 1.0 (rand 3.0))
   :z         (rand-between 0.2 0.7)
   :render-fn render-star})

(defn random-star []
  (create-star (rand-coord 1000)))

(defn render-smoke [smoke]
  (let [age (:age smoke)
        size (max 0.0 (- 10.0 (* 5.0 age)))
        [r g b] (:col smoke)]
    (q/fill r g b 200)
    (q/ellipse 0 0 size size)))

(defn create-smoke [[x y]]
  {:pos       [(+ x (rand-between -3 3))
               (+ y (rand-between -3 3))]
   :dir       0.0
   :age       0.0
   :z         1.0
   :col       [(rand-between 150 255)
               (rand-between 100 200)
               (rand-between 0 100)]
   :render-fn render-smoke})

(defn render-planet [planet]
  (let [size (:size planet)
        [r g b] (:color planet)]
    (q/fill r g b)
    (let [rs (:rs planet)
          step (/ q/TWO-PI (count rs))]
      (q/begin-shape)
      (doseq [[angle radius] (map vector
                                  (range 0 q/TWO-PI step) rs)]
        (q/vertex (* size radius (q/cos angle))
                  (* size radius (q/sin angle))))
      (q/end-shape))))

(defn generate-radiuses []
  (into [] (take (+ 5 (rand-int 7))
                 (repeatedly #(rand-between 0.5 1.0)))))

(defn create-planet [pos color]
  {:pos        pos
   :dir        (rand q/TWO-PI)
   :dir-change (rand-between -0.01 0.01)
   :size       (+ 50.0 (rand 50.0))
   :drift      [(rand-between -0.3 0.3) (rand-between -0.3 0.3)]
   :color      color
   :z          1.0
   :rs         (generate-radiuses)
   :render-fn  render-planet})

(defn random-planet []
  (create-planet (rand-coord 1000)
                 [(rand-between 0 255)
                  (rand-between 50 150)
                  (rand-between 50 150)]))

(defn setup []
  (q/rect-mode :center)
  (q/frame-rate 30)
  (init-text-attrs)
  {:ship    (create-ship)
   :smoke   []
   :stars   (take 3000 (repeatedly random-star))
   :planets (take 50 (repeatedly random-planet))
   :paused  false})


(defn move-ship
  " m.a = ∑ f = m.dv/dt
    dv = (∑ f).(1/m).dt
    u_1 = u_0 + du = u_0 + (v_0 + dv).dt

    dx = (vx + dvx).dt

  "
  [ship dt]
  (let [speed (+ 0.0 (* 7.0 (:speed ship)))
        dir (:dir ship)
        [vx vy] (:velocity ship)
        invm 0.00005
        ;;---
        fx (* speed (q/cos dir))
        fy (* speed (q/sin dir))
        dvx (* fx invm dt)
        dvy (* fy invm dt)
        nvx (+ vx dvx)
        nvy (+ vy dvy)
        normalizer (Math/sqrt (+ (* nvx nvx) (* nvy nvy)))
        normalizer (if (< 0.25 normalizer)
                     (/ 0.25 normalizer)
                     1)
        nvx (* nvx normalizer)
        nvy (* nvy normalizer)
        dx (* nvx dt)
        dy (* nvy dt)]
    (-> ship
        (assoc :velocity [nvx nvy])
        (update-in [:pos] translate-v2 [dx dy]))))

(defn auto-rotate [entity]
  (let [dir-change (:dir-change entity)]
    (update-in entity [:dir] #(+ % dir-change))))

(defn wiggle-ship [ship]
  (let [speed (:speed ship)
        a (+ 0.01 (* 0.03 speed))]
    (update-in ship [:dir] #(+ % (pulse (- a) a 0.1)))))

(defn drift-planet [planet]
  (let [[dx dy] (:drift planet)]
    (update-in planet [:pos] translate-v2 [dx dy])))

(defn emit-smoke [state]
  (let [speed (-> state :ship :speed)]
    (if (< (rand) (+ 0.2 speed))
      (let [ship-pos (-> state :ship :pos)]
        (update-in state [:smoke] conj (create-smoke ship-pos)))
      state)))

(defn age-smoke [smoke]
  (update-in smoke [:age] #(+ % 0.033)))

(defn old? [smoke]
  (< 3.0 (:age smoke)))

(defn remove-old-smokes [smokes]
  (remove old? smokes))

(defn remember-last-tick [state]
  (assoc state :lastTick (q/millis)))

(defn last-tick [state]
  (get state :lastTick (q/millis)))

(defn update-state [state]
  (if (:paused state)
    (remember-last-tick state)
    (let [dt (- (q/millis) (last-tick state))]
      (-> state
          (update-in [:ship] auto-rotate)
          ;(update-in [:ship] wiggle-ship)
          (update-in [:ship] move-ship dt)
          emit-smoke
          (update-in [:smoke] (fn [smokes] (map age-smoke smokes)))
          (update-in [:smoke] remove-old-smokes)
          (update-in [:planets] #(map auto-rotate %))
          (update-in [:planets] #(map drift-planet %))
          (remember-last-tick)))))

(defn faster [speed]
  (min 1.0 (+ speed 0.25)))

(defn slower [speed]
  (max 0.0 (- speed 0.25)))

(defn on-key-down [state event]
  (let [key (:key event)]
    (println (str "on-key-down >" key "<")))
  (case (:key event)
    (:p) (update-in state [:paused] not)
    (:w :up) (update-in state [:ship :speed] faster)
    (:s :down) (update-in state [:ship :speed] slower)
    (:a :left) (assoc-in state [:ship :dir-change] -0.15)
    (:d :right) (assoc-in state [:ship :dir-change] 0.15)
    state))

(defn on-key-up [state]
  (if (contains? #{:left :right :a :d}
                 (q/key-as-keyword))
    (assoc-in state [:ship :dir-change] 0)
    state))

(defn on-screen? [x y]
  (let [margin 100]
    (and (<= (- margin) x (+ margin (q/width)))
         (<= (- margin) y (+ margin (q/height))))))

(defn draw-entity [entity [cam-x cam-y]]
  (let [[x y] (:pos entity)
        dir (:dir entity)
        z (:z entity)
        render-fn (:render-fn entity)
        screen-x (- x (* z cam-x))
        screen-y (- y (* z cam-y))]
    (when (on-screen? screen-x screen-y)
      (q/push-matrix)
      (q/translate screen-x screen-y)
      (if (draw-axis? entity)
        (draw-axis 100))
      (q/rotate dir)
      (render-fn entity)
      (q/pop-matrix))))

(def RAD-TO-DEG (/ 180 Math/PI))
(defn rad-to-deg [angle]
  (mod (int (* RAD-TO-DEG angle)) 360))

(defn draw-stats [state]
  (let [nbSmokes (count (get-in state [:smoke]))
        speed (get-in state [:ship :speed])
        dir (get-in state [:ship :dir])
        [vx vy] (get-in state [:ship :velocity])
        nv (+ (* vx vx) (* vy vy))
        [px py] (get-in state [:ship :pos])]
    (q/text-align :left)
    (q/fill 255 255 255)
    (q/text (str "smoke particles: " nbSmokes) 10 20)
    (q/text (str "speed: " speed) 10 36)
    (q/text (str "dir: " (rad-to-deg dir) "°") 10 52)
    (q/text (format "v: %.5f, %.5f (%.5f)" vx vy nv) 10 68)
    (q/text (format "p: %.1f, %.1f" px py) 10 84)
    ))

(defn- draw-smokes [smokes cam-pos]
  (doseq [smoke smokes]
    (draw-entity smoke cam-pos)))

(def draw-smoke? true)

(defn draw-state [state]
  (q/background (pulse 20 40 15.0)
                (pulse 40 60 40.0)
                (pulse 50 70 5.0))
  (q/no-stroke)
  (let [ship-pos (-> state :ship :pos)
        cam-pos (translate-v2 ship-pos [(- (/ (q/width) 2))
                                        (- (/ (q/height) 2))])]
    (doseq [star (:stars state)]
      (draw-entity star cam-pos))
    (doseq [planet (:planets state)]
      (draw-entity planet cam-pos))
    (draw-smokes (if draw-smoke? (:smoke state) []) cam-pos)

    (draw-entity (:ship state) cam-pos)
    (draw-stats state)))

(q/defsketch nanoscopic
             :host "canvas"
             :size [500 500]
             :setup setup
             :update update-state
             :key-pressed on-key-down
             :key-released on-key-up
             :draw draw-state
             :middleware [m/fun-mode])