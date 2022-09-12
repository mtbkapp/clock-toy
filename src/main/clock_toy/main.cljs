(ns clock-toy.main
  (:require [clojure.string :as string]
            [reagent.core :as r] 
            [reagent.dom :as rdom]
            ["moment" :as moment]
            ["react" :as react]))



(def width 600)
(def height 600)
(def clock-pad 8)
(def center-x (/ width 2))
(def center-y (/ height 2))
(def clock-radius (- center-x clock-pad))
(def label-radius (- clock-radius 24))
(def tick-len 10)
(def hour-hand-len (- clock-radius 100))
(def min-hand-len (- clock-radius 40))
(def label-bump-y 12)
(def grab-margin 20)

(def clock-time (r/atom (moment)))
(def drag-state (r/atom {:drag? false}))


(defn angle->mins
  [angle]
  (mod (Math/round (- 15 (/ (* 30 angle) Math/PI))) 60))


(defn find-angle
  [part whole]
  (- (* 0.5 Math/PI) (/ (* 2 Math/PI part) whole)))


(defn change-time!
  [field amount]
  (swap! clock-time
         (fn [t]
           (let [nt (.clone t)]
             (.add nt amount field)
             nt))))


(defn set-mins!
  [next-mins]
  (swap! clock-time
         (fn [t]
           (let [curr-mins (.minutes t)
                 nt (.clone t)]
             (cond (and (< -1 curr-mins 16)
                        (< 44 next-mins 60))
                   (.add nt -1 "hours")
                   (and (< 44 curr-mins 60)
                        (< -1 next-mins 16))
                   (.add nt 1 "hours"))
             (.minutes nt next-mins)
             nt))))


(defn wrap-prevent-default
  [f]
  (fn [e]
    (.preventDefault e)
    (f e)))


(defn drag-start 
  [e]
  (reset! drag-state {:drag? true}))


(defn drag-end 
  [e]
  (reset! drag-state {:drag? false}))


(defn drag-move
  [e]
  (swap! drag-state
         (fn [{:keys [drag?] :as s}]
           (when drag?
             (let [rect (.getBoundingClientRect (.-currentTarget e))
                   x (- (.-pageX e) (.-left rect))
                   y (- (.-pageY e) (.-top rect))
                   rx (- x center-x) 
                   ry (- center-y y)
                   mag (Math/sqrt (+ (Math/pow rx 2)
                                     (Math/pow ry 2)))
                   angle (if (< ry 0)
                           (- (* 2 Math/PI) (Math/acos (/ rx mag)))
                           (Math/acos (/ rx mag)))
                   mins (angle->mins angle)]
               (set-mins! mins)))
           s)))


(def center-bounding-rect 
  (delay
    (.getBoundingClientRect (js/document.getElementById "center"))))


(defn touch-move
  [e]
  (swap! drag-state 
         (fn [{:keys [drag?] :as s}]
           (when drag?
             (let [rect (.getBoundingClientRect (.-currentTarget e))
                   touch (aget (.-touches e) 0)
                   x (.-clientX touch) 
                   y (.-clientY touch)
                   center-rect @center-bounding-rect 
                   cx (+ (.-x center-rect) (/ (.-width center-rect) 2))
                   cy (+ (.-y center-rect) (/ (.-height  center-rect) 2))
                   rx (- x cx)
                   ry (- cy y)
                   mag (Math/sqrt (+ (Math/pow rx 2)
                                     (Math/pow ry 2)))
                   angle (if (< ry 0)
                           (- (* 2 Math/PI) (Math/acos (/ rx mag)))
                           (Math/acos (/ rx mag)))
                   mins (angle->mins angle)]
               (set-mins! mins)))
           s)))


(defn hour-hand
  [t]
  (let [hours (.hours t)
        mins (.minutes t)
        min-extra (/ (* 2 Math/PI mins) 720)
        angle (- (find-angle hours 12) min-extra) 
        x (* hour-hand-len (Math/cos angle))
        y (* hour-hand-len (Math/sin angle))]
    [:line {:id "hour"
            :class "hand" 
            :x1 center-x
            :y1 center-y
            :x2 (+ center-x x)
            :y2 (- center-y y)}]))



(defn svg-points
  [points]
  (->> points 
       (map (fn [[x y]]
              [(+ center-x x)
               (- center-y y)]))
       (map #(string/join "," %))
       (string/join " ")))


(defn minute-hand
  [t]
  (let [mins (.minutes t)
        angle (find-angle mins 60)
        x (* min-hand-len (Math/cos angle))
        y (* min-hand-len (Math/sin angle))
        pos-angle (mod angle (* 2 Math/PI))
        right (+ pos-angle (* 0.5 Math/PI))
        left (- pos-angle (* 0.5 Math/PI))
        ]
    [:g {:on-mouse-down drag-start
         :onTouchStart (wrap-prevent-default drag-start)}
     [:line {:id "minute"
             :class "hand"
             :x1 center-x
             :y1 center-y
             :x2 (+ center-x x)
             :y2 (- center-y y)}]
     [:polygon {:id "min-grab"
                :points (svg-points [[0 0]
                                     [(* grab-margin (Math/cos right)) 
                                      (* grab-margin (Math/sin right))]
                                     [(+ x (* grab-margin (Math/cos right))) 
                                      (+ y (* grab-margin (Math/sin right)))]
                                     [(+ x (* grab-margin (Math/cos left)))
                                      (+ y (* grab-margin (Math/sin left)))]
                                     [(* grab-margin (Math/cos left)) 
                                      (* grab-margin (Math/sin left))]])}]]))

(defn labels
  []
  (into [:g]
        (map (fn [hour]
               (let [angle (find-angle hour 12)
                     x (* label-radius (Math/cos angle))
                     y (- (* label-radius (Math/sin angle)) label-bump-y)]
                 [:text {:x (+ center-x x)
                         :y (- center-y y)
                         :class "label"} hour])))
        (range 1 13)))


(defn ticks
  []
  (into [:g]
        (comp (remove #(= 0 (mod % 5)))
              (map (fn [mins]
                     (let [inner-r (- clock-radius tick-len) 
                           outer-r clock-radius 
                           angle (- (* 0.5 Math/PI) (/ (* 2 Math/PI mins) 60))]
                       [:line {:x1 (+ center-x (* inner-r (Math/cos angle)))
                               :y1 (- center-y (* inner-r (Math/sin angle)))
                               :x2 (+ center-x (* outer-r (Math/cos angle)))
                               :y2 (- center-y (* outer-r (Math/sin angle)))
                               :class "tick"}]))))
        (range 1 60)))

(defn analog
  [t]
  [:div {:id "analog"}
   [:svg {:on-mouse-up drag-end
          :on-mouse-move drag-move
          :on-mouse-leave drag-end
          :onTouchMove (wrap-prevent-default touch-move)
          :onTouchEnd  (wrap-prevent-default drag-end)
          :onTouchCancel  (wrap-prevent-default drag-end)}
    [:circle {:cx center-x :cy center-y :r clock-radius}]
    [:circle {:id "center" :cx center-x :cy center-y :r 2}]
    [labels]
    [ticks]
    [hour-hand t]
    [minute-hand t]]])


(defn digital
  [t]
  [:table {:id "digital"}
   [:tbody
    [:tr
     [:td [:div {:class "buttons"} 
           [:button {:id "inc-hour" :on-click #(change-time! "hours" 1)} "^"]
           [:button {:id "dec-hour" :on-click #(change-time! "hours" -1)} "v"]]]
     [:td [:div {:id "digital-face"} (.format t "h:mm")]]
     [:td [:div {:class "buttons"}
           [:button {:id "inc-min" :on-click #(change-time! "minutes" 1)} "^"]
           [:button {:id "dec-min" :on-click #(change-time! "minutes" -1)} "v"]]]]]])


(defn root
  []
  (let [t @clock-time]
    [:table {:id "clocks"} 
     [:tbody
      [:tr 
       [:td [analog t]]
       [:td [digital t]]]]]))


(defn init
  []
  (js/console.log "clock toy init" react)
  (rdom/render [root] (js/document.getElementById "app"))
  (js/console.log "clock toy init done")
  )
