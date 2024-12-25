(ns aoc2024.day14
  (:require [clojure.string :as str])
  (:import [javax.swing JFrame JLabel JPanel BoxLayout
             Box JSpinner SwingUtilities]
           [javax.swing.event ChangeListener]
           [java.awt Dimension Color Graphics BorderLayout]))

(defrecord Coord [x y])
(defrecord Robot [position velocity])
(def width 101)
(def center-x (int (/ width 2)))
(def height 103)
(def center-y (int (/ height 2)))
(defn str->Robot
  [s]
  (->> (rest (re-find #"p=(\d+),(\d+) v=(-?\d+),(-?\d+)" s))
       (map parse-long)
       (partition 2)
       (map #(apply ->Coord %))
       (apply ->Robot)))
(def input
  (->> (slurp "resources/day14.txt")
       (str/split-lines)
       (map str->Robot)))

(defn pos-after-time
  [seconds
   {{px :x py :y} :position
    {vx :x vy :y} :velocity}]
  [(mod (+ px (* vx seconds)) width)
   (mod (+ py (* vy seconds)) height)])

(defn count-by-quadrant
  [robots-pos]
  (->> [(count (filter (fn [[x y]] (and (< x center-x) (< y center-y))) robots-pos))
        (count (filter (fn [[x y]] (and (> x center-x) (< y center-y))) robots-pos))
        (count (filter (fn [[x y]] (and (< x center-x) (> y center-y))) robots-pos))
        (count (filter (fn [[x y]] (and (> x center-x) (> y center-y))) robots-pos))]
       (reduce *)))

;; part 1
(time (->> input
           (map (partial pos-after-time 100))
           (filter (fn [[x y]] (and (not= center-x x) (not= center-y y))))
           count-by-quadrant))

;; part 2
(defn get-robots-after-time
  [seconds]
  (->> input
           (map (partial pos-after-time seconds))
           (filter (fn [[x y]] (and (not= center-x x) (not= center-y y))))
           frequencies))

;; GUI
(defn create-pixel-robot-grid-panel
  [rows cols cell-size robots-map]
  (proxy [JPanel] []
    (paintComponent [^Graphics g]
      (proxy-super paintComponent g) ; Call the parent method to ensure proper rendering
      (.setColor g Color/GREEN) ; Set grid line color
      ;; Draw the grid of robots
        (doseq [row (range rows)
                col (range cols)]
          (let [x (* col cell-size)
                y (* row cell-size)]
            ;; Draw grid cell outline
            (if (robots-map [col row])
              (do
                (.setColor g (Color. 15 15 35))
                (.drawRect g x y cell-size cell-size)
                (.setColor g Color/GREEN)
                (.fillRect g x y cell-size cell-size))
              (do
                (.setColor g (Color. 15 15 35))
                (.drawRect g x y cell-size cell-size)
                (.fillRect g x y cell-size cell-size))))))))

(defn create-grid-gui
  []
  (let [frame (JFrame. "Robots")
        panel (JPanel. (BorderLayout. ))
        cell-size 7
        robots-map (get-robots-after-time 0)
        pixl-panel (atom (create-pixel-robot-grid-panel height width cell-size robots-map))
        control-panel (JPanel. )
        seconds-panel (JPanel. )
        seconds-label (JLabel. "Second: ")
        seconds-jspinner (JSpinner. )]

    ;; main panel
    (.setBackground panel (Color. 15 15 35))

    ;; pixel grid display
    (.setPreferredSize @pixl-panel (Dimension. (* cell-size width) (* cell-size height)))
    (.setBackground @pixl-panel (Color. 15 15 35))
    (.add panel @pixl-panel BorderLayout/NORTH)

    ;;; seconds panel
    (.setForeground seconds-label (Color. 204 204 204))
    (.setLayout seconds-panel (BoxLayout. seconds-panel BoxLayout/X_AXIS))
    (.setBackground seconds-panel (Color. 15 15 35))
    (.setPreferredSize seconds-panel (Dimension. 50 50))
    (.add seconds-panel seconds-label)
    (.add seconds-panel seconds-jspinner)
    (.add control-panel seconds-panel)
    (.add control-panel (Box/createHorizontalStrut 20))

    (.addChangeListener seconds-jspinner
      (proxy [ChangeListener] []
        (stateChanged [event]
          (let [new-value (.getValue (.getSource event))
                new-robots-map (get-robots-after-time new-value)]
            (.remove panel @pixl-panel)
            (reset! pixl-panel (create-pixel-robot-grid-panel height width cell-size new-robots-map))
            (.setBackground @pixl-panel (Color. 15 15 35))
            (.add panel @pixl-panel )
            (.revalidate panel)
            (.repaint panel)))))

    ;(.setBorder seconds-panel (BorderFactory/createEmptyBorder 20 150 30 150))
    (.add panel seconds-panel BorderLayout/SOUTH)

    (.setSize frame 800 900)
    (.setDefaultCloseOperation frame JFrame/DISPOSE_ON_CLOSE)
    (.add frame panel)
    (.setVisible frame true)))

(SwingUtilities/invokeLater (create-grid-gui))
