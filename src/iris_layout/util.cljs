(ns iris-layout.util
  (:require-macros [iris-layout.css :refer [inline-css]]))

(defn generate-id []
  (str (random-uuid)))

(defn liquid-glass-svg
  "Generate an SVG displacement map matching the jh3y liquid glass technique.
   Red channel = X displacement, Blue channel = Y displacement,
   blended with mix-blend-mode:difference. Inner rect creates the glass center."
  [width height radius border-pct blur lightness alpha]
  (let [border (* (min width height) (* border-pct 0.5))
        inner-x border
        inner-y border
        inner-w (- width (* border 2))
        inner-h (- height (* border 2))]
    (str "data:image/svg+xml,"
         (js/encodeURIComponent
           (str "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 " width " " height "'>"
                "<defs>"
                "<linearGradient id='red' x1='100%' y1='0%' x2='0%' y2='0%'>"
                "<stop offset='0%' stop-color='#000'/>"
                "<stop offset='100%' stop-color='red'/>"
                "</linearGradient>"
                "<linearGradient id='blue' x1='0%' y1='0%' x2='0%' y2='100%'>"
                "<stop offset='0%' stop-color='#000'/>"
                "<stop offset='100%' stop-color='blue'/>"
                "</linearGradient>"
                "</defs>"
                "<rect x='0' y='0' width='" width "' height='" height "' fill='black'/>"
                "<rect x='0' y='0' width='" width "' height='" height "' rx='" radius "' fill='url(#red)'/>"
                "<rect x='0' y='0' width='" width "' height='" height "' rx='" radius "' fill='url(#blue)' style='mix-blend-mode:difference'/>"
                "<rect x='" inner-x "' y='" inner-y "' width='" inner-w "' height='" inner-h
                "' rx='" radius "' fill='hsl(0 0% " lightness "% / " alpha
                ")' style='filter:blur(" blur "px)'/>"
                "</svg>")))))

(defn inject-liquid-glass-filter!
  "Inject an SVG filter element for the liquid glass displacement effect."
  []
  (when-not (js/document.getElementById "iris-liquid-glass-svg")
    (let [svg-ns   "http://www.w3.org/2000/svg"
          svg      (.createElementNS js/document svg-ns "svg")
          ;; Dimensions for the displacement map texture
          width    336
          height   96
          radius   16
          border   0.07
          blur     11
          lightness 50
          alpha    0.93
          scale    -180
          data-uri (liquid-glass-svg width height radius border blur lightness alpha)]
      (set! (.-id svg) "iris-liquid-glass-svg")
      (.setAttribute svg "width" "0")
      (.setAttribute svg "height" "0")
      (.setAttribute svg "style" "position:absolute;pointer-events:none")
      (set! (.-innerHTML svg)
            (str "<defs>"
                 "<filter id='iris-liquid-glass' x='0%' y='0%' width='100%' height='100%' color-interpolation-filters='sRGB'>"
                 ;; Load displacement map image
                 "<feImage href='" data-uri "' x='0' y='0' width='100%' height='100%' result='map' preserveAspectRatio='none'/>"
                 ;; RED channel — strongest displacement
                 "<feDisplacementMap in='SourceGraphic' in2='map' scale='" scale
                 "' xChannelSelector='R' yChannelSelector='G' result='dispRed'/>"
                 "<feColorMatrix in='dispRed' type='matrix' "
                 "values='1 0 0 0 0  0 0 0 0 0  0 0 0 0 0  0 0 0 1 0' result='red'/>"
                 ;; GREEN channel — least displaced
                 "<feDisplacementMap in='SourceGraphic' in2='map' scale='" (* scale 0.5)
                 "' xChannelSelector='R' yChannelSelector='G' result='dispGreen'/>"
                 "<feColorMatrix in='dispGreen' type='matrix' "
                 "values='0 0 0 0 0  0 1 0 0 0  0 0 0 0 0  0 0 0 1 0' result='green'/>"
                 ;; BLUE channel — medium displacement
                 "<feDisplacementMap in='SourceGraphic' in2='map' scale='" (* scale 0.75)
                 "' xChannelSelector='R' yChannelSelector='G' result='dispBlue'/>"
                 "<feColorMatrix in='dispBlue' type='matrix' "
                 "values='0 0 0 0 0  0 0 0 0 0  0 0 1 0 0  0 0 0 1 0' result='blue'/>"
                 ;; Blend channels back together
                 "<feBlend in='red' in2='green' mode='screen' result='rg'/>"
                 "<feBlend in='rg' in2='blue' mode='screen' result='output'/>"
                 ;; Slight blur to smooth chromatic edges
                 "<feGaussianBlur in='output' stdDeviation='0.7'/>"
                 "</filter>"
                 ;; Glass surface distortion — fractal noise displacement (Chrome-only)
                 "<filter id='iris-glass-distort' x='-5%' y='-5%' width='110%' height='110%'"
                 " color-interpolation-filters='sRGB'>"
                 "<feTurbulence type='fractalNoise' baseFrequency='0.65 0.75'"
                 " numOctaves='3' seed='8' result='noise'/>"
                 "<feDisplacementMap in='SourceGraphic' in2='noise'"
                 " scale='2.5' xChannelSelector='R' yChannelSelector='G'/>"
                 "</filter>"
                 "</defs>"))
      (.appendChild (.-body js/document) svg))))

(defn inject-css! []
  (when-not (js/document.getElementById "iris-layout-styles")
    (let [style (.createElement js/document "style")]
      (set! (.-id style) "iris-layout-styles")
      (set! (.-textContent style) (inline-css))
      (.appendChild (.-head js/document) style)))
  (inject-liquid-glass-filter!))
