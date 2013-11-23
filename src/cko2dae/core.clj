(ns cko2dae.core
  (:require clojure.string
            [clojure.data.xml :as xml]
            fs.core)
  (:import java.text.SimpleDateFormat
           java.util.Calendar
           java.util.TimeZone
           java.lang.Math
           java.lang.Double)
  (:gen-class))

(defrecord vec3 [x y z])
(defrecord mat [colour])
(defrecord poly [rgvec3 mat])
(defrecord cube [vec3 mat])

(defn rgpoly-quad [v1 v2 v3 v4 mat]
  [(->poly [v1 v2 v3] mat) (->poly [v3 v2 v4] mat)])

(defn rgpoly-cube [vec3 mat]
  (let [{:keys [x y z]} vec3
        x_ (+ 1 x),  y_ (+ 1 y),  z_ (+ 1 z)
        qFront (rgpoly-quad (->vec3 x y z)  (->vec3 x y_ z)  (->vec3 x_ y z)  (->vec3 x_ y_ z) mat)
        qBack  (rgpoly-quad (->vec3 x y z_) (->vec3 x_ y z_) (->vec3 x y_ z_) (->vec3 x_ y_ z_) mat)
        qTop   (rgpoly-quad (->vec3 x y_ z) (->vec3 x y_ z_) (->vec3 x_ y_ z) (->vec3 x_ y_ z_) mat)
        qBot   (rgpoly-quad (->vec3 x y z)  (->vec3 x_ y z)  (->vec3 x y z_)  (->vec3 x_ y z_) mat)
        qLeft  (rgpoly-quad (->vec3 x y z)  (->vec3 x y z_)  (->vec3 x y_ z)  (->vec3 x y_ z_) mat)
        qRight (rgpoly-quad (->vec3 x_ y z) (->vec3 x_ y_ z) (->vec3 x_ y z_) (->vec3 x_ y_ z_) mat)]
    (concat qTop qBot qFront qBack qLeft qRight)))

(defn minmax [rgval]
  (reduce (fn [{:keys [min max]} val] {:min (Math/min min val) :max (Math/max max val)})
          {:min java.lang.Double/POSITIVE_INFINITY :max java.lang.Double/NEGATIVE_INFINITY}
          rgval))

(defn delta-from-minmax [{:keys [min max]}]
  (- (/ (- min max) 2) min))

(defn rgpoly-center [rgpoly]
  (let [rgvec3 (apply concat (for [poly rgpoly] (:rgvec3 poly)))
        {dx :x dy :y dz :z}
        (reduce (fn [deltas key]
                  (let [rgval (map #(double (get % key)) rgvec3)]
                    (assoc deltas key (-> rgval minmax delta-from-minmax))))
                {}
                [:x :y :z])]
    (for [poly rgpoly]
      (let [rgvec3New
            (for [{:keys [x y z]} (:rgvec3 poly)]
              (->vec3 (+ dx x) (+ dy y) (+ dz z)))]
        (->poly rgvec3New (:mat poly))))))

(defn dedup [rgval]
  (loop [i 0
         rgval rgval
         mpval_i {}
         rgval_result []]
    (if (seq rgval)
      (let [val (first rgval)]
        (if (nil? (get mpval_i val))
          (recur (inc i) (next rgval) (assoc mpval_i val i) (conj rgval_result val))
          (recur i (next rgval) mpval_i rgval_result)))
      [mpval_i rgval_result])))


(defn dae-merge-libs [& rglibs]
  (apply merge-with #(concat %1 %2) rglibs))

(defn dae-vec3-array [rgvec3 id x y z]
  (let [rgfloat (apply concat (map (fn [{:keys [x y z]}] [x y z]) rgvec3))]
    [:source {:id id}
       [:float_array {:id (str id "_array") :count (count rgfloat)}
                     (clojure.string/join " " rgfloat)]
       [:technique_common {}
          [:accessor {:source (str "#" id "_array") :count (count rgvec3) :stride 3}
             [:param {:name x :type "float"}]
             [:param {:name y :type "float"}]
             [:param {:name z :type "float"}]]]]))

(defn id-from-mat [mat]
  (str "mat_" (:colour mat)))

(defn dae-colour-from-mat [mat]
  (let [colour (:colour mat)
        rgbaHex [(subs colour 2 4) (subs colour 4 6) (subs colour 6 8) (subs colour 0 2)]
        rgba (for [hex rgbaHex]
               (-> (Integer/parseInt hex 16)
                   (/ 255.0)))]
    [:color {} (clojure.string/join " " rgba)]))

(defn dae-mat-from-mat [mat]
  (if mat
    (let [idMat (id-from-mat mat)
          idFx (str idMat "_fx")]
      {:materials
       [[:material {:id idMat }
         [:instance_effect {:url (str "#" idFx)}]]]
       :effects
       [[:effect {:id idFx}
         [:profile_COMMON {}
          [:technique {:sid "common"}
           [:phong {}
            [:diffuse {} (dae-colour-from-mat mat)]]]]]]})))

(defn rgmat-from-rgpoly [rgpoly]
  (set (for [{mat :mat} rgpoly :when (not (nil? mat))] mat)))

(defn dae-mat-from-rgpoly [rgpoly]
  (reduce dae-merge-libs
          {}
          (for [mat (rgmat-from-rgpoly rgpoly)] (dae-mat-from-mat mat))))

(defn dae-geom-from-rgpoly [rgpoly id name]
  (let [rgtri (apply concat (map :rgvec3 rgpoly))
        [mpvec3_i rgvec3] (dedup rgtri)
        mpmat_rgtri (reduce (fn [mp poly] (merge-with concat mp {(:mat poly) (:rgvec3 poly)})) {} rgpoly)
        points_id (str id "_points")
        vertices_id (str id "_vertices")]
    {:geometries
     [[:geometry {:id id :name name}
       (into [] (concat
         [:mesh {}
            (dae-vec3-array rgvec3 points_id "X" "Y" "Z")
            ;todo: textures
            [:vertices {:id vertices_id}
               [:input {:semantic "POSITION" :source (str "#" points_id)}]]]
         (for [[mat rgtri] mpmat_rgtri]
           [:triangles {:count (/ (count rgtri) 3) :material (id-from-mat mat)}
             [:input {:semantic "VERTEX" :source (str "#" vertices_id) :offset 0}]
             [:p {} (clojure.string/join " " (map #(get mpvec3_i %) rgtri))]])))]]}))

(defn now
  "Returns current ISO 8601 compliant date."
  []
  (let [f (SimpleDateFormat. "yyyy-MM-dd'T'HH:mm:ss'Z'")]
    (.setTimeZone f (TimeZone/getTimeZone "GMT"))
    (.format f (.getTime (Calendar/getInstance)))))

(defn dae-from-desc [libs scene]
  (-> [:COLLADA {:xmlns "http://www.collada.org/2005/11/COLLADASchema" :version "1.4.1"}
         [:asset
           [:created {} (now)]
           [:modified {} (now)]
           [:up_axis {} "Y_UP"]]]
      (into
        (for [[lib nodes] libs]
          (into [(keyword (str "library_" (name lib))) {}] nodes)))
      (conj [:scene {} scene])))

(defn dae-from-rgpoly [rgpoly name]
  (dae-from-desc
   (dae-merge-libs
    (dae-geom-from-rgpoly rgpoly (str "id_" name) name)
    (dae-mat-from-rgpoly rgpoly)
    {:visual_scenes [[:visual_scene {:id (str "id_" name "_scene") :name (str name "_scene")}
                       [:node {:id (str name "_node") :name name :type "NODE"}
                         [:instance_geometry {:url (str "#id_" name)}
                          [:bind_material {}
                           [:technique_common {}
                            (for [mat (rgmat-from-rgpoly rgpoly)]
                              [:instance_material {:symbol (id-from-mat mat)
                                                   :target (str "#" (id-from-mat mat))}])]]]]]]})
   [:instance_visual_scene {:url (str "#id_" name "_scene")}]))

(defn parse-number
  "Reads a number from a string. Returns nil if not a number."
  [s]
  (if (re-find #"^-?\d+\.?\d*$" s)
    (read-string s)))

(defn rgcube-from-cko [cko]
  (let [[header version count & rgstcube] (clojure.string/split-lines cko)]

    ; todo check header, version, count
    (for [stcube rgstcube]
      (let [[x y z mat] (clojure.string/split stcube (re-pattern " +"))]
        (->cube (->vec3 (parse-number x) (parse-number y) (parse-number z)) (->mat mat))))))

(defn rgpoly-from-rgcube [rgcube]
  (apply concat (for [{:keys [vec3 mat]} rgcube] (rgpoly-cube vec3 mat))))

(defn name-from-filename [filename]
  (first (fs.core/split-ext filename)))

(defn filename-with-ext [filename ext]
  (let [extPrev (second (fs.core/split-ext filename))
        filenameStripped (subs filename 0 (- (count filename) (count extPrev)))]
    (str filenameStripped ext)))

(defn convert-cko-to-dae [filenameIn]
  (let [filenameOut (filename-with-ext filenameIn ".dae")
        name (name-from-filename filenameIn)]
    (-> (slurp filenameIn)
        rgcube-from-cko
        rgpoly-from-rgcube
        rgpoly-center
        (write-dae name filenameOut))))

(defn write-dae [rgpoly name filename]
  (let [xml (-> rgpoly
                (dae-from-rgpoly name)
                xml/sexp-as-element
                xml/emit-str)]
    (spit filename xml)))

(defn write-test-cube []
  (write-dae (rgpoly-cube (->vec3 0 0 0) nil) "cube" "C:/dev/unity/cube.dae"))

(defn -main
  "Takes all the CKO files on the command line and converts them to DAEs."
  [& args]
  (doseq [filename args]
    (convert-cko-to-dae filename)))
