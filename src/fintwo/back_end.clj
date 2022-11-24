(ns fintwo.back-end
  (require
    [clojure.string :as str]
    [clojure.java.io :as io]
    ))

(defn process-string [inc-str]                              ;; NOTE refactor this devil's clipboard
  (into {}
        (for [[k v]
              (for [ns (str/split inc-str #"&")]
                (str/split ns #"="))]
          [(keyword k) v])))

(defn new-farmer-to-backend [farm-str file-name]            ;; NOTE rename
  (spit (str (io/resource "saved")
             (str "\\" file-name ".txt")) farm-str))

(defn wheat-gross-yeild [acres fertility]                   ;; NOTE this if for the crab world. In real life medieval farms had 10 bushels an acre for good lands(we think).
  (* (* (/ fertility 100) 20) (* 60 acres))
  )

(defn peanut-gross-yeild [acres fertility]
  (* (* (/ fertility 100) 50) (* 22 acres))
  )

(defn loss-calculation [pounds skl hrvst thrsh raid anml plgue] ;; This assumes plagues happen after raids and animals and junk but before harvesting.
  (let [idiocy (- pounds (* pounds (* skl 0.01)))           ;; Animals and raiders should maybe be a flat number but It's cool for now.
        thieves (- idiocy (* idiocy (* (+ raid anml) 0.01)))
        disease (- thieves (* thieves (* plgue 0.01)))
        harvest (- disease (* disease (* hrvst 0.01)))
        thrsh (- harvest (* harvest (* thrsh 0.01)))        ;; We're not calculating milling losses at the moment but we should be
        ]
    thrsh))

(defn farmer-production [crp-typ acrs frt]
  (case crp-typ
    "wheat" (wheat-gross-yeild acrs frt)
    "peanut" (peanut-gross-yeild acrs frt)                  ;; NOTE This is modern peanuts and maybe pre shelling. I have no idea.
    :else "Bruh"))

(defn get-group-type [grp]
  (:group-type
    (process-string
      (slurp
        (str (io/resource
               (str "\\saved\\" grp ".txt")))))))

(defn get-production-details [production]
  (let [new-prod                                            ;; NOTE temp.
        (process-string
          (slurp
            (str (io/resource
                   (str "\\saved\\" production ".txt")))))
        total-pay-daily (* (bigdec (:average-pay new-prod)) (bigdec (:workers new-prod)))
        total-pay-per-season (* 60 total-pay-daily)
        units-per-season (* (bigdec (:units-per-day new-prod)) 60)
        income-per-day (* (bigdec (:units-per-day new-prod)) (bigdec (:sales-price new-prod)))
        cost-per-day (* (bigdec (:units-per-day new-prod)) (bigdec (:ingredient-cost-per-unit new-prod)))
        cost-per-season (* 60 cost-per-day)
        profit-per-day (- income-per-day cost-per-day)
        profit-per-season (* 60 (- income-per-day cost-per-day))]

    {:total-pay-daily      total-pay-daily
     :total-pay-seasonal   total-pay-per-season
     :units-per-season     units-per-season
     :gross-income-per-day income-per-day
     :cost-per-day         cost-per-day
     :cost-per-season      cost-per-season
     :profit-per-day       profit-per-day
     :profit-per-season    profit-per-season
     }))

(defn get-soldier-details [soldier]
  (let [new-sldr
        (process-string
          (slurp
            (str (io/resource
                   (str "\\saved\\" soldier ".txt")))))]
    {:total-pay-daily    (* (bigdec (:average-pay new-sldr)) (bigdec (:numbers new-sldr)))
     :total-pay-seasonal (* (* (bigdec (:average-pay new-sldr)) (bigdec (:numbers new-sldr))) 60)
     }))

(defn get-livestock-details [animals]
  (let [new-animals
        (process-string
          (slurp
            (str (io/resource
                   (str "\\saved\\" animals ".txt")))))]
    (println new-animals)
    {:animal-type            "Pig"
     :feed-pounds-per-day    1
     :workers-pay-per-day    1
     :workers-pay-per-season 1
     :feed-cost-per-day      1
     :total-feed-per-animal  1                              ;; over it's whole life
     :meat-per-day           1                              ;; Meat isn't really correct as it's total biomass
     :meat-per-season        1
     :meat-per-animal        1
     :kids-from-one-litter   1                              ;one litter of all the animals.
     }))

(defn get-farm-details [farm]
  (let [new-frm
        (process-string
          (slurp
            (str (io/resource
                   (str "\\saved\\" farm ".txt")))))
        pot-yield (farmer-production (:crop-type new-frm) (Integer. (:acres new-frm)) (Integer. (:fertility new-frm)))]
    {:net-yield       (loss-calculation pot-yield (Integer. (:skill-stupidity new-frm)) (Integer. (:harvest-losses new-frm)) (Integer. (:threshing-losses new-frm))
                                        (Integer. (:raiding-losses new-frm)) (Integer. (:animal-losses new-frm)) (Integer. (:plague-losses new-frm)))
     :potential-yield pot-yield}))