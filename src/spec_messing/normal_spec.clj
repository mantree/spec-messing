(ns  spec-messing.normal-spec
  (:require [clojure.spec :as spec]))

(def conform-integer? 
  (spec/conformer 
   (fn [x]
     (if (integer? x)
       x
       (if (string? x)
         (try
           (java.lang.Integer/valueOf x)
           (catch Exception e
             :clojure.spec/invalid))
         :clojure.spec/invalid)))))

(defn split-conform
  [pat conf]
  (spec/conformer
   (fn [x]
     (spec/conform (spec/coll-of conf) (clojure.string/split x pat)))))

(spec/def ::id string?)
(spec/def ::another-id string?)
(spec/def ::id-set (split-conform #"," conform-integer?))
(spec/def ::record (spec/cat :id ::id
                             :another-id ::another-id
                             :id-set ::id-set))

(defn conform-lotame
  [raw]  
  (spec/conform ::record raw))


