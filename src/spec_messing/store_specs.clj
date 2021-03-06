(ns spec-messing.store-specs
  (:require [cognitect.transit :as transit]
            [clojure.spec :as spec])
  (:import [java.io ByteArrayInputStream
            ByteArrayOutputStream]
           [java.util.regex Pattern]))

(def spec-store (atom {}))

(defn tag [name]
  (str "kixi.some.sensible.name" name))

(def write-handlers
  {:handlers {Pattern (transit/write-handler (tag "Pattern") str)}})

(def read-handlers
  {:handlers {(tag "Pattern") (transit/read-handler #(Pattern/compile %))}})

(defn persist-spec
  [kw s]
  (swap! spec-store
         (fn [store]
           (let [out (ByteArrayOutputStream. 4096)
                 wr (transit/writer out :json write-handlers)
                 _ (transit/write wr s)]
             (update store
                     kw
                     (constantly (.toString out)))))))
(defn read-spec
  "Very basic. Doesn't support alias"
  [kw]
  (prn "R:" kw)
  (let [^String raw (get @spec-store kw)
        in (ByteArrayInputStream. (.getBytes raw))
        raw (transit/read
             (transit/reader in :json read-handlers))
        evald (eval raw)]
    (if (map? evald)
      (spec/cat-impl (keys evald)
                     (map read-spec (vals evald))
                     '())
      (spec/spec evald))))

(defn conform
  [spec x]
  (spec/conform 
   (read-spec spec)
   x))

(defn show-and-tell
  []
  (persist-spec :gcc-code 'clojure.core/integer?)
  (prn "Read: " (read-spec :gcc-code))
  (conform :gcc-code 1)
  )

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

(defn conform
  [raw]
  (persist-spec :id 'string?)
  (persist-spec :another-id 'string?)
  (persist-spec :id-set '(split-conform #"," conform-integer?))
  (persist-spec :record {:id :id
                         :another-id :another-id
                         :id-set :id-set})
  (conform :record raw))
