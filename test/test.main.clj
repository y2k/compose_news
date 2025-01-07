(ns _ (:require ["../src/main" :as m]
                ["../vendor/cf-xmlparser/main" :as hrw]))

(defn- decorate_mock [log env world]
  (let [local_log (.toReversed log)]
    (reduce
     (fn [acc [fx_name fx_fn]]
       (assoc acc fx_name (fn [args]
                            (if (empty? local_log)
                              (fx_fn args)
                              (Promise.resolve (.pop local_log))))))
     world
     (Object.entries world))))

(defn- decorate_save_log [out_log world]
  (reduce
   (fn [acc [fx_name fx_fn]]
     (assoc acc fx_name (fn [args]
                          (swap! out_log (fn [xs] (conj xs {:effect fx_name :in args})))
                          (.then
                           (fx_fn args)
                           (fn [result]
                             (swap! out_log (fn [xs] (conj xs {:out result})))
                             result)))))
   world
   (Object.entries world)))

(defn- decorate_raw_fetch [world]
  (assoc world :fetch
         (fn [{url :url props :props}]
           (let [decoder (:decoder props)]
             (->
              ((:raw-fetch world) {:url url :props props})
              (.then (fn [response]
                       (if (and (some? decoder) (= (:type decoder) :htmlrewriter))
                         (hrw/parse response (:config decoder))
                         (Promise.resolve response)))))))))

(export-default
 {:fetch
  (fn [request env ctx]
    (->
     (.json request)
     (.then (fn [log]
              (let [out_log (atom [])]
                (->
                 ((m/main) (->>
                            (m/create_env env)
                            (decorate_mock log env)
                            (decorate_save_log out_log)
                            (decorate_raw_fetch)))
                 (.then (fn [] (Response. (JSON.stringify (deref out_log)))))
                 (.catch (fn [e] (Response. "" {:status 500 :statusText e})))))))))})
